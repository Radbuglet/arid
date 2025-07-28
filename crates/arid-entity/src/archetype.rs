use std::{hash::Hash, ops::Range, rc::Rc};

use hashbrown::hash_map::RawEntryMut;
use index_vec::{IndexVec, define_index_type};

use crate::{
    ComponentId,
    utils::{FxBuildHasher, FxHashMap, FxHashSet, IterHashExt, MergeIter, RemoveIter},
};

// === ArchetypeId === //

define_index_type! {
    pub struct ArchetypeId = usize;
}

impl ArchetypeId {
    pub const EMPTY: Self = Self { _raw: 0 };
}

// === ArchetypeStore === //

#[derive(Debug)]
pub struct ArchetypeStore {
    arena: IndexVec<ArchetypeId, ArchetypeData>,
    comp_buf: Vec<ComponentId>,
    map: FxHashMap<ArchetypeKey, ArchetypeId>,
    comp_arches: FxHashMap<ComponentId, Vec<ArchetypeId>>,
}

#[derive(Debug)]
struct ArchetypeKey {
    hash: u64,
    comps: Range<usize>,
}

#[derive(Debug)]
struct ArchetypeData {
    comps: Range<usize>,
    comp_map: Rc<FxHashSet<ComponentId>>,
    pos: FxHashMap<ComponentId, ArchetypeId>,
    neg: FxHashMap<ComponentId, ArchetypeId>,
}

impl Default for ArchetypeStore {
    fn default() -> Self {
        Self::new()
    }
}

impl ArchetypeStore {
    pub fn new() -> Self {
        let mut arena = IndexVec::new();
        arena.push(ArchetypeData {
            comps: 0..0,
            comp_map: Rc::default(),
            pos: FxHashMap::default(),
            neg: FxHashMap::default(),
        });

        let mut map = FxHashMap::default();
        let hash = FxBuildHasher::new().hash_one_iter([] as [ComponentId; 0]);

        let RawEntryMut::Vacant(entry) = map.raw_entry_mut().from_hash(hash, |_| unreachable!())
        else {
            unreachable!();
        };

        entry.insert_with_hasher(
            hash,
            ArchetypeKey { hash, comps: 0..0 },
            ArchetypeId::EMPTY,
            |_| unreachable!(),
        );

        Self {
            arena,
            comp_buf: Vec::new(),
            map,
            comp_arches: FxHashMap::default(),
        }
    }

    fn lookup<M: LookupMode>(&mut self, base: ArchetypeId, with: ComponentId) -> ArchetypeId {
        let base_data = &self.arena[base];

        // Attempt to find a cached extension/de-extension of an existing archetype.
        if let Some(&shortcut) = ternary(M::POLARITY, &base_data.pos, &base_data.neg).get(&with) {
            return shortcut;
        }

        // Determine the set of components for which we're looking.
        let comps = M::comps(&self.comp_buf[base_data.comps.clone()], with);
        let hash = FxBuildHasher::new().hash_one_iter(comps.clone());

        let entry = self.map.raw_entry_mut().from_hash(hash, |other| {
            if hash != other.hash {
                return false;
            }

            self.comp_buf[other.comps.clone()]
                .iter()
                .copied()
                .eq(comps.clone())
        });

        // If it exists already, use it!
        let entry = match entry {
            RawEntryMut::Occupied(entry) => {
                let target = *entry.get();

                let base_data = &mut self.arena[base];
                ternary(M::POLARITY, &mut base_data.pos, &mut base_data.neg).insert(with, target);

                return target;
            }
            RawEntryMut::Vacant(entry) => entry,
        };

        // Otherwise, we have to create an entirely new archetype.

        // Create the `comps` range.
        let range_start = self.comp_buf.len();
        let comps_vec = comps.collect::<Vec<_>>();
        self.comp_buf.extend(comps_vec.iter().copied());
        let comps = range_start..self.comp_buf.len();

        // Create the `new` archetype with an appropriate back-ref to its original archetype.
        let mut new_data = ArchetypeData {
            comps: comps.clone(),
            comp_map: Rc::new(comps_vec.iter().copied().collect()),
            pos: FxHashMap::default(),
            neg: FxHashMap::default(),
        };

        ternary(!M::POLARITY, &mut new_data.pos, &mut new_data.neg).insert(with, base);

        let new = self.arena.push(new_data);

        // Update `base` to contain a shortcut to this new archetype.
        let base_data = &mut self.arena[base];
        ternary(M::POLARITY, &mut base_data.pos, &mut base_data.neg).insert(with, new);

        // Update the `map`.
        entry.insert_with_hasher(hash, ArchetypeKey { hash, comps }, new, |entry| entry.hash);

        // Update `comp_arches`.
        for &comp in &comps_vec {
            self.comp_arches.entry(comp).or_default().push(new);
        }

        new
    }

    pub fn lookup_extend(&mut self, base: ArchetypeId, with: ComponentId) -> ArchetypeId {
        self.lookup::<ExtendLookupMode>(base, with)
    }

    pub fn lookup_remove(&mut self, base: ArchetypeId, without: ComponentId) -> ArchetypeId {
        self.lookup::<RemoveLookupMode>(base, without)
    }

    pub fn component_set(&self, id: ArchetypeId) -> &Rc<FxHashSet<ComponentId>> {
        &self.arena[id].comp_map
    }
}

trait LookupMode {
    const POLARITY: bool;

    fn comps(base: &[ComponentId], with: ComponentId) -> impl Iterator<Item = ComponentId> + Clone;
}

struct ExtendLookupMode;

impl LookupMode for ExtendLookupMode {
    const POLARITY: bool = true;

    fn comps(base: &[ComponentId], with: ComponentId) -> impl Iterator<Item = ComponentId> + Clone {
        MergeIter::new(base.iter().copied(), [with])
    }
}

struct RemoveLookupMode;

impl LookupMode for RemoveLookupMode {
    const POLARITY: bool = true;

    fn comps(base: &[ComponentId], with: ComponentId) -> impl Iterator<Item = ComponentId> + Clone {
        RemoveIter::new(base.iter().copied(), [with])
    }
}

fn ternary<T>(cond: bool, pos: T, neg: T) -> T {
    if cond { pos } else { neg }
}
