use std::{any::type_name, cell::RefCell, fmt, iter, mem, rc::Rc, slice};

use derive_where::derive_where;
use hashbrown::hash_map;
use thunderdome::{Arena, Index};

use crate::{
    archetype::{ArchetypeId, ArchetypeStore, ComponentId},
    late_struct::{LateField, late_field},
    utils::{FxHashMap, FxHashSet},
    world::{World, WorldDebug},
};

// === Format Reentrancy === //

#[derive(Debug, Default)]
struct FmtReentrancyState {
    depth: u32,
    entities: FxHashSet<Entity>,
    objects: FxHashSet<(ComponentId, Index)>,
}

thread_local! {
    static FMT_REENTRANCY: RefCell<Option<FmtReentrancyState>> = const { RefCell::new(None) };
}

pub(crate) fn guard_entity_reentrancy_checks() -> impl Sized {
    FMT_REENTRANCY.with(|v| {
        let mut v = v.borrow_mut();

        let v = v.get_or_insert_default();
        v.depth += 1;
    });

    scopeguard::guard((), |()| {
        FMT_REENTRANCY.with(|v| {
            let mut v = v.borrow_mut();
            let v_inner = v.as_mut().unwrap();
            v_inner.depth -= 1;

            if v_inner.depth == 0 {
                *v = None;
            }
        });
    })
}

pub(crate) fn can_format_entity(entity: Entity) -> bool {
    FMT_REENTRANCY.with(|v| {
        let mut v = v.borrow_mut();
        let v = v
            .as_mut()
            .expect("cannot call `can_format_entity` without a bound immutable world");

        v.entities.insert(entity)
    })
}

pub(crate) fn can_format_obj<T: Handle>(obj: T) -> bool {
    FMT_REENTRANCY.with(|v| {
        let mut v = v.borrow_mut();
        let v = v
            .as_mut()
            .expect("cannot call `can_format_obj` without a bound immutable world");

        v.objects
            .insert((ComponentId::of::<T::Component>(), obj.raw()))
    })
}

// === Component === //

pub type Obj<T> = <T as Component>::Handle;
pub type Val<T> = <T as Handle>::Component;

pub trait Component:
    'static + Sized + fmt::Debug + LateField<World, Value = Storage<Self>>
{
    type Handle: Handle<Component = Self>;
}

pub trait Handle: Sized + 'static + fmt::Debug + Copy + Eq + Ord {
    type Component: Component<Handle = Self>;

    const DANGLING: Self;

    fn wrap_raw(index: Index) -> Self;

    fn raw(self) -> Index;

    fn is_alive(self, w: &World) -> bool {
        w.resource::<Self::Component>().arena.contains(self.raw())
    }

    fn try_entity(self, w: &World) -> Option<Entity> {
        w.resource::<Self::Component>()
            .arena
            .get(self.raw())
            .map(|v| v.0)
    }

    fn entity(self, w: &World) -> Entity {
        w.resource::<Self::Component>().arena[self.raw()].0
    }

    fn try_get(self, w: &World) -> Option<&Self::Component> {
        w.resource::<Self::Component>()
            .arena
            .get(self.raw())
            .map(|v| &v.1)
    }

    fn try_get_mut(self, w: &mut World) -> Option<&mut Self::Component> {
        w.resource_mut::<Self::Component>()
            .arena
            .get_mut(self.raw())
            .map(|v| &mut v.1)
    }

    fn get(self, w: &World) -> &Self::Component {
        &w.resource::<Self::Component>().arena[self.raw()].1
    }

    fn get_mut(self, w: &mut World) -> &mut Self::Component {
        &mut w.resource_mut::<Self::Component>().arena[self.raw()].1
    }

    fn r(self, w: &World) -> &Self::Component {
        self.get(w)
    }

    fn m(self, w: &mut World) -> &mut Self::Component {
        self.get_mut(w)
    }

    fn debug(self, w: &World) -> WorldDebug<'_, Self> {
        w.debug(self)
    }
}

#[doc(hidden)]
pub mod component_internals {
    use super::can_format_obj;

    pub use {
        crate::{
            entity::{Component, Handle, Storage},
            late_struct::late_field,
            world::World,
        },
        paste::paste,
        std::fmt,
        thunderdome::Index,
    };

    pub fn format_obj<T: Handle>(f: &mut fmt::Formatter<'_>, handle: T) -> fmt::Result {
        let index = handle.raw().to_bits();

        World::try_fetch_tls_ref(|world| {
            if let Some(Ok(world)) = world.filter(|_| can_format_obj::<T>(handle)) {
                let storage = world.resource::<T::Component>();

                if let Some((_owner, value)) = storage.arena.get(handle.raw()) {
                    f.debug_tuple("Obj")
                        .field(&format_args!("0x{index:x}"))
                        .field(value)
                        .finish()
                } else {
                    struct Dead;
                    impl fmt::Debug for Dead {
                        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                            f.write_str("<dead>")
                        }
                    }
                    f.debug_tuple("Obj")
                        .field(&format_args!("0x{index:x}"))
                        .field(&Dead)
                        .finish()
                }
            } else {
                f.debug_tuple("Obj")
                    .field(&format_args!("0x{index:x}"))
                    .finish()
            }
        })
    }
}

#[macro_export]
macro_rules! component {
    ($($name:ident),*$(,)?) => {$(
        $crate::entity::component_internals::paste! {
            #[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
            pub struct [<Obj $name>] {
                pub raw: $crate::entity::component_internals::Index,
            }

            impl $crate::entity::component_internals::fmt::Debug for [<Obj $name>] {
                fn fmt(
                    &self,
                    f: &mut $crate::entity::component_internals::fmt::Formatter<'_>,
                ) -> $crate::entity::component_internals::fmt::Result {
                    $crate::entity::component_internals::format_obj(f, *self)
                }
            }

            impl [<Obj $name>] {
                pub const DANGLING: Self =
                    Self::wrap_raw($crate::entity::component_internals::Index::DANGLING);

                pub const fn wrap_raw(raw: $crate::entity::component_internals::Index) -> Self {
                    Self { raw }
                }

                pub const fn raw(self) -> $crate::entity::component_internals::Index {
                    self.raw
                }
            }

            impl $crate::entity::component_internals::Component for $name {
                type Handle = [<Obj $name>];
            }

            impl $crate::entity::component_internals::Handle for [<Obj $name>] {
                type Component = $name;

                const DANGLING: Self = Self::DANGLING;

                fn wrap_raw(index: $crate::entity::component_internals::Index) -> Self {
                    Self::wrap_raw(index)
                }

                fn raw(self) -> $crate::entity::component_internals::Index {
                    self.raw()
                }
            }

            $crate::entity::component_internals::late_field!(
                $name [$crate::entity::component_internals::World]
                    => $crate::entity::component_internals::Storage<$name>
            );
        }
    )*};
}

pub use component;

// === Entity === //

#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Entity(Index);

impl Entity {
    pub const DANGLING: Self = Self(Index::DANGLING);
}

impl fmt::Debug for Entity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        World::try_fetch_tls_ref(|world| {
            if let Some(Ok(world)) = world.filter(|_| can_format_entity(*self)) {
                let store = world.resource::<EntityStore>();

                let mut f = f.debug_struct("Entity");

                f.field("id", &format_args!("0x{:x}", self.0.to_bits()));

                if let Some(entity) = store.entities.get(self.0) {
                    let comps = store.archetypes.components(entity.archetype);

                    for comp in comps {
                        (comp.debug_fmt)(world, *self, &mut f);
                    }

                    f.field("children", &entity.children.vec);
                } else {
                    f.field("is_alive", &false);
                }

                f.finish()
            } else {
                f.debug_struct("Entity")
                    .field("id", &format_args!("0x{:x}", self.0.to_bits()))
                    .finish()
            }
        })
    }
}

#[derive(Clone)]
pub struct EntityChildren {
    vec: Rc<Vec<Entity>>,
}

impl fmt::Debug for EntityChildren {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.vec.iter()).finish()
    }
}

impl EntityChildren {
    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn is_empty(&self) -> bool {
        self.vec.is_empty()
    }

    fn mutate(&mut self) -> &mut Vec<Entity> {
        Rc::get_mut(&mut self.vec)
            .expect("cannot modify the children of an entity while they're being iterated")
    }
}

impl<'a> IntoIterator for &'a EntityChildren {
    type Item = Entity;
    type IntoIter = iter::Copied<slice::Iter<'a, Entity>>;

    fn into_iter(self) -> Self::IntoIter {
        self.vec.iter().copied()
    }
}

// === EntityStore === //

#[derive(Debug)]
#[derive_where(Default)]
pub struct Storage<T> {
    pub arena: Arena<(Entity, T)>,
    pub entity_map: FxHashMap<Entity, Index>,
}

#[derive(Debug)]
pub struct EntityStore {
    /// The root entity of the game. This is the only entity not to have a parent.
    root: Entity,

    /// An arena of all live entities.
    entities: Arena<EntityInfo>,

    /// Tracks all component archetypes in use.
    archetypes: ArchetypeStore,

    /// A snapshot of members of each archetype since the last `flush`.
    ///
    /// This `Rc` is cloned while the storage is being iterated and is otherwise exclusive.
    query_state: Rc<EntityQueryState>,

    /// Maps entities that have been reshaped to their original archetype. Destroyed entities are
    /// included in the `dead_entities` map instead.
    ///
    /// This is only used to patch up `archetype_members` during a `flush`.
    reshaped_entities: FxHashMap<Entity, ArchetypeId>,

    /// The set of original archetypes and position therein of entities destroyed through
    /// `destroy_now`. Entities in the empty archetype are not included.
    ///
    /// This is only used to patch up `archetype_members` during a `flush`.
    dead_entities: FxHashSet<(ArchetypeId, u32)>,

    // TODO: Document
    target_queue_state: EntityQueueState,

    // TODO: Document
    view_queue_state: Rc<EntityQueueState>,
}

#[derive(Debug)]
struct EntityInfo {
    /// The archetype describing the set of components the entity actively owns. This accounts for
    /// changes by `add` and `remove_now` but not queued operations such as `remove`, which defers
    /// its call to `remove_now` until before reshapes are applied.
    archetype: ArchetypeId,

    /// The index of the entity in its pre-`flush` archetype.
    index_in_archetype: u32,

    /// The index of the node in its parent's `children` vector.
    ///
    /// The MSB indicates whether the entity was condemned.
    index_in_parent_bitor_condemned: u32,

    /// The parent of the entity. This is guaranteed to be alive.
    parent: Option<Entity>,

    /// The entity's set of children. These are guaranteed to be alive.
    children: EntityChildren,
}

impl EntityInfo {
    fn index_in_parent(&self) -> u32 {
        self.index_in_parent_bitor_condemned & !(1 << 31)
    }

    fn condemned(&self) -> bool {
        self.index_in_parent_bitor_condemned >> 31 != 0
    }

    fn set_index_in_parent_unchecked(&mut self, idx: u32) {
        self.index_in_parent_bitor_condemned &= 1 << 31;
        self.index_in_parent_bitor_condemned |= idx;
    }

    fn set_index_in_parent(&mut self, idx: usize) {
        assert!(idx < (1u32 << 31) as usize, "node has too many children");
        self.set_index_in_parent_unchecked(idx as u32);
    }

    fn mark_condemned(&mut self) {
        self.index_in_parent_bitor_condemned |= 1 << 31;
    }
}

#[derive(Debug, Default)]
pub struct EntityQueryState {
    pub index_members: FxHashMap<ArchetypeId, Vec<Entity>>,
    pub comp_members: FxHashMap<(ArchetypeId, ComponentId), Vec<Index>>,
}

#[derive(Debug, Default)]
pub struct EntityQueueState {
    /// The set of entities which have been condemned to be destroyed.
    pub condemned: Vec<Entity>,

    /// The set of components condemned to be removed.
    ///
    /// The second vector is filled during queue normalization.
    pub to_remove: FxHashMap<ComponentId, (FxHashSet<Entity>, Vec<Index>)>,
}

impl EntityQueueState {
    pub fn is_empty(&self) -> bool {
        self.condemned.is_empty()
    }
}

impl Default for EntityStore {
    fn default() -> Self {
        let mut store = Self {
            root: Entity::DANGLING,
            entities: Arena::new(),
            archetypes: ArchetypeStore::new(),
            query_state: Rc::default(),
            reshaped_entities: FxHashMap::default(),
            dead_entities: FxHashSet::default(),
            target_queue_state: EntityQueueState::default(),
            view_queue_state: Rc::default(),
        };

        store.root = store.spawn_orphan();
        store
    }
}

late_field!(EntityStore [World] => EntityStore);

impl EntityStore {
    fn spawn_orphan(&mut self) -> Entity {
        let index = self.entities.insert(EntityInfo {
            archetype: ArchetypeId::EMPTY,
            index_in_parent_bitor_condemned: 0,
            index_in_archetype: 0,
            parent: None,
            children: EntityChildren { vec: Rc::default() },
        });

        Entity(index)
    }
}

impl Entity {
    pub fn new_orphan(w: &mut World) -> Self {
        w.resource_mut::<EntityStore>().spawn_orphan()
    }

    pub fn new(parent: Entity, w: &mut World) -> Self {
        let node = Self::new_orphan(w);
        node.set_parent(Some(parent), w);
        node
    }

    pub fn root(w: &World) -> Self {
        w.resource::<EntityStore>().root
    }

    pub fn service<T: Handle>(w: &World) -> T {
        Self::root(w).get(w)
    }

    pub fn archetype(self, w: &World) -> ArchetypeId {
        w.resource::<EntityStore>().entities[self.0].archetype
    }

    pub fn is_alive(self, w: &World) -> bool {
        w.resource::<EntityStore>().entities.contains(self.0)
    }

    pub fn parent(self, w: &World) -> Option<Self> {
        w.resource::<EntityStore>().entities[self.0].parent
    }

    pub fn children(self, w: &World) -> EntityChildren {
        w.resource::<EntityStore>().entities[self.0]
            .children
            .clone()
    }

    pub fn set_parent(self, parent: Option<Entity>, w: &mut World) {
        let store = w.resource_mut::<EntityStore>();

        // Remove from old parent
        let me = &mut store.entities[self.0];
        let old_parent = me.parent.take();
        let old_index = me.index_in_parent();

        if let Some(parent) = old_parent {
            let parent = &mut store.entities[parent.0];

            parent.children.mutate().swap_remove(old_index as usize);
            if let Some(&moved) = parent.children.vec.get(old_index as usize) {
                store.entities[moved.0].set_index_in_parent_unchecked(old_index);
            }
        }

        // Add to new parent
        if let Some(parent) = parent {
            let (me, parent_val) = store.entities.get2_mut(self.0, parent.0);
            let me = me.unwrap();
            let parent_val = parent_val.unwrap();

            me.parent = Some(parent);
            me.set_index_in_parent(parent_val.children.len());

            parent_val.children.mutate().push(self);
        }
    }

    pub fn add<T: Component>(self, value: T, w: &mut World) -> T::Handle {
        let (store, storage) = w.resources_two::<EntityStore, T>();

        // Ensure that the entity is alive
        let entity = &mut store
            .entities
            .get_mut(self.0)
            .unwrap_or_else(|| panic!("{self:?} is not alive"));

        assert!(!entity.condemned(), "{self:?} is condemned");

        // See if we can update the existing component in-place.
        let entry = match storage.entity_map.entry(self) {
            hash_map::Entry::Occupied(entry) => {
                let handle = *entry.get();
                storage.arena[handle].1 = value;
                return <T::Handle>::wrap_raw(handle);
            }
            hash_map::Entry::Vacant(entry) => entry,
        };

        // Otherwise, create the new `Obj`...
        let handle = storage.arena.insert((self, value));
        entry.insert(handle);

        // ...and update the `EntityStore` to reflect the additional component.
        Self::mark_shape_dirty_before_update(&mut store.reshaped_entities, self, entity);

        entity.archetype = store
            .archetypes
            .lookup_extend(entity.archetype, ComponentId::of::<T>());

        <T::Handle>::wrap_raw(handle)
    }

    pub fn with<T: Component>(self, value: T, w: &mut World) -> Self {
        self.add(value, w);
        self
    }

    fn mark_shape_dirty_before_update(
        reshaped_entities: &mut FxHashMap<Entity, ArchetypeId>,
        entity: Entity,
        entity_info: &EntityInfo,
    ) {
        reshaped_entities
            .entry(entity)
            .or_insert(entity_info.archetype);
    }

    pub fn try_get<T: Handle>(self, w: &World) -> Option<T> {
        w.resource::<T::Component>()
            .entity_map
            .get(&self)
            .copied()
            .map(T::wrap_raw)
    }

    pub fn get<T: Handle>(self, w: &World) -> T {
        self.try_get::<T>(w).unwrap_or_else(|| {
            panic!(
                "{self:?} does not have component of type `{}`",
                type_name::<T::Component>()
            )
        })
    }

    pub fn try_get_deep<T: Handle>(self, w: &World) -> Option<T> {
        let mut iter = Some(self);

        while let Some(curr) = iter {
            if let Some(obj) = curr.try_get::<T>(w) {
                return Some(obj);
            }

            iter = self.parent(w);
        }

        None
    }

    pub fn get_deep<T: Handle>(self, w: &World) -> T {
        self.try_get_deep::<T>(w).unwrap_or_else(|| {
            panic!(
                "{self:?} and its ancestry do not have component of type `{}`",
                type_name::<T>(),
            )
        })
    }

    pub fn remove<T: Component>(self, w: &mut World) {
        w.resource_mut::<EntityStore>()
            .target_queue_state
            .to_remove
            .entry(ComponentId::of::<T>())
            .or_default()
            .0
            .insert(self);
    }

    pub fn remove_now<T: Component>(self, w: &mut World) -> Option<T> {
        let store = w.resource_mut::<EntityStore>();

        let entity = store.entities.get_mut(self.0)?;

        Self::mark_shape_dirty_before_update(&mut store.reshaped_entities, self, entity);

        entity.archetype = store
            .archetypes
            .lookup_remove(entity.archetype, ComponentId::of::<T>());

        self.remove_from_storage(w)
    }

    pub(crate) fn remove_from_storage<T: Component>(self, w: &mut World) -> Option<T> {
        let storage = w.resource_mut::<T>();

        let obj = storage.entity_map.remove(&self)?;

        Some(storage.arena.remove(obj).unwrap().1)
    }

    pub fn destroy(self, w: &mut World) {
        let store = w.resource_mut::<EntityStore>();
        let entity = &mut store.entities[self.0];

        if entity.condemned() {
            return;
        }

        entity.mark_condemned();
        store.target_queue_state.condemned.push(self);
    }

    pub fn destroy_now(self, w: &mut World) {
        let store = w.resource_mut::<EntityStore>();

        // Destroy entity information before calling destructors to avoid reentrant operations on
        // dying entities.
        let Some(entity) = store.entities.remove(self.0) else {
            return;
        };

        // Remove ourself from our parent
        if let Some(parent) = entity
            .parent
            .and_then(|parent| store.entities.get_mut(parent.0))
        {
            let children = parent.children.mutate();
            let index_in_parent = entity.index_in_parent();
            children.swap_remove(index_in_parent as usize);

            if let Some(&moved) = children.get(index_in_parent as usize) {
                store.entities[moved.0].set_index_in_parent_unchecked(index_in_parent);
            }
        }

        // Remove from the reshaped map and into the dead map.
        let old_archetype = store
            .reshaped_entities
            .remove(&self)
            .unwrap_or(entity.archetype);

        if old_archetype != ArchetypeId::EMPTY {
            store
                .dead_entities
                .insert((entity.archetype, entity.index_in_archetype));
        }

        // Destroy all the components.
        let arch = entity.archetype;
        let arch_len = store.archetypes.components(arch).len();

        for i in 0..arch_len {
            let comp = w.resource_mut::<EntityStore>().archetypes.components(arch)[i];

            (comp.remove_no_tracking)(w, self);
        }

        // Destroy all the children.
        for child in &entity.children {
            child.destroy_now(w);
        }
    }

    pub fn debug(self, w: &World) -> WorldDebug<'_, Self> {
        w.debug(self)
    }
}

impl World {
    pub fn archetypes(&self) -> &ArchetypeStore {
        &self.resource::<EntityStore>().archetypes
    }

    pub fn query_state(&self) -> &Rc<EntityQueryState> {
        &self.resource::<EntityStore>().query_state
    }

    pub fn queue_state(&self) -> &Rc<EntityQueueState> {
        &self.resource::<EntityStore>().view_queue_state
    }

    pub fn flush(&mut self, mut f: impl FnMut(&mut World)) {
        // Process queued operations
        loop {
            // See if there are any operations remaining.
            let store = self.resource_mut::<EntityStore>();
            let mut queue = mem::take(&mut store.target_queue_state);
            if queue.is_empty() {
                break;
            }

            // Normalize queue
            let orig_condemned_len = queue.condemned.len();

            for i in 0.. {
                let Some(condemned) = queue.condemned.get(i) else {
                    break;
                };

                let Some(info) = store.entities.get(condemned.0) else {
                    continue;
                };

                for child in &info.children {
                    queue.condemned.push(child);
                }
            }

            for &condemned in &queue.condemned {
                let Some(info) = store.entities.get(condemned.0) else {
                    continue;
                };

                for &comp in store.archetypes.components(info.archetype) {
                    queue.to_remove.entry(comp).or_default().0.insert(condemned);
                }
            }

            for (comp_id, (entities, list)) in queue.to_remove.iter_mut() {
                (comp_id.populate_indices)(self, entities, list);
            }

            // Freeze queue
            let queue = Rc::new(queue);
            self.resource_mut::<EntityStore>().view_queue_state = queue.clone();

            // Run handler
            f(self);

            // Kill condemned entities
            // We don't need to delete the implicit children we added to the condemnation queue
            // because they'll be deleted by `destroy_now`'s recursive logic.
            for &condemned in &queue.condemned[..orig_condemned_len] {
                // Potential double-free on children is OK because this ignores deletions of
                // entities that have already been destroyed.
                condemned.destroy_now(self);
            }

            // Remove components
            for (comp, (deleted, _indices)) in &queue.to_remove {
                (comp.remove_for_deferred)(self, deleted);
            }
        }

        // Process reshape requests.
        let w = self.raw.dynamic();
        let mut store = w.borrow_mut::<EntityStore>();
        let store = &mut *store;
        let archetype_members = Rc::get_mut(&mut store.query_state)
            .expect("cannot `flush` the world while it is still being iterated over");

        // Begin with entity destruction since we don't want to try to move the indices of dead
        // entities as we update the archetypes.
        for (arch, idx) in store.dead_entities.drain() {
            let index_members = archetype_members.index_members.get_mut(&arch).unwrap();
            let comp_members = store.archetypes.components(arch);

            // We're going to be swap-removing a bunch of entities out of archetypes and we really
            // don't want to update the indices of dead entities we moved into the middle of the
            // archetype so let's trim all dead entities at the end of the archetype.
            while index_members
                .last()
                .is_some_and(|&entity| !store.entities.contains(entity.0))
            {
                index_members.pop();

                for &comp in comp_members {
                    archetype_members
                        .comp_members
                        .get_mut(&(arch, comp))
                        .unwrap()
                        .pop();
                }
            }

            // If the index is out of bound, we know that end-trimming took care of the entity
            // already.
            if idx as usize >= index_members.len() {
                continue;
            }

            // Otherwise, we need to swap-remove the entity...
            index_members.swap_remove(idx as usize);

            for &comp in comp_members {
                archetype_members
                    .comp_members
                    .get_mut(&(arch, comp))
                    .unwrap()
                    .swap_remove(idx as usize);
            }

            // ...and patch the index of the moved entity.
            if let Some(&moved) = index_members.get(idx as usize) {
                store.entities[moved.0].index_in_archetype = idx;
            }
        }

        // Now, we can handle move requests involving entirely live entities.
        for (entity, old_arch) in store.reshaped_entities.drain() {
            let own_info = &store.entities[entity.0];
            let curr_arch = own_info.archetype;
            let old_idx = own_info.index_in_archetype;

            // Skip over entities which haven't actually changed archetype.
            if curr_arch == old_arch {
                continue;
            }

            // Remove from the old archetype.
            if old_arch != ArchetypeId::EMPTY {
                let index_members = archetype_members.index_members.get_mut(&old_arch).unwrap();
                let comp_members = store.archetypes.components(old_arch);

                index_members.swap_remove(old_idx as usize);

                for &comp in comp_members {
                    archetype_members
                        .comp_members
                        .get_mut(&(old_arch, comp))
                        .unwrap()
                        .swap_remove(old_idx as usize);
                }

                // ...and patch the index of the moved entity.
                if let Some(&moved) = index_members.get(old_idx as usize) {
                    store.entities[moved.0].index_in_archetype = old_idx;
                }
            }

            // Move into the new archetype.
            if curr_arch != ArchetypeId::EMPTY {
                let comp_members = store.archetypes.components(curr_arch);
                let index_members = archetype_members
                    .index_members
                    .entry(curr_arch)
                    .or_insert_with(|| {
                        for &comp in comp_members {
                            archetype_members
                                .comp_members
                                .insert((curr_arch, comp), Vec::new());
                        }

                        Vec::new()
                    });

                // Move into the entity index
                store.entities[entity.0].index_in_archetype = u32::try_from(index_members.len())
                    .unwrap_or_else(|_| panic!("too many entities in archetype {curr_arch:?}"));

                index_members.push(entity);

                // ...and attach their components.
                let comp_members = store.archetypes.components(curr_arch);

                for &comp in comp_members {
                    archetype_members
                        .comp_members
                        .get_mut(&(curr_arch, comp))
                        .unwrap()
                        .push((comp.fetch_idx)(w, entity));
                }
            }
        }
    }
}
