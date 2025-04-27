use std::{fmt, marker::PhantomData, mem::transmute, ptr::null, rc::Rc, slice, vec};

use thunderdome::Index;

use crate::{
    archetype::{ArchetypeId, ComponentId},
    entity::{Entity, EntityQueryState, EntityQueueState, Handle},
    world::World,
};

// === Query === //

#[derive(Debug)]
pub struct Query<R: QueryResult> {
    query_state: Rc<EntityQueryState>,
    archetypes: vec::IntoIter<ArchetypeId>,
    state: R::State,
    leader: R::Leader,
}

impl<R: QueryResult> Query<R> {
    pub fn new(w: &World) -> Self {
        let query_state = w.query_state().clone();
        let archetypes = w
            .archetypes()
            .archetypes_with_set(R::comp_ids())
            .into_iter();

        Self {
            query_state,
            archetypes,
            state: R::empty_state(),
            leader: R::empty_leader(),
        }
    }

    pub fn new_with(w: &World, additional: impl IntoIterator<Item = ComponentId>) -> Self {
        let query_state = w.query_state().clone();
        let archetypes = w
            .archetypes()
            .archetypes_with_set(R::comp_ids().into_iter().chain(additional))
            .into_iter();

        Self {
            query_state,
            archetypes,
            state: R::empty_state(),
            leader: R::empty_leader(),
        }
    }
}

impl<R: QueryResult> Iterator for Query<R> {
    type Item = R;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if R::state_has_next(&self.state, &self.leader) {
                break Some(unsafe { R::next_unchecked_leader(&mut self.state, &mut self.leader) });
            }

            let archetype = self.archetypes.next()?;

            R::update_state_leader(
                &self.query_state,
                archetype,
                &mut self.state,
                &mut self.leader,
            );
        }
    }
}

// === QueryResult === //

pub trait QueryResult: Sized {
    type State;
    type Leader;

    fn comp_ids() -> impl IntoIterator<Item = ComponentId>;

    fn empty_state() -> Self::State;

    fn empty_leader() -> Self::Leader;

    fn update_state_leader(
        query_state: &EntityQueryState,
        archetype: ArchetypeId,
        state: &mut Self::State,
        leader: &mut Self::Leader,
    ) -> bool;

    fn update_state_follower(
        query_state: &EntityQueryState,
        archetype: ArchetypeId,
        state: &mut Self::State,
    );

    fn state_has_next(state: &Self::State, leader: &Self::Leader) -> bool;

    unsafe fn next_unchecked_leader(state: &mut Self::State, leader: &mut Self::Leader) -> Self;

    unsafe fn next_unchecked_follower(state: &mut Self::State) -> Self;
}

impl QueryResult for Entity {
    type State = *const Entity; // (finger)
    type Leader = *const Entity; // (last element)

    fn comp_ids() -> impl IntoIterator<Item = ComponentId> {
        []
    }

    fn empty_state() -> Self::State {
        null()
    }

    fn empty_leader() -> Self::Leader {
        null()
    }

    fn update_state_leader(
        query_state: &EntityQueryState,
        archetype: ArchetypeId,
        state: &mut Self::State,
        leader: &mut Self::Leader,
    ) -> bool {
        let Some(members) = &query_state.index_members.get(&archetype) else {
            *state = null();
            *leader = null();
            return false;
        };

        *state = members.as_ptr();
        *leader = unsafe { members.as_ptr().add(members.len()) };

        true
    }

    fn update_state_follower(
        query_state: &EntityQueryState,
        archetype: ArchetypeId,
        state: &mut Self::State,
    ) {
        *state = query_state.index_members[&archetype].as_ptr();
    }

    fn state_has_next(state: &Self::State, leader: &Self::Leader) -> bool {
        *state != *leader
    }

    unsafe fn next_unchecked_leader(state: &mut Self::State, _leader: &mut Self::Leader) -> Self {
        unsafe { Self::next_unchecked_follower(state) }
    }

    unsafe fn next_unchecked_follower(state: &mut Self::State) -> Self {
        unsafe {
            let curr = **state;
            *state = state.add(1);
            curr
        }
    }
}

impl<T: Handle> QueryResult for T {
    type State = *const Index; // (finger)
    type Leader = *const Index; // (last element)

    fn comp_ids() -> impl IntoIterator<Item = ComponentId> {
        [ComponentId::of::<T::Component>()]
    }

    fn empty_state() -> Self::State {
        null()
    }

    fn empty_leader() -> Self::Leader {
        null()
    }

    fn update_state_leader(
        query_state: &EntityQueryState,
        archetype: ArchetypeId,
        state: &mut Self::State,
        leader: &mut Self::Leader,
    ) -> bool {
        let Some(members) = &query_state
            .comp_members
            .get(&(archetype, ComponentId::of::<T::Component>()))
        else {
            *state = null();
            *leader = null();
            return false;
        };

        *state = members.as_ptr();
        *leader = unsafe { members.as_ptr().add(members.len()) };
        true
    }

    fn update_state_follower(
        query_state: &EntityQueryState,
        archetype: ArchetypeId,
        state: &mut Self::State,
    ) {
        *state = query_state.comp_members[&(archetype, ComponentId::of::<T::Component>())].as_ptr();
    }

    fn state_has_next(state: &Self::State, leader: &Self::Leader) -> bool {
        *state != *leader
    }

    unsafe fn next_unchecked_leader(state: &mut Self::State, _leader: &mut Self::Leader) -> Self {
        unsafe { Self::next_unchecked_follower(state) }
    }

    unsafe fn next_unchecked_follower(state: &mut Self::State) -> Self {
        unsafe {
            let curr = **state;
            *state = state.add(1);
            T::wrap_raw(curr)
        }
    }
}

macro_rules! impl_tuples {
	// Internal
	(
		$target:path : []
		$(| [
			$({$($pre:tt)*})*
		])?
	) => { /* terminal recursion case */ };
	(
		$target:path : [
			{$($next:tt)*}
			// Remaining invocations
			$($rest:tt)*
		] $(| [
			// Accumulated arguments
			$({$($pre:tt)*})*
		])?
	) => {
		$target!(
			$($($($pre)*,)*)?
			$($next)*
		);
		impl_tuples!(
			$target : [
				$($rest)*
			] | [
				$($({$($pre)*})*)?
				{$($next)*}
			]
		);
	};

	// Public
	($target:path; no_unit) => {
		impl_tuples!(
			$target : [
				{A: 0}
				{B: 1}
				{C: 2}
				{D: 3}
				{E: 4}
				{F: 5}
				{G: 6}
				{H: 7}
				{I: 8}
				{J: 9}
				{K: 10}
				{L: 11}
			]
		);
	};
	($target:path; only_full) => {
		$target!(
			A:0,
			B:1,
			C:2,
			D:3,
			E:4,
			F:5,
			G:6,
			H:7,
			I:8,
			J:9,
			K:10,
			L:11
		);
	};
	($target:path) => {
		$target!();
		impl_tuples!($target; no_unit);
	};
}

macro_rules! impl_tup_query_result {
    ($leader:ident:$ignored:tt $(, $para:ident:$field:tt)*) => {
        impl<$leader: QueryResult $(, $para: QueryResult)*> QueryResult for ($leader, $($para,)*) {
            type State = ($leader::State, $($para::State,)*);
            type Leader = $leader::Leader;

            fn comp_ids() -> impl IntoIterator<Item = ComponentId> {
                let iter = $leader::comp_ids().into_iter();
                $( let iter = iter.chain($para::comp_ids()); )*
                iter
            }

            fn empty_state() -> Self::State {
                ($leader::empty_state(), $($para::empty_state(),)*)
            }

            fn empty_leader() -> Self::Leader {
                $leader::empty_leader()
            }

            fn update_state_leader(
                query_state: &EntityQueryState,
                archetype: ArchetypeId,
                state: &mut Self::State,
                leader: &mut Self::Leader,
            ) -> bool {
                if !$leader::update_state_leader(query_state, archetype, &mut state.0, leader) {
                    return false;
                }

                $( $para::update_state_follower(query_state, archetype, &mut state.$field); )*

                true
            }

            fn update_state_follower(
                query_state: &EntityQueryState,
                archetype: ArchetypeId,
                state: &mut Self::State,
            ) {
                $leader::update_state_follower(query_state, archetype, &mut state.0);
                $( $para::update_state_follower(query_state, archetype, &mut state.$field); )*
            }

            fn state_has_next(state: &Self::State, leader: &Self::Leader) -> bool {
                $leader::state_has_next(&state.0, leader)
            }

            unsafe fn next_unchecked_leader(state: &mut Self::State, leader: &mut Self::Leader) -> Self {
                unsafe {
                    (
                        $leader::next_unchecked_leader(&mut state.0, leader),
                        $($para::next_unchecked_follower(&mut state.$field), )*
                    )
                }
            }

            unsafe fn next_unchecked_follower(state: &mut Self::State) -> Self {
                unsafe {
                    (
                        $leader::next_unchecked_follower(&mut state.0),
                        $($para::next_unchecked_follower(&mut state.$field), )*
                    )
                }
            }
        }
    };
}

impl_tuples!(impl_tup_query_result; no_unit);

// === ComponentsRemoved === //

pub fn query_removed<T: Handle>(w: &World) -> ComponentsRemoved<T> {
    ComponentsRemoved::new(w)
}

#[derive(Clone)]
pub struct ComponentsRemoved<T: Handle> {
    _ty: PhantomData<fn(T) -> T>,
    iter: slice::Iter<'static, Index>,
    _queue: Rc<EntityQueueState>,
}

impl<T: Handle> fmt::Debug for ComponentsRemoved<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ComponentsRemoved").finish_non_exhaustive()
    }
}

impl<T: Handle> ComponentsRemoved<T> {
    fn empty_iter() -> slice::Iter<'static, Index> {
        [].iter()
    }

    pub fn new(w: &World) -> Self {
        let queue = w.queue_state().clone();
        let iter = queue
            .to_remove
            .get(&ComponentId::of::<T::Component>())
            .map_or(Self::empty_iter(), |(_set, vec)| vec.iter());

        Self {
            _ty: PhantomData,
            iter: unsafe { transmute::<slice::Iter<'_, Index>, slice::Iter<'static, Index>>(iter) },
            _queue: queue,
        }
    }
}

impl<T: Handle> Iterator for ComponentsRemoved<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().copied().map(T::wrap_raw)
    }
}
