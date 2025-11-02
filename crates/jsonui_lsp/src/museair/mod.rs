pub type BfastHashSet<T> = std::collections::HashSet<T, museair::impls::FixedState<true>>;
pub type BfastDashSet<T> = dashmap::DashSet<T, museair::impls::FixedState<true>>;

pub type BfastHashMap<K, V> = std::collections::HashMap<K, V, museair::impls::FixedState<true>>;
pub type BfastDashMap<K, V> = dashmap::DashMap<K, V, museair::impls::FixedState<true>>;
pub type BfastMultiMap<K, V> = multimap::MultiMap<K, V, museair::impls::FixedState<true>>;