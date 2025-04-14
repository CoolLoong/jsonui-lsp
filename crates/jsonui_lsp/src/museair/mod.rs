use museair::BaseHasher;

pub type BfastHashSet<T> = std::collections::HashSet<T, BfastHash<true>>;
pub type BfastDashSet<T> = dashmap::DashSet<T, BfastHash<true>>;

pub type BfastHashMap<K, V> = std::collections::HashMap<K, V, BfastHash<true>>;
pub type BfastDashMap<K, V> = dashmap::DashMap<K, V, BfastHash<true>>;
pub type BfastMultiMap<K, V> = multimap::MultiMap<K, V, BfastHash<true>>;

#[derive(Clone, Default)]
pub(crate) struct BfastHash<const BFAST: bool>();
impl<const BFAST: bool> BfastHash<BFAST> {
    pub fn new() -> Self {
        BfastHash::<BFAST> {}
    }
}
impl<const BFAST: bool> core::hash::BuildHasher for BfastHash<BFAST> {
    type Hasher = BaseHasher<BFAST>;

    fn build_hasher(&self) -> Self::Hasher {
        BaseHasher::<BFAST>::new()
    }
}
