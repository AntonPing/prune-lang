use std::borrow::Borrow;
use std::collections::{HashMap, hash_map};
use std::fmt::Debug;
use std::hash::Hash;

/// `EnvOpr<K,V>` records all the operation have done.
/// When the `EnvMap` do [`EnvMap::insert`] or [`EnvMap::remove`], an `EnvOpr` is created

#[derive(Clone, Debug, PartialEq)]
enum EnvOpr<K, V> {
    /// When doing [`EnvMap::insert`]
    /// If has such key in environment, the old value is covered.
    /// `Update(K,V)` record the key `K` and old value `V`
    Update(K, V),
    /// When doing [`EnvMap::insert`]
    /// If there was no such key in environment, the key and new value were inserted
    /// `Insert(K)` record only the key `K`
    Insert(K),
    /// When doing [`EnvMap::remove`]
    /// If has such key in environment, the old value is deleted.
    /// `Delete(K,V)` record the key `K` and old value `V`
    Delete(K, V),
    /// When doing [`EnvMap::remove`]
    /// If there was no such key in environment, nothing happend.
    /// Therefore, nothing to record.
    Nothing,
}

/// EnvMap is a wrapper of HashMap, but with the ability to backtrack and recover from modification
#[derive(Clone, Debug)]
pub struct EnvMap<K, V> {
    /// The wrapped HashMap allow us to do all the work.
    base_map: HashMap<K, V>,
    /// History records all the operations that have done.
    history: Vec<EnvOpr<K, V>>,
    /// Scopes records the hisory pivot for each scope
    scopes: Vec<usize>,
}

/// Many implementations are just the same as HashMap
impl<K, V> Default for EnvMap<K, V>
where
    K: Eq + Hash + Clone,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> EnvMap<K, V>
where
    K: Eq + Hash + Clone,
{
    /// Creating an empty EnvMap
    pub fn new() -> EnvMap<K, V> {
        EnvMap {
            base_map: HashMap::new(),
            history: Vec::new(),
            scopes: Vec::new(),
        }
    }

    /// Creating an empty EnvMap with capacity
    pub fn with_capacity(capacity: usize) -> EnvMap<K, V> {
        EnvMap {
            base_map: HashMap::with_capacity(capacity),
            history: Vec::new(),
            scopes: Vec::new(),
        }
    }

    /// Returns the number of elements the map can hold without reallocating.
    pub fn capacity(&self) -> usize {
        self.base_map.capacity()
    }

    /// An iterator visiting all keys in arbitrary order.
    pub fn keys(&self) -> hash_map::Keys<'_, K, V> {
        self.base_map.keys()
    }

    /// An iterator visiting all values in arbitrary order.
    pub fn values(&self) -> hash_map::Values<'_, K, V> {
        self.base_map.values()
    }

    /// An iterator visiting all key-value pairs in arbitrary order.
    pub fn iter(&self) -> hash_map::Iter<'_, K, V> {
        self.base_map.iter()
    }

    /// Returns `true` if the map contains no elements.
    pub fn is_empty(&self) -> bool {
        self.base_map.is_empty()
    }

    /// Returns `true` if there is no saved scope frame
    pub fn is_scope_empty(&self) -> bool {
        self.scopes.is_empty()
    }

    /// Clears the map, removing all key-value pairs. Keeps the allocated memory
    /// for reuse.
    pub fn clear(&mut self) {
        self.base_map.clear();
        self.history.clear();
        self.scopes.clear();
    }

    /// Returns a reference to the value corresponding to the key.
    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.base_map.get(k)
    }

    /// Returns the key-value pair corresponding to the supplied key.
    pub fn get_key_value<Q>(&self, k: &Q) -> Option<(&K, &V)>
    where
        K: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.base_map.get_key_value(k)
    }

    /// Returns `true` if the map contains a value for the specified key.
    pub fn contains_key<Q>(&self, k: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.base_map.contains_key(k)
    }

    /// Inserts a key-value pair into the map.
    pub fn insert(&mut self, k: K, v: V) -> bool {
        if let Some(old) = self.base_map.insert(k.clone(), v) {
            self.history.push(EnvOpr::Update(k, old));
            true
        } else {
            self.history.push(EnvOpr::Insert(k));
            false
        }
    }

    /// Removes a key from the map
    pub fn remove(&mut self, k: &K) -> bool {
        if let Some(old) = self.base_map.remove(k) {
            self.history.push(EnvOpr::Delete(k.clone(), old));
            true
        } else {
            self.history.push(EnvOpr::Nothing);
            false
        }
    }

    /// Enter a new scope, record the current pivot of history
    pub fn enter_scope(&mut self) {
        self.scopes.push(self.history.len())
    }

    /// Leave from a scope, unwind the history and recover.
    pub fn leave_scope(&mut self) {
        let n = self.scopes.pop().unwrap();
        for _ in n..self.history.len() {
            match self.history.pop().unwrap() {
                EnvOpr::Update(k, v) => {
                    // recover the old value that was covered by insert
                    let r = self.base_map.insert(k, v);
                    assert!(r.is_some());
                }
                EnvOpr::Insert(k) => {
                    // remove the inserted key and value
                    let r = self.base_map.remove(&k);
                    assert!(r.is_some());
                }
                EnvOpr::Delete(k, v) => {
                    // recover the deleted key and value
                    let r = self.base_map.insert(k, v);
                    assert!(r.is_none());
                }
                EnvOpr::Nothing => {
                    // Well, do nothing...
                }
            }
        }
    }
}

impl<K, V> std::ops::Index<&K> for EnvMap<K, V>
where
    K: Hash + Eq,
{
    type Output = V;

    fn index(&self, index: &K) -> &Self::Output {
        &self.base_map[index]
    }
}

#[test]
fn env_map_test() {
    let mut env = EnvMap::new();
    env.insert(&1, 'a');
    env.enter_scope();
    env.insert(&1, 'd');
    env.insert(&2, 'b');
    env.insert(&3, 'c');
    assert_eq!(env.get(&1), Some(&'d'));
    assert_eq!(env.get(&2), Some(&'b'));
    assert_eq!(env.get(&3), Some(&'c'));
    env.leave_scope();
    assert_eq!(env.get(&1), Some(&'a'));
    assert_eq!(env.get(&2), None);
    assert_eq!(env.get(&3), None);
}
