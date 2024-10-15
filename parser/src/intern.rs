use fxhash::FxHashMap;
use serde::Serialize;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash, Serialize)]
pub struct StrId(u32);

#[derive(Default, Debug, Clone)]
pub struct Interner {
    map: FxHashMap<String, StrId>,
    vec: Vec<String>,
}
impl Interner {
    pub fn intern(&mut self, name: &str) -> StrId {
        if let Some(&idx) = self.map.get(name) {
            return idx;
        }
        let idx = self.map.len() as u32;
        self.map.insert(name.to_owned(), StrId(idx));
        self.vec.push(name.to_owned());
        debug_assert!(self.lookup(StrId(idx)) == name);
        debug_assert!(self.intern(name) == StrId(idx));
        StrId(idx)
    }
    pub fn lookup(&self, idx: StrId) -> &str {
        self.vec[idx.0 as usize].as_str()
    }
}
