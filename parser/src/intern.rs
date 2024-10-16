use std::fmt::{Debug, Display};

use fxhash::FxHashMap;
use serde::Serialize;
use std::mem;

use crate::parser::parser::intern_lookup;

#[derive(Clone, Copy, Eq, PartialEq, Hash, Serialize)]
pub struct StrId(pub u32);

impl Debug for StrId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", intern_lookup(*self))
    }
}

impl Display for StrId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct Interner {
    map: FxHashMap<&'static str, StrId>,
    vec: Vec<&'static str>,
    buf: String,
    full: Vec<String>,
}
impl Interner {
    pub fn with_capacity(cap: usize) -> Interner {
        let cap = cap.next_power_of_two();
        Interner {
            map: FxHashMap::default(),
            vec: Vec::new(),
            buf: String::with_capacity(cap),
            full: Vec::new(),
        }
    }
    pub fn intern(&mut self, name: &str) -> StrId {
        if let Some(&id) = self.map.get(name) {
            return id;
        }
        let name = unsafe { self.alloc(name) };
        let id = self.map.len() as u32;
        self.map.insert(name, StrId(id));
        self.vec.push(name);
        debug_assert!(self.lookup(StrId(id)) == name);
        debug_assert!(self.intern(name) == StrId(id));

        StrId(id)
    }
    pub fn lookup(&self, id: StrId) -> &'static str {
        self.vec[id.0 as usize]
    }
    unsafe fn alloc(&mut self, name: &str) -> &'static str {
        let cap = self.buf.capacity();
        if cap < self.buf.len() + name.len() {
            let new_cap = (cap.max(name.len()) + 1).next_power_of_two();
            let new_buf = String::with_capacity(new_cap);
            let old_buf = mem::replace(&mut self.buf, new_buf);
            self.full.push(old_buf);
        }
        let interned = {
            let start = self.buf.len();
            self.buf.push_str(name);
            &self.buf[start..]
        };
        &*(interned as *const str)
    }
}
