use super::intern::InternStr;
use std::fmt;
use std::sync::atomic;

// counter zero is reserved for dummy ident
static COUNTER: atomic::AtomicUsize = atomic::AtomicUsize::new(1);

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident {
    name: InternStr,
    index: usize,
}

impl Ident {
    pub fn fresh<S: AsRef<str>>(s: &S) -> Ident {
        let name = InternStr::new(s.as_ref());
        let index = COUNTER.fetch_add(1, atomic::Ordering::Relaxed);
        Ident { name, index }
    }

    pub fn dummy<S: AsRef<str>>(s: &S) -> Ident {
        let name = InternStr::new(s.as_ref());
        Ident { name, index: 0 }
    }

    pub fn is_dummy(&self) -> bool {
        self.index == 0
    }

    pub fn uniquify(&self) -> Ident {
        let name = self.name;
        let index = COUNTER.fetch_add(1, atomic::Ordering::Relaxed);
        Ident { name, index }
    }

    pub fn as_str(&self) -> &'static str {
        self.name.as_str()
    }

    pub fn tag_ctx(self, ctx: usize) -> IdentCtx {
        IdentCtx { ident: self, ctx }
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_dummy() {
            write!(f, "{}", self.name)
        } else {
            write!(f, "{}_{}", self.name, self.index)
        }
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_dummy() {
            write!(f, "{}", self.name)
        } else {
            write!(f, "{}_{}", self.name, self.index)
        }
    }
}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        self.name.as_str()
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct IdentCtx {
    pub ident: Ident,
    pub ctx: usize,
}

impl fmt::Display for IdentCtx {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}_ctx_{}", self.ident, self.ctx)
    }
}

#[test]
fn ident_fresh_test() {
    let s1 = InternStr::new(&"foo");
    let x1 = Ident::fresh(&s1);
    let x2 = Ident::fresh(&s1);
    let x3 = Ident::fresh(&s1);
    assert_ne!(x1, x2);
    assert_ne!(x1, x3);
    assert_ne!(x2, x3);
    assert_eq!(x1.name, x2.name);
    assert_eq!(x1.name, x3.name);
    assert_eq!(x2.name, x3.name);
}

#[test]
fn ident_uniquify_test() {
    let s1 = InternStr::new(&"foo");
    let x1 = Ident::fresh(&s1);
    let x2 = x1.uniquify();
    assert_ne!(x1, x2);
    assert_eq!(x1.name, x2.name);
}
