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

#[test]
fn uniquify_test() {
    let s1 = InternStr::new(&"foo");
    let x1 = Ident::fresh(&s1);
    let x2 = x1.uniquify();
    assert_ne!(x1, x2);
    assert_eq!(x1.name, x2.name);
}
