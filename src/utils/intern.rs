use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::fmt;
use std::sync;

static INTERNER: Lazy<sync::Mutex<Interner>> = Lazy::new(|| {
    let interner = Interner {
        str_to_idx: HashMap::new(),
        idx_to_str: Vec::new(),
    };
    sync::Mutex::new(interner)
});

struct Interner {
    str_to_idx: HashMap<String, usize>,
    idx_to_str: Vec<&'static str>,
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct InternStr(usize);
impl InternStr {
    pub fn new<S: AsRef<str>>(s: S) -> InternStr {
        let mut interner = INTERNER.lock().unwrap();
        if let Some(idx) = interner.str_to_idx.get(s.as_ref()) {
            InternStr(*idx)
        } else {
            let s = s.as_ref().to_string();
            let idx = interner.idx_to_str.len();
            interner.str_to_idx.insert(s.clone(), idx);
            interner.idx_to_str.push(Box::leak(Box::new(s)));
            InternStr(idx)
        }
    }

    pub fn as_str(&self) -> &'static str {
        let interner = INTERNER.lock().unwrap();
        &interner.idx_to_str[self.0]
    }
}

impl fmt::Debug for InternStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl fmt::Display for InternStr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl AsRef<str> for InternStr {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

#[test]
fn intern_test() {
    // test function InternStr::new()
    let foo1: &str = "foo";
    let foo2: String = "foo".to_string();
    let bar1: &str = "bar";
    let bar2: String = "bar".to_string();
    let s1 = InternStr::new(&foo1);
    let s2 = InternStr::new(&foo2);
    let s3 = InternStr::new(&bar1);
    let s4 = InternStr::new(&bar2);
    assert_eq!(s1, s2);
    assert_eq!(s3, s4);
    assert_ne!(s1, s3);
    assert_ne!(s2, s4);
    assert_eq!(format!("{}", s1), "foo");
    assert_eq!(format!("{}", s2), "foo");
    assert_eq!(format!("{}", s3), "bar");
    assert_eq!(format!("{}", s4), "bar");
}
