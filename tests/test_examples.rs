use norem_lang::driver::action;

#[test]
fn test_append_good() -> Result<(), ()> {
    action::test_good_prog("append", "is_elem_after_append", 5, 30, 5)
}

#[test]
fn test_append_bad() -> Result<(), ()> {
    action::test_bad_prog("append", "is_elem_after_append", 5, 100, 5)
}

#[test]
fn test_reverse_good() -> Result<(), ()> {
    action::test_good_prog("reverse", "twice_reverse", 5, 30, 5)
}

#[test]
fn test_reverse_bad() -> Result<(), ()> {
    action::test_bad_prog("reverse", "twice_reverse", 5, 100, 5)
}

#[test]
fn test_tree_insert_good() -> Result<(), ()> {
    action::test_good_prog("tree_insert", "always_sorted", 5, 30, 5)
}

#[test]
fn test_tree_insert_bad() -> Result<(), ()> {
    action::test_bad_prog("tree_insert", "always_sorted", 5, 100, 5)
}
