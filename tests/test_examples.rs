use norem_lang::driver::action;

#[test]
fn test_bu_append_good() -> Result<(), ()> {
    action::test_bu_good_prog("append", 20)
}

#[test]
fn test_bu_append_bad() -> Result<(), ()> {
    action::test_bu_bad_prog("append", 20)
}

#[test]
fn test_bu_reverse_good() -> Result<(), ()> {
    action::test_bu_good_prog("reverse", 20)
}

#[test]
fn test_bu_reverse_bad() -> Result<(), ()> {
    action::test_bu_bad_prog("reverse", 20)
}

#[test]
fn test_bu_tree_insert_good() -> Result<(), ()> {
    action::test_bu_good_prog("tree_insert", 4)
}

#[test]
fn test_bu_tree_insert_bad() -> Result<(), ()> {
    action::test_bu_bad_prog("tree_insert", 5)
}

#[test]
fn test_td_append_good() -> Result<(), ()> {
    action::test_td_good_prog("append", "is_elem_after_append", 20)
}

#[test]
fn test_td_append_bad() -> Result<(), ()> {
    action::test_td_bad_prog("append", "is_elem_after_append", 20)
}

#[test]
fn test_td_reverse_good() -> Result<(), ()> {
    action::test_td_good_prog("reverse", "twice_reverse", 20)
}

#[test]
fn test_td_reverse_bad() -> Result<(), ()> {
    action::test_td_bad_prog("reverse", "twice_reverse", 20)
}

#[test]
fn test_td_tree_insert_good() -> Result<(), ()> {
    action::test_td_good_prog("tree_insert", "always_sorted", 5)
}

#[test]
fn test_td_tree_insert_bad() -> Result<(), ()> {
    action::test_td_bad_prog("tree_insert", "always_sorted", 5)
}
