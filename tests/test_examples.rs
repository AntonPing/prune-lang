use norem_lang::driver::action;

#[test]
fn test_append_good() -> Result<(), ()> {
    action::test_example_good_prog("append", 20)
}

#[test]
fn test_append_bad() -> Result<(), ()> {
    action::test_example_bad_prog("append", 20)
}

#[test]
fn test_reverse_good() -> Result<(), ()> {
    action::test_example_good_prog("reverse", 20)
}

#[test]
fn test_reverse_bad() -> Result<(), ()> {
    action::test_example_bad_prog("reverse", 20)
}

#[test]
fn test_tree_insert_good() -> Result<(), ()> {
    action::test_example_good_prog("tree_insert", 4)
}

#[test]
fn test_tree_insert_bad() -> Result<(), ()> {
    action::test_example_bad_prog("tree_insert", 10)
}
