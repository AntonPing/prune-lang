use norem_lang::driver::action;

#[test]
fn test_append_good() {
    action::test_good_prog("append", "is_elem_after_append", 30, 31, 1).unwrap()
}

#[test]
fn test_append_bad() {
    action::test_bad_prog("append", "is_elem_after_append", 10, 1000, 10).unwrap()
}

#[test]
fn test_reverse_good() {
    action::test_good_prog("reverse", "twice_reverse", 30, 31, 1).unwrap()
}

#[test]
fn test_reverse_bad() {
    action::test_bad_prog("reverse", "twice_reverse", 10, 1000, 10).unwrap()
}

#[test]
fn test_tree_insert_good() {
    action::test_good_prog("tree_insert", "always_sorted", 30, 31, 1).unwrap()
}

#[test]
fn test_tree_insert_bad() {
    action::test_bad_prog("tree_insert", "always_sorted", 10, 1000, 10).unwrap()
}

#[test]
fn test_avl_tree_good() {
    action::test_good_prog("avl_tree", "always_sorted_balanced", 30, 31, 1).unwrap()
}

// #[test]
// fn test_avl_tree_bad() -> Result<(), ()> {
//     action::test_bad_prog("avl_tree", "always_sorted_balanced", 5, 100, 5)
// }

#[test]
fn test_long_fail() {
    let mut log = std::io::empty();
    let res = action::test_prog(
        std::path::Path::new("./examples/long_fail.nrm"),
        "long_fail",
        10,
        1000,
        10,
        &mut log,
    )
    .unwrap();
    assert_eq!(res, false);
}
