use norem_lang::driver::action;

#[test]
fn test_append() {
    action::test_unsat_prog("append", "is_elem_after_append", 30, 31, 1).unwrap()
}

#[test]
fn test_append_bad() {
    action::test_sat_prog("append_bad", "is_elem_after_append", 10, 1000, 10).unwrap()
}

#[test]
fn test_double_reverse() {
    action::test_unsat_prog("double_reverse", "double_reverse_same", 30, 31, 1).unwrap()
}

#[test]
fn test_double_reverse_bad() {
    action::test_sat_prog("double_reverse_bad", "double_reverse_same", 10, 1000, 10).unwrap()
}

#[test]
fn test_reverse_length() {
    action::test_unsat_prog("reverse_length", "same_length_after_reverse", 30, 31, 1).unwrap()
}

#[test]
fn test_reverse_length_bad() {
    action::test_sat_prog(
        "reverse_length_bad",
        "same_length_after_reverse",
        10,
        1000,
        10,
    )
    .unwrap()
}

#[test]
fn test_tree_insert() {
    action::test_unsat_prog("tree_insert", "always_sorted", 30, 31, 1).unwrap()
}

#[test]
fn test_tree_insert_bad() {
    action::test_sat_prog("tree_insert_bad", "always_sorted", 10, 1000, 10).unwrap()
}

#[test]
fn test_avl_tree() {
    action::test_unsat_prog("avl_tree", "always_sorted_balanced", 30, 31, 1).unwrap()
}

// #[test]
// fn test_avl_tree_bad() {
//     action::test_unsat_prog("avl_tree_bad", "always_sorted_balanced", 10, 1000, 10).unwrap()
// }

#[test]
fn test_long_fail() {
    action::test_unsat_prog("long_fail", "long_fail", 30, 31, 1).unwrap()
}
