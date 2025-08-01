use prune_lang::driver::action;

#[test]
fn test_append() {
    action::test_unsat_prog("append").unwrap()
}

#[test]
fn test_append_bad() {
    action::test_sat_prog("append_bad").unwrap()
}

#[test]
fn test_double_reverse() {
    action::test_unsat_prog("double_reverse").unwrap()
}

#[test]
fn test_double_reverse_bad() {
    action::test_sat_prog("double_reverse_bad").unwrap()
}

#[test]
fn test_reverse_length() {
    action::test_unsat_prog("reverse_length").unwrap()
}

#[test]
fn test_reverse_length_bad() {
    action::test_sat_prog("reverse_length_bad").unwrap()
}

#[test]
fn test_tree_insert() {
    action::test_unsat_prog("tree_insert").unwrap()
}

#[test]
fn test_tree_insert_bad() {
    action::test_sat_prog("tree_insert_bad").unwrap()
}

#[test]
fn test_avl_tree() {
    action::test_unsat_prog("avl_tree").unwrap()
}

// #[test]
// fn test_avl_tree_bad() {
//     action::test_sat_prog("avl_tree_bad").unwrap()
// }

#[test]
fn test_long_fail() {
    action::test_unsat_prog("long_fail").unwrap()
}
