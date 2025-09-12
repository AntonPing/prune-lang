use prune_lang::driver::action;

#[test]
fn test_append() {
    action::test_sym_exec_good_prog("append_good").unwrap()
}

#[test]
fn test_append_bad() {
    action::test_sym_exec_bad_prog("append_bad").unwrap()
}

#[test]
fn test_double_reverse() {
    action::test_sym_exec_good_prog("double_reverse_good").unwrap()
}

#[test]
fn test_double_reverse_bad() {
    action::test_sym_exec_bad_prog("double_reverse_bad").unwrap()
}

#[test]
fn test_reverse_length() {
    action::test_sym_exec_good_prog("reverse_length_good").unwrap()
}

#[test]
fn test_reverse_length_bad() {
    action::test_sym_exec_bad_prog("reverse_length_bad").unwrap()
}

#[test]
fn test_tree_insert() {
    action::test_sym_exec_good_prog("tree_insert_good").unwrap()
}

#[test]
fn test_tree_insert_bad() {
    action::test_sym_exec_bad_prog("tree_insert_bad").unwrap()
}

#[test]
fn test_avl_tree() {
    action::test_sym_exec_good_prog("avl_tree_good").unwrap()
}

// #[test]
// fn test_avl_tree_bad() {
//     action::test_sym_exec_bad_prog("avl_tree_bad").unwrap()
// }
