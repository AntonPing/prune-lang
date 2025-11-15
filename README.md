# Prune Programming Language

## Language Overview

- Prune is a constraint logic programming language with branching heuristic.

- It is designed as a scalable solver for recursive logic constraints.

- Suitable but not only for test generation, symbolic execution and program synthesis.

# Quick Start

## Prerequisites
### Required
- [Rust toolchain](https://rustup.rs/) (Cargo)

### Optional (for arithmetic constraints)
Choose one SMT solver:
- [Z3 solver](https://github.com/Z3Prover/z3) (Recommended)
- [CVC5 solver](https://github.com/cvc5/cvc5)

## Install Prune compiler

### Method 1: Install via Cargo (Recommended)
```bash
cargo install prune-lang
```

### Method 2: Build binary from source
```bash
git clone https://github.com/yourusername/prune-lang
cd prune-lang
cargo build --release
# The binary will be available at: ./target/release/prune
# Add it to your system PATH for global access.
```

### Verify Installation

```bash
# Check if prune is correctly installed.
prune --version

# Optional: Verify SMT solver installation
z3 --version    # for Z3 solver
cvc5 --version  # for CVC5 solver
```

## Run code without SMT solver

You can use the Prune compiler without installing an SMT solver, though this limits functionality to programs without arithmetic constraints.

The following example solves the equation `x > 0 && y > 0 && z > 0 && x^2 + y^2 = z^2` using Peano arithmetic encoded in algebraic data types, eliminating the need for an SMT solver. [This example](examples/arith/unary_arith.pr) is available in the repository.

```
datatype Nat where
| Z
| S(Nat)
end

function add(x: Nat, y: Nat) -> Nat
begin
    match x with
    | Z => y
    | S(k) => S(add(k, y))
    end
end

function mul(x: Nat, y: Nat) -> Nat
begin
    match x with
    | Z => Z
    | S(k) => add(y, mul(k, y))
    end
end

function pythagorean_triple(x: Nat, y: Nat, z: Nat)
begin
    let S(_) = x;
    let S(_) = y;
    let S(_) = z;
    guard add(mul(x, x), mul(y, y)) = mul(z, z);
end

query pythagorean_triple(depth_step=20, depth_limit=200, answer_limit=1)
```

To run this example, save the code as test1.pr and execute:

```bash
prune no-smt test1.pr
```

Sample output:

```
[RUN]: try depth = 20... (found answer: 0)
[STAT]: step = 15, step_la = 0(ratio 0), total = 15, 
......
[RUN]: try depth = 180... (found answer: 0)
[ANSWER]: (depth = 175)
x = S(S(S(Z)))
y = S(S(S(S(Z))))
z = S(S(S(S(S(Z)))))
res_func = ()
[STAT]: step = 2944, step_la = 0(ratio 0), total = 2944, acc_total = 28442
```

The result can be interpreted as `x=3, y=4, z=5`, which is correct.

## Run code with SMT solver backend

Before running the following example, please make sure that you have external SMT solver (Z3 or CVC5) installed.

This version of the Pythagorean triple problem uses integer arithmetic constraints. [This example](examples/other/pythagorean.pr) is also available in the repository.


```
function pythagorean_triple(a: Int, b: Int, c: Int)
begin
    guard a > 0;
    guard b > 0;
    guard c > 0;
    guard a * a + b * b = c * c;
end

query pythagorean_triple(depth_step=1, depth_limit=1, answer_limit=1)
```

Save this code as test2.pr and run:

```bash
prune z3-inc test2.pr   # for Z3 solver
prune cvc5-inc test2.pr # for CVC5 solver
```

Sample output:

```
[RUN]: try depth = 1... (found answer: 0)
[ANSWER]: (depth = 1)
a = 3
b = 4
c = 5
res_func = ()
[STAT]: step = 1, step_la = 0(ratio 0), total = 1, acc_total = 1
```

The resulting triple (a, b, c) may vary between runs. You can verify that each result satisfies the constraints.

# License

Licensed under the Apache License, Version 2.0.
See [LICENSE](LICENSE) or http://www.apache.org/licenses/LICENSE-2.0 for details.