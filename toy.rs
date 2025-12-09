//! Implementation of Unicode Standard Annex #31 for 
//! determining which `char` vs are valid in programming language identifiers.
#![feature
( 
    
)]

#![allow
( 
    bare_trait_objects,
    deprecated,
    mismatched_lifetime_syntaxes,
    non_camel_case_types,
    non_fmt_panics,
    non_snake_case,
    non_upper_case_globals,
    static_mut_refs,
    unpredictable_function_pointer_comparisons,
    unused_attributes,
    unused_imports,
    unused_macros,
    unused_variables,
 )]
/*
pub mod _
{
    pub use std::_::{ * };
}

pub mod __
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    */
}

pub mod error;
pub mod obj;
pub mod tup;
pub mod types;
pub mod v;
pub mod parse;

#[macro_use] extern crate bitflags;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate rand;
#[macro_use] extern crate regex as re;
#[macro_use] extern crate smallvec;
#[macro_use] extern crate time as temporal;
#[macro_use] extern crate unicode_normalization;
#[macro_use] extern crate unicode_width;

mod gen;
mod lexer;
mod opts;
mod parser;
*/
pub mod collections
{
    pub use std::collections::{ * };
}

pub mod error
{
    pub use std::error::{ * };
}

pub mod examples
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    */
    pub mod sort
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        */
        pub mod bubble
        {
            /*!
            */
            use ::
            {
                *,
            };
            /*
            */
        }

        pub mod input
        {
            /*!
            */
            use ::
            {
                *,
            };
            /*
            */
        }
    }
    /*
    hello_crust.c

    int main(void)
    {
        printf("Hello, CRUST!\n");
        printf("This is a simple sample code that can be compiled by crust.\n");
        return 0;
    }
    */
    /*
    input_data_array.c

    int a[100];
    int main(void) {
            int n  = 10;
            printf("Input array len: ");
            scanf("%lld", &n);
            printf("Now input %d numbers:\n", n);
            for (int i = 0; i < n; i = i + 1) {
                    scanf("%d", &a[i]);
            }
            printf("Your input array:\n");
            for (int i = 0; i < n; i = i + 1) {
                    printf("%d ", a[i]);
            }
            printf("\n");
            return 0;
    }
    */
    /*
    simple_print.c
    
    int main(void)
    {
        for (int i = 0; i < 10; i = i + 1)
        {
            printf("hello world\n");
        }
        return 0;
    }
    */
}

pub mod fs
{
    pub use std::fs::{ * };
}

pub mod gen
{
    /*!
    */
    use ::
    {
        collections::{HashMap, HashSet},
        sync::{ atomic },
        *,
    };
    /*
    use crate::lexer::TokType;
    use crate::parser::{DataType, NodeType, ParseNode, StmtType};
    use std::collections::{HashMap, HashSet};
    use std::sync::atomic;
    */
    static LABEL_COUNTER: atomic::AtomicUsize = atomic::AtomicUsize::new(0);
    fn gen_labels(prefix: &str) -> String {
        let label_counter = LABEL_COUNTER.fetch_add(1, atomic::Ordering::SeqCst);
        let label = format!(".L{}{}", prefix, label_counter);

        label
    }

    static FLAG_FOR_MAIN_HAS_RET: atomic::AtomicBool = atomic::AtomicBool::new(false);
    fn fn_main_has_ret() {
        FLAG_FOR_MAIN_HAS_RET.swap(true, atomic::Ordering::SeqCst);
    }

    fn gen_fn_prologue(fn_name: &str) -> String {
        let p = "        ";
        format!(
            "{}.text\n\
            {}.global {}\n\
            {}.type {}, @function\n\
            {}:\n\
            {}:\n\
            {}.cfi_startproc\n\
            {}pushq	%rbp\n\
            {}.cfi_def_cfa_offset 16\n\
            {}.cfi_offset 6, -16\n\
            {}movq	%rsp, %rbp\n\
            {}.cfi_def_cfa_register 6\n\
            ",
            p,
            p,
            fn_name,
            p,
            fn_name,
            fn_name,
            gen_labels("FB"),
            p,
            p,
            p,
            p,
            p,
            p
        )
    }

    fn gen_fn_epilogue() -> String {
        let p = "        ";
        format!(
            "{}movq %rbp, %rsp\n\
            {}popq	%rbp\n\
            {}.cfi_def_cfa 7, 8\n",
            p, p, p
        )
    }

    fn compute_const(tree: &ParseNode) -> i64 {
        match &tree.entry {
            NodeType::BinExp(op) => {
                let lhs = compute_const(tree.child.get(0).unwrap());
                let rhs = compute_const(tree.child.get(1).unwrap());
                match op {
                    TokType::Plus => {
                        return lhs + rhs;
                    }
                    TokType::Multi => {
                        return lhs * rhs;
                    }
                    TokType::Splash => {
                        return lhs / rhs;
                    }
                    TokType::And => {
                        if lhs != 0 && rhs != 0 {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    TokType::Or => {
                        if lhs != 0 || rhs != 0 {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    TokType::Equal => {
                        if lhs == rhs {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    TokType::NotEqual => {
                        if lhs != rhs {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    TokType::LessEqual => {
                        if lhs <= rhs {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    TokType::GreaterEqual => {
                        if lhs >= rhs {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    TokType::Lt => {
                        if lhs < rhs {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    TokType::Gt => {
                        if lhs > rhs {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    _ => panic!("{:?} should not occur in global variable initialization"),
                }
            }
            NodeType::UnExp(op) => {
                let child_val = compute_const(tree.child.get(0).unwrap());
                match op {
                    TokType::Minus => {
                        return -child_val;
                    }
                    TokType::Tilde => {
                        return !child_val;
                    }
                    TokType::Exclamation => {
                        if child_val == 0 {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    _ => panic!("Expected Unary Operator, found {:?}", op),
                }
            }
            NodeType::Const(val) => {
                return val * 1;
            }
            _ => return compute_const(tree.child.get(0).unwrap()),
        }
    }
    pub fn gen_prog(tree: &ParseNode) -> String {
        let p = "        ".to_string();

        // iter every function node
        let mut prog_body = String::new();
        let index_map: HashMap<String, isize> = HashMap::new();
        let mut global_variable_scope: HashSet<String> = HashSet::new();
        let idx: isize = 0;
        for it in tree.child.iter() {
            match &it.entry {
                NodeType::Declare(var_name, DataType::I64) => {
                    // record it in the scope, index_map,
                    global_variable_scope.insert(var_name.to_string());
                    if it.child.is_empty() {
                        // uninitialized global variable
                        // just put them in .comm
                        // now we use value has 8 bytes by default.
                        // XXX: should be vary-length based on the data type.
                        prog_body.push_str(&format!("{}.comm {}, 8, 8\n", p, var_name,))
                    } else {
                        let val = compute_const(&it.child.get(0).unwrap());
                        prog_body.push_str(&format!(
                            "{}.globl	{}\n\
                            {}.data\n\
                            {}.align 8\n\
                            {}.type	{}, @object\n\
                            {}.size	{}, 8\n\
                            {}:\n\
                            {}.long	{}\n",
                            p, var_name, p, p, p, var_name, p, var_name, var_name, p, val
                        ));
                    }
                }
                NodeType::Declare(var_name, DataType::Arr64(len)) => {
                    global_variable_scope.insert(var_name.to_string());
                    prog_body.push_str(&format!("{}.comm {}, {}, 32\n", p, var_name, len * 8));
                }
                NodeType::Fn(fn_name, var_list_opt) => {
                    let fn_prologue = gen_fn_prologue(fn_name);
                    let fn_epilogue = gen_fn_epilogue();
                    // cause in function, we have to pass the offset of argument and scope contains argument
                    // to function body
                    let call_by_function = true;
                    let mut index_map: HashMap<String, isize> = HashMap::new();
                    let mut scope: HashMap<String, bool> = HashMap::new();
                    match var_list_opt {
                        Some(var_list) => {
                            let mut param_offset = 16 + (var_list.len() as isize - 6 - 1) * 8; // EBP + 16 (old EBP at 0, return address at 8)
                            for i in 0..var_list.len() {
                                scope.insert(var_list[i].to_string(), true);
                                if i >= 6 {
                                    // this is stored in stack, starting from EBP + 16
                                    index_map.insert(var_list[i].to_string(), param_offset);
                                    param_offset -= 8;
                                } else {
                                    // stored in regs, we use offset from 0-5 as index to regs.
                                    // and use (i+1)*-8 as their index, cause we will push them one by one at the new frame stack
                                    index_map.insert(var_list[i].to_string(), -(i as isize + 1) * 8);
                                }
                            }
                        }
                        None => {}
                    }
                    let fn_body = &gen_block(
                        it,
                        &index_map,
                        &scope,
                        idx,
                        None,
                        None,
                        true,
                        call_by_function,
                        &global_variable_scope,
                    );

                    let tmp = if FLAG_FOR_MAIN_HAS_RET.load(atomic::Ordering::SeqCst) == false {
                        format!(
                            "{}movq $0, %rax\n\
                            {}\
                            {}ret\n",
                            p,
                            gen_fn_epilogue(),
                            p
                        )
                    } else {
                        "".to_string()
                    };
                    let fn_tot = format!(
                        "{}\
                        {}\
                        {}\
                        {}\
                        {}.cfi_endproc\n\
                        {}:\n\
                        {}.size   {}, .-{}\n",
                        fn_prologue,
                        fn_body,
                        tmp,
                        fn_epilogue,
                        p,
                        gen_labels("FE"),
                        p,
                        fn_name,
                        fn_name
                    );
                    prog_body.push_str(&fn_tot);
                }
                _ => panic!("`{:?}` type should not be here", it.entry),
            }
        }

        match &tree.entry {
            NodeType::Prog(prog_name) => format!(
                "{}.file \"{}\"\n\
                {}\
                {}.ident	\"crust: 0.1 (By Haoran Wang)\"\n\
                {}.section	.note.GNU-stack,\"\",@progbits\n",
                p, prog_name, prog_body, p, p
            ),
            _ => panic!("Something went wrong in gen_prog"),
        }
    }

    pub fn gen_declare(
        tree: &ParseNode,
        index_map: &HashMap<String, isize>,
        scope: &HashMap<String, bool>, // 1 -> function argument, 0 -> local variables
        idx: isize,
        lbb: &str,
        leb: &str,
        loop_in_label: Option<&str>,
        loop_out_label: Option<&str>,
        global_variable_scope: &HashSet<String>,
    ) -> (HashMap<String, isize>, HashMap<String, bool>, isize, String) {
        // println!("in gen_declare with {:?}", tree.entry);
        let p = "        ";
        let mut index_map = index_map.clone();
        let mut scope = scope.clone();
        let mut idx = idx;
        match &tree.entry {
            NodeType::Declare(var_name, data_type) => {
                let get_opt = scope.get(var_name);
                match get_opt {
                    Some(flag) => {
                        match flag {
                            true => {
                                // this variable is in scope, but was passed by function argument, so just shallow it
                                scope.insert(var_name.to_string(), false);
                                // println!("scope after insert: {:?}", scope);
                                index_map.insert(var_name.to_string(), idx - 8);
                                idx -= 8;
                            }
                            false => {
                                panic!(
                                    "Error: redeclaration of variable `{}` in the same scope",
                                    var_name
                                );
                            }
                        }
                    }
                    None => {
                        // not declared
                        scope.insert(var_name.to_string(), false);
                        // println!("scope after insert: {:?}", scope);
                        index_map.insert(var_name.to_string(), idx - 8);
                        idx -= 8;
                    }
                }

                // judge whether it's initialized
                let mut e1 = String::new();

                if tree.child.is_empty() {
                    // just declare, we initialized it with 0
                    e1 = format!("        movq $0, %rax\n");
                } else {
                    e1 = gen_stmt(
                        tree.child
                            .get(0)
                            .expect("Statement::Declare Node has no child"),
                        &index_map,
                        idx,
                        lbb,
                        leb,
                        loop_in_label,
                        loop_out_label,
                        &global_variable_scope,
                    );
                }
                let s = format!(
                    "{}\
                    {}pushq %rax # gen_declare\n",
                    e1, p
                );
                (index_map, scope, idx, s)
            }
            _ => panic!("Type `{:?}` should not occur here", tree.entry),
        }
    }

    pub fn gen_for(
        tree: &ParseNode,
        index_map: &HashMap<String, isize>,
        idx: isize,
        global_variable_scope: &HashSet<String>,
    ) -> String {
        let p = "        ".to_string();
        let label_begin_loop = gen_labels("BFOR");
        let label_end_loop = gen_labels("EFOR");

        let mut index_map = index_map.clone();
        let mut idx: isize = idx;
        // now in a new block now
        let mut scope: HashMap<String, bool> = HashMap::new();
        match tree.entry {
            NodeType::Stmt(StmtType::ForDecl) => {
                let (index_map_new, scope_new, idx_new, init) = gen_declare(
                    tree.child.get(0).unwrap(),
                    &index_map,
                    &scope,
                    idx,
                    &label_begin_loop,
                    &label_end_loop,
                    Some(&label_begin_loop),
                    Some(&label_end_loop),
                    &global_variable_scope,
                );
                index_map = index_map_new.clone();
                idx = idx_new;
                scope = scope_new.clone();
                let condition = gen_stmt(
                    tree.child.get(1).unwrap(),
                    &index_map,
                    idx,
                    &label_begin_loop,
                    &label_end_loop,
                    Some(&label_begin_loop),
                    Some(&label_end_loop),
                    &global_variable_scope,
                );
                let post_exp = gen_stmt(
                    tree.child.get(2).unwrap(),
                    &index_map,
                    idx,
                    &label_begin_loop,
                    &label_end_loop,
                    Some(&label_begin_loop),
                    Some(&label_end_loop),
                    &global_variable_scope,
                );
                let stmt = gen_block(
                    tree.child.get(3).unwrap(),
                    &index_map,
                    &scope,
                    idx,
                    Some(&label_begin_loop),
                    Some(&label_end_loop),
                    true,
                    false,
                    &global_variable_scope,
                );
                //           generate init (declare)
                // BEGN_LOOP:
                //           generate condition
                //           cmpq $0, %rax
                //           je  END_LOOP
                //           generate statement
                //           pos-expression
                //           jmp BEGIN_LOOP
                // END_LOOP:
                //let b_deallocate = 8 * scope.len();
                let mut b_deallocate = 0;
                for (_, val) in scope.iter() {
                    if *val == false {
                        b_deallocate += 8;
                    }
                }
                format!(
                    "{}\
                    {}:\n\
                    {}\
                    {}cmpq $0, %rax\n\
                    {}je {}\n\
                    {}\
                    {}\
                    {}jmp {}\n\
                    {}:\n\
                    {}addq ${}, %rsp # for out clear block\n",
                    init,
                    label_begin_loop,
                    condition,
                    p,
                    p,
                    label_end_loop,
                    stmt,
                    post_exp,
                    p,
                    label_begin_loop,
                    label_end_loop,
                    p,
                    b_deallocate
                )
            }
            NodeType::Stmt(StmtType::For) => {
                let init = gen_stmt(
                    tree.child.get(0).unwrap(),
                    &index_map,
                    idx,
                    &label_begin_loop,
                    &label_end_loop,
                    Some(&label_begin_loop),
                    Some(&label_end_loop),
                    &global_variable_scope,
                );
                let condition = gen_stmt(
                    tree.child.get(1).unwrap(),
                    &index_map,
                    idx,
                    &label_begin_loop,
                    &label_end_loop,
                    Some(&label_begin_loop),
                    Some(&label_end_loop),
                    &global_variable_scope,
                );
                let post_exp = gen_stmt(
                    tree.child.get(2).unwrap(),
                    &index_map,
                    idx,
                    &label_begin_loop,
                    &label_end_loop,
                    Some(&label_begin_loop),
                    Some(&label_end_loop),
                    &global_variable_scope,
                );
                let stmt = gen_block(
                    tree.child.get(3).unwrap(),
                    &index_map,
                    &scope,
                    idx,
                    Some(&label_begin_loop),
                    Some(&label_end_loop),
                    true,
                    false,
                    &global_variable_scope,
                );
                //           generate init
                // BEGN_LOOP:
                //           generate condition
                //           cmpq $0, %rax
                //           je  END_LOOP
                //           generate statement
                //           pos-expression
                //           jmp BEGIN_LOOP
                // END_LOOP:
                // let b_deallocate = 8 * scope.len();
                let mut b_deallocate = 0;
                for (_, val) in scope.iter() {
                    if *val == false {
                        b_deallocate += 8;
                    }
                }
                format!(
                    "{}\
                    {}:\n\
                    {}\
                    {}cmpq $0, %rax\n\
                    {}je {}\n\
                    {}\
                    {}\
                    {}jmp {}\n\
                    {}:\n\
                    {}addq ${}, %rsp # for out clear stack\n",
                    init,
                    label_begin_loop,
                    condition,
                    p,
                    p,
                    label_end_loop,
                    stmt,
                    post_exp,
                    p,
                    label_begin_loop,
                    label_end_loop,
                    p,
                    b_deallocate
                )
            }
            _ => panic!("Something wrong in gen_for"),
        }
    }
    // gen_block() - into a new block, will have empty scope
    pub fn gen_block(
        tree: &ParseNode,
        index_map: &HashMap<String, isize>,
        scope: &HashMap<String, bool>,
        idx: isize,
        loop_in_label: Option<&str>,
        loop_out_label: Option<&str>,
        flag: bool,
        fn_def: bool,
        global_variable_scope: &HashSet<String>,
    ) -> String {
        let p = "        ".to_string(); // 8 white spaces
        let label_begin_block = gen_labels("BB");
        let label_end_block = gen_labels("EB");
        // iter every block
        let mut stmts = String::new();
        let mut index_map = index_map.clone();
        let mut idx: isize = idx;
        let mut current_scope: HashMap<String, bool> = scope.clone();
        if fn_def == false {
            current_scope = HashMap::new();
        } else {
            // this is a function definition block
            // we need to store the input argument in the stack
            // first push them in stack
            let regs: Vec<&'static str> = vec!["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
            if current_scope.len() > 6 {
                for i in 0..6 {
                    stmts.push_str(&format!("{}pushq {}\n", p, regs[i]));
                }
            } else {
                for i in 0..current_scope.len() {
                    stmts.push_str(&format!("{}pushq {}\n", p, regs[i]));
                }
            }
            // XXX: cause right now the generated will use small amout of registers,
            // but in the future will need to save callee-saved registers in the function stack
        }

        for it in &tree.child {
            // iter through every block-item
            match &it.entry {
                NodeType::Declare(_var_name, DataType::I64) => {
                    let (index_map_new, scope_new, idx_new, s) = gen_declare(
                        it,
                        &index_map,
                        &current_scope,
                        idx,
                        &label_begin_block,
                        &label_end_block,
                        loop_in_label,
                        loop_out_label,
                        &global_variable_scope,
                    );
                    index_map = index_map_new.clone();
                    idx = idx_new;
                    current_scope = scope_new.clone();
                    stmts.push_str(&s);
                }
                NodeType::Stmt(StmtType::Compound) => {
                    stmts.push_str(&gen_block(
                        it,
                        &index_map,
                        &current_scope,
                        idx,
                        loop_in_label,
                        loop_out_label,
                        true,
                        false, // call by  function not true
                        &global_variable_scope,
                    ));
                }
                _ => {
                    let s = gen_stmt(
                        it,
                        &index_map,
                        idx,
                        &label_begin_block,
                        &label_end_block,
                        loop_in_label,
                        loop_out_label,
                        &global_variable_scope,
                    );
                    stmts.push_str(&s);
                }
            }
        }
        let mut b_deallocate = 0;
        for (_, val) in current_scope.iter() {
            if *val == false {
                b_deallocate += 8;
            }
        }

        format!(
            "{}:\n\
            {}\
            {}:\n\
            {}addq ${}, %rsp # block out\n",
            label_begin_block, stmts, label_end_block, p, b_deallocate
        )
    }

    fn gen_addr(
        tree: &ParseNode,
        index_map: &HashMap<String, isize>,
        idx: isize,
        lbb: &str, // label_begin_block
        leb: &str, // label_end_block
        loop_in_label: Option<&str>,
        loop_out_label: Option<&str>,
        global_variable_scope: &HashSet<String>,
    ) -> String {
        let p = "        ".to_string();
        // first judge whether it is a global variable or local variable
        match &tree.entry {
            NodeType::ArrayRef(var_name) => {
                match index_map.get(var_name) {
                    Some(c) => {
                        // local array
                        panic!(format!("Error: address to local array not implemented"));
                    }
                    None => {
                        // not local but should check in global variable scope
                        match global_variable_scope.contains(var_name) {
                            true => {
                                // address of array[exp]
                                // should generate exp -> rax
                                let get_index = gen_stmt(
                                    tree.child
                                        .get(0)
                                        .expect("Statement::Declare Node has no child"),
                                    index_map,
                                    idx,
                                    lbb,
                                    leb,
                                    loop_in_label,
                                    loop_out_label,
                                    &global_variable_scope,
                                );
                                //                get index => rax
                                //        leaq    0(,%rax,8), %rdx
                                //        movq    a@GOTPCREL(%rip), %rax
                                //        addq    %rdx, %rax
                                format!(
                                    "{}\
                                    {}pushq %rdx\n\
                                    {}leaq  0(,%rax,8), %rdx\n\
                                    {}movq {}@GOTPCREL(%rip), %rax\n\
                                    {}addq %rdx, %rax\n\
                                    {}popq %rdx\n",
                                    get_index, p, p, p, var_name,p, p,
                                )
                            }
                            false => {
                                panic!(format!("Error: Using address operator against an undeclared variable `{}`", var_name));
                            }
                        }
                    }
                }
            }
            NodeType::Var(name) => {
                match index_map.get(name) {
                    Some(c) => {
                        // local variable
                        format!("{}leaq {}(%rbp), %rax\n", p, c) // put address in rax
                    }
                    None => {
                        // not local but should check in global
                        if global_variable_scope.contains(name) {
                            // ok
                            format!("{}movq {}@GOTPCREL(%rip), %rax\n", p, name)
                        } else {
                            panic!(format!("Using address operator against an undeclared variable `{}`", name));
                        }
                    }
                }
            }
            _ => {
                if tree.child.is_empty() {
                    panic!(format!("Can not use address(&) operator to rhs({:?})", tree.entry));
                } else {
                    gen_addr(
                        tree.child.get(0).expect("In gen address no child node now"),
                        index_map,
                        idx,
                        lbb,
                        leb,
                        loop_in_label,
                        loop_out_label,
                        global_variable_scope)
                }
            }
        }
    }
    pub fn gen_stmt(
        tree: &ParseNode,
        index_map: &HashMap<String, isize>,
        idx: isize,
        lbb: &str, // label_begin_block
        leb: &str, // label_end_block
        loop_in_label: Option<&str>,
        loop_out_label: Option<&str>,
        global_variable_scope: &HashSet<String>,
    ) -> String {
        let p = "        ".to_string(); // 8 white spaces
        match &tree.entry {
            NodeType::StringLiteral(data, tag) => format!(
                "{}.section .rodata\n\
                {}:\n\
                {}.string \"{}\"\n\
                {}.text\n\
                {}leaq {}(%rip), %rax\n",
                p, tag, p, data, p, p, tag,
            ),
            NodeType::ConditionalExp => {
                if tree.child.len() == 1 {
                    // just one <logical-or-exp>
                    gen_stmt(
                        tree.child
                            .get(0)
                            .expect("Conditional Expression has no child"),
                        index_map,
                        idx,
                        lbb,
                        leb,
                        loop_in_label,
                        loop_out_label,
                        &global_variable_scope,
                    )
                } else if tree.child.len() == 3 {
                    // <logical-or-exp> "?" <exp> ":" <conditional-exp>
                    let e1_as = gen_stmt(
                        tree.child.get(0).expect("Conditional expression no e1"),
                        index_map,
                        idx,
                        lbb,
                        leb,
                        loop_in_label,
                        loop_out_label,
                        &global_variable_scope,
                    );
                    let e2_as = gen_stmt(
                        tree.child.get(1).expect("conditional expression no e2"),
                        index_map,
                        idx,
                        lbb,
                        leb,
                        loop_in_label,
                        loop_out_label,
                        &global_variable_scope,
                    );
                    let e3_as = gen_stmt(
                        tree.child.get(2).expect("conditional expression no e3"),
                        index_map,
                        idx,
                        lbb,
                        leb,
                        loop_in_label,
                        loop_out_label,
                        &global_variable_scope,
                    );

                    let label_e3 = gen_labels("E3");
                    let label_end = gen_labels("ENDCOND");
                    format!(
                        "{}\
                        {}cmpq $0, %rax\n\
                        {}je {}\n\
                        {}\
                        {}jmp {}\n\
                        {}:\n\
                        {}\
                        {}:\n",
                        e1_as, p, p, label_e3, e2_as, p, label_end, label_e3, e3_as, label_end,
                    )
                } else {
                    panic!("Error: something wrong in conditional expression")
                }
            }
            NodeType::FnCall(fn_name) => {
                // now change to x64 calling convetion
                // arguments: 1st 2nd 3rd 4th 5th 6th ...
                //            rdi rsi rdx rcx r8  r9  stack
                // iter every expression in reverse direction
                // and then push them in stack
                let mut s: String = String::new();
                // should follow AMD System V ABI,
                // The begin of main function stack is aligned 8,
                // And end of the input argument area shall be aligned on a 16 (32, if __m256 is passed on stack) byte boundary.
                // so if we have n local variables, we pushed them into the stack.
                // and we need to store r10 and r11, and we should put argument with index bigger than 6 into stack
                // so the total element pushed into stack should be (n + 2 + (arg_list.len() - 6 > 0 ? arg_list.len() - 6 : 0))
                // if this value % 2 == 1, then we should push one element into stack.
                // Now I only handled this in main function, and I should also track the stack align for every function that we defined,
                // so we can make sure every function follows the ABI

                // first judge whether we need to push one extra element into stack
                let tmp = match tree.child.len() {
                    0...6 => 0,
                    _ => tree.child.len() - 6,
                };
                if cfg!(feature="debug") {
                    println!("index_map.len() = {}", index_map.len());
                    println!("tmp = {}", tmp);
                }
                let extra: bool = match (index_map.len() + tmp + 2) % 2 {
                    0 => false,
                    _ => true,
                };

                if extra == true {
                    // then we need to add one element to stack to make sure follow the abi
                    s.push_str(&format!("{}pushq $0\n", p));
                }

                //then save the caller saves regs: r10, r11
                s.push_str(&format!("{}pushq %r10\n", p));
                s.push_str(&format!("{}pushq %r11\n", p));

                // mov argument into registers or stack if it's 7th element or later argument
                let regs: Vec<&'static str> = vec!["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
                for i in 0..tree.child.len() {
                    s.push_str(&gen_stmt(
                        tree.child.get(i).unwrap(),
                        index_map,
                        idx,
                        lbb,
                        leb,
                        loop_in_label,
                        loop_out_label,
                        &global_variable_scope,
                    ));
                    if i >= 6 {
                        // store in stack
                        s.push_str(&format!("{}pushq %rax\n", p));
                    } else {
                        // store into regs.
                        s.push_str(&format!(
                            "{}movq %rax, {}\n{}movq $0, %rax\n",
                            p, regs[i], p
                        ));
                    }
                }
                // call the function
                s.push_str(&format!("{}call {}@PLT\n", p, fn_name));
                // after the callee function returns, remove the arguments from stack
                if tree.child.len() > 6 {
                    s.push_str(&format!(
                        "{}addq ${}, %rsp # remove the arguments\n",
                        p,
                        8 * (tree.child.len() - 6)
                    ));
                }
                s.push_str(&format!("{}popq %r11\n", p));
                s.push_str(&format!("{}popq %r10\n", p));

                if extra == true {
                    s.push_str(&format!("{}addq $8, %rsp\n", p));
                }
                s
            }
            NodeType::Stmt(stmt) => match stmt {
                StmtType::Return => format!(
                    "{}\
                    {}\
                    {}ret\n",
                    gen_stmt(
                        tree.child.get(0).expect("Statement node no child"),
                        index_map,
                        idx,
                        lbb,
                        leb,
                        loop_in_label,
                        loop_out_label,
                        &global_variable_scope,
                    ),
                    gen_fn_epilogue(),
                    p
                ),
                StmtType::Conditional(_) => {
                    let e1_as = gen_stmt(
                        tree.child.get(0).expect("Conditional node no e1"),
                        index_map,
                        idx,
                        lbb,
                        leb,
                        loop_in_label,
                        loop_out_label,
                        &global_variable_scope,
                    );
                    let s1_as = gen_stmt(
                        tree.child.get(1).expect("conditional node no s1"),
                        index_map,
                        idx,
                        lbb,
                        leb,
                        loop_in_label,
                        loop_out_label,
                        &global_variable_scope,
                    );
                    let s2_as: String = if tree.child.len() == 2 {
                        "".to_string()
                    } else {
                        gen_stmt(
                            tree.child.get(2).expect("conditional node no s2"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        )
                    };
                    let label_s2 = gen_labels("S2");
                    let label_end = gen_labels("ENDIF");
                    format!(
                        "{}\
                        {}cmpq $0, %rax\n\
                        {}je {}\n\
                        {}\
                        {}jmp {}\n\
                        {}:\n\
                        {}\
                        {}:\n",
                        e1_as, p, p, label_s2, s1_as, p, label_end, label_s2, s2_as, label_end,
                    )
                }
                StmtType::Exp => gen_stmt(
                    tree.child.get(0).expect("Statement Node no child"),
                    index_map,
                    idx,
                    lbb,
                    leb,
                    loop_in_label,
                    loop_out_label,
                    &global_variable_scope,
                ),
                StmtType::Continue => match loop_in_label {
                    Some(l) => format!("{}jmp {} # Continue\n", p, l),
                    None => panic!("Continue should be in the loop scope"),
                },
                StmtType::Break => match loop_out_label {
                    Some(l) => format!("{}jmp {} # Break\n", p, l),
                    None => panic!("Break shoule be in the loop scope"),
                },
                StmtType::For | StmtType::ForDecl => {
                    gen_for(tree, index_map, idx, &global_variable_scope)
                }
                StmtType::Do => {
                    // LBB.
                    // stmt
                    // exp
                    // cmpq $1, %rax
                    // je  LBB
                    // LEB
                    let lbb = gen_labels("BDO");
                    let leb = gen_labels("EDO");
                    let scope: HashMap<String, bool> = HashMap::new();
                    let stmts = gen_block(
                        tree.child.get(0).unwrap(),
                        index_map,
                        &scope,
                        idx,
                        loop_in_label,
                        loop_out_label,
                        true,
                        false,
                        &global_variable_scope,
                    ); // should enter a new scope
                    let exp = gen_stmt(
                        tree.child.get(1).unwrap(),
                        index_map,
                        idx,
                        &lbb,
                        &leb,
                        Some(&lbb),
                        Some(&leb),
                        &global_variable_scope,
                    );
                    format!(
                        "{}:\n\
                        {}\
                        {}\
                        {}cmpq $1, %rax\n\
                        {}je   {}\n\
                        {}:\n",
                        lbb, stmts, exp, p, p, lbb, leb
                    )
                }
                StmtType::While => {
                    // LBB.
                    // exp
                    // cmpq $1, %rax
                    // jne LEB
                    // stmt
                    // jmp LBB
                    // LEB.
                    let lbb = gen_labels("BWHILE");
                    let leb = gen_labels("EWHILE");
                    let scope: HashMap<String, bool> = HashMap::new();
                    let exp = gen_stmt(
                        tree.child.get(0).unwrap(),
                        index_map,
                        idx,
                        &lbb,
                        &leb,
                        Some(&lbb),
                        Some(&leb),
                        &global_variable_scope,
                    );
                    let stmts = gen_block(
                        tree.child.get(1).unwrap(),
                        index_map,
                        &scope,
                        idx,
                        Some(&lbb),
                        Some(&leb),
                        true,
                        false,
                        &global_variable_scope,
                    ); // should enter a new scope
                    format!(
                        "{}:\n\
                        {}\
                        {}cmpq $1, %rax\n\
                        {}jne {}\n\
                        {}\
                        {}jmp {}\n\
                        {}:\n",
                        lbb, exp, p, p, leb, stmts, p, lbb, leb
                    )
                }
                StmtType::Compound => {
                    let scope: HashMap<String, bool> = HashMap::new();
                    gen_block(
                        tree,
                        index_map,
                        &scope,
                        idx,
                        loop_in_label,
                        loop_out_label,
                        true,
                        false,
                        &global_variable_scope,
                    )
                }
            },
            NodeType::ArrayRef(var_name) => {
                let get_index = gen_stmt(
                    tree.child
                        .get(0)
                        .expect("Statement::Declare Node has no child"),
                    index_map,
                    idx,
                    lbb,
                    leb,
                    loop_in_label,
                    loop_out_label,
                    &global_variable_scope,
                );
                // get index => rdx,
                // movq array_index var@GOTPCREL(%rip) => %rbx
                // movq (%rbx, rdx, data size), %rax
                format!(
                    "{}\
                    {}pushq %rdx\n\
                    {}pushq %rbx\n\
                    {}movq %rax, %rdx\n\
                    {}movq {}@GOTPCREL(%rip), %rbx\n\
                    {}movq (%rbx, %rdx, 8), %rax\n\
                    {}popq %rbx\n\
                    {}popq %rdx\n",
                    get_index, p, p, p, p, var_name, p, p, p,
                )
            }
            NodeType::AssignNode(var_name, true) => {
                match index_map.get(var_name) {
                    None => {
                        // not in current scope, try to search global scope
                        match global_variable_scope.contains(var_name) {
                            true => {
                                // declared in global scope, that's ok
                                let get_index = gen_stmt(
                                    tree.child
                                        .get(0)
                                        .expect("Statement::Declare Node has no child"),
                                    index_map,
                                    idx,
                                    lbb,
                                    leb,
                                    loop_in_label,
                                    loop_out_label,
                                    &global_variable_scope,
                                );
                                let get_res = gen_stmt(
                                    tree.child.get(1).unwrap(),
                                    index_map,
                                    idx,
                                    lbb,
                                    leb,
                                    loop_in_label,
                                    loop_out_label,
                                    &global_variable_scope,
                                );
                                // movq array_index var@GOTPCREL(%rip) => %rbx
                                // get index => rdx,
                                // get res => rax
                                // movq %rax, (%rbx, rdx, data size)
                                format!(
                                    "{}\
                                    {}movq %rax, %rdx\n\
                                    {}\
                                    {}movq {}@GOTPCREL(%rip), %rbx\n\
                                    {}movq %rax, (%rbx, %rdx, 8)\n",
                                    get_index, p, get_res, p, var_name, p,
                                )
                            }
                            false => {
                                // Not declared before, that's not ok
                                panic!("Error: Use un-declared variable `{}`", var_name)
                            }
                        }
                    }
                    Some(t) => {
                        // declared before, that's ok
                        let e1 = gen_stmt(
                            tree.child
                                .get(0)
                                .expect("Statement::Declare Node has no child"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        );
                        let get_result = index_map.get(var_name);
                        let mut va_offset: isize = -8;
                        match get_result {
                            Some(t) => {
                                va_offset = *t;
                            }
                            None => panic!("Something went wrong in gen::gen_stmt()"),
                        }
                        format!(
                            "{}\
                            {}movq %rax, {}(%rbp)\n",
                            e1, p, va_offset
                        )
                    }
                }
            }
            NodeType::AssignNode(var_name, false) => {
                // assign to int variable
                match index_map.get(var_name) {
                    None => {
                        // not in current scope, try to search global scope
                        match global_variable_scope.contains(var_name) {
                            true => {
                                // declared in global scope, that's ok
                                let e1 = gen_stmt(
                                    tree.child
                                        .get(0)
                                        .expect("Statement::Declare Node has no child"),
                                    index_map,
                                    idx,
                                    lbb,
                                    leb,
                                    loop_in_label,
                                    loop_out_label,
                                    &global_variable_scope,
                                );
                                format!(
                                    "{}\
                                    {}movq %rax, {}(%rip)\n",
                                    e1, p, var_name
                                )
                            }
                            false => {
                                // Not declared before, that's not ok
                                panic!("Error: Use un-declared variable `{}`", var_name)
                            }
                        }
                    }
                    Some(t) => {
                        // declared before, that's ok
                        let e1 = gen_stmt(
                            tree.child
                                .get(0)
                                .expect("Statement::Declare Node has no child"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        );
                        let get_result = index_map.get(var_name);
                        let mut va_offset: isize = -8;
                        match get_result {
                            Some(t) => {
                                va_offset = *t;
                            }
                            None => panic!("Something went wrong in gen::gen_stmt()"),
                        }
                        format!(
                            "{}\
                            {}movq %rax, {}(%rbp)\n",
                            e1, p, va_offset
                        )
                    }
                }
            }
            NodeType::UnExp(op) => match op {
                TokType::Addr => format!(
                    // put address of the factor in %rax
                    "{}", gen_addr(
                        tree.child.get(0).expect("Addressing node no child"),
                        index_map,
                        idx,
                        lbb,
                        leb,
                        loop_in_label,
                        loop_out_label,
                        global_variable_scope)
                ),
                TokType::Minus => format!(
                    "{}\
                    {}neg %rax\n",
                    gen_stmt(
                        tree.child.get(0).expect("UnExp<-> no child"),
                        index_map,
                        idx,
                        lbb,
                        leb,
                        loop_in_label,
                        loop_out_label,
                        &global_variable_scope,
                    ),
                    p
                ),
                TokType::Tilde => format!(
                    "{}\
                    {}not %rax\n",
                    gen_stmt(
                        tree.child.get(0).expect("UnExp<~> no child"),
                        index_map,
                        idx,
                        lbb,
                        leb,
                        loop_in_label,
                        loop_out_label,
                        &global_variable_scope,
                    ),
                    p
                ),
                TokType::Exclamation => format!(
                    "{}\
                    {}cmp  $0, %rax\n\
                    {}movq $0, %rax\n\
                    {}sete %al\n",
                    gen_stmt(
                        tree.child.get(0).expect("UnExp<!> node no child"),
                        index_map,
                        idx,
                        lbb,
                        leb,
                        loop_in_label,
                        loop_out_label,
                        &global_variable_scope,
                    ),
                    p,
                    p,
                    p
                ),
                _ => panic!(format!(
                    "Unary Operator `{:?}` not implemented in gen::gen_unexp()\n",
                    op
                )),
            },
            NodeType::BinExp(op) => {
                match op {
                    TokType::Plus => format!(
                        "{}\
                        {}pushq %rax\n\
                        {}\
                        {}popq %rcx\n\
                        {}addq %rcx, %rax\n",
                        gen_stmt(
                            tree.child.get(0).expect("BinExp has no lhs"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        gen_stmt(
                            tree.child.get(1).expect("BinExp has no rhs"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        p
                    ),
                    TokType::Minus => format!(
                        "{}\
                        {}pushq %rax\n\
                        {}\
                        {}popq %rcx\n\
                        {}subq %rcx, %rax\n", // subl src, dst : dst - src -> dst
                        //   let %rax = dst = e1, %rcx = src = e2
                        gen_stmt(
                            tree.child.get(1).expect("BinExp has no rhs"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        gen_stmt(
                            tree.child.get(0).expect("BinExp has no lhs"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        p
                    ),
                    TokType::Multi => format!(
                        "{}\
                        {}pushq %rax\n\
                        {}\
                        {}popq %rcx\n\
                        {}imul %rcx, %rax\n",
                        gen_stmt(
                            tree.child.get(0).expect("BinExp has no lhs"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        gen_stmt(
                            tree.child.get(1).expect("BinExp has no rhs"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        p
                    ),
                    TokType::Splash => format!(
                        "{}\
                        {}pushq %rax\n\
                        {}\
                        {}popq %rcx\n\
                        {}xorq %rdx, %rdx\n\
                        {}idivq %rcx\n",
                        // let eax = e1, edx = 0, ecx = e2
                        gen_stmt(
                            tree.child.get(1).expect("BinExp has no rhs"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        gen_stmt(
                            tree.child.get(0).expect("BinExp has no lhs"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        p,
                        p
                    ),
                    TokType::Equal => format!(
                        "{}\
                        {}pushq %rax\n\
                        {}\
                        {}popq %rcx\n\
                        {}cmpq %rax, %rcx # set ZF on if %rax == %rcx, set it off otherwise\n\
                        {}movq $0, %rax   # zero out EAX, does not change flag\n\
                        {}sete %al\n",
                        gen_stmt(
                            tree.child.get(0).expect("BinExp<==> node no child"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        gen_stmt(
                            tree.child.get(1).expect("BinExp<==> node no child"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        p,
                        p,
                        p
                    ),
                    TokType::NotEqual => format!(
                        "{}\
                        {}pushq %rax\n\
                        {}\
                        {}popq %rcx\n\
                        {}cmpq %rax, %rcx # set ZF on if %rax == %rcx, set it off otherwise\n\
                        {}movq $0, %rax   # zero out EAX, does not change flag\n\
                        {}setne %al\n",
                        gen_stmt(
                            tree.child.get(0).expect("BinExp<==> node no child"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        gen_stmt(
                            tree.child.get(1).expect("BinExp<==> node no child"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        p,
                        p,
                        p
                    ),
                    TokType::LessEqual => format!(
                        "{}\
                        {}pushq %rax\n\
                        {}\
                        {}popq %rcx\n\
                        {}cmpq %rax, %rcx # set ZF on if %rax == %rcx, set it off otherwise\n\
                        {}movq $0, %rax   # zero out EAX, does not change flag\n\
                        {}setle %al\n",
                        gen_stmt(
                            tree.child.get(0).expect("BinExp<==> node no child"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        gen_stmt(
                            tree.child.get(1).expect("BinExp<==> node no child"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        p,
                        p,
                        p
                    ),
                    TokType::GreaterEqual => format!(
                        "{}\
                        {}pushq %rax\n\
                        {}\
                        {}popq %rcx\n\
                        {}cmpq %rax, %rcx # set ZF on if %rax == %rcx, set it off otherwise\n\
                        {}movq $0, %rax   # zero out EAX, does not change flag\n\
                        {}setge %al\n",
                        gen_stmt(
                            tree.child.get(0).expect("BinExp<==> node no child"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        gen_stmt(
                            tree.child.get(1).expect("BinExp<==> node no child"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        p,
                        p,
                        p
                    ),
                    TokType::Or => {
                        let clause2_label = gen_labels("CLAUSE");
                        let end_label = gen_labels("END");
                        format!(
                            "{}\
                            {}cmpq $0, %rax\n\
                            {}je {}\n\
                            {}movq $1, %rax\n\
                            {}jmp {}\n\
                            {}:\n\
                            {}\
                            {}cmpq $0, %rax\n\
                            {}movq $0, %rax\n\
                            {}setne %al\n\
                            {}: # end of clause here\n",
                            gen_stmt(
                                tree.child.get(0).expect("BinExp<||> node no child"),
                                index_map,
                                idx,
                                lbb,
                                leb,
                                loop_in_label,
                                loop_out_label,
                                &global_variable_scope,
                            ),
                            p,
                            p,
                            clause2_label,
                            p,
                            p,
                            end_label,
                            clause2_label,
                            gen_stmt(
                                tree.child.get(1).expect("BinExp<||> node no child"),
                                index_map,
                                idx,
                                lbb,
                                leb,
                                loop_in_label,
                                loop_out_label,
                                &global_variable_scope,
                            ),
                            p,
                            p,
                            p,
                            end_label
                        )
                    }
                    TokType::And => {
                        let clause2_label = gen_labels("clause");
                        let end_label = gen_labels("end");
                        format!(
                            "{}\
                            {}cmpq $0, %rax\n\
                            {}jne {}\n\
                            {}jmp {}\n\
                            {}:\n\
                            {}\
                            {}cmpq $0, %rax\n\
                            {}movq $0, %rax\n\
                            {}setne %al\n\
                            {}: # end of clause here\n",
                            gen_stmt(
                                tree.child.get(0).expect("BinExp<||> node no child"),
                                index_map,
                                idx,
                                lbb,
                                leb,
                                loop_in_label,
                                loop_out_label,
                                &global_variable_scope,
                            ),
                            p,
                            p,
                            clause2_label,
                            p,
                            end_label,
                            clause2_label,
                            gen_stmt(
                                tree.child.get(1).expect("BinExp<||> node no child"),
                                index_map,
                                idx,
                                lbb,
                                leb,
                                loop_in_label,
                                loop_out_label,
                                &global_variable_scope,
                            ),
                            p,
                            p,
                            p,
                            end_label
                        )
                    }
                    TokType::Lt => format!(
                        "{}\
                        {}pushq %rax\n\
                        {}\
                        {}popq %rcx\n\
                        {}cmpq %rax, %rcx # set ZF on if %rax == %rcx, set it off otherwise\n\
                        {}movq $0, %rax   # zero out EAX, does not change flag\n\
                        {}setl %al\n",
                        gen_stmt(
                            tree.child.get(0).expect("BinExp<==> node no child"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        gen_stmt(
                            tree.child.get(1).expect("BinExp<==> node no child"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        p,
                        p,
                        p
                    ),
                    TokType::Gt => format!(
                        "{}\
                        {}pushq %rax\n\
                        {}\
                        {}popq %rcx\n\
                        {}cmpq %rax, %rcx # set ZF on if %rax == %rcx, set it off otherwise\n\
                        {}movq $0, %rax   # zero out EAX, does not change flag\n\
                        {}setg %al\n",
                        gen_stmt(
                            tree.child.get(0).expect("BinExp<==> node no child"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        gen_stmt(
                            tree.child.get(1).expect("BinExp<==> node no child"),
                            index_map,
                            idx,
                            lbb,
                            leb,
                            loop_in_label,
                            loop_out_label,
                            &global_variable_scope,
                        ),
                        p,
                        p,
                        p,
                        p
                    ),
                    _ => panic!(format!(
                        "Error: Binary Operator `{:?}` not implemented in gen::gen_binexp()\n",
                        op
                    )),
                }
            }
            NodeType::Const(n) => format!("{}movq ${}, %rax\n", p, n),
            NodeType::Var(var_name) => {
                let var_offset = index_map.get(var_name);
                match var_offset {
                    Some(t) => {
                        let var_offset = t;
                        format!("{}movq {}(%rbp), %rax\n", p, var_offset)
                    }
                    None => {
                        // try to search global scope
                        match global_variable_scope.contains(var_name) {
                            true => {
                                // in global scope
                                let var_offset = var_name;
                                format!("{}movq {}(%rip), %rax\n", p, var_offset)
                            }
                            false => panic!(format!("Use of undeclared variable `{}`", var_name)),
                        }
                    }
                }
            }
            NodeType::ExpOption => {
                if tree.child.len() == 1 {
                    gen_stmt(
                        tree.child
                            .get(0)
                            .expect(&format!("{:?} node no child", &tree.entry)),
                        index_map,
                        idx,
                        lbb,
                        leb,
                        loop_in_label,
                        loop_out_label,
                        &global_variable_scope,
                    )
                } else {
                    // null exp
                    // movq 1, %rax
                    format!("{}movq $1, %rax\n", p)
                }
            }
            NodeType::EqualityExp
            | NodeType::RelationalExp
            | NodeType::Term
            | NodeType::Exp
            | NodeType::Factor
            | NodeType::AdditiveExp
            | NodeType::LogicalOrExp
            | NodeType::Block
            | NodeType::LogicalAndExp => gen_stmt(
                tree.child
                    .get(0)
                    .expect(&format!("{:?} node no child", &tree.entry)),
                index_map,
                idx,
                lbb,
                leb,
                loop_in_label,
                loop_out_label,
                &global_variable_scope,
            ),
            _ => panic!(format!(
                "Node `{:?}` not implemented in gen::gen_stmt()\n",
                &tree.entry
            )),
        }
    }
}

pub mod lexer
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    */
    #[derive(Eq, PartialEq, Clone, Debug)]
    pub enum KwdType 
    {
        Int,      // int
        Void,     // void
        Ret,      // return
        If,       // if
        Else,     // else
        While,    // while
        For,      // for
        Do,       // do
        Break,    // break
        Continue, // continue
    }

    #[derive(Eq, PartialEq, Clone, Debug)]
    pub enum TokType 
    {
        Kwd(KwdType),
        LBrace,             // {
        RBrace,             // }
        LParen,             // (
        RParen,             // )
        LBracket,           // [
        RBracket,           // ]
        Semicolon,          // ;
        Assign,             // =
        Lt,                 // <
        Gt,                 // >
        Minus,              // -
        Tilde,              // ~
        Exclamation,        // !
        Plus,               // +
        Multi,              // *
        Splash,             // /
        Literal(i64),       // [0-9]+
        Identifier(String), // identifier
        And,                // &&
        Or,                 // ||
        Equal,              // ==
        NotEqual,           // !=
        LessEqual,          // <=
        GreaterEqual,       // >=
        Colon,              // :
        QuestionMark,       // ?
        Comma,              // ,
        String(String, String),
        Addr,               // &var
    }

    static mut LABEL_COUNTER: i64 = -1;
    fn gen_string_tag() -> String {
        unsafe {
            LABEL_COUNTER = LABEL_COUNTER + 1;
            return format!(".LSTR{}", LABEL_COUNTER);
        }
    }
    pub fn lex(input: &str) -> Result<Vec<TokType>, String> {
        let mut result = Vec::new();

        let mut it = input.chars().peekable();

        while let Some(&c) = it.peek() {
            match c {
                '"' => {
                    it.next();
                    let mut s = "".to_string();
                    loop {
                        let &c = it.peek().unwrap();
                        if c == '"' {
                            break;
                        }
                        s.push(c);
                        it.next();
                    }
                    result.push(TokType::String(s, gen_string_tag()));
                    it.next();
                }
                '\'' => {
                    // try parse a char
                    // now just use int to represent char
                    // transform it to int
                    it.next(); // skip '
                    let &c = it.peek().unwrap();
                    if c == '\'' {
                        return Err(format!("Error: empty character constant"));
                    }
                    if c == '\\' {
                        it.next();
                        let &c = it.peek().unwrap();
                        match c {
                            'a' => {
                                result.push(TokType::Literal(0x07));
                            } // Alert (Beep, Bell) (added in C89)
                            'b' => {
                                result.push(TokType::Literal(0x08));
                            } // Backspace
                            'e' => {
                                result.push(TokType::Literal(0x1B));
                            } // escape character
                            'f' => {
                                result.push(TokType::Literal(0x0C));
                            } // Formfeed Page Break
                            'n' => {
                                result.push(TokType::Literal(0x0A));
                            } // Newline (Line Feed)
                            'r' => {
                                result.push(TokType::Literal(0x0D));
                            } // Carriage Return
                            't' => {
                                result.push(TokType::Literal(0x09));
                            } // Horizontal Tab
                            'v' => {
                                result.push(TokType::Literal(0x0B));
                            } // Vertical Tab
                            '\\' => {
                                result.push(TokType::Literal(0x5C));
                            } // Backslash
                            '\'' => {
                                result.push(TokType::Literal(0x27));
                            } // Apostrophe or single quotation mark
                            '\"' => {
                                result.push(TokType::Literal(0x22));
                            } // Double quotation mark
                            '?' => {
                                result.push(TokType::Literal(0x3F));
                            } // question mark
                            _ => {
                                return Err(format!("unrecongnized character"));
                            }
                        }
                        it.next();
                        if it.peek().unwrap() != &'\'' {
                            return Err(format!("Error: unmatched '"));
                        }
                        it.next();
                    } else {
                        result.push(TokType::Literal(c as i64));
                        it.next(); // skip char
                        it.next(); // skip '
                    }
                }
                '0'...'9' => {
                    it.next();
                    let mut number = c
                        .to_string()
                        .parse::<i64>()
                        .expect("The caller should have passed a digit.");

                    while let Some(Ok(digit)) = it.peek().map(|c| c.to_string().parse::<i64>()) {
                        number = number * 10 + digit;
                        it.next();
                    }
                    result.push(TokType::Literal(number));
                }
                'a'...'z' | 'A'...'Z' | '_' => {
                    it.next();
                    let mut s = String::new();
                    s.push(c);
                    while let Some(&tmp) = it.peek() {
                        match tmp {
                            'a'...'z' | 'A'...'Z' | '_' => {
                                s.push(tmp);
                                it.next();
                            }
                            _ => {
                                break;
                            }
                        }
                    }
                    match s.as_ref() {
                        "int" => result.push(TokType::Kwd(KwdType::Int)),
                        "char" => result.push(TokType::Kwd(KwdType::Int)),
                        "return" => result.push(TokType::Kwd(KwdType::Ret)),
                        "void" => result.push(TokType::Kwd(KwdType::Void)),
                        "if" => result.push(TokType::Kwd(KwdType::If)),
                        "else" => result.push(TokType::Kwd(KwdType::Else)),
                        "while" => result.push(TokType::Kwd(KwdType::While)),
                        "for" => result.push(TokType::Kwd(KwdType::For)),
                        "do" => result.push(TokType::Kwd(KwdType::Do)),
                        "continue" => result.push(TokType::Kwd(KwdType::Continue)),
                        "break" => result.push(TokType::Kwd(KwdType::Break)),
                        _ => result.push(TokType::Identifier(s)),
                    }
                }
                '(' => {
                    result.push(TokType::LParen);
                    it.next();
                }
                ')' => {
                    result.push(TokType::RParen);
                    it.next();
                }
                '{' => {
                    result.push(TokType::LBrace);
                    it.next();
                }
                '}' => {
                    result.push(TokType::RBrace);
                    it.next();
                }
                '[' => {
                    result.push(TokType::LBracket);
                    it.next();
                }
                ']' => {
                    result.push(TokType::RBracket);
                    it.next();
                }
                ';' => {
                    result.push(TokType::Semicolon);
                    it.next();
                }
                '=' => {
                    it.next();
                    match it.peek() {
                        Some(tmp) => match tmp {
                            '=' => {
                                result.push(TokType::Equal);
                                it.next();
                            }
                            '>' => {
                                result.push(TokType::GreaterEqual);
                                it.next();
                            }
                            _ => {
                                result.push(TokType::Assign);
                            }
                        },
                        _ => return Err(format!("Can not peek next char")),
                    }
                }
                '<' => {
                    it.next();
                    match it.peek() {
                        Some(tmp) => match tmp {
                            '=' => {
                                it.next();
                                result.push(TokType::LessEqual);
                                it.next();
                            }
                            _ => {
                                result.push(TokType::Lt);
                                it.next();
                            }
                        },
                        _ => return Err(format!("Can not peek next char")),
                    }
                }
                '>' => {
                    it.next();
                    match it.peek() {
                        Some(tmp) => match tmp {
                            '=' => {
                                result.push(TokType::GreaterEqual);
                                it.next();
                            }
                            _ => {
                                result.push(TokType::Gt);
                                it.next();
                            }
                        },
                        _ => return Err(format!("Can not peek next char")),
                    }
                }
                '-' => {
                    result.push(TokType::Minus);
                    it.next();
                }
                '~' => {
                    result.push(TokType::Tilde);
                    it.next();
                }
                '!' => {
                    it.next();
                    match it.peek() {
                        Some(tmp) => match tmp {
                            '=' => {
                                result.push(TokType::NotEqual);
                                it.next();
                            }
                            _ => {
                                result.push(TokType::Exclamation);
                            }
                        },
                        _ => return Err(format!("Can not peek next char")),
                    }
                }
                '+' => {
                    result.push(TokType::Plus);
                    it.next();
                }
                '*' => {
                    result.push(TokType::Multi);
                    it.next();
                }
                '/' => {
                    result.push(TokType::Splash);
                    it.next();
                }
                '&' => {
                    it.next();
                    match it.peek() {
                        Some(tmp) => match tmp {
                            '&' => {
                                result.push(TokType::And);
                                it.next();
                            }
                            _ => {
                                // now don't support bitwise and, so just return Err
                                // & operator to get the address of a variable
                                result.push(TokType::Addr);
                            }
                        },
                        _ => return Err(format!("Can not peek next char")),
                    }
                }
                '|' => {
                    it.next();
                    match it.peek() {
                        Some(tmp) => match tmp {
                            '|' => {
                                result.push(TokType::Or);
                                it.next();
                            }
                            _ => {
                                // now don't support bitwise or, so just return Err
                                return Err(format!("unexpected token {}", c));
                            }
                        },
                        _ => return Err(format!("Can not peek next char")),
                    }
                }
                '?' => {
                    result.push(TokType::QuestionMark);
                    it.next();
                }
                ':' => {
                    result.push(TokType::Colon);
                    it.next();
                }
                ',' => {
                    result.push(TokType::Comma);
                    it.next();
                }
                ' ' | '\n' | '\t' | '\r' => {
                    // skip
                    it.next();
                }
                _ => {
                    return Err(format!("unexpected character {}", c));
                }
            }
        }
        Ok(result)
    }
}

pub mod opts
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    */
    mod arg_groups
    {
        /*!
        */
        use ::
        {
            *,
        };
        /*
        pub fn stop_stage_conflict_resolver_arg_group() -> clap::ArgGroup<'static> {
            clap::ArgGroup::with_name("stop_stage_conflict_resolver")
        }
        */
    }
    /*
    #[derive(structopt::StructOpt, Clone, Debug)]
    pub struct Opts {
        #[structopt(flatten)]
        crust_debug_flags: CrustDebugFlags,
        /// The input files(s)
        #[structopt(parse(from_os_str), required = true)]
        input: Vec<path::PathBuf>,
        /// The output file
        #[structopt(short = "-o", parse(from_os_str))]
        output: path::PathBuf,
        #[structopt(flatten)]
        stop_stage: StopStage,
    }
    
    #[derive(structopt::StructOpt, Clone, Copy, Debug)]
    pub struct CrustDebugFlags {
        /// Print file contents
        #[structopt(long = "--crust-print-file-contents")]
        print_file_contents: bool,
        /// Print filenames as they are processed
        #[structopt(long = "--crust-print-filenames")]
        print_filenames: bool,
        /// Print the source file ast.
        #[structopt(long = "--crust-print-source-ast")]
        print_source_ast: bool,
    }
    
    #[derive(structopt::StructOpt, Clone, Copy, Debug)]
    #[structopt(raw(group = "self::arg_groups::stop_stage_conflict_resolver_arg_group()"))]
    pub struct StopStage {
        /// Stop after the assembly stage
        #[structopt(group = "stop_stage_conflict_resolver", short = "-c")]
        assemble: bool,
        /// Stop after the compilation stage
        #[structopt(group = "stop_stage_conflict_resolver", short = "-S")]
        compile: bool,
        /// Stop after the preprocessing stage
        #[structopt(group = "stop_stage_conflict_resolver", short = "-E")]
        preprocess: bool,
    }

    impl Opts {
        pub fn crust_debug_flags(&self) -> CrustDebugFlags {
            self.crust_debug_flags
        }

        pub fn input(&self) -> &[path::PathBuf] {
            &self.input
        }

        pub fn output(&self) -> &path::PathBuf {
            &self.output
        }

        pub fn stop_stage(&self) -> StopStage {
            self.stop_stage
        }
    }

    impl CrustDebugFlags {
        pub fn print_file_contents(&self) -> bool {
            self.print_file_contents
        }

        pub fn print_filenames(&self) -> bool {
            self.print_filenames
        }

        pub fn print_source_ast(&self) -> bool {
            self.print_source_ast
        }
    }

    impl StopStage {
        pub fn assemble(&self) -> bool {
            self.assemble
        }

        pub fn compile(&self) -> bool {
            self.compile
        }

        pub fn preprocess(&self) -> bool {
            self.preprocess
        }
    } */
}

pub mod parser
{
    /*!
    */
    use ::
    {
        *,
    };
    /*
    */
    #[derive(Eq, PartialEq, Clone, Debug)]
    pub enum NodeType 
    {
        Prog(String),
        // TODO: now only support int parameters
        // <function> ::= "int" <id> "(" [ "int" <id> { "," "int" <id> } ] ")" "{" {<block-item>} "}"
        Fn(String, Option<Vec<String>>),
        Stmt(StmtType),
        // <statement> ::= "return" <exp> ";"
        //               | <exp-option> ";"
        //               | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
        //               | "{" { <block-item> } "}
        //               | "for" "(" <exp-option> ";" <exp-option> ";" <exp-option> ")" <statement>
        //               | "for" "(" <declaration> <exp-option> ";" <exp-option> ")" <statement>
        //               | "while" "(" <exp> ")" <statement>
        //               | "do" <statement> "while" <exp> ";"
        //               | "break" ";"
        //               | "continue" ";"
        Block, // <block> ::= <statement> | <declaration>
        Const(i64),
        StringLiteral(String, String), // data, tag
        Var(String),
        ArrayRef(String),          // referencing to array
        AssignNode(String, bool), // String -> variable name, bool -> true if this is a assign to array element
        UnExp(lexer::TokType),    // Unary Expression
        BinExp(lexer::TokType),   // Binary Operator
        Exp,                      // <exp> ::= <id> ["[" <exp> "]"] "=" <exp> | <conditional-exp>
        ExpOption,                // <exp-option> :: <exp> | ""
        ConditionalExp, // <conditional-exp> ::= <logical-or-exp> [ "?" <exp> ":" <conditional-exp> ]
        LogicalOrExp,   // <logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
        LogicalAndExp,  // <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
        EqualityExp,    // <EqualityExp> ::= <relational-exp> { ("!="|"==") <relational-exp> }
        RelationalExp, // <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
        AdditiveExp,   // <additive-exp> ::= <term> { ("+" | "-") <term> }
        Term,          // <term> ::= <factor> { ("*" | "/") <factor> }
        Factor, // <factor> ::= <function-call> | "(" <exp> ")" | <unary_op> <factor> | <int> | string | <id> "[" <exp> "]" | <id>
        FnCall(String), // <function-call> ::= id "(" [ <exp> { "," <exp> } ] ")"
        Declare(String, DataType), // <declaration> ::= "int" <id> "[" <int> "]" ";" | "int" <id> [ = <exp> ] ";"
    }

    #[derive(Eq, PartialEq, Clone, Debug)]
    pub enum DataType {
        I64,        // now int in c was translated in 64 bits int
        Arr64(i64), // int array[len]
    }

    #[derive(Eq, PartialEq, Clone, Debug)]
    pub enum StmtType {
        Return,
        Exp,
        Conditional(String),
        Compound,
        For,     // kids: exp-opion, exp-option, exp-option
        ForDecl, // kids: declaration, exp, exp-option, statement
        While,   // kids: exp, stmt
        Do,      // kids: stmt, exp
        Break,
        Continue,
    }

    #[derive(Eq, PartialEq, Clone, Debug)]
    pub struct ParseNode {
        pub child: Vec<ParseNode>,
        pub entry: NodeType,
    }

    impl ParseNode {
        pub fn new() -> ParseNode {
            ParseNode {
                child: Vec::new(),
                entry: NodeType::Prog("root".to_string()),
            }
        }
    }

    fn p_logical_or_exp(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        let mut log_or_exp_node = ParseNode::new();
        log_or_exp_node.entry = NodeType::LogicalOrExp;
        // Parse <logical-and-exp> first

        // <LogicalOrExp> -> <LogicalAndExp>
        let mut pos = pos;
        let (log_and_exp_node, tmp_pos) = r#try!(p_logical_and_exp(toks, pos));
        pos = tmp_pos;
        // peek next node
        let mut tok = &toks[pos];
        pos = pos + 1;
        if *tok != lexer::TokType::Or {
            // only one child_node
            log_or_exp_node.child.push(log_and_exp_node);
            pos = pos - 1;
            return Ok((log_or_exp_node, pos));
        }

        // log_or_exp -> BinExp -> (left: logAndExp, right logAndExp)
        let mut lhs = log_and_exp_node;
        while *tok == lexer::TokType::Or {
            let mut binexp_node = ParseNode::new();
            binexp_node.entry = NodeType::BinExp(lexer::TokType::Or);

            let (rhs, tmp_pos) = r#try!(p_logical_and_exp(toks, pos));

            binexp_node.child.push(lhs);
            binexp_node.child.push(rhs);
            lhs = binexp_node;
            pos = tmp_pos;
            tok = &toks[pos];
            pos = pos + 1;
        }
        log_or_exp_node.child.push(lhs);
        pos = pos - 1;
        return Ok((log_or_exp_node, pos));
    }

    fn p_conditional_exp(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        // <conditional-exp> ::= <logical-or-exp> [ "?" <exp> ":" <conditional-exp> ]
        let mut conditional_exp_node = ParseNode::new();
        conditional_exp_node.entry = NodeType::ConditionalExp;
        // parse <logical-or-exp> first
        let (logical_or_exp_node, pos) = r#try!(p_logical_or_exp(toks, pos));
        conditional_exp_node.child.push(logical_or_exp_node);

        // it's optional if you got a "?"
        if toks[pos] == lexer::TokType::QuestionMark {
            // parse <exp>
            let pos = pos + 1;
            let (exp_node, pos) = r#try!(p_exp(toks, pos));

            if toks[pos] != lexer::TokType::Colon {
                return Err(format!(
                    "Expected `:` in conditional expression, but got {:?} at {}",
                    toks[pos], pos
                ));
            }
            let pos = pos + 1;
            // parse next <conditonal-exp>
            let (next_conditional_exp_node, pos) = r#try!(p_conditional_exp(toks, pos));
            conditional_exp_node.child.push(exp_node);
            conditional_exp_node.child.push(next_conditional_exp_node);
            return Ok((conditional_exp_node, pos));
        } else {
            return Ok((conditional_exp_node, pos));
        }
    }

    fn p_exp_opt(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        // <exp-option> ::= <exp> | ""
        let mut exp_opt_node = ParseNode::new();
        exp_opt_node.entry = NodeType::ExpOption;
        let res = p_exp(toks, pos);
        match res {
            Ok((exp_node, pos)) => {
                // <exp>
                exp_opt_node.child.push(exp_node);
                return Ok((exp_opt_node, pos));
            }
            Err(_) => {
                // ""
                // no child, means null statement
                return Ok((exp_opt_node, pos));
            }
        }
    }

    fn p_exp(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        // println!("in fn: p_exp, with pos:{}", pos);
        // <exp> ::= <id> [ "[" <exp> "]" ] "=" <exp> | <conditional-exp>
        let mut exp_node = ParseNode::new();
        exp_node.entry = NodeType::Exp;

        let tok = &toks[pos];
        match tok {
            lexer::TokType::Identifier(var_name) => {
                // check next token is Assign
                let mut pos = pos + 1;
                let tok = &toks[pos];
                match tok {
                    lexer::TokType::Assign => {
                        pos = pos + 1;
                        // something like a = 1
                        let mut assign_node = ParseNode::new();
                        assign_node.entry = NodeType::AssignNode(var_name.to_string(), false); // assign a int variable
                        let (next_exp_node, pos) = r#try!(p_exp(toks, pos));
                        assign_node.child.push(next_exp_node);
                        return Ok((assign_node, pos));
                    }
                    lexer::TokType::LBracket => {
                        // something like a[<exp>] = 1;
                        let back_pos = pos - 1;
                        pos = pos + 1;
                        // parse exp.
                        let (index_node, new_pos) = r#try!(p_exp(toks, pos));
                        pos = new_pos;
                        // parse ']'
                        if toks[pos] != lexer::TokType::RBracket {
                            return Err(format!(
                                "Expected ']' for bracket closing, found {:?} at {}",
                                toks[pos], pos
                            ));
                        }

                        // try '='
                        pos = pos + 1;
                        if toks[pos] != lexer::TokType::Assign {
                            pos = back_pos;
                            let (conditional_node, pos) = r#try!(p_conditional_exp(toks, pos));
                            exp_node.child.push(conditional_node);
                            return Ok((exp_node, pos));
                        }
                        pos = pos + 1;
                        // try parse exp
                        let mut assign_node = ParseNode::new();
                        assign_node.entry = NodeType::AssignNode(var_name.to_string(), true); // assign to a array element
                        let (res_node, new_pos) = r#try!(p_exp(toks, pos));
                        pos = new_pos;
                        assign_node.child.push(index_node);
                        assign_node.child.push(res_node);
                        return Ok((assign_node, pos));
                    }
                    _ => {
                        pos = pos - 1;
                        let (conditional_node, pos) = r#try!(p_conditional_exp(toks, pos));
                        exp_node.child.push(conditional_node);
                        return Ok((exp_node, pos));
                    }
                }
            }
            _ => {
                // try <conditional-exp>
                let (cond_node, pos) = r#try!(p_conditional_exp(toks, pos));
                exp_node.child.push(cond_node);
                return Ok((exp_node, pos));
            }
        }
    }

    fn p_fn(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        // println!("in p_fn with pos: {}", pos);
        // <function> ::= "int" <id> "(" ")" "{" { <statement> } "}"
        if pos >= toks.len() {
            return Err("Out of program length".to_string());
        }
        let tok = &toks[pos];
        if *tok != lexer::TokType::Kwd(lexer::KwdType::Int) {
            return Err(format!("Expected `int`, found {:?} at {}", toks[pos], pos));
        }
        let mut pos = pos + 1;

        let tok = &toks[pos];
        let mut fn_name = String::new();
        match tok {
            lexer::TokType::Identifier(n) => {
                fn_name = n.to_string();
            }
            _ => {
                return Err(format!("Expected function name, but not function name"));
            }
        }
        pos = pos + 1;

        let tok = &toks[pos];
        if *tok != lexer::TokType::LParen {
            return Err(format!("Expected `(`, found {:?} at {}", toks[pos], pos));
        }
        pos = pos + 1;
        // XXX: add void support, now only support int arg list
        let mut arg_list: Vec<String> = Vec::new();
        let mut arg_count = 0;
        while pos < toks.len() && toks[pos] != lexer::TokType::RParen {
            // try to parse argument list
            // match int
            match &toks[pos] {
                lexer::TokType::Kwd(lexer::KwdType::Int) => {
                    pos = pos + 1;
                }
                lexer::TokType::Kwd(lexer::KwdType::Void) => {
                    if arg_count > 0 {
                        return Err(format!(
                            "Error: void after other argument in one function definition"
                        ));
                    }
                    pos = pos + 1;
                    break;
                }
                _ => {
                    return Err(format!("Expected `int`, found {:?} at {}", toks[pos], pos));
                }
            }
            // match identifier
            match &toks[pos] {
                lexer::TokType::Identifier(var_name) => {
                    arg_list.push(var_name.to_string());
                    pos = pos + 1;
                }
                _ => {
                    return Err(format!(
                        "Expected identifier name, found {:?} at {}",
                        toks[pos], pos
                    ));
                }
            }
            arg_count = arg_count + 1;
            // match ,
            match &toks[pos] {
                lexer::TokType::Comma => {
                    pos = pos + 1;
                }
                lexer::TokType::RParen => {
                    continue;
                }
                _ => {
                    return Err(format!(
                        "Expected `,` or `)` at the end of one var_name, found {:?} at {}",
                        toks[pos], pos
                    ));
                }
            }
            if toks[pos] == lexer::TokType::RParen {
                break;
            }
        }
        let tok = &toks[pos];
        if *tok != lexer::TokType::RParen {
            return Err(format!("Expected `)`, found {:?} at {}", toks[pos], pos));
        }
        pos = pos + 1;

        let tok = &toks[pos];
        if *tok != lexer::TokType::LBrace {
            return Err(format!("Expected `{{`, found {:?} at {}", toks[pos], pos));
        }
        pos = pos + 1;

        let mut fn_node = ParseNode::new();
        if arg_list.is_empty() {
            fn_node.entry = NodeType::Fn(fn_name, None);
        } else {
            fn_node.entry = NodeType::Fn(fn_name, Some(arg_list));
        }

        while pos < toks.len() && toks[pos] != lexer::TokType::RBrace {
            let (block_node, tmp_pos) = r#try!(p_block(toks, pos));
            pos = tmp_pos;
            fn_node.child.push(block_node);
        }

        if pos >= toks.len() {
            return Err(format!("Missing `}}`"));
        }
        if toks[pos] != lexer::TokType::RBrace {
            return Err(format!("Expected `}}`, found {:?} at {}", toks[pos], pos));
        }
        pos = pos + 1;

        //println!("out p_fn with pos: {}", pos);
        Ok((fn_node, pos))
    }

    fn p_declare(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        // println!("in p_declare with pos = {}", pos);
        let tok = &toks[pos];
        match tok {
            lexer::TokType::Kwd(lexer::KwdType::Int) => {
                // "int" <id> [ = <exp> ] ";"
                // or "int" <id> "[" <int> "]" ";"
                let pos = pos + 1;

                let tok = &toks[pos];
                match tok {
                    lexer::TokType::Identifier(var_name) => {
                        let mut stmt_node = ParseNode::new();
                        stmt_node.entry = NodeType::Declare(var_name.to_string(), DataType::I64);
                        let pos = pos + 1;
                        let tok = &toks[pos];
                        match tok {
                            lexer::TokType::Assign => {
                                // parse exp
                                // e.g. int a = exp;
                                let pos = pos + 1;
                                let (exp_node, pos) = r#try!(p_exp(toks, pos));

                                let tok = &toks[pos];
                                if *tok != lexer::TokType::Semicolon {
                                    return Err(format!(
                                        "Expected `;`, found {:?} at {}",
                                        toks[pos], pos
                                    ));
                                }
                                let pos = pos + 1;
                                stmt_node.child.push(exp_node);
                                return Ok((stmt_node, pos));
                            }
                            lexer::TokType::Semicolon => {
                                // if just declare, but no assignment, just record the var_name
                                // e.g. int var;
                                let pos = pos + 1;
                                return Ok((stmt_node, pos));
                            }
                            lexer::TokType::LBracket => {
                                // array declare
                                // e.g. int a[100];
                                // XXX: now only just support literal array length
                                if cfg!(feature = "debug") {
                                    println!("here in p_declare -> LBraket");
                                }
                                let mut declare_node = ParseNode::new();
                                let pos = pos + 1;
                                let tok = &toks[pos];
                                match tok {
                                    lexer::TokType::Literal(n) => {
                                        declare_node.entry = NodeType::Declare(
                                            var_name.to_string(),
                                            DataType::Arr64(*n),
                                        );
                                        let pos = pos + 1;
                                        let tok = &toks[pos];
                                        if *tok != lexer::TokType::RBracket {
                                            return Err(format!(
                                                "Expected `]` for array declaration, found {:?} at {}",
                                                toks[pos], pos
                                            ));
                                        }

                                        let pos = pos + 1;
                                        let tok = &toks[pos];
                                        if *tok != lexer::TokType::Semicolon {
                                            return Err(format!("Expected `;` at end of array declaration, found {:?} at {}", toks[pos], pos));
                                        }
                                        let pos = pos + 1;
                                        if cfg!(feature = "debug") {
                                            println!("got declare_node: {:?}", declare_node);
                                        }
                                        return Ok((declare_node, pos));
                                    }
                                    _ => {
                                        return Err(format!(
                                            "Expected Array length `literal`, found {:?} at {}",
                                            toks[pos], pos
                                        ));
                                    }
                                }
                            }
                            _ => {
                                return Err(format!(
                                    "Expected Assignment `;` or `=`, found {:?} at {}",
                                    toks[pos], pos
                                ));
                            }
                        }
                    }
                    _ => {
                        return Err(format!(
                            "Expected identifier name, found {:?} at {}",
                            toks[pos], pos
                        ));
                    }
                }
            }
            _ => {
                return Err(format!(
                    "Error: Expected type definition `int`, found {:?} at {}",
                    toks[pos], pos
                ));
            }
        }
    }

    fn p_block(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        let tok = &toks[pos];
        match tok {
            lexer::TokType::Kwd(lexer::KwdType::Int) => {
                // try to parse declare
                // let mut block_node = ParseNode::new();
                // block_node.entry = NodeType::Block;

                let (declare_node, pos) = r#try!(p_declare(toks, pos));
                // block_node.child.push(declare_node);
                // return Ok((block_node, pos));
                return Ok((declare_node, pos));
            }
            _ => {
                // try to parse statement
                // let mut block_node = ParseNode::new();
                // block_node.entry = NodeType::Block;

                let (stmt_node, pos) = r#try!(p_stmt(toks, pos));
                return Ok((stmt_node, pos));
                // block_node.child.push(stmt_node);
                //return Ok((block_node, pos));
            }
        }
    }
    fn p_stmt(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        // println!("in fn : p_stmt, with pos {}", pos);
        let tok = &toks[pos];
        match tok {
            lexer::TokType::LBrace => {
                // "{" { <block-item> } "}"
                let mut pos = pos + 1;
                let mut stmt_node = ParseNode::new();
                stmt_node.entry = NodeType::Stmt(StmtType::Compound);

                // try to get some block item
                while toks[pos] != lexer::TokType::RBrace {
                    let (block_node, tmp_pos) = r#try!(p_block(toks, pos));
                    stmt_node.child.push(block_node);
                    pos = tmp_pos;
                }

                // throw "}"
                pos = pos + 1;
                return Ok((stmt_node, pos));
            }
            lexer::TokType::Kwd(lexer::KwdType::Ret) => {
                // "return" <exp> ";"
                let pos = pos + 1;
                let (exp_node, mut pos) = r#try!(p_exp(toks, pos));

                let tok = &toks[pos];
                if *tok != lexer::TokType::Semicolon {
                    return Err(format!(
                        "Expected `;` in statement, found {:?} at {}",
                        toks[pos], pos
                    ));
                }
                pos = pos + 1;

                let mut stmt_node = ParseNode::new();
                stmt_node.entry = NodeType::Stmt(StmtType::Return);
                stmt_node.child.push(exp_node);
                return Ok((stmt_node, pos));
            }
            lexer::TokType::Kwd(lexer::KwdType::If) => {
                // "if" "(" <exp> ")" <statement> [ "else" <statement> ]
                // this is the conditional statement
                let mut stmt_node = ParseNode::new();
                stmt_node.entry = NodeType::Stmt(StmtType::Conditional("if".to_string()));
                let pos = pos + 1;
                if pos >= toks.len() || toks[pos] != lexer::TokType::LParen {
                    return Err(format!("Missing `(`"));
                }
                // try to parse exp
                if cfg!(feature = "debug") {
                    println!("here pos = {}", pos);
                }
                let pos = pos + 1;
                let (exp_node, pos) = r#try!(p_exp(toks, pos));
                // println!("pos = {}", pos);
                if pos >= toks.len() || toks[pos] != lexer::TokType::RParen {
                    return Err(format!("Missing `)`"));
                }

                let pos = pos + 1;
                // try to parse statement
                if cfg!(feature = "debug") {
                    println!("If: parse stmt from pos = {}, tok: {:?}", pos, toks[pos]);
                }
                let (clause_1_node, pos) = r#try!(p_stmt(toks, pos));
                stmt_node.child.push(exp_node);
                stmt_node.child.push(clause_1_node);

                // if has 'else'
                // println!("SHOULD BE HERE , POS = {}", pos);
                if pos < toks.len() && toks[pos] == lexer::TokType::Kwd(lexer::KwdType::Else) {
                    // try to parse statement 2
                    let pos = pos + 1;
                    let (clause_2_node, pos) = r#try!(p_stmt(toks, pos));
                    stmt_node.child.push(clause_2_node);
                    return Ok((stmt_node, pos));
                } else {
                    return Ok((stmt_node, pos));
                }
            }
            lexer::TokType::Kwd(lexer::KwdType::For) => {
                // "for" "(" <exp-option> ";" <exp-option> ";" <exp-option> ")" <statement>
                // "for" "(" <declaration> <exp-option> ";" <exp-option> ")" <statement>
                let mut stmt_node = ParseNode::new();
                let pos = pos + 1;
                if pos >= toks.len() || toks[pos] != lexer::TokType::LParen {
                    return Err(format!("Missing `(`"));
                }
                let pos = pos + 1;
                // try to parse declaration
                let decl_res = p_declare(toks, pos);
                match decl_res {
                    Ok((declare_node, pos)) => {
                        // "for" "(" <declaration> <exp-option> ";" <exp-option> ")" <statement>
                        stmt_node.child.push(declare_node);
                        stmt_node.entry = NodeType::Stmt(StmtType::ForDecl);

                        let (exp_opt_node, pos) = r#try!(p_exp_opt(toks, pos));
                        stmt_node.child.push(exp_opt_node);

                        if pos >= toks.len() || toks[pos] != lexer::TokType::Semicolon {
                            return Err(format!("Missing `;` needed by For"));
                        }
                        let pos = pos + 1;

                        let (exp_opt_node, pos) = r#try!(p_exp_opt(toks, pos));
                        stmt_node.child.push(exp_opt_node);
                        if pos >= toks.len() || toks[pos] != lexer::TokType::RParen {
                            return Err(format!("Missing `)` needed by For"));
                        }
                        let pos = pos + 1;
                        if cfg!(feature = "debug") {
                            println!("pos: {} tok: {:?} before compound layer", pos, toks[pos]);
                        }
                        let mut compound_layer_node = ParseNode::new();
                        compound_layer_node.entry = NodeType::Stmt(StmtType::Compound);
                        let (next_stmt_node, pos) = r#try!(p_stmt(toks, pos));
                        compound_layer_node.child.push(next_stmt_node);
                        stmt_node.child.push(compound_layer_node);
                        return Ok((stmt_node, pos));
                    }
                    Err(_) => {
                        // "for" "(" <exp-option> ";" <exp-option> ";" <exp-option> ")" <statement>
                        stmt_node.entry = NodeType::Stmt(StmtType::For);
                        let (exp_opt_node, pos) = r#try!(p_exp_opt(toks, pos));
                        stmt_node.child.push(exp_opt_node);

                        if pos >= toks.len() || toks[pos] != lexer::TokType::Semicolon {
                            return Err(format!("Missing `;` needed by for"));
                        }
                        let pos = pos + 1;

                        let (exp_opt_node, pos) = r#try!(p_exp_opt(toks, pos));
                        stmt_node.child.push(exp_opt_node);

                        if pos >= toks.len() || toks[pos] != lexer::TokType::Semicolon {
                            return Err(format!("Missing `;` needed by for"));
                        }
                        let pos = pos + 1;

                        let (exp_opt_node, pos) = r#try!(p_exp_opt(toks, pos));
                        stmt_node.child.push(exp_opt_node);

                        if pos >= toks.len() || toks[pos] != lexer::TokType::RParen {
                            return Err(format!("Missing `)` needed by for"));
                        }
                        let pos = pos + 1;
                        let mut compound_layer_node = ParseNode::new();
                        let (next_stmt_node, pos) = r#try!(p_stmt(toks, pos));
                        compound_layer_node.child.push(next_stmt_node);
                        compound_layer_node.entry = NodeType::Stmt(StmtType::Compound);
                        stmt_node.child.push(compound_layer_node);
                        return Ok((stmt_node, pos));
                    }
                }
            }
            lexer::TokType::Kwd(lexer::KwdType::While) => {
                // "while" "(" <exp> ")" <statement>
                let mut stmt_node = ParseNode::new();
                stmt_node.entry = NodeType::Stmt(StmtType::While);
                let pos = pos + 1;
                if pos >= toks.len() || toks[pos] != lexer::TokType::LParen {
                    return Err(format!("Missing `(` needed by While"));
                }

                let pos = pos + 1;
                let (exp_node, pos) = r#try!(p_exp(toks, pos));
                stmt_node.child.push(exp_node);
                if pos >= toks.len() || toks[pos] != lexer::TokType::RParen {
                    return Err(format!("Missing `)`"));
                }
                let pos = pos + 1;

                let (next_stmt_node, pos) = r#try!(p_stmt(toks, pos));
                stmt_node.child.push(next_stmt_node);
                return Ok((stmt_node, pos));
            }
            lexer::TokType::Kwd(lexer::KwdType::Do) => {
                // "do" <statement> "while" "(" <exp> ")" ";"
                let mut stmt_node = ParseNode::new();
                stmt_node.entry = NodeType::Stmt(StmtType::Do);
                let pos = pos + 1;
                let (next_stmt_node, pos) = r#try!(p_stmt(toks, pos));
                stmt_node.child.push(next_stmt_node);
                // parse while
                if pos >= toks.len() || toks[pos] != lexer::TokType::Kwd(lexer::KwdType::While) {
                    return Err(format!("Missing `while` needed by do"));
                }
                let pos = pos + 1;

                if pos >= toks.len() || toks[pos] != lexer::TokType::LParen {
                    return Err(format!("Missing `(` needed by do"));
                }
                let pos = pos + 1;

                let (exp_node, pos) = r#try!(p_exp_opt(toks, pos));

                if pos >= toks.len() || toks[pos] != lexer::TokType::RParen {
                    return Err(format!("Missing `)` needed by do"));
                }
                let pos = pos + 1;

                if pos >= toks.len() || toks[pos] != lexer::TokType::Semicolon {
                    return Err(format!("Missing `;` needed by do"));
                }
                let pos = pos + 1;

                stmt_node.child.push(exp_node);
                return Ok((stmt_node, pos));
            }
            lexer::TokType::Kwd(lexer::KwdType::Continue) => {
                let mut stmt_node = ParseNode::new();
                stmt_node.entry = NodeType::Stmt(StmtType::Continue);
                let pos = pos + 1;
                if pos >= toks.len() || toks[pos] != lexer::TokType::Semicolon {
                    return Err(format!("Missing `;` needed by continue"));
                }
                let pos = pos + 1;
                return Ok((stmt_node, pos));
            }
            lexer::TokType::Kwd(lexer::KwdType::Break) => {
                let mut stmt_node = ParseNode::new();
                stmt_node.entry = NodeType::Stmt(StmtType::Break);
                let pos = pos + 1;
                if pos >= toks.len() || toks[pos] != lexer::TokType::Semicolon {
                    return Err(format!("Missing `;` needed by break"));
                }
                let pos = pos + 1;
                return Ok((stmt_node, pos));
            }
            _ => {
                // try to parse exp-option;
                let mut stmt_node = ParseNode::new();
                stmt_node.entry = NodeType::Stmt(StmtType::Exp);
                //let pos = pos + 1;
                let (exp_opt_node, pos) = r#try!(p_exp_opt(toks, pos));

                let tok = &toks[pos];
                if *tok != lexer::TokType::Semicolon {
                    return Err(format!("Expected `;`, found {:?} at {}", toks[pos], pos));
                }
                let pos = pos + 1;
                stmt_node.child.push(exp_opt_node);
                return Ok((stmt_node, pos));
            }
        }
    }

    fn p_factor(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        if cfg!(feature = "debug") {
            println!("in p_factor with pos: {}, tok = {:?}", pos, toks[pos]);
        }
        let mut next = &toks[pos];
        let mut pos = pos + 1;

        match next {
            lexer::TokType::LParen => {
                // parse expression inside parens
                // factor -> exp
                let (exp_node, tmp_pos) = r#try!(p_exp(toks, pos));
                pos = tmp_pos;
                next = &toks[pos];
                pos = pos + 1;
                if *next != lexer::TokType::RParen {
                    return Err(format!(
                        "Expected `)` in file:parser.rs, found {:?} at {}",
                        toks[pos], pos
                    ));
                }
                let mut factor_node = ParseNode::new();
                factor_node.entry = NodeType::Factor;
                factor_node.child.push(exp_node);
                // println!("out p_factor with pos: {}", pos);
                return Ok((factor_node, pos));
            }
            lexer::TokType::Minus | lexer::TokType::Tilde | lexer::TokType::Exclamation | lexer::TokType::Addr => {
                // factor -> UnExp -> factor
                let mut factor_node = ParseNode::new();
                let mut unexp_node = ParseNode::new();
                factor_node.entry = NodeType::Factor;
                unexp_node.entry = NodeType::UnExp(match next {
                    lexer::TokType::Minus => lexer::TokType::Minus,
                    lexer::TokType::Tilde => lexer::TokType::Tilde,
                    lexer::TokType::Exclamation => lexer::TokType::Exclamation,
                    lexer::TokType::Addr => lexer::TokType::Addr,
                    _ => panic!("Something strange"),
                });
                let (next_factor_node, pos) = r#try!(p_factor(toks, pos));
                unexp_node.child.push(next_factor_node);
                factor_node.child.push(unexp_node);
                return Ok((factor_node, pos));
            }
            lexer::TokType::String(chars, tag) => {
                let mut string_node = ParseNode::new();
                let mut factor_node = ParseNode::new();
                string_node.entry = NodeType::StringLiteral(chars.to_string(), tag.to_string());
                factor_node.entry = NodeType::Factor;
                factor_node.child.push(string_node);

                return Ok((factor_node, pos));
            }
            lexer::TokType::Literal(n) => {
                // Factor -> Const
                let mut const_node = ParseNode::new();
                let mut factor_node = ParseNode::new();
                const_node.entry = NodeType::Const(*n);
                factor_node.entry = NodeType::Factor;
                factor_node.child.push(const_node);
                // println!("out p_factor with pos: {}", pos);
                return Ok((factor_node, pos));
            }
            lexer::TokType::Identifier(var_name) => {
                if cfg!(feature = "debug") {
                    println!("here\n");
                }
                if pos < toks.len() && toks[pos] == lexer::TokType::LParen {
                    // Factor -> FnCall
                    let mut factor_node = ParseNode::new();
                    pos = pos - 1;
                    factor_node.entry = NodeType::Factor;
                    let (fn_call_node, pos) = r#try!(p_fn_call(toks, pos));
                    factor_node.child.push(fn_call_node);
                    return Ok((factor_node, pos));
                } else if pos < toks.len() && toks[pos] == lexer::TokType::LBracket {
                    // Factor -> Array referencing
                    let mut factor_node = ParseNode::new();
                    pos = pos - 1;
                    factor_node.entry = NodeType::Factor;
                    let (arr_ref_node, pos) = r#try!(p_arr_ref(toks, pos));
                    factor_node.child.push(arr_ref_node);
                    return Ok((factor_node, pos));
                } else {
                    // Factor -> Var
                    let mut var_node = ParseNode::new();
                    let mut factor_node = ParseNode::new();
                    var_node.entry = NodeType::Var(var_name.to_string());
                    factor_node.entry = NodeType::Factor;
                    factor_node.child.push(var_node);
                    // println!("out p_factor with pos: {}", pos);
                    return Ok((factor_node, pos));
                }
            }
            _ => Err(format!("Factor rule not allowed.")),
        }
    }

    fn p_arr_ref(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        // array reference ::= <id> "[" <exp> "]"
        let mut arr_ref_node = ParseNode::new();
        let mut var_name = String::new();
        match &toks[pos] {
            lexer::TokType::Identifier(name) => {
                var_name = name.to_string();
            }
            _ => {
                return Err(format!(
                    "Expected array identifier, foudn {:?} at {}",
                    toks[pos], pos
                ));
            }
        }
        arr_ref_node.entry = NodeType::ArrayRef(var_name);

        let mut pos = pos + 1;
        // match '['
        match toks[pos] {
            lexer::TokType::LBracket => {
                pos = pos + 1;
            }
            _ => {
                return Err(format!(
                    "Expected `[` needed by array referencing, found {:?} at {}",
                    toks[pos], pos
                ));
            }
        }

        let (exp_node, new_pos) = r#try!(p_exp(toks, pos));
        arr_ref_node.child.push(exp_node);
        match toks[new_pos] {
            lexer::TokType::RBracket => {
                pos = new_pos + 1;
            }
            _ => {
                return Err(format!(
                    "Expected ']' needed by array referencing, found {:?} at {}",
                    toks[pos], pos
                ));
            }
        }
        return Ok((arr_ref_node, pos));
    }
    fn p_fn_call(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        // <function-call> ::= id "(" [ <exp> { "," <exp> } ] ")"
        //println!("in fn p_fn_call");
        let mut fn_call_node = ParseNode::new();
        let mut fn_name = String::new();
        match &toks[pos] {
            lexer::TokType::Identifier(name) => {
                fn_name = name.to_string();
            }
            _ => {
                return Err(format!(
                    "Expected function name, found {:?} at {}",
                    toks[pos], pos
                ));
            }
        }
        fn_call_node.entry = NodeType::FnCall(fn_name);
        let mut pos = pos + 1;
        // match '('
        match toks[pos] {
            lexer::TokType::LParen => {
                pos = pos + 1;
            }
            _ => {
                return Err(format!(
                    "Expected `(` needed by function call, found {:?} at {}",
                    toks[pos], pos
                ));
            }
        }
        while pos < toks.len() && toks[pos] != lexer::TokType::RParen {
            // try to parse argument exp
            let (exp_node, new_pos) = r#try!(p_exp(toks, pos));
            fn_call_node.child.push(exp_node);
            pos = new_pos;

            // match ,
            match &toks[pos] {
                lexer::TokType::Comma => {
                    pos = pos + 1;
                }
                lexer::TokType::RParen => {
                    continue;
                }
                _ => {
                    return Err(format!(
                        "Expected `,` or `)` at the end of exp, found {:?} at {}",
                        toks[pos], pos
                    ));
                }
            }
            if toks[pos] == lexer::TokType::RParen {
                break;
            }
        }
        pos = pos + 1;
        return Ok((fn_call_node, pos));
    }

    fn p_logical_and_exp(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        let mut logAndExp_node = ParseNode::new();
        logAndExp_node.entry = NodeType::LogicalAndExp;

        // LogicalAndExp -> EqualityExp
        let mut pos = pos;
        let (eq_node, tmp_pos) = r#try!(p_eq_exp(toks, pos));
        pos = tmp_pos;
        let mut tok = &toks[pos];
        pos = pos + 1;
        if *tok != lexer::TokType::And {
            logAndExp_node.child.push(eq_node);
            pos = pos - 1;
            return Ok((logAndExp_node, pos));
        }
        // Term -> BinExp -> (EqualityExp, EqualityExp)
        let mut eq_node = eq_node; // change to mutable
        while *tok == lexer::TokType::And {
            let mut binexp_node = ParseNode::new();
            binexp_node.entry = NodeType::BinExp(lexer::TokType::And);

            let (rhs, tmp_pos) = r#try!(p_eq_exp(toks, pos));

            binexp_node.child.push(eq_node);
            binexp_node.child.push(rhs);
            eq_node = binexp_node;
            pos = tmp_pos;
            tok = &toks[pos];
            pos = pos + 1;
        }
        logAndExp_node.child.push(eq_node);
        pos = pos - 1;
        return Ok((logAndExp_node, pos));
    }

    fn p_eq_exp(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        let mut eq_node = ParseNode::new();
        eq_node.entry = NodeType::EqualityExp;

        let mut pos = pos;
        let (relational_node, tmp_pos) = r#try!(p_relational_exp(toks, pos));
        pos = tmp_pos;
        let mut tok = &toks[pos];
        pos = pos + 1;
        if *tok != lexer::TokType::NotEqual && *tok != lexer::TokType::Equal {
            eq_node.child.push(relational_node);
            pos = pos - 1;
            return Ok((eq_node, pos));
        }

        let mut relational_node = relational_node;
        while *tok == lexer::TokType::Equal || *tok == lexer::TokType::NotEqual {
            let mut binexp_node = ParseNode::new();
            binexp_node.entry = NodeType::BinExp(match tok {
                lexer::TokType::Equal => lexer::TokType::Equal,
                lexer::TokType::NotEqual => lexer::TokType::NotEqual,
                _ => panic!("in p_eq_exp, something went wrong"),
            });

            let (next_relational_node, tmp_pos) = r#try!(p_relational_exp(toks, pos));

            binexp_node.child.push(relational_node);
            binexp_node.child.push(next_relational_node);
            relational_node = binexp_node;
            pos = tmp_pos;
            tok = &toks[pos];
            pos = pos + 1;
        }
        eq_node.child.push(relational_node);
        pos = pos - 1;
        return Ok((eq_node, pos));
    }

    fn p_relational_exp(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        let mut relational_node = ParseNode::new();
        relational_node.entry = NodeType::RelationalExp;

        let mut pos = pos;
        let (additive_exp_node, tmp_pos) = r#try!(p_additive_exp(toks, pos));
        pos = tmp_pos;
        let mut tok = &toks[pos];
        pos = pos + 1;
        if *tok != lexer::TokType::Lt
            && *tok != lexer::TokType::Gt
            && *tok != lexer::TokType::GreaterEqual
            && *tok != lexer::TokType::LessEqual
        {
            relational_node.child.push(additive_exp_node);
            pos = pos - 1;
            return Ok((relational_node, pos));
        }

        let mut additive_exp_node = additive_exp_node;
        while *tok == lexer::TokType::Lt
            || *tok == lexer::TokType::Gt
            || *tok == lexer::TokType::GreaterEqual
            || *tok == lexer::TokType::LessEqual
        {
            let mut binexp_node = ParseNode::new();
            binexp_node.entry = NodeType::BinExp(match tok {
                lexer::TokType::Lt => lexer::TokType::Lt,
                lexer::TokType::Gt => lexer::TokType::Gt,
                lexer::TokType::GreaterEqual => lexer::TokType::GreaterEqual,
                lexer::TokType::LessEqual => lexer::TokType::LessEqual,
                _ => panic!("in p_relational_exp, something went wrong"),
            });
            let (next_additive_exp_node, tmp_pos) = r#try!(p_additive_exp(toks, pos));
            binexp_node.child.push(additive_exp_node);
            binexp_node.child.push(next_additive_exp_node);
            additive_exp_node = binexp_node;
            pos = tmp_pos;
            tok = &toks[pos];
            pos = pos + 1;
        }
        relational_node.child.push(additive_exp_node);
        pos = pos - 1;
        return Ok((relational_node, pos));
    }
    fn p_term(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        // println!("in p_term with pos: {}", pos);
        let mut term_node = ParseNode::new();
        term_node.entry = NodeType::Term;

        // term -> factor
        let mut pos = pos;
        let (factor_node, tmp_pos) = r#try!(p_factor(toks, pos));
        pos = tmp_pos;
        let mut tok = &toks[pos];
        pos = pos + 1;
        if *tok != lexer::TokType::Multi && *tok != lexer::TokType::Splash {
            term_node.child.push(factor_node);
            pos = pos - 1;
            // println!("1. out p_term with pos: {}", pos);
            return Ok((term_node, pos));
        }

        // term -> BinExp -> (factor_left, factor_right)
        let mut factor_node = factor_node;
        while *tok == lexer::TokType::Multi || *tok == lexer::TokType::Splash {
            let mut binexp_node = ParseNode::new();
            binexp_node.entry = NodeType::BinExp(match tok {
                lexer::TokType::Multi => lexer::TokType::Multi,
                lexer::TokType::Splash => lexer::TokType::Splash,
                _ => panic!("in p_term, something went wrong"),
            });

            let (next_factor_node, tmp_pos) = r#try!(p_factor(toks, pos));

            binexp_node.child.push(factor_node);
            binexp_node.child.push(next_factor_node);
            factor_node = binexp_node;
            pos = tmp_pos;
            tok = &toks[pos];
            pos = pos + 1;
        }
        term_node.child.push(factor_node);
        pos = pos - 1;
        // println!("2. out p_term with pos: {}", pos);
        return Ok((term_node, pos));
    }

    fn p_additive_exp(toks: &[lexer::TokType], pos: usize) -> Result<(ParseNode, usize), String> {
        // println!("in p_exp with pos: {}", pos);
        let mut exp_node = ParseNode::new();
        exp_node.entry = NodeType::AdditiveExp;
        // exp -> term
        let mut pos = pos;
        let (term_node, tmp_pos) = r#try!(p_term(toks, pos));
        pos = tmp_pos;
        let mut tok = &toks[pos];
        if *tok != lexer::TokType::Plus && *tok != lexer::TokType::Minus {
            exp_node.child.push(term_node);
            // println!("1.out p_exp with pos: {}", pos);
            return Ok((exp_node, pos));
        }
        // exp -> BinExp()
        //peek next token, if it is lexer::TokType::Plus or lexer::TokType::Minus
        let mut term_node = term_node;
        let mut pos = pos;
        while *tok == lexer::TokType::Plus || *tok == lexer::TokType::Minus {
            let mut binexp_node = ParseNode::new();
            binexp_node.entry = NodeType::BinExp(match tok {
                lexer::TokType::Plus => lexer::TokType::Plus,
                lexer::TokType::Minus => lexer::TokType::Minus,
                _ => panic!("in parser::p_exp, something went wrong"),
            });
            pos = pos + 1;
            let (next_term_node, tmp_pos) = r#try!(p_term(toks, pos));
            pos = tmp_pos;
            binexp_node.child.push(term_node);
            binexp_node.child.push(next_term_node);
            term_node = binexp_node;
            tok = &toks[pos];
        }
        exp_node.child.push(term_node);
        return Ok((exp_node, pos));
    }

    pub fn parse_prog(input: &str, c_src_name: &str) -> Result<ParseNode, String> {
        let toks = r#try!(lexer::lex(&input));
        let mut prog_node = ParseNode::new();
        prog_node.entry = NodeType::Prog(c_src_name.to_string());
        let mut pos = 0;
        // now we need to add support for global variables
        while pos < toks.len() {
            // try to parse global variable declaration
            let p_res = p_declare(&toks, pos);
            match p_res {
                Ok((decl_node, new_pos)) => {
                    pos = new_pos;
                    prog_node.child.push(decl_node);
                }
                Err(_) => {
                    // try to parse fn definition
                    if cfg!(feature = "debug") {
                        println!("try to parse fn definition");
                    }
                    let (fn_node, new_pos) = r#try!(p_fn(&toks, pos));
                    prog_node.child.push(fn_node);
                    pos = new_pos;
                }
            }
        }

        return Ok(prog_node);
    }

    // XXX: should change the return type to Result<String, String> to remove panic!()
    pub fn print(tree: &ParseNode, idt: usize) -> String {
        let mut idt_prefix = String::new();
        for _i in 0..idt {
            idt_prefix = idt_prefix + " ";
        }
        match &tree.entry {
            NodeType::StringLiteral(data, tag) => format!(
                "{}n_type: StringLiteral, tag: {}, data: [{}]",
                idt_prefix, tag, data,
            ),
            NodeType::ArrayRef(var_name) => format!(
                "{}n_type: ArrayRef, var_name : {}, [\n{}\n{}]",
                idt_prefix,
                var_name,
                print(tree.child.get(0).unwrap(), idt + 1),
                idt_prefix
            ),
            NodeType::Factor => format!(
                "{}n_type: Factor, [\n{}\n{}]",
                idt_prefix,
                print(
                    tree.child.get(0).expect("Factor Node has no child"),
                    idt + 1
                ),
                idt_prefix
            ),
            NodeType::AssignNode(var_name, flag) => {
                match flag {
                    false => format!(
                        "{}n_type: AssignNode, Var: {} [\n{}\n{}]",
                        idt_prefix,
                        var_name,
                        print(
                            tree.child.get(0).expect("Assign Node has no child"),
                            idt + 1
                        ),
                        idt_prefix
                    ),
                    true => {
                        // assign to array
                        format!(
                            "{}n_type: AssignNode array: {} [\n{}\n{}\n{}]",
                            idt_prefix,
                            var_name,
                            print(
                                tree.child
                                    .get(0)
                                    .expect("Assign to array Node has no index node "),
                                idt + 1
                            ),
                            print(
                                tree.child.get(1).expect("Assign to array node has no rhs"),
                                idt + 1
                            ),
                            idt_prefix,
                        )
                    }
                }
            }
            NodeType::BinExp(op) => format!(
                "{}n_type: BinExp, Op: {} [\n{}\n{}\n{}]",
                idt_prefix,
                match op {
                    lexer::TokType::Minus => format!("-"),
                    lexer::TokType::Plus => format!("+"),
                    lexer::TokType::Multi => format!("*"),
                    lexer::TokType::Splash => format!("/"),
                    lexer::TokType::And => format!("&&"),
                    lexer::TokType::Or => format!("||"),
                    lexer::TokType::Equal => format!("=="),
                    lexer::TokType::NotEqual => format!("!="),
                    lexer::TokType::GreaterEqual => format!(">="),
                    lexer::TokType::LessEqual => format!("<="),
                    lexer::TokType::Lt => format!("<"),
                    lexer::TokType::Gt => format!(">"),
                    _ => panic!(format!(
                        "Operator `{:?}` for Binary Expression not supported",
                        &op
                    )),
                },
                print(tree.child.get(0).expect("BinExp no lhs"), idt + 1),
                print(tree.child.get(1).expect("BinExp no rhs"), idt + 1),
                idt_prefix
            ),
            NodeType::Term => format!(
                "{}n_type: Term, [\n{}\n{}]",
                idt_prefix,
                print(tree.child.get(0).expect("Term Node has no child"), idt + 1),
                idt_prefix
            ),
            NodeType::Prog(prog_name) => {
                let mut prog_body = String::new();
                for it in tree.child.iter() {
                    prog_body.push_str(&print(it, idt + 1));
                    prog_body.push_str("\n");
                }
                format!(
                    "{}n_type: Prog, Name:{} [\n{}\n{}]",
                    idt_prefix, prog_name, prog_body, idt_prefix
                )
            }
            NodeType::FnCall(fn_name) => {
                let mut tmp = String::new();
                let mut inc = 0;
                for it in tree.child.iter() {
                    if inc > 0 {
                        tmp.push_str("\n");
                    }
                    tmp.push_str(&print(it, idt + 1));
                    inc = inc + 1;
                }

                format!(
                    "{}n_type: FnCall, Name: {} exp_list: [\n{}\n{}]",
                    idt_prefix, fn_name, tmp, idt_prefix
                )
                // list of exp
            }
            NodeType::Fn(fn_name, vars) => {
                let mut tmp = String::new();
                let mut inc = 0;
                for it in tree.child.iter() {
                    if inc > 0 {
                        tmp.push_str("\n");
                    }
                    tmp.push_str(&print(it, idt + 1));
                    inc = inc + 1;
                }
                let mut var_list_string = String::new();
                match vars {
                    Some(var_list) => {
                        for var in var_list {
                            var_list_string.push_str(" ");
                            var_list_string.push_str(var);
                            var_list_string.push_str(" ");
                        }
                    }
                    None => {}
                }
                format!(
                    "{}n_type: Fn, Name: {} var_list: [{}]\n\
                    {}[\n{}\n{}]",
                    idt_prefix, fn_name, var_list_string, idt_prefix, tmp, idt_prefix
                )
            }
            NodeType::Declare(var_name, t) => match t {
                DataType::I64 => {
                    if tree.child.is_empty() {
                        format!(
                            "{}n_type: Declare, type: Int var_name: {}",
                            idt_prefix, var_name
                        )
                    } else {
                        format!(
                            "{}n_type: Declare, type: Int var_name: {}, [\n{}\n{}]",
                            idt_prefix,
                            var_name,
                            print(
                                tree.child.get(0).expect("Declare Node has no child"),
                                idt + 1
                            ),
                            idt_prefix
                        )
                    }
                }
                DataType::Arr64(len) => format!(
                    "{}n_type: Declare, type: Array  var_name: {}, length: {}",
                    idt_prefix, var_name, len,
                ),
            },
            NodeType::ConditionalExp => {
                let mut tmp = String::new();
                let mut inc = 0;
                for it in tree.child.iter() {
                    if inc > 0 {
                        tmp.push_str("\n");
                    }
                    tmp.push_str(&print(it, idt + 1));
                    inc = inc + 1;
                }
                format!(
                    "{}n_type: Stmt::ConditionalExp, [\n{}\n{}]",
                    idt_prefix, tmp, idt_prefix
                )
            }
            NodeType::Stmt(stmt) => match stmt {
                StmtType::For => {
                    let exp_opt_1 = print(tree.child.get(0).expect("No exp1 in for"), idt + 1);
                    let exp_opt_2 = print(tree.child.get(1).expect("No exp2 in for"), idt + 1);
                    let exp_opt_3 = print(tree.child.get(2).expect("No exp3 in for"), idt + 1);
                    let stmt = print(tree.child.get(3).expect("No stmt in for"), idt + 1);
                    format!(
                        "{}n_type: Stmt:ForDeclare, [\n\
                        {}declare: [\n{}\n{}]\n\
                        {}exp1:    [\n{}\n{}]\n\
                        {}exp2:    [\n{}\n{}]\n\
                        {}stmt:    [\n{}\n{}]\n\
                        {}]",
                        idt_prefix,
                        idt_prefix,
                        exp_opt_1,
                        idt_prefix,
                        idt_prefix,
                        exp_opt_2,
                        idt_prefix,
                        idt_prefix,
                        exp_opt_3,
                        idt_prefix,
                        idt_prefix,
                        stmt,
                        idt_prefix,
                        idt_prefix
                    )
                }
                StmtType::ForDecl => {
                    let d = print(tree.child.get(0).expect("No declaration in for"), idt + 1);
                    let exp_opt_1 = print(tree.child.get(1).expect("No exp1 in for"), idt + 1);
                    let exp_opt_2 = print(tree.child.get(2).expect("No exp2 in for"), idt + 1);
                    let stmt = print(tree.child.get(3).expect("No stmt in for"), idt + 1);

                    format!(
                        "{}n_type: Stmt:ForDeclare, [\n\
                        {}declare: [\n{}\n{}]\n\
                        {}exp1:    [\n{}\n{}]\n\
                        {}exp2:    [\n{}\n{}]\n\
                        {}stmt:    [\n{}\n{}]\n\
                        {}]",
                        idt_prefix,
                        idt_prefix,
                        d,
                        idt_prefix,
                        idt_prefix,
                        exp_opt_1,
                        idt_prefix,
                        idt_prefix,
                        exp_opt_2,
                        idt_prefix,
                        idt_prefix,
                        stmt,
                        idt_prefix,
                        idt_prefix
                    )
                }
                StmtType::Do => {
                    let stmt = print(tree.child.get(0).expect("No stmt in do"), idt + 1);
                    let exp = print(tree.child.get(1).expect("No exp in do"), idt + 1);
                    format!(
                        "{}n_type: Stmt:Do, [\n\
                        {}           stmt: [\n{}\n{}]\n\
                        {}           exp:  [\n{}\n{}]\n\
                        {}]",
                        idt_prefix,
                        idt_prefix,
                        stmt,
                        idt_prefix,
                        idt_prefix,
                        exp,
                        idt_prefix,
                        idt_prefix
                    )
                }
                StmtType::While => {
                    let exp = print(tree.child.get(0).expect("No stmt in do"), idt + 1);
                    let stmt = print(tree.child.get(1).expect("No exp in do"), idt + 1);
                    format!(
                        "{}n_type: Stmt:While, [\n\
                        {}               exp: [\n{}\n{}]\n\
                        {}              stmt:  [\n{}\n{}]\n\
                        {}]",
                        idt_prefix,
                        idt_prefix,
                        exp,
                        idt_prefix,
                        idt_prefix,
                        stmt,
                        idt_prefix,
                        idt_prefix
                    )
                }
                StmtType::Continue => format!("{}n_type: Continue", idt_prefix),
                StmtType::Break => format!("{}n_type: Break", idt_prefix),
                StmtType::Return => format!(
                    "{}n_type: Stmt::Return, [\n{}\n{}]",
                    idt_prefix,
                    print(
                        tree.child
                            .get(0)
                            .expect("Statement::Return Node has no child"),
                        idt + 1
                    ),
                    idt_prefix
                ),
                StmtType::Exp => format!(
                    "{}n_type: Stmt::Exp, [\n{}\n{}]",
                    idt_prefix,
                    print(
                        tree.child.get(0).expect("Statement::Exp Node has no child"),
                        idt + 1
                    ),
                    idt_prefix
                ),
                StmtType::Conditional(op) => {
                    let mut tmp = String::new();
                    let mut inc = 0;
                    for it in tree.child.iter() {
                        if inc > 0 {
                            tmp.push_str("\n");
                        }
                        tmp.push_str(&print(it, idt + 1));
                        inc = inc + 1;
                    }
                    format!(
                        "{}n_type: Stmt::Conditional, Op: {} [\n{}\n{}]",
                        idt_prefix, op, tmp, idt_prefix
                    )
                }
                StmtType::Compound => {
                    let mut tmp = String::new();
                    let mut inc = 0;

                    for it in tree.child.iter() {
                        if inc > 0 {
                            tmp.push_str("\n");
                        }
                        tmp.push_str(&print(it, idt + 1));
                        inc += 1;
                    }
                    format!(
                        "{}n_type: Stmt::Compound, [\n{}\n{}]",
                        idt_prefix, tmp, idt_prefix
                    )
                }
            },
            NodeType::LogicalOrExp |
            NodeType::LogicalAndExp|
            NodeType::EqualityExp  |
            NodeType::RelationalExp|
            NodeType::AdditiveExp =>
                print(tree.child.get(0).expect(&format!("{:?} Node has no child", tree.entry)), idt),

            NodeType::ExpOption => match tree.child.len() {
                0 => format!("{}n_type: ExpOption", idt_prefix),
                1 => format!(
                    "{}n_type: ExpOption, [\n{}\n{}]",
                    idt_prefix,
                    print(tree.child.get(0).expect("ExpOption has no child"), idt + 1),
                    idt_prefix
                ),
                _ => panic!(format!(
                    "ExpOption can only have 0 or 1 child node, but found {} child node",
                    tree.child.len()
                )),
            },
            NodeType::Exp => format!(
                "{}n_type: Exp, [\n{}\n{}]",
                idt_prefix,
                print(
                    tree.child.get(0).expect("Expression Node has no child"),
                    idt + 1
                ),
                idt_prefix
            ),
            NodeType::Block => format!(
                "{}n_type: Block, [\n{}\n{}]",
                idt_prefix,
                print(tree.child.get(0).expect("Block node has no child"), idt + 1),
                idt_prefix
            ),
            NodeType::UnExp(Op) => format!(
                "{}n_type: UnExp, Op: {}, [\n{}\n{}]",
                idt_prefix,
                match Op {
                    lexer::TokType::Minus => "-".to_string(),
                    lexer::TokType::Tilde => "~".to_string(),
                    lexer::TokType::Exclamation => "!".to_string(),
                    lexer::TokType::Addr => "&".to_string(),
                    _ => panic!("Operator for Unary Expression not supported"),
                },
                print(
                    tree.child
                        .get(0)
                        .expect("Unary Expression Node has no child"),
                    idt + 1
                ),
                idt_prefix
            ),
            NodeType::Var(var_name) => format!("{}n_type, Variable, Name : {}", idt_prefix, var_name),
            NodeType::Const(n) => format!("{}n_type: Const, Value: {}", idt_prefix, n),
            _ => panic!(format!(
                "in parser::print, {:?} Node type not implemented",
                &tree.entry
            )),
        }
    }
}

pub mod sync
{
    pub use std::sync::{ * };
}

pub fn main() ->  Result<(), Box<dyn error::Error>>
{
    unsafe
    {
        /*
        let opts: opts::Opts =
        {
            use structopt::StructOpt;

            opts::Opts::from_args()
        };
        
        let input_file = opts.input()[0].clone();

        if opts.crust_debug_flags().print_filenames() {
            println!("Source file: {}\n", input_file.display())
        }

        let input_file_contents = fs::read_to_string(input_file.clone())?;

        if opts.crust_debug_flags().print_file_contents() {
            println!("File contents:\n{}\n", input_file_contents)
        }

        let tokens = lexer::lex(&input_file_contents)?;
        let root_node = parser::parse_prog(&input_file_contents, &input_file.display().to_string())?;

        if opts.crust_debug_flags().print_source_ast() {
            println!("Source AST:\n{}\n", parser::print(&root_node, 0))
        }

        if opts.crust_debug_flags().print_filenames() {
            println!("Output file: {}\n", opts.output().display());
        }

        let output_file_contents = gen::gen_prog(&root_node);

        if opts.crust_debug_flags().print_file_contents() {
            println!("File contents:\n{}\n", output_file_contents)
        }

        fs::write(opts.output(), output_file_contents)?;
        */
        Ok(())
    }
}
// 4119 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
