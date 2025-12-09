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
        lexer::{ Tokens },
        parser::{ Data, Node, Parse, Statement },
        sync::{ atomic },
        *,
    };
    /*
    */
    static LABEL_COUNTER: atomic::AtomicUsize = atomic::AtomicUsize::new(0);

    fn gen_labels(prefix: &str) -> String 
    {
        let label_counter = LABEL_COUNTER.fetch_add(1, atomic::Ordering::SeqCst);
        let label = format!(".L{}{}", prefix, label_counter);
        label
    }

    static FLAG_FOR_MAIN_HAS_RET: atomic::AtomicBool = atomic::AtomicBool::new(false);
    
    fn fn_main_has_ret() 
    {
        FLAG_FOR_MAIN_HAS_RET.swap(true, atomic::Ordering::SeqCst);
    }

    fn gen_fn_prologue(fn_name: &str) -> String 
    {
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

    fn gen_fn_epilogue() -> String 
    {
        let p = "        ";
        format!(
            "{}movq %rbp, %rsp\n\
            {}popq	%rbp\n\
            {}.cfi_def_cfa 7, 8\n",
            p, p, p
        )
    }

    fn compute_const(tree: &Parse) -> i64 
    {
        match &tree.entry 
        {
            Node::BinExp(op) => {
                let lhs = compute_const(tree.child.get(0).unwrap());
                let rhs = compute_const(tree.child.get(1).unwrap());
                match op {
                    Tokens::Plus => {
                        return lhs + rhs;
                    }
                    Tokens::Multi => {
                        return lhs * rhs;
                    }
                    Tokens::Splash => {
                        return lhs / rhs;
                    }
                    Tokens::And => {
                        if lhs != 0 && rhs != 0 {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    Tokens::Or => {
                        if lhs != 0 || rhs != 0 {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    Tokens::Equal => {
                        if lhs == rhs {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    Tokens::NotEqual => {
                        if lhs != rhs {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    Tokens::LessEqual => {
                        if lhs <= rhs {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    Tokens::GreaterEqual => {
                        if lhs >= rhs {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    Tokens::Lt => {
                        if lhs < rhs {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    Tokens::Gt => {
                        if lhs > rhs {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    _ => panic!("{:?} should not occur in global variable initialization"),
                }
            }
            Node::UnExp(op) => {
                let child_val = compute_const(tree.child.get(0).unwrap());
                match op {
                    Tokens::Minus => {
                        return -child_val;
                    }
                    Tokens::Tilde => {
                        return !child_val;
                    }
                    Tokens::Exclamation => {
                        if child_val == 0 {
                            return 1;
                        } else {
                            return 0;
                        }
                    }
                    _ => panic!("Expected Unary Operator, found {:?}", op),
                }
            }
            Node::Const(val) => {
                return val * 1;
            }
            _ => return compute_const(tree.child.get(0).unwrap()),
        }
    }

    pub fn gen_prog(tree: &Parse) -> String 
    {
        let p = "        ".to_string();


        let mut prog_body = String::new();
        let index_map: HashMap<String, isize> = HashMap::new();
        let mut global_variable_scope: HashSet<String> = HashSet::new();
        let idx: isize = 0;
        for it in tree.child.iter() {
            match &it.entry {
                Node::Declare(var_name, Data::I64) => {

                    global_variable_scope.insert(var_name.to_string());
                    if it.child.is_empty() {
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
                Node::Declare(var_name, Data::Arr64(len)) => {
                    global_variable_scope.insert(var_name.to_string());
                    prog_body.push_str(&format!("{}.comm {}, {}, 32\n", p, var_name, len * 8));
                }
                Node::Fn(fn_name, var_list_opt) => {
                    let fn_prologue = gen_fn_prologue(fn_name);
                    let fn_epilogue = gen_fn_epilogue();


                    let call_by_function = true;
                    let mut index_map: HashMap<String, isize> = HashMap::new();
                    let mut scope: HashMap<String, bool> = HashMap::new();
                    match var_list_opt {
                        Some(var_list) => {
                            let mut param_offset = 16 + (var_list.len() as isize - 6 - 1) * 8;
                            for i in 0..var_list.len() {
                                scope.insert(var_list[i].to_string(), true);
                                if i >= 6 {

                                    index_map.insert(var_list[i].to_string(), param_offset);
                                    param_offset -= 8;
                                } else {


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
            Node::Prog(prog_name) => format!(
                "{}.file \"{}\"\n\
                {}\
                {}.ident	\"crust: 0.1 (By Haoran Wang)\"\n\
                {}.section	.note.GNU-stack,\"\",@progbits\n",
                p, prog_name, prog_body, p, p
            ),
            _ => panic!("Something went wrong in gen_prog"),
        }
    }

    pub fn gen_declare
    (
        tree: &Parse,
        index_map: &HashMap<String, isize>,
        scope: &HashMap<String, bool>,
        idx: isize,
        lbb: &str,
        leb: &str,
        loop_in_label: Option<&str>,
        loop_out_label: Option<&str>,
        global_variable_scope: &HashSet<String>,
    ) -> (HashMap<String, isize>, HashMap<String, bool>, isize, String) 
    {

        let p = "        ";
        let mut index_map = index_map.clone();
        let mut scope = scope.clone();
        let mut idx = idx;
        match &tree.entry {
            Node::Declare(var_name, data_type) => {
                let get_opt = scope.get(var_name);
                match get_opt {
                    Some(flag) => {
                        match flag {
                            true => {

                                scope.insert(var_name.to_string(), false);

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

                        scope.insert(var_name.to_string(), false);

                        index_map.insert(var_name.to_string(), idx - 8);
                        idx -= 8;
                    }
                }


                let mut e1 = String::new();

                if tree.child.is_empty() {

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
        tree: &Parse,
        index_map: &HashMap<String, isize>,
        idx: isize,
        global_variable_scope: &HashSet<String>,
    ) -> String {
        let p = "        ".to_string();
        let label_begin_loop = gen_labels("BFOR");
        let label_end_loop = gen_labels("EFOR");

        let mut index_map = index_map.clone();
        let mut idx: isize = idx;

        let mut scope: HashMap<String, bool> = HashMap::new();
        match tree.entry {
            Node::Stmt(Statement::ForDecl) => {
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









                //let deblock = 8 * scope.len();
                let mut deblock = 0;
                
                for (_, val) in scope.iter()
                {
                    if *val == false {
                        deblock += 8;
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
                    deblock
                )
            }
            Node::Stmt(Statement::For) => {
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










                let mut deblock = 0;
                for (_, val) in scope.iter() {
                    if *val == false {
                        deblock += 8;
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
                    deblock
                )
            }
            _ => panic!("Something wrong in gen_for"),
        }
    }

    pub fn gen_block(
        tree: &Parse,
        index_map: &HashMap<String, isize>,
        scope: &HashMap<String, bool>,
        idx: isize,
        loop_in_label: Option<&str>,
        loop_out_label: Option<&str>,
        flag: bool,
        fn_def: bool,
        global_variable_scope: &HashSet<String>,
    ) -> String {
        let p = "        ".to_string();
        let label_begin_block = gen_labels("BB");
        let label_end_block = gen_labels("EB");

        let mut stmts = String::new();
        let mut index_map = index_map.clone();
        let mut idx: isize = idx;
        let mut current_scope: HashMap<String, bool> = scope.clone();
        if fn_def == false {
            current_scope = HashMap::new();
        } else {



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


        }

        for it in &tree.child {

            match &it.entry {
                Node::Declare(_var_name, Data::I64) => {
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
                Node::Stmt(Statement::Compound) => {
                    stmts.push_str(&gen_block(
                        it,
                        &index_map,
                        &current_scope,
                        idx,
                        loop_in_label,
                        loop_out_label,
                        true,
                        false,
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
        let mut deblock = 0;
        for (_, val) in current_scope.iter() {
            if *val == false {
                deblock += 8;
            }
        }

        format!(
            "{}:\n\
            {}\
            {}:\n\
            {}addq ${}, %rsp # block out\n",
            label_begin_block, stmts, label_end_block, p, deblock
        )
    }

    fn gen_addr(
        tree: &Parse,
        index_map: &HashMap<String, isize>,
        idx: isize,
        lbb: &str,
        leb: &str,
        loop_in_label: Option<&str>,
        loop_out_label: Option<&str>,
        global_variable_scope: &HashSet<String>,
    ) -> String {
        let p = "        ".to_string();

        match &tree.entry {
            Node::ArrayRef(var_name) => {
                match index_map.get(var_name) {
                    Some(c) => {

                        panic!(format!("Error: address to local array not implemented"));
                    }
                    None => {

                        match global_variable_scope.contains(var_name) {
                            true => {


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
            Node::Var(name) => {
                match index_map.get(name) {
                    Some(c) => {

                        format!("{}leaq {}(%rbp), %rax\n", p, c)
                    }
                    None => {

                        if global_variable_scope.contains(name) {

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
        tree: &Parse,
        index_map: &HashMap<String, isize>,
        idx: isize,
        lbb: &str,
        leb: &str,
        loop_in_label: Option<&str>,
        loop_out_label: Option<&str>,
        global_variable_scope: &HashSet<String>,
    ) -> String {
        let p = "        ".to_string();
        match &tree.entry {
            Node::StringLiteral(data, tag) => format!(
                "{}.section .rodata\n\
                {}:\n\
                {}.string \"{}\"\n\
                {}.text\n\
                {}leaq {}(%rip), %rax\n",
                p, tag, p, data, p, p, tag,
            ),
            Node::ConditionalExp => {
                if tree.child.len() == 1 {

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
            Node::FnCall(fn_name) => {





                let mut s: String = String::new();











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

                    s.push_str(&format!("{}pushq $0\n", p));
                }
                
                s.push_str(&format!("{}pushq %r10\n", p));
                s.push_str(&format!("{}pushq %r11\n", p));


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

                        s.push_str(&format!("{}pushq %rax\n", p));
                    } else {

                        s.push_str(&format!(
                            "{}movq %rax, {}\n{}movq $0, %rax\n",
                            p, regs[i], p
                        ));
                    }
                }

                s.push_str(&format!("{}call {}@PLT\n", p, fn_name));

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
            Node::Stmt(stmt) => match stmt {
                Statement::Return => format!(
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
                Statement::Conditional(_) => {
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
                Statement::Exp => gen_stmt(
                    tree.child.get(0).expect("Statement Node no child"),
                    index_map,
                    idx,
                    lbb,
                    leb,
                    loop_in_label,
                    loop_out_label,
                    &global_variable_scope,
                ),
                Statement::Continue => match loop_in_label {
                    Some(l) => format!("{}jmp {} # Continue\n", p, l),
                    None => panic!("Continue should be in the loop scope"),
                },
                Statement::Break => match loop_out_label {
                    Some(l) => format!("{}jmp {} # Break\n", p, l),
                    None => panic!("Break shoule be in the loop scope"),
                },
                Statement::For | Statement::ForDecl => {
                    gen_for(tree, index_map, idx, &global_variable_scope)
                }
                Statement::Do => {






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
                    );
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
                Statement::While => {







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
                    );
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
                Statement::Compound => {
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
            Node::ArrayRef(var_name) => {
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
            Node::AssignNode(var_name, true) => {
                match index_map.get(var_name) {
                    None => {

                        match global_variable_scope.contains(var_name) {
                            true => {

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

                                panic!("Error: Use un-declared variable `{}`", var_name)
                            }
                        }
                    }
                    Some(t) => {

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
            Node::AssignNode(var_name, false) => {

                match index_map.get(var_name) {
                    None => {

                        match global_variable_scope.contains(var_name) {
                            true => {

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

                                panic!("Error: Use un-declared variable `{}`", var_name)
                            }
                        }
                    }
                    Some(t) => {

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
            Node::UnExp(op) => match op {
                Tokens::Addr => format!(

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
                Tokens::Minus => format!(
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
                Tokens::Tilde => format!(
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
                Tokens::Exclamation => format!(
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
            Node::BinExp(op) => {
                match op {
                    Tokens::Plus => format!(
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
                    Tokens::Minus => format!(
                        "{}\
                        {}pushq %rax\n\
                        {}\
                        {}popq %rcx\n\
                        {}subq %rcx, %rax\n",

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
                    Tokens::Multi => format!(
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
                    Tokens::Splash => format!(
                        "{}\
                        {}pushq %rax\n\
                        {}\
                        {}popq %rcx\n\
                        {}xorq %rdx, %rdx\n\
                        {}idivq %rcx\n",

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
                    Tokens::Equal => format!(
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
                    Tokens::NotEqual => format!(
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
                    Tokens::LessEqual => format!(
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
                    Tokens::GreaterEqual => format!(
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
                    Tokens::Or => {
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
                    Tokens::And => {
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
                    Tokens::Lt => format!(
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
                    Tokens::Gt => format!(
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
            Node::Const(n) => format!("{}movq ${}, %rax\n", p, n),
            Node::Var(var_name) => {
                let var_offset = index_map.get(var_name);
                match var_offset {
                    Some(t) => {
                        let var_offset = t;
                        format!("{}movq {}(%rbp), %rax\n", p, var_offset)
                    }
                    None => {

                        match global_variable_scope.contains(var_name) {
                            true => {

                                let var_offset = var_name;
                                format!("{}movq {}(%rip), %rax\n", p, var_offset)
                            }
                            false => panic!(format!("Use of undeclared variable `{}`", var_name)),
                        }
                    }
                }
            }
            Node::ExpOption => {
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


                    format!("{}movq $1, %rax\n", p)
                }
            }
            Node::EqualityExp
            | Node::RelationalExp
            | Node::Term
            | Node::Exp
            | Node::Factor
            | Node::AdditiveExp
            | Node::LogicalOrExp
            | Node::Block
            | Node::LogicalAndExp => gen_stmt(
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
    pub enum Keyword 
    {
        Int,
        Void,
        Ret,
        If,
        Else,
        While,
        For,
        Do,
        Break,
        Continue,
    }

    #[derive(Eq, PartialEq, Clone, Debug)]
    pub enum Tokens 
    {
        Kwd(Keyword),
        LBrace,
        RBrace,
        LParen,
        RParen,
        LBracket,
        RBracket,
        Semicolon,
        Assign,
        Lt,
        Gt,
        Minus,
        Tilde,
        Exclamation,
        Plus,
        Multi,
        Splash,
        Literal(i64),
        Identifier(String),
        And,
        Or,
        Equal,
        NotEqual,
        LessEqual,
        GreaterEqual,
        Colon,
        QuestionMark,
        Comma,
        String(String, String),
        Addr,
    }

    static mut LABEL_COUNTER: i64 = -1;
    fn gen_string_tag() -> String
    {
        unsafe
        {
            LABEL_COUNTER = LABEL_COUNTER + 1;
            return format!(".LSTR{}", LABEL_COUNTER);
        }
    }

    pub fn lex(input: &str) -> Result<Vec<Tokens>, String> 
    {
        let mut result = Vec::new();
        let mut it = input.chars().peekable();

        while let Some(&c) = it.peek()
        {
            match c
            {
                '"' =>
                {
                    it.next();
                    let mut s = "".to_string();
                    
                    loop
                    {
                        let &c = it.peek().unwrap();

                        if c == '"' { break; }

                        s.push(c);
                        it.next();
                    }

                    result.push(Tokens::String(s, gen_string_tag()));
                    it.next();
                }
                
                '\'' => 
                {
                    it.next();
                    let &c = it.peek().unwrap();

                    if c == '\'' { return Err(format!("Error: empty character constant")); }

                    if c == '\\' 
                    {
                        it.next();
                        let &c = it.peek().unwrap();

                        match c 
                        {
                            'a' => { result.push(Tokens::Literal(0x07)); }
                                
                            'b' => { result.push(Tokens::Literal(0x08)); }
                                
                            'e' => { result.push(Tokens::Literal(0x1B)); }
                                
                            'f' => { result.push(Tokens::Literal(0x0C)); }
                                
                            'n' => { result.push(Tokens::Literal(0x0A)); }
                                
                            'r' => { result.push(Tokens::Literal(0x0D)); }
                                
                            't' => { result.push(Tokens::Literal(0x09)); }
                                
                            'v' => { result.push(Tokens::Literal(0x0B)); }
                                
                            '\\' => { result.push(Tokens::Literal(0x5C)); }
                                
                            '\'' => { result.push(Tokens::Literal(0x27)); }
                                
                            '\"' => { result.push(Tokens::Literal(0x22)); }
                                
                            '?' => { result.push(Tokens::Literal(0x3F)); }
                            _ => { return Err(format!("unrecongnized character")); }
                        }
                        
                        it.next();

                        if it.peek().unwrap() != &'\'' { return Err(format!("Error: unmatched '")); }

                        it.next();
                    }
                    
                    else 
                    {
                        result.push(Tokens::Literal(c as i64));
                        it.next();
                        it.next();
                    }
                }
                
                '0'...'9' => 
                {
                    it.next();
                    let mut number = c
                    .to_string()
                    .parse::<i64>()
                    .expect("The caller should have passed a digit.");

                    while let Some(Ok(digit)) = it.peek().map(|c| c.to_string().parse::<i64>()) 
                    {
                        number = number * 10 + digit;
                        it.next();
                    }

                    result.push(Tokens::Literal(number));
                }
                
                'a'...'z' | 'A'...'Z' | '_' =>
                {
                    it.next();
                    let mut s = String::new();
                    s.push(c);
                    
                    while let Some(&tmp) = it.peek()
                    {
                        match tmp
                        {
                            'a'...'z' | 'A'...'Z' | '_' =>
                            {
                                s.push(tmp);
                                it.next();
                            }

                            _ => { break; }
                        }
                    }

                    match s.as_ref()
                    {
                        "int" => result.push(Tokens::Kwd(Keyword::Int)),
                        "char" => result.push(Tokens::Kwd(Keyword::Int)),
                        "return" => result.push(Tokens::Kwd(Keyword::Ret)),
                        "void" => result.push(Tokens::Kwd(Keyword::Void)),
                        "if" => result.push(Tokens::Kwd(Keyword::If)),
                        "else" => result.push(Tokens::Kwd(Keyword::Else)),
                        "while" => result.push(Tokens::Kwd(Keyword::While)),
                        "for" => result.push(Tokens::Kwd(Keyword::For)),
                        "do" => result.push(Tokens::Kwd(Keyword::Do)),
                        "continue" => result.push(Tokens::Kwd(Keyword::Continue)),
                        "break" => result.push(Tokens::Kwd(Keyword::Break)),
                        _ => result.push(Tokens::Identifier(s)),
                    }
                }
                
                '(' =>
                {
                    result.push(Tokens::LParen);
                    it.next();
                }
                
                ')' =>
                {
                    result.push(Tokens::RParen);
                    it.next();
                }
                
                '{' =>
                {
                    result.push(Tokens::LBrace);
                    it.next();
                }
                
                '}' =>
                {
                    result.push(Tokens::RBrace);
                    it.next();
                }
                
                '[' =>
                {
                    result.push(Tokens::LBracket);
                    it.next();
                }
                
                ']' =>
                {
                    result.push(Tokens::RBracket);
                    it.next();
                }
                
                ';' =>
                {
                    result.push(Tokens::Semicolon);
                    it.next();
                }
                
                '=' =>
                {
                    it.next();
                    match it.peek()
                    {
                        Some(tmp) => match tmp
                        {
                            '=' =>
                            {
                                result.push(Tokens::Equal);
                                it.next();
                            }
                            
                            '>' =>
                            {
                                result.push(Tokens::GreaterEqual);
                                it.next();
                            }

                            _ => { result.push(Tokens::Assign); }
                        },
                        _ => return Err(format!("Can not peek next char")),
                    }
                }
                
                '<' =>
                {
                    it.next();
                    match it.peek()
                    {
                        Some(tmp) => match tmp
                        {
                            '=' =>
                            {
                                it.next();
                                result.push(Tokens::LessEqual);
                                it.next();
                            }

                            _ =>
                            {
                                result.push(Tokens::Lt);
                                it.next();
                            }
                        },
                        _ => return Err(format!("Can not peek next char")),
                    }
                }
                
                '>' =>
                {
                    it.next();
                    match it.peek()
                    {
                        Some(tmp) => match tmp
                        {
                            '=' =>
                            {
                                result.push(Tokens::GreaterEqual);
                                it.next();
                            }

                            _ =>
                            {
                                result.push(Tokens::Gt);
                                it.next();
                            }
                        },
                        _ => return Err(format!("Can not peek next char")),
                    }
                }
                
                '-' =>
                {
                    result.push(Tokens::Minus);
                    it.next();
                }
                
                '~' =>
                {
                    result.push(Tokens::Tilde);
                    it.next();
                }
                
                '!' =>
                {
                    it.next();
                    match it.peek()
                    {
                        Some(tmp) => match tmp
                        {
                            '=' =>
                            {
                                result.push(Tokens::NotEqual);
                                it.next();
                            }

                            _ => { result.push(Tokens::Exclamation); }
                        },
                        _ => return Err(format!("Can not peek next char")),
                    }
                }
                
                '+' =>
                {
                    result.push(Tokens::Plus);
                    it.next();
                }
                
                '*' =>
                {
                    result.push(Tokens::Multi);
                    it.next();
                }
                
                '/' =>
                {
                    result.push(Tokens::Splash);
                    it.next();
                }
                
                '&' =>
                {
                    it.next();
                    match it.peek()
                    {
                        Some(tmp) => match tmp
                        {
                            '&' =>
                            {
                                result.push(Tokens::And);
                                it.next();
                            }

                            _ => { result.push(Tokens::Addr); }
                        },
                        _ => return Err(format!("Can not peek next char")),
                    }
                }
                
                '|' => 
                {
                    it.next();
                    match it.peek() 
                    {
                        Some(tmp) => match tmp 
                        {
                            '|' => 
                            {
                                result.push(Tokens::Or);
                                it.next();
                            }

                            _ => { return Err(format!("unexpected token {}", c)); }
                        },
                        _ => return Err(format!("Can not peek next char")),
                    }
                }
                
                '?' =>
                {
                    result.push(Tokens::QuestionMark);
                    it.next();
                }
                
                ':' =>
                {
                    result.push(Tokens::Colon);
                    it.next();
                }
                
                ',' =>
                {
                    result.push(Tokens::Comma);
                    it.next();
                }

                ' ' | '\n' | '\t' | '\r' => { it.next(); }

                _ => { return Err(format!("unexpected character {}", c)); }
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
    pub enum Node 
    {
        Prog(String),


        Fn(String, Option<Vec<String>>),
        Stmt(Statement),










        Block,
        Const(i64),
        StringLiteral(String, String),
        Var(String),
        ArrayRef(String),
        AssignNode(String, bool),
        UnExp(lexer::Tokens),
        BinExp(lexer::Tokens),
        Exp,
        ExpOption,
        ConditionalExp,
        LogicalOrExp,
        LogicalAndExp,
        EqualityExp,
        RelationalExp,
        AdditiveExp,
        Term,
        Factor,
        FnCall(String),
        Declare(String, Data),
    }

    #[derive(Eq, PartialEq, Clone, Debug)]
    pub enum Data {
        I64,
        Arr64(i64),
    }

    #[derive(Eq, PartialEq, Clone, Debug)]
    pub enum Statement {
        Return,
        Exp,
        Conditional(String),
        Compound,
        For,
        ForDecl,
        While,
        Do,
        Break,
        Continue,
    }

    #[derive(Eq, PartialEq, Clone, Debug)]
    pub struct Parse {
        pub child: Vec<Parse>,
        pub entry: Node,
    }

    impl Parse {
        pub fn new() -> Parse {
            Parse {
                child: Vec::new(),
                entry: Node::Prog("root".to_string()),
            }
        }
    }

    fn p_logical_or_exp(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {
        let mut log_or_exp_node = Parse::new();
        log_or_exp_node.entry = Node::LogicalOrExp;



        let mut pos = pos;
        let (log_and_exp_node, tmp_pos) = r#try!(p_logical_and_exp(toks, pos));
        pos = tmp_pos;

        let mut tok = &toks[pos];
        pos = pos + 1;
        if *tok != lexer::Tokens::Or {

            log_or_exp_node.child.push(log_and_exp_node);
            pos = pos - 1;
            return Ok((log_or_exp_node, pos));
        }


        let mut lhs = log_and_exp_node;
        while *tok == lexer::Tokens::Or {
            let mut binexp_node = Parse::new();
            binexp_node.entry = Node::BinExp(lexer::Tokens::Or);

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

    fn p_conditional_exp(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {

        let mut conditional_exp_node = Parse::new();
        conditional_exp_node.entry = Node::ConditionalExp;

        let (logical_or_exp_node, pos) = r#try!(p_logical_or_exp(toks, pos));
        conditional_exp_node.child.push(logical_or_exp_node);


        if toks[pos] == lexer::Tokens::QuestionMark {

            let pos = pos + 1;
            let (exp_node, pos) = r#try!(p_exp(toks, pos));

            if toks[pos] != lexer::Tokens::Colon {
                return Err(format!(
                    "Expected `:` in conditional expression, but got {:?} at {}",
                    toks[pos], pos
                ));
            }
            let pos = pos + 1;

            let (next_conditional_exp_node, pos) = r#try!(p_conditional_exp(toks, pos));
            conditional_exp_node.child.push(exp_node);
            conditional_exp_node.child.push(next_conditional_exp_node);
            return Ok((conditional_exp_node, pos));
        } else {
            return Ok((conditional_exp_node, pos));
        }
    }

    fn p_exp_opt(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {

        let mut exp_opt_node = Parse::new();
        exp_opt_node.entry = Node::ExpOption;
        let res = p_exp(toks, pos);
        match res {
            Ok((exp_node, pos)) => {

                exp_opt_node.child.push(exp_node);
                return Ok((exp_opt_node, pos));
            }
            Err(_) => {


                return Ok((exp_opt_node, pos));
            }
        }
    }

    fn p_exp(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {


        let mut exp_node = Parse::new();
        exp_node.entry = Node::Exp;

        let tok = &toks[pos];
        match tok {
            lexer::Tokens::Identifier(var_name) => {

                let mut pos = pos + 1;
                let tok = &toks[pos];
                match tok {
                    lexer::Tokens::Assign => {
                        pos = pos + 1;

                        let mut assign_node = Parse::new();
                        assign_node.entry = Node::AssignNode(var_name.to_string(), false);
                        let (next_exp_node, pos) = r#try!(p_exp(toks, pos));
                        assign_node.child.push(next_exp_node);
                        return Ok((assign_node, pos));
                    }
                    lexer::Tokens::LBracket => {

                        let back_pos = pos - 1;
                        pos = pos + 1;

                        let (index_node, new_pos) = r#try!(p_exp(toks, pos));
                        pos = new_pos;

                        if toks[pos] != lexer::Tokens::RBracket {
                            return Err(format!(
                                "Expected ']' for bracket closing, found {:?} at {}",
                                toks[pos], pos
                            ));
                        }


                        pos = pos + 1;
                        if toks[pos] != lexer::Tokens::Assign {
                            pos = back_pos;
                            let (conditional_node, pos) = r#try!(p_conditional_exp(toks, pos));
                            exp_node.child.push(conditional_node);
                            return Ok((exp_node, pos));
                        }
                        pos = pos + 1;

                        let mut assign_node = Parse::new();
                        assign_node.entry = Node::AssignNode(var_name.to_string(), true);
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

                let (cond_node, pos) = r#try!(p_conditional_exp(toks, pos));
                exp_node.child.push(cond_node);
                return Ok((exp_node, pos));
            }
        }
    }

    fn p_fn(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {


        if pos >= toks.len() {
            return Err("Out of program length".to_string());
        }
        let tok = &toks[pos];
        if *tok != lexer::Tokens::Kwd(lexer::Keyword::Int) {
            return Err(format!("Expected `int`, found {:?} at {}", toks[pos], pos));
        }
        let mut pos = pos + 1;

        let tok = &toks[pos];
        let mut fn_name = String::new();
        match tok {
            lexer::Tokens::Identifier(n) => {
                fn_name = n.to_string();
            }
            _ => {
                return Err(format!("Expected function name, but not function name"));
            }
        }
        pos = pos + 1;

        let tok = &toks[pos];
        if *tok != lexer::Tokens::LParen {
            return Err(format!("Expected `(`, found {:?} at {}", toks[pos], pos));
        }
        pos = pos + 1;

        let mut arg_list: Vec<String> = Vec::new();
        let mut arg_count = 0;
        while pos < toks.len() && toks[pos] != lexer::Tokens::RParen {


            match &toks[pos] {
                lexer::Tokens::Kwd(lexer::Keyword::Int) => {
                    pos = pos + 1;
                }
                lexer::Tokens::Kwd(lexer::Keyword::Void) => {
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

            match &toks[pos] {
                lexer::Tokens::Identifier(var_name) => {
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

            match &toks[pos] {
                lexer::Tokens::Comma => {
                    pos = pos + 1;
                }
                lexer::Tokens::RParen => {
                    continue;
                }
                _ => {
                    return Err(format!(
                        "Expected `,` or `)` at the end of one var_name, found {:?} at {}",
                        toks[pos], pos
                    ));
                }
            }
            if toks[pos] == lexer::Tokens::RParen {
                break;
            }
        }
        let tok = &toks[pos];
        if *tok != lexer::Tokens::RParen {
            return Err(format!("Expected `)`, found {:?} at {}", toks[pos], pos));
        }
        pos = pos + 1;

        let tok = &toks[pos];
        if *tok != lexer::Tokens::LBrace {
            return Err(format!("Expected `{{`, found {:?} at {}", toks[pos], pos));
        }
        pos = pos + 1;

        let mut fn_node = Parse::new();
        if arg_list.is_empty() {
            fn_node.entry = Node::Fn(fn_name, None);
        } else {
            fn_node.entry = Node::Fn(fn_name, Some(arg_list));
        }

        while pos < toks.len() && toks[pos] != lexer::Tokens::RBrace {
            let (block_node, tmp_pos) = r#try!(p_block(toks, pos));
            pos = tmp_pos;
            fn_node.child.push(block_node);
        }

        if pos >= toks.len() {
            return Err(format!("Missing `}}`"));
        }
        if toks[pos] != lexer::Tokens::RBrace {
            return Err(format!("Expected `}}`, found {:?} at {}", toks[pos], pos));
        }
        pos = pos + 1;

        //println!("out p_fn with pos: {}", pos);
        Ok((fn_node, pos))
    }

    fn p_declare(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {

        let tok = &toks[pos];
        match tok {
            lexer::Tokens::Kwd(lexer::Keyword::Int) => {


                let pos = pos + 1;

                let tok = &toks[pos];
                match tok {
                    lexer::Tokens::Identifier(var_name) => {
                        let mut stmt_node = Parse::new();
                        stmt_node.entry = Node::Declare(var_name.to_string(), Data::I64);
                        let pos = pos + 1;
                        let tok = &toks[pos];
                        match tok {
                            lexer::Tokens::Assign => {


                                let pos = pos + 1;
                                let (exp_node, pos) = r#try!(p_exp(toks, pos));

                                let tok = &toks[pos];
                                if *tok != lexer::Tokens::Semicolon {
                                    return Err(format!(
                                        "Expected `;`, found {:?} at {}",
                                        toks[pos], pos
                                    ));
                                }
                                let pos = pos + 1;
                                stmt_node.child.push(exp_node);
                                return Ok((stmt_node, pos));
                            }
                            lexer::Tokens::Semicolon => {


                                let pos = pos + 1;
                                return Ok((stmt_node, pos));
                            }
                            lexer::Tokens::LBracket => {



                                if cfg!(feature = "debug") {
                                    println!("here in p_declare -> LBraket");
                                }
                                let mut declare_node = Parse::new();
                                let pos = pos + 1;
                                let tok = &toks[pos];
                                match tok {
                                    lexer::Tokens::Literal(n) => {
                                        declare_node.entry = Node::Declare(
                                            var_name.to_string(),
                                            Data::Arr64(*n),
                                        );
                                        let pos = pos + 1;
                                        let tok = &toks[pos];
                                        if *tok != lexer::Tokens::RBracket {
                                            return Err(format!(
                                                "Expected `]` for array declaration, found {:?} at {}",
                                                toks[pos], pos
                                            ));
                                        }

                                        let pos = pos + 1;
                                        let tok = &toks[pos];
                                        if *tok != lexer::Tokens::Semicolon {
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

    fn p_block(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {
        let tok = &toks[pos];
        match tok {
            lexer::Tokens::Kwd(lexer::Keyword::Int) => {




                let (declare_node, pos) = r#try!(p_declare(toks, pos));


                return Ok((declare_node, pos));
            }
            _ => {




                let (stmt_node, pos) = r#try!(p_stmt(toks, pos));
                return Ok((stmt_node, pos));

                //return Ok((block_node, pos));
            }
        }
    }
    fn p_stmt(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {

        let tok = &toks[pos];
        match tok {
            lexer::Tokens::LBrace => {

                let mut pos = pos + 1;
                let mut stmt_node = Parse::new();
                stmt_node.entry = Node::Stmt(Statement::Compound);


                while toks[pos] != lexer::Tokens::RBrace {
                    let (block_node, tmp_pos) = r#try!(p_block(toks, pos));
                    stmt_node.child.push(block_node);
                    pos = tmp_pos;
                }


                pos = pos + 1;
                return Ok((stmt_node, pos));
            }
            lexer::Tokens::Kwd(lexer::Keyword::Ret) => {

                let pos = pos + 1;
                let (exp_node, mut pos) = r#try!(p_exp(toks, pos));

                let tok = &toks[pos];
                if *tok != lexer::Tokens::Semicolon {
                    return Err(format!(
                        "Expected `;` in statement, found {:?} at {}",
                        toks[pos], pos
                    ));
                }
                pos = pos + 1;

                let mut stmt_node = Parse::new();
                stmt_node.entry = Node::Stmt(Statement::Return);
                stmt_node.child.push(exp_node);
                return Ok((stmt_node, pos));
            }
            lexer::Tokens::Kwd(lexer::Keyword::If) => {


                let mut stmt_node = Parse::new();
                stmt_node.entry = Node::Stmt(Statement::Conditional("if".to_string()));
                let pos = pos + 1;
                if pos >= toks.len() || toks[pos] != lexer::Tokens::LParen {
                    return Err(format!("Missing `(`"));
                }

                if cfg!(feature = "debug") {
                    println!("here pos = {}", pos);
                }
                let pos = pos + 1;
                let (exp_node, pos) = r#try!(p_exp(toks, pos));

                if pos >= toks.len() || toks[pos] != lexer::Tokens::RParen {
                    return Err(format!("Missing `)`"));
                }

                let pos = pos + 1;

                if cfg!(feature = "debug") {
                    println!("If: parse stmt from pos = {}, tok: {:?}", pos, toks[pos]);
                }
                let (clause_1_node, pos) = r#try!(p_stmt(toks, pos));
                stmt_node.child.push(exp_node);
                stmt_node.child.push(clause_1_node);



                if pos < toks.len() && toks[pos] == lexer::Tokens::Kwd(lexer::Keyword::Else) {

                    let pos = pos + 1;
                    let (clause_2_node, pos) = r#try!(p_stmt(toks, pos));
                    stmt_node.child.push(clause_2_node);
                    return Ok((stmt_node, pos));
                } else {
                    return Ok((stmt_node, pos));
                }
            }
            lexer::Tokens::Kwd(lexer::Keyword::For) => {


                let mut stmt_node = Parse::new();
                let pos = pos + 1;
                if pos >= toks.len() || toks[pos] != lexer::Tokens::LParen {
                    return Err(format!("Missing `(`"));
                }
                let pos = pos + 1;

                let decl_res = p_declare(toks, pos);
                match decl_res {
                    Ok((declare_node, pos)) => {

                        stmt_node.child.push(declare_node);
                        stmt_node.entry = Node::Stmt(Statement::ForDecl);

                        let (exp_opt_node, pos) = r#try!(p_exp_opt(toks, pos));
                        stmt_node.child.push(exp_opt_node);

                        if pos >= toks.len() || toks[pos] != lexer::Tokens::Semicolon {
                            return Err(format!("Missing `;` needed by For"));
                        }
                        let pos = pos + 1;

                        let (exp_opt_node, pos) = r#try!(p_exp_opt(toks, pos));
                        stmt_node.child.push(exp_opt_node);
                        if pos >= toks.len() || toks[pos] != lexer::Tokens::RParen {
                            return Err(format!("Missing `)` needed by For"));
                        }
                        let pos = pos + 1;
                        if cfg!(feature = "debug") {
                            println!("pos: {} tok: {:?} before compound layer", pos, toks[pos]);
                        }
                        let mut compound_layer_node = Parse::new();
                        compound_layer_node.entry = Node::Stmt(Statement::Compound);
                        let (next_stmt_node, pos) = r#try!(p_stmt(toks, pos));
                        compound_layer_node.child.push(next_stmt_node);
                        stmt_node.child.push(compound_layer_node);
                        return Ok((stmt_node, pos));
                    }
                    Err(_) => {

                        stmt_node.entry = Node::Stmt(Statement::For);
                        let (exp_opt_node, pos) = r#try!(p_exp_opt(toks, pos));
                        stmt_node.child.push(exp_opt_node);

                        if pos >= toks.len() || toks[pos] != lexer::Tokens::Semicolon {
                            return Err(format!("Missing `;` needed by for"));
                        }
                        let pos = pos + 1;

                        let (exp_opt_node, pos) = r#try!(p_exp_opt(toks, pos));
                        stmt_node.child.push(exp_opt_node);

                        if pos >= toks.len() || toks[pos] != lexer::Tokens::Semicolon {
                            return Err(format!("Missing `;` needed by for"));
                        }
                        let pos = pos + 1;

                        let (exp_opt_node, pos) = r#try!(p_exp_opt(toks, pos));
                        stmt_node.child.push(exp_opt_node);

                        if pos >= toks.len() || toks[pos] != lexer::Tokens::RParen {
                            return Err(format!("Missing `)` needed by for"));
                        }
                        let pos = pos + 1;
                        let mut compound_layer_node = Parse::new();
                        let (next_stmt_node, pos) = r#try!(p_stmt(toks, pos));
                        compound_layer_node.child.push(next_stmt_node);
                        compound_layer_node.entry = Node::Stmt(Statement::Compound);
                        stmt_node.child.push(compound_layer_node);
                        return Ok((stmt_node, pos));
                    }
                }
            }
            lexer::Tokens::Kwd(lexer::Keyword::While) => {

                let mut stmt_node = Parse::new();
                stmt_node.entry = Node::Stmt(Statement::While);
                let pos = pos + 1;
                if pos >= toks.len() || toks[pos] != lexer::Tokens::LParen {
                    return Err(format!("Missing `(` needed by While"));
                }

                let pos = pos + 1;
                let (exp_node, pos) = r#try!(p_exp(toks, pos));
                stmt_node.child.push(exp_node);
                if pos >= toks.len() || toks[pos] != lexer::Tokens::RParen {
                    return Err(format!("Missing `)`"));
                }
                let pos = pos + 1;

                let (next_stmt_node, pos) = r#try!(p_stmt(toks, pos));
                stmt_node.child.push(next_stmt_node);
                return Ok((stmt_node, pos));
            }
            lexer::Tokens::Kwd(lexer::Keyword::Do) => {

                let mut stmt_node = Parse::new();
                stmt_node.entry = Node::Stmt(Statement::Do);
                let pos = pos + 1;
                let (next_stmt_node, pos) = r#try!(p_stmt(toks, pos));
                stmt_node.child.push(next_stmt_node);

                if pos >= toks.len() || toks[pos] != lexer::Tokens::Kwd(lexer::Keyword::While) {
                    return Err(format!("Missing `while` needed by do"));
                }
                let pos = pos + 1;

                if pos >= toks.len() || toks[pos] != lexer::Tokens::LParen {
                    return Err(format!("Missing `(` needed by do"));
                }
                let pos = pos + 1;

                let (exp_node, pos) = r#try!(p_exp_opt(toks, pos));

                if pos >= toks.len() || toks[pos] != lexer::Tokens::RParen {
                    return Err(format!("Missing `)` needed by do"));
                }
                let pos = pos + 1;

                if pos >= toks.len() || toks[pos] != lexer::Tokens::Semicolon {
                    return Err(format!("Missing `;` needed by do"));
                }
                let pos = pos + 1;

                stmt_node.child.push(exp_node);
                return Ok((stmt_node, pos));
            }
            lexer::Tokens::Kwd(lexer::Keyword::Continue) => {
                let mut stmt_node = Parse::new();
                stmt_node.entry = Node::Stmt(Statement::Continue);
                let pos = pos + 1;
                if pos >= toks.len() || toks[pos] != lexer::Tokens::Semicolon {
                    return Err(format!("Missing `;` needed by continue"));
                }
                let pos = pos + 1;
                return Ok((stmt_node, pos));
            }
            lexer::Tokens::Kwd(lexer::Keyword::Break) => {
                let mut stmt_node = Parse::new();
                stmt_node.entry = Node::Stmt(Statement::Break);
                let pos = pos + 1;
                if pos >= toks.len() || toks[pos] != lexer::Tokens::Semicolon {
                    return Err(format!("Missing `;` needed by break"));
                }
                let pos = pos + 1;
                return Ok((stmt_node, pos));
            }
            _ => {

                let mut stmt_node = Parse::new();
                stmt_node.entry = Node::Stmt(Statement::Exp);
                //let pos = pos + 1;
                let (exp_opt_node, pos) = r#try!(p_exp_opt(toks, pos));

                let tok = &toks[pos];
                if *tok != lexer::Tokens::Semicolon {
                    return Err(format!("Expected `;`, found {:?} at {}", toks[pos], pos));
                }
                let pos = pos + 1;
                stmt_node.child.push(exp_opt_node);
                return Ok((stmt_node, pos));
            }
        }
    }

    fn p_factor(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {
        if cfg!(feature = "debug") {
            println!("in p_factor with pos: {}, tok = {:?}", pos, toks[pos]);
        }
        let mut next = &toks[pos];
        let mut pos = pos + 1;

        match next {
            lexer::Tokens::LParen => {


                let (exp_node, tmp_pos) = r#try!(p_exp(toks, pos));
                pos = tmp_pos;
                next = &toks[pos];
                pos = pos + 1;
                if *next != lexer::Tokens::RParen {
                    return Err(format!(
                        "Expected `)` in file:parser.rs, found {:?} at {}",
                        toks[pos], pos
                    ));
                }
                let mut factor_node = Parse::new();
                factor_node.entry = Node::Factor;
                factor_node.child.push(exp_node);

                return Ok((factor_node, pos));
            }
            lexer::Tokens::Minus | lexer::Tokens::Tilde | lexer::Tokens::Exclamation | lexer::Tokens::Addr => {

                let mut factor_node = Parse::new();
                let mut unexp_node = Parse::new();
                factor_node.entry = Node::Factor;
                unexp_node.entry = Node::UnExp(match next {
                    lexer::Tokens::Minus => lexer::Tokens::Minus,
                    lexer::Tokens::Tilde => lexer::Tokens::Tilde,
                    lexer::Tokens::Exclamation => lexer::Tokens::Exclamation,
                    lexer::Tokens::Addr => lexer::Tokens::Addr,
                    _ => panic!("Something strange"),
                });
                let (next_factor_node, pos) = r#try!(p_factor(toks, pos));
                unexp_node.child.push(next_factor_node);
                factor_node.child.push(unexp_node);
                return Ok((factor_node, pos));
            }
            lexer::Tokens::String(chars, tag) => {
                let mut string_node = Parse::new();
                let mut factor_node = Parse::new();
                string_node.entry = Node::StringLiteral(chars.to_string(), tag.to_string());
                factor_node.entry = Node::Factor;
                factor_node.child.push(string_node);

                return Ok((factor_node, pos));
            }
            lexer::Tokens::Literal(n) => {

                let mut const_node = Parse::new();
                let mut factor_node = Parse::new();
                const_node.entry = Node::Const(*n);
                factor_node.entry = Node::Factor;
                factor_node.child.push(const_node);

                return Ok((factor_node, pos));
            }
            lexer::Tokens::Identifier(var_name) => {
                if cfg!(feature = "debug") {
                    println!("here\n");
                }
                if pos < toks.len() && toks[pos] == lexer::Tokens::LParen {

                    let mut factor_node = Parse::new();
                    pos = pos - 1;
                    factor_node.entry = Node::Factor;
                    let (fn_call_node, pos) = r#try!(p_fn_call(toks, pos));
                    factor_node.child.push(fn_call_node);
                    return Ok((factor_node, pos));
                } else if pos < toks.len() && toks[pos] == lexer::Tokens::LBracket {

                    let mut factor_node = Parse::new();
                    pos = pos - 1;
                    factor_node.entry = Node::Factor;
                    let (arr_ref_node, pos) = r#try!(p_arr_ref(toks, pos));
                    factor_node.child.push(arr_ref_node);
                    return Ok((factor_node, pos));
                } else {

                    let mut var_node = Parse::new();
                    let mut factor_node = Parse::new();
                    var_node.entry = Node::Var(var_name.to_string());
                    factor_node.entry = Node::Factor;
                    factor_node.child.push(var_node);

                    return Ok((factor_node, pos));
                }
            }
            _ => Err(format!("Factor rule not allowed.")),
        }
    }

    fn p_arr_ref(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {

        let mut arr_ref_node = Parse::new();
        let mut var_name = String::new();
        match &toks[pos] {
            lexer::Tokens::Identifier(name) => {
                var_name = name.to_string();
            }
            _ => {
                return Err(format!(
                    "Expected array identifier, foudn {:?} at {}",
                    toks[pos], pos
                ));
            }
        }
        arr_ref_node.entry = Node::ArrayRef(var_name);

        let mut pos = pos + 1;

        match toks[pos] {
            lexer::Tokens::LBracket => {
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
            lexer::Tokens::RBracket => {
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
    fn p_fn_call(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {

        //println!("in fn p_fn_call");
        let mut fn_call_node = Parse::new();
        let mut fn_name = String::new();
        match &toks[pos] {
            lexer::Tokens::Identifier(name) => {
                fn_name = name.to_string();
            }
            _ => {
                return Err(format!(
                    "Expected function name, found {:?} at {}",
                    toks[pos], pos
                ));
            }
        }
        fn_call_node.entry = Node::FnCall(fn_name);
        let mut pos = pos + 1;

        match toks[pos] {
            lexer::Tokens::LParen => {
                pos = pos + 1;
            }
            _ => {
                return Err(format!(
                    "Expected `(` needed by function call, found {:?} at {}",
                    toks[pos], pos
                ));
            }
        }
        while pos < toks.len() && toks[pos] != lexer::Tokens::RParen {

            let (exp_node, new_pos) = r#try!(p_exp(toks, pos));
            fn_call_node.child.push(exp_node);
            pos = new_pos;


            match &toks[pos] {
                lexer::Tokens::Comma => {
                    pos = pos + 1;
                }
                lexer::Tokens::RParen => {
                    continue;
                }
                _ => {
                    return Err(format!(
                        "Expected `,` or `)` at the end of exp, found {:?} at {}",
                        toks[pos], pos
                    ));
                }
            }
            if toks[pos] == lexer::Tokens::RParen {
                break;
            }
        }
        pos = pos + 1;
        return Ok((fn_call_node, pos));
    }

    fn p_logical_and_exp(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {
        let mut logAndExp_node = Parse::new();
        logAndExp_node.entry = Node::LogicalAndExp;


        let mut pos = pos;
        let (eq_node, tmp_pos) = r#try!(p_eq_exp(toks, pos));
        pos = tmp_pos;
        let mut tok = &toks[pos];
        pos = pos + 1;
        if *tok != lexer::Tokens::And {
            logAndExp_node.child.push(eq_node);
            pos = pos - 1;
            return Ok((logAndExp_node, pos));
        }

        let mut eq_node = eq_node;
        while *tok == lexer::Tokens::And {
            let mut binexp_node = Parse::new();
            binexp_node.entry = Node::BinExp(lexer::Tokens::And);

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

    fn p_eq_exp(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {
        let mut eq_node = Parse::new();
        eq_node.entry = Node::EqualityExp;

        let mut pos = pos;
        let (relational_node, tmp_pos) = r#try!(p_relational_exp(toks, pos));
        pos = tmp_pos;
        let mut tok = &toks[pos];
        pos = pos + 1;
        if *tok != lexer::Tokens::NotEqual && *tok != lexer::Tokens::Equal {
            eq_node.child.push(relational_node);
            pos = pos - 1;
            return Ok((eq_node, pos));
        }

        let mut relational_node = relational_node;
        while *tok == lexer::Tokens::Equal || *tok == lexer::Tokens::NotEqual {
            let mut binexp_node = Parse::new();
            binexp_node.entry = Node::BinExp(match tok {
                lexer::Tokens::Equal => lexer::Tokens::Equal,
                lexer::Tokens::NotEqual => lexer::Tokens::NotEqual,
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

    fn p_relational_exp(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {
        let mut relational_node = Parse::new();
        relational_node.entry = Node::RelationalExp;

        let mut pos = pos;
        let (additive_exp_node, tmp_pos) = r#try!(p_additive_exp(toks, pos));
        pos = tmp_pos;
        let mut tok = &toks[pos];
        pos = pos + 1;
        if *tok != lexer::Tokens::Lt
            && *tok != lexer::Tokens::Gt
            && *tok != lexer::Tokens::GreaterEqual
            && *tok != lexer::Tokens::LessEqual
        {
            relational_node.child.push(additive_exp_node);
            pos = pos - 1;
            return Ok((relational_node, pos));
        }

        let mut additive_exp_node = additive_exp_node;
        while *tok == lexer::Tokens::Lt
            || *tok == lexer::Tokens::Gt
            || *tok == lexer::Tokens::GreaterEqual
            || *tok == lexer::Tokens::LessEqual
        {
            let mut binexp_node = Parse::new();
            binexp_node.entry = Node::BinExp(match tok {
                lexer::Tokens::Lt => lexer::Tokens::Lt,
                lexer::Tokens::Gt => lexer::Tokens::Gt,
                lexer::Tokens::GreaterEqual => lexer::Tokens::GreaterEqual,
                lexer::Tokens::LessEqual => lexer::Tokens::LessEqual,
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
    fn p_term(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {

        let mut term_node = Parse::new();
        term_node.entry = Node::Term;


        let mut pos = pos;
        let (factor_node, tmp_pos) = r#try!(p_factor(toks, pos));
        pos = tmp_pos;
        let mut tok = &toks[pos];
        pos = pos + 1;
        if *tok != lexer::Tokens::Multi && *tok != lexer::Tokens::Splash {
            term_node.child.push(factor_node);
            pos = pos - 1;

            return Ok((term_node, pos));
        }


        let mut factor_node = factor_node;
        while *tok == lexer::Tokens::Multi || *tok == lexer::Tokens::Splash {
            let mut binexp_node = Parse::new();
            binexp_node.entry = Node::BinExp(match tok {
                lexer::Tokens::Multi => lexer::Tokens::Multi,
                lexer::Tokens::Splash => lexer::Tokens::Splash,
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

        return Ok((term_node, pos));
    }

    fn p_additive_exp(toks: &[lexer::Tokens], pos: usize) -> Result<(Parse, usize), String> {

        let mut exp_node = Parse::new();
        exp_node.entry = Node::AdditiveExp;

        let mut pos = pos;
        let (term_node, tmp_pos) = r#try!(p_term(toks, pos));
        pos = tmp_pos;
        let mut tok = &toks[pos];
        if *tok != lexer::Tokens::Plus && *tok != lexer::Tokens::Minus {
            exp_node.child.push(term_node);

            return Ok((exp_node, pos));
        }

        //peek next token, if it is lexer::Tokens::Plus or lexer::Tokens::Minus
        let mut term_node = term_node;
        let mut pos = pos;
        while *tok == lexer::Tokens::Plus || *tok == lexer::Tokens::Minus {
            let mut binexp_node = Parse::new();
            binexp_node.entry = Node::BinExp(match tok {
                lexer::Tokens::Plus => lexer::Tokens::Plus,
                lexer::Tokens::Minus => lexer::Tokens::Minus,
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

    pub fn parse_prog(input: &str, c_src_name: &str) -> Result<Parse, String> {
        let toks = r#try!(lexer::lex(&input));
        let mut prog_node = Parse::new();
        prog_node.entry = Node::Prog(c_src_name.to_string());
        let mut pos = 0;

        while pos < toks.len() {

            let p_res = p_declare(&toks, pos);
            match p_res {
                Ok((decl_node, new_pos)) => {
                    pos = new_pos;
                    prog_node.child.push(decl_node);
                }
                Err(_) => {

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


    pub fn print(tree: &Parse, idt: usize) -> String {
        let mut idt_prefix = String::new();
        for _i in 0..idt {
            idt_prefix = idt_prefix + " ";
        }
        match &tree.entry {
            Node::StringLiteral(data, tag) => format!(
                "{}n_type: StringLiteral, tag: {}, data: [{}]",
                idt_prefix, tag, data,
            ),
            Node::ArrayRef(var_name) => format!(
                "{}n_type: ArrayRef, var_name : {}, [\n{}\n{}]",
                idt_prefix,
                var_name,
                print(tree.child.get(0).unwrap(), idt + 1),
                idt_prefix
            ),
            Node::Factor => format!(
                "{}n_type: Factor, [\n{}\n{}]",
                idt_prefix,
                print(
                    tree.child.get(0).expect("Factor Node has no child"),
                    idt + 1
                ),
                idt_prefix
            ),
            Node::AssignNode(var_name, flag) => {
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
            Node::BinExp(op) => format!(
                "{}n_type: BinExp, Op: {} [\n{}\n{}\n{}]",
                idt_prefix,
                match op {
                    lexer::Tokens::Minus => format!("-"),
                    lexer::Tokens::Plus => format!("+"),
                    lexer::Tokens::Multi => format!("*"),
                    lexer::Tokens::Splash => format!("/"),
                    lexer::Tokens::And => format!("&&"),
                    lexer::Tokens::Or => format!("||"),
                    lexer::Tokens::Equal => format!("=="),
                    lexer::Tokens::NotEqual => format!("!="),
                    lexer::Tokens::GreaterEqual => format!(">="),
                    lexer::Tokens::LessEqual => format!("<="),
                    lexer::Tokens::Lt => format!("<"),
                    lexer::Tokens::Gt => format!(">"),
                    _ => panic!(format!(
                        "Operator `{:?}` for Binary Expression not supported",
                        &op
                    )),
                },
                print(tree.child.get(0).expect("BinExp no lhs"), idt + 1),
                print(tree.child.get(1).expect("BinExp no rhs"), idt + 1),
                idt_prefix
            ),
            Node::Term => format!(
                "{}n_type: Term, [\n{}\n{}]",
                idt_prefix,
                print(tree.child.get(0).expect("Term Node has no child"), idt + 1),
                idt_prefix
            ),
            Node::Prog(prog_name) => {
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
            Node::FnCall(fn_name) => {
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

            }
            Node::Fn(fn_name, vars) => {
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
            Node::Declare(var_name, t) => match t {
                Data::I64 => {
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
                Data::Arr64(len) => format!(
                    "{}n_type: Declare, type: Array  var_name: {}, length: {}",
                    idt_prefix, var_name, len,
                ),
            },
            Node::ConditionalExp => {
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
            Node::Stmt(stmt) => match stmt {
                Statement::For => {
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
                Statement::ForDecl => {
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
                Statement::Do => {
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
                Statement::While => {
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
                Statement::Continue => format!("{}n_type: Continue", idt_prefix),
                Statement::Break => format!("{}n_type: Break", idt_prefix),
                Statement::Return => format!(
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
                Statement::Exp => format!(
                    "{}n_type: Stmt::Exp, [\n{}\n{}]",
                    idt_prefix,
                    print(
                        tree.child.get(0).expect("Statement::Exp Node has no child"),
                        idt + 1
                    ),
                    idt_prefix
                ),
                Statement::Conditional(op) => {
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
                Statement::Compound => {
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
            Node::LogicalOrExp |
            Node::LogicalAndExp|
            Node::EqualityExp  |
            Node::RelationalExp|
            Node::AdditiveExp =>
                print(tree.child.get(0).expect(&format!("{:?} Node has no child", tree.entry)), idt),

            Node::ExpOption => match tree.child.len() {
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
            Node::Exp => format!(
                "{}n_type: Exp, [\n{}\n{}]",
                idt_prefix,
                print(
                    tree.child.get(0).expect("Expression Node has no child"),
                    idt + 1
                ),
                idt_prefix
            ),
            Node::Block => format!(
                "{}n_type: Block, [\n{}\n{}]",
                idt_prefix,
                print(tree.child.get(0).expect("Block node has no child"), idt + 1),
                idt_prefix
            ),
            Node::UnExp(Op) => format!(
                "{}n_type: UnExp, Op: {}, [\n{}\n{}]",
                idt_prefix,
                match Op {
                    lexer::Tokens::Minus => "-".to_string(),
                    lexer::Tokens::Tilde => "~".to_string(),
                    lexer::Tokens::Exclamation => "!".to_string(),
                    lexer::Tokens::Addr => "&".to_string(),
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
            Node::Var(var_name) => format!("{}n_type, Variable, Name : {}", idt_prefix, var_name),
            Node::Const(n) => format!("{}n_type: Const, Value: {}", idt_prefix, n),
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
// 4194 /////////////////////////////////////////////////////////////////////////////////////////////////////////////
