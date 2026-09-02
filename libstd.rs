//! # The Rust Standard Library
#![cfg_attr(not(restricted_std), stable(feature = "rust1", since = "1.0.0"))]
#![cfg_attr(
    restricted_std,
    unstable(
        feature = "restricted_std",
        issue = "none",
        reason = "You have attempted to use a standard library built for a platform that it doesn't \
            know how to support. Consider building it for a known environment, disabling it with \
            `#![no_std]` or overriding this warning by enabling this feature."
    )
)]
#![rustc_preserve_ub_checks]
#![doc(
    html_playground_url = "https://play.rust-lang.org/",
    issue_tracker_base_url = "https://github.com/rust-lang/rust/issues/",
    test(no_crate_inject, attr(deny(warnings))),
    test(attr(allow(dead_code, deprecated, unused_variables, unused_mut)))
)]
#![doc(rust_logo)]
#![doc(auto_cfg(hide(no_global_oom_handling)))]
// Don't link to std. We are std.
//#![no_std]
// Tell the compiler to link to either panic_abort or panic_unwind
#![needs_panic_runtime]
//
// Lints:
#![warn(deprecated_in_future)]
#![warn(missing_docs)]
#![warn(missing_debug_implementations)]
#![allow(explicit_outlives_requirements)]
#![allow(unused_lifetimes)]
#![allow(internal_features)]
#![deny(implicit_provenance_casts)]
#![deny(unsafe_op_in_unsafe_fn)]
#![allow(rustdoc::redundant_explicit_links)]
#![warn(rustdoc::unescaped_backticks)]
// Ensure that std can be linked against panic_abort despite compiled with `-C panic=unwind`
#![deny(ffi_unwind_calls)]
// std may use features in a platform-specific way
#![allow(unused_features)]
//
// Features:
#![cfg_attr(
    test,
    feature(internal_output_capture, print_internals, super_let, update_panic_count, rt)
)]
#![cfg_attr(
    all(target_vendor = "fortanix", target_env = "sgx"),
    feature(slice_index_methods, coerce_unsized, sgx_platform)
)]
#![cfg_attr(all(test, target_os = "uefi"), feature(uefi_std))]
#![cfg_attr(target_family = "wasm", feature(stdarch_wasm_atomic_wait))]
#![cfg_attr(target_arch = "wasm64", feature(simd_wasm64))]
//
// Language features:
// tidy-alphabetical-start
#![feature(alloc_error_handler)]
#![feature(allocator_internals)]
#![feature(allow_internal_unsafe)]
#![feature(allow_internal_unstable)]
#![feature(asm_experimental_arch)]
#![feature(autodiff)]
#![feature(cfg_sanitizer_cfi)]
#![feature(cfg_target_thread_local)]
#![feature(cfi_encoding)]
#![feature(const_trait_impl)]
#![feature(decl_macro)]
#![feature(deprecated_suggestion)]
#![feature(diagnostic_on_move)]
#![feature(doc_cfg)]
#![feature(doc_masked)]
#![feature(doc_notable_trait)]
#![feature(dropck_eyepatch)]
#![feature(f16)]
#![feature(f128)]
#![feature(ffi_const)]
#![feature(gpu_offload)]
#![feature(impl_restriction)]
#![feature(intra_doc_pointers)]
#![feature(lang_items)]
#![feature(link_cfg)]
#![feature(linkage)]
#![feature(macro_metavar_expr_concat)]
#![feature(min_specialization)]
#![feature(must_not_suspend)]
#![feature(needs_panic_runtime)]
#![feature(negative_impls)]
#![feature(never_type)]
#![feature(optimize_attribute)]
#![feature(prelude_import)]
#![feature(rustc_attrs)]
#![feature(rustdoc_internals)]
#![feature(staged_api)]
#![feature(stmt_expr_attributes)]
#![feature(strict_provenance_lints)]
#![feature(thread_local)]
#![feature(try_blocks)]
#![feature(try_trait_v2)]
#![feature(type_alias_impl_trait)]
#![feature(unwrap_infallible)]
// tidy-alphabetical-end
//
// Library features (core):
// tidy-alphabetical-start
#![feature(borrowed_buf_init)]
#![feature(bstr)]
#![feature(bstr_internals)]
#![feature(cast_maybe_uninit)]
#![feature(char_internals)]
#![feature(clone_to_uninit)]
#![feature(const_convert)]
#![feature(const_default)]
#![feature(core_float_math)]
#![feature(core_intrinsics)]
#![feature(core_io)]
#![feature(core_io_borrowed_buf)]
#![feature(core_io_internals)]
#![feature(cstr_display)]
#![feature(cursor_split)]
#![feature(drop_guard)]
#![feature(duration_constants)]
#![feature(error_generic_member_access)]
#![feature(error_iter)]
#![feature(exact_size_is_empty)]
#![feature(exclusive_wrapper)]
#![feature(extend_one)]
//#![feature(float_gamma)]
#![feature(float_minimum_maximum)]
#![feature(fmt_internals)]
//#![feature(fn_ptr_trait)]
#![feature(formatting_options)]
#![feature(funnel_shifts)]
#![feature(generic_atomic)]
//#![feature(hash_map_internals)]
//#![feature(hash_map_macro)]
#![feature(hasher_prefixfree_extras)]
#![feature(hashmap_internals)]
#![feature(hint_must_use)]
#![feature(int_from_ascii)]
#![feature(io_error_inprogress)]
#![feature(io_error_more)]
#![feature(io_error_too_many_open_files)]
#![feature(io_error_uncategorized)]
#![feature(io_slice_as_bytes)]
#![feature(ip)]
#![feature(iter_advance_by)]
#![feature(iter_next_chunk)]
#![feature(maybe_dangling)]
#![feature(maybe_uninit_array_assume_init)]
#![feature(maybe_uninit_fill)]
#![feature(panic_can_unwind)]
#![feature(panic_internals)]
#![feature(pin_coerce_unsized_trait)]
#![feature(pointer_is_aligned_to)]
#![feature(portable_simd)]
#![feature(ptr_as_uninit)]
#![feature(ptr_cast_slice)]
#![feature(ptr_mask)]
#![feature(random)]
#![feature(raw_os_error_ty)]
#![feature(seek_io_take_position)]
#![feature(share_trait)]
#![feature(slice_internals)]
#![feature(slice_ptr_get)]
#![feature(slice_range)]
#![feature(slice_split_once)]
#![feature(std_internals)]
#![feature(str_internals)]
#![feature(sync_unsafe_cell)]
#![feature(temporary_niche_types)]
#![feature(ub_checks)]
#![feature(uint_carryless_mul)]
#![feature(used_with_arg)]
// tidy-alphabetical-end
//
// Library features (alloc):
// tidy-alphabetical-start
#![feature(alloc_io)]
#![feature(allocator_api)]
#![feature(clone_from_ref)]
#![feature(get_mut_unchecked)]
#![feature(map_try_insert)]
#![feature(slice_concat_trait)]
#![feature(thin_box)]
#![feature(try_reserve_kind)]
#![feature(try_with_capacity)]
#![feature(unique_rc_arc)]
#![feature(wtf8_internals)]
// tidy-alphabetical-end
//
// Library features (unwind):
// tidy-alphabetical-start
#![feature(panic_unwind)]
// tidy-alphabetical-end
// tidy-alphabetical-start
#![feature(async_iterator)]
#![feature(c_variadic)]
#![feature(cfg_accessible)]
#![feature(cfg_eval)]
#![feature(concat_bytes)]
#![feature(const_format_args)]
#![feature(custom_test_frameworks)]
#![feature(edition_panic)]
#![feature(format_args_nl)]
#![feature(log_syntax)]
#![feature(test)]
#![feature(trace_macros)]
// tidy-alphabetical-end
//
// Only used in tests/benchmarks:
//
// Only for const-ness:
// tidy-alphabetical-start
#![feature(io_const_error)]
// tidy-alphabetical-end
//
#![default_lib_allocator]
// Removed features
#![unstable_removed(
    feature = "concat_idents",
    reason = "Replaced by the macro_metavar_expr_concat feature",
    link = "https://github.com/rust-lang/rust/issues/29599#issuecomment-2986866250",
    since = "1.90.0"
)]

#![allow
(
    deprecated,
    deprecated_in_future,
    unused,
    unused_imports,
    unused_extern_crates,
)]

#[macro_use] extern crate alloc as alloc_crate;
pub mod libc
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod unwind
{
    use crate::
    {
        *,
    };
    /*
    */
}

// The standard macros that are not built-in to the compiler.
#[macro_use] mod macros
{
    use crate::
    {
        *,
    };
    /*
    */
}

// The runtime entry point and a few unstable public functions used by the compiler
#[macro_use]
pub mod rt
{
    //! Runtime services
    use crate::
    {
        *,
    };
    /*
    #[rustfmt::skip]
    pub use crate::panicking::{begin_panic, panic_count};
    pub use core::panicking::{panic_display, panic_fmt};

    #[rustfmt::skip]
    use crate::any::Any;
    use crate::sync::Once;
    use crate::thread::{self, main_thread};
    use crate::{mem, panic, sys};
    */
    /*
    This function is needed by the panic runtime. */
    #[cfg(not(test))] #[rustc_std_internal_symbol] fn __rust_abort() { crate::process::abort(); }
    /*
    Prints to the "panic output", depending on the platform this may be:
    - the standard error output
    - some dedicated platform specific output
    - nothing (so this macro is a no-op) */
    macro_rules! rtprintpanic
    {
        ($($t:tt)*) =>
        {
            #[cfg(not(panic = "immediate-abort"))]
            if let Some(mut out) = crate::sys::stdio::panic_output()
            {
                let _ = crate::io::Write::write_fmt(&mut out, format_args!($($t)*));
            }

            #[cfg(panic = "immediate-abort")]
            {
                let _ = format_args!($($t)*);
            }
        }
    }

    macro_rules! rtabort
    {
        ($($t:tt)*) =>
        {
            {
                rtprintpanic!("fatal runtime error: {}, aborting\n", format_args!($($t)*));
                crate::process::abort();
            }
        }
    }

    macro_rules! rtassert
    {
        ($e:expr) =>
        {
            if !$e { rtabort!(concat!("assertion failed: ", stringify!($e))); }
        };
    }

    macro_rules! rtunwrap
    {
        ($ok:ident, $e:expr) =>
        {
            match $e
            {
                $ok(v) => v,
                ref err =>
                {
                    let err = err.as_ref().map(drop);
                    rtabort!(concat!("unwrap failed: ", stringify!($e), " = {:?}"), err)
                }
            }
        };
    }

    fn handle_rt_panic<T>(e: Box<dyn Any + Send>) -> T
    {
        mem::forget(e);
        rtabort!("initialization or cleanup bug");
    }
    // One-time runtime initialization.
    #[cfg_attr(test, allow(dead_code))]
    unsafe fn init(argc: isize, argv: *const *const u8, sigpipe: u8)
    {
        unsafe { main_thread::set(thread::current_id()) };
    }
    /*
    Clean up the thread-local runtime state. */
    pub(crate) fn thread_cleanup(){ panic::catch_unwind( || { crate::thread::drop_current(); }).unwrap_or_else(handle_rt_panic); }

    // One-time runtime cleanup.
    pub(crate) fn cleanup()
    {
        static CLEANUP: Once = Once::new();
        CLEANUP.call_once(|| unsafe
        {
            crate::io::cleanup();
            sys::cleanup();
        });
    }

    // To reduce the generated code of the new `lang_start`, this function is doing the real work.
    #[cfg(not(test))]
    fn lang_start_internal( a: &(dyn Fn() -> i32 + Sync + crate::panic::RefUnwindSafe), b: isize, c: *const *const u8, d: u8 ) -> isize
    {
        // Guard against the code called by this function from unwinding outside of the Rust-controlled code, which is UB.
        panic::catch_unwind( move ||
        {
            unsafe { init(b, c, d) };

            let g = panic::catch_unwind( a ).unwrap_or_else( move | e |
            {
                let e = panic::AssertUnwindSafe(e);
                panic::catch_unwind(move || drop({ e }.0)).unwrap_or_else(move |f| 
                {
                    mem::forget(f);
                    rtabort!("drop of the panic payload panicked");
                });
                
                101
            });

            let g = g as isize;

            cleanup();
            crate::sys::exit::unique_thread_exit();

            g
        }).unwrap_or_else( handle_rt_panic )
    }

    #[cfg(not(any(test, doctest)))]
    #[lang = "start"]
    fn lang_start<T: crate::process::Termination + 'static>( a: fn() -> T, b: isize, c: *const *const u8, d: u8 ) -> isize 
    {  lang_start_internal( &move || crate::sys::backtrace::__rust_begin_short_backtrace( a ).report().to_i32(), b, c, d )  }
}

pub use std::
{
    any,
    array,
    async_iter,
    cell,
    char,
    clone,
    cmp,
    convert,
    default,
    field,
    future,
    hint,
    i8,
    i16,
    i32,
    i64,
    i128,
    intrinsics,
    isize,
    iter,
    marker,
    mem,
    ops,
    option,
    pin,
    ptr,
    range,
    result,
    u8,
    u16,
    u32,
    u64,
    u128,
    unsafe_binder,
    usize,
};

pub use std::
{
    borrow,
    boxed,
    fmt,
    format,
    rc,
    slice,
    str,
    string,
    vec,
};

pub mod f128
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod f16
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod f32
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod f64
{
    use crate::
    {
        *,
    };
    /*
    */
}

#[macro_use] pub mod thread
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod ascii
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod backtrace
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod bstr
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod collections
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod env
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod error
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod ffi
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod fs
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod hash
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod io
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod net
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod num
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod os
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod panic
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod pat
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod path
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod process
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod random
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod sync
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod time
{
    use crate::
    {
        *,
    };
    /*
    */
}

mod std_float
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod simd
{
    pub use core::simd::*;
    pub use crate::std_float::StdFloat;
}

pub mod autodiff
{
    /// This macro handles automatic differentiation.
    pub use core::autodiff::{autodiff_forward, autodiff_reverse};
}

pub mod offload
{
    pub use core::offload::{offload, offload_kernel};
}

pub mod task 
{
    //! Types and Traits for working with asynchronous tasks.

    pub use alloc::task::*;
    pub use core::task::*;
}

pub mod arch
{
    pub use core::arch::*;
    pub use std_detect::is_aarch64_feature_detected;
    pub use std_detect::is_arm_feature_detected;
    pub use std_detect::is_loongarch_feature_detected;
    pub use std_detect::is_riscv_feature_detected;
    pub use std_detect::is_s390x_feature_detected;
    pub use std_detect::is_x86_feature_detected;
    pub use std_detect::{is_mips_feature_detected, is_mips64_feature_detected};
    pub use std_detect::{is_powerpc_feature_detected, is_powerpc64_feature_detected};
} pub use std_detect::is_x86_feature_detected;

mod sys
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub mod alloc
{
    use crate::
    {
        *,
    };
    /*
    */
}

mod panicking
{
    use crate::
    {
        *,
    };
    /*
    */
}

#[allow(dead_code, unused_attributes, implicit_provenance_casts, unsafe_op_in_unsafe_fn)]
mod backtrace_rs
{
    use crate::
    {
        *,
    };
    /*
    */
}

pub use std::cfg_select;
pub use core::concat_bytes;
pub use core::derive;
pub use core::matches;
pub use core::primitive;
pub use core::todo;

pub use core::
{
    assert, cfg, column, compile_error, concat, const_format_args, env, file, format_args,
    format_args_nl, include, include_bytes, include_str, line, log_syntax, module_path, option_env,
    stringify, trace_macros,
};


pub use core::
{
    assert_eq, assert_ne, debug_assert, debug_assert_eq, debug_assert_ne, r#try, unimplemented,
    unreachable, write, writeln,
};

pub use core::{assert_matches, debug_assert_matches};

pub mod from
{
    pub use core::from::From;
}


mod __restricted_std_workaround
{

}

mod sealed
{
    pub trait Sealed {}
}

macro_rules! impl_sealed
{
    ($($t:ty)*) => 
    {
        $(
            impl crate::sealed::Sealed for $t {}
        )*
    }
}

impl_sealed! { isize i8 i16 i32 i64 i128 usize u8 u16 u32 u64 u128 f32 f64 }
