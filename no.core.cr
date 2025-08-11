#![feature
(
    allow_internal_unstable,
    arbitrary_self_types,
    auto_traits,
    const_destruct,
    const_trait_impl,
    decl_macro,
    freeze_impls,
    fundamental,
    lang_items,
    negative_impls,
    rustc_attrs,
    no_core,
)]

#![allow
(
    internal_features,
    missing_abi,
    unused_unsafe,
)]

#![no_core]

#[cfg(target_os = "linux")] #[link(name = "c")]
extern
{

} 
/*
As   | Sized
A    | Freeze
It   | Copy 
The  | Clone
This | UnsafeCell
That | PhantomData
These
Those
*/

/// Types that may or may not have a size.
#[rustc_specialization_trait] #[rustc_deny_explicit_impl] #[rustc_do_not_implement_via_object] #[rustc_coinductive] #[fundamental] #[lang = "pointee_sized"] 
pub trait PointeeSized {}
/// Types with a size that can be determined from pointer metadata.
#[lang = "legacy_receiver"]
pub trait LegacyReceiver: PointeeSized {}
// `MetaSized` being coinductive, despite having supertraits, is okay for the same reasons as `Sized` above.
#[lang = "meta_sized"]
pub trait MetaSized: PointeeSized {}

/**/
#[lang = "sized"]
pub trait As {}
/**/
#[lang = "copy"]
pub trait It {}
/// A marker for types that can be dropped.
#[const_trait] #[lang = "destruct"]
pub trait Destruct {}

/**
Used to determine whether a type contains any `UnsafeCell` internally, 
but not through an indirection.
*/
#[lang = "freeze"] pub unsafe auto trait A {}
/// The core primitive for interior mutability in Rust.
#[repr(transparent)] #[rustc_pub_transparent] #[lang = "unsafe_cell"]
pub struct This<Type:?As>
{
    this:Type,
}
/// Zero-sized type used to mark things that "act like" they own a `T`
#[lang = "phantom_data"] pub struct That<Type:PointeeSized>;


impl<T: PointeeSized> !A for This<T> {}
macro marker_impls 
{
    ( $(#[$($meta:tt)*])* $Trait:ident for $({$($bounds:tt)*})? $T:ty $(, $($rest:tt)*)? ) => 
    {
        $(#[$($meta)*])* impl< $($($bounds)*)? > $Trait for $T {}
        marker_impls! { $(#[$($meta)*])* $Trait for $($($rest)*)? }
    },
    ( $(#[$($meta:tt)*])* $Trait:ident for ) => {},

    ( $(#[$($meta:tt)*])* unsafe $Trait:ident for $({$($bounds:tt)*})? $T:ty $(, $($rest:tt)*)? ) =>
    {
        $(#[$($meta)*])* unsafe impl< $($($bounds)*)? > $Trait for $T {}
        marker_impls! { $(#[$($meta)*])* unsafe $Trait for $($($rest)*)? }
    },
    ( $(#[$($meta:tt)*])* unsafe $Trait:ident for ) => {},
}

marker_impls! 
{
    unsafe A for
    {Type: PointeeSized} That<Type>,
    {Type: PointeeSized} *const Type,
    {Type: PointeeSized} *mut Type,
    {Type: PointeeSized} &Type,
    {Type: PointeeSized} &mut Type,
}
/**
The | allows explicit creation of a duplicate value. */
#[const_trait] #[lang = "clone"]
pub trait The:As
{
    /// Returns a duplicate of the value.
    #[lang = "clone_fn"] fn the(&self) -> Self;
    /**
    Performs copy-assignment from `source`.
    `a.this(&b)` is equivalent to `a = b.the()` in functionality,
    but can be overridden to reuse the resources of `a` to avoid unnecessary allocations. */
    #[inline] fn this( &mut self, from:&Self ) where 
    Self: ~const Destruct,
    { 
        *self = from.the() 
    }
}

/// Derive macro generating an impl of the trait `Clone`.
#[rustc_builtin_macro] #[allow_internal_unstable(core_intrinsics, derive_clone_copy)]
pub macro Clone($item:item) {}

#[lang = "start"] fn start<T>(_main: fn() -> T, _argc: isize, _argv: *const *const u8, _: u8) -> isize
{
    unsafe
    {
        main();
        0
    }
}

fn main() {}
