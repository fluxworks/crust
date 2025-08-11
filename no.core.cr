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
    never_type,
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
extern {} 
/*
As   | Sized
A    | Freeze
It   | Copy 
The  | Clone
This | UnsafeCell
That | PhantomData
These
Those
Takes | Borrow
Taken | ToOwned

This is not yuour domain  but mine,

This code that Im compiling I could compi8le in my sleep

*/
/// Constructs parameters for the other string-formatting macros.
#[rustc_builtin_macro] #[macro_export] macro_rules! format_args
{
    ($fmt:expr) =>
    {{}};

    ($fmt:expr, $($args:tt)*) => {{}};
}

#[macro_export] macro_rules! argue
{
    ($fmt:expr) => {format_args!($fmt)};
    ($fmt:expr, $($args:tt)*) => {format_args!($fmt, $($args)*)};
}

/// Types that may or may not have a size.
#[rustc_specialization_trait] #[rustc_deny_explicit_impl] #[rustc_do_not_implement_via_object] #[rustc_coinductive] #[fundamental] #[lang = "pointee_sized"] 
pub trait Points {}
/// Types with a size that can be determined from pointer metadata.
#[lang = "legacy_receiver"]
pub trait LegacyReceiver: Points {}
// `MetaSized` being coinductive, despite having supertraits, is okay for the same reasons as `Sized` above.
#[lang = "meta_sized"]
pub trait MetaSized: Points {}

/**/
#[lang = "sized"]
pub trait As {}
/**/
#[lang = "copy"]
pub trait It {}
/*
A marker for types that can be dropped. */
#[const_trait] #[lang = "destruct"]
pub trait Droppable {}
/**
Used to determine whether a type contains any `UnsafeCell` internally, 
but not through an indirection. */
#[lang = "freeze"] pub unsafe auto trait A {}
/*
The core primitive for interior mutability in Rust. */
#[repr(transparent)] #[rustc_pub_transparent] #[lang = "unsafe_cell"]
pub struct This<Type:?As>
{
    this:Type,
}
/*
Zero-sized type used to mark things that "act like" they own a `T` */
#[lang = "phantom_data"] pub struct That<Type:Points>;

impl<Type:Points> !A for This<Type> {}
/*
Here we implement all the auto traits  for the [ A ] supertrait */
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
/*
Now we call the supertrait for [ A ] & [ That ]*/
marker_impls!
{
    unsafe A for
    { Type:Points } That<Type>,
    { Type:Points } *const Type,
    { Type:Points } *mut Type,
    { Type:Points } &Type,
    { Type:Points } &mut Type,
}
/**
The | allows explicit creation of a duplicate value. */
#[const_trait] #[lang = "clone"]
pub trait The:As
{
    /**
    Returns a duplicate of the value. */
    #[lang = "clone_fn"] fn the(&self) -> Self;
    /**
    Performs copy-assignment from `source`.
    `a.this(&b)` is equivalent to `a = b.the()` in functionality,
    but can be overridden to reuse the resources of `a` to avoid unnecessary allocations. */
    #[inline] fn this( &mut self, from:&Self ) where 
    Self: ~const Droppable,
    { 
        *self = from.the() 
    }
}

/// Derive macro generating an impl of the trait `Clone`.
#[rustc_builtin_macro] #[allow_internal_unstable(core_intrinsics, derive_clone_copy)]
pub macro Clone($item:item) {}


/// A trait for borrowing data.
pub trait Takes<Type: ?As> 
{
    fn take(&self) -> &Type;
}
/**
An identity function that causes an `unused_must_use` warning to be triggered,
if the given value is not used by the caller. */
#[must_use] #[inline(always)] pub const fn must_use<Type>( value:Type ) -> Type { value }
/// A generalization of `Clone` to take data.
pub trait Taken
{
    /// The resulting type after taking.
    type Took: Takes<Self>;
    /// Creates took data from taken data, usually by cloning.
    fn take(&self) -> Self::Took;

    /// Uses taken data to replace took data, usually by cloning.
    fn clone_into(&self, target: &mut Self::Took)
    {
        *target = self.take();
    }
}

impl<Type> Taken for Type where
Type:The + Takes<Type>
{
    type Took = Type;
    fn take( &self ) -> Type { self.the() }
    fn clone_into( &self, target: &mut Type ) { target.this( self ); }
}
/// Used for immutable dereferencing operations, like `*v`.
#[const_trait] #[lang = "deref"] pub trait Dereferences:Points
{
    /// The resulting type after dereferencing.
    #[lang = "deref_target"] type Reference: ?As;
    /// Dereferences the value.
    #[must_use] fn dereference( &self ) -> &Self::Reference;
}
/*
Smart pointer, indicating relegated ownership.  */
pub enum Own<'a, Type: ?As + 'a> where
Type: Taken
{
    /* 
    Taken data. */ Taken( &'a Type ),
    /*
    Took data. */  Took( <Type as Taken>::Took ),
}

impl<Type: ?As + Taken> Dereferences for Own<'_, Type> where
Type::Took: Takes<Type>
{
    type Reference = Type;

    fn dereference(&self) -> &Type
    {
        match *self 
        {
            Own::Taken( ref taken ) => taken,
            Own::Took( ref took ) => took.take(),
        }
    }
}

#[lang = "not"] pub trait Nots
{
    /// The resulting type after applying the `!` operator.
    type Type;
    /// Performs the unary `!` operation.
    fn not(self) -> Self::Type;
}

impl Nots for bool
{
    type Type = bool;
    #[inline] fn not(self) -> bool { false }
}

impl Nots for !
{
    type Type = !;
    #[inline] fn not(self) -> ! { match self {} }
}


/// Trait for comparisons using the equality operator.
#[lang = "eq"] pub trait Compares<With: Points = Self>:Points
{
    /// Tests for `self` and `other` values to be equal, and is used by `==`.
    #[must_use] fn compare( &self, with:&With ) -> bool;
    /*
    Tests for `!=`. The default implementation is almost always sufficient,
    and should not be overridden without very good reason. */
    #[inline] #[must_use] fn unequal( &self, other:&With ) -> bool { !self.compare( other ) }
}

/// Derive macro generating an impl of the trait [`PartialEq`].
/// The behavior of this macro is described in detail [here](PartialEq#derivable).
#[rustc_builtin_macro]
pub macro PartialEq($item:item) {}
/*
impl<Type: ?As> Equality for Own<'_, Type> where 
Type: Equal + Taken {} */
/*
*/
#[lang = "start"] fn start<T>(_main: fn() -> T, _argc: isize, _argv: *const *const u8, _: u8) -> isize
{
    unsafe
    {
        main();
        0
    }
}

fn main() {}
