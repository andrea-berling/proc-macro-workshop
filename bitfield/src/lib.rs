// Crates that have the "proc-macro" crate type are only allowed to export
// procedural macros. So we cannot have one crate that defines procedural macros
// alongside other types of public APIs like traits and structs.
//
// For this project we are going to need a #[bitfield] macro but also a trait
// and some structs. We solve this by defining the trait and structs in this
// crate, defining the attribute macro in a separate bitfield-impl crate, and
// then re-exporting the macro from this crate so that users only have one crate
// that they need to import.
//
// From the perspective of a user of this crate, they get all the necessary APIs
// (macro, trait, struct) through the one bitfield crate.
pub use bitfield_impl::{bitfield, BitfieldSpecifier};
use bitfield_impl::{define_specifiers, make_field_size_helpers};

make_field_size_helpers!();

// TODO other things
pub trait Specifier {
    const BITS: usize;
    type SizeModEight;
    type ValueType;

    fn from_usize(value: usize) -> Self::ValueType;
}

pub mod checks {
    pub use bitfield_impl::define_mod8_enums_and_n_types;
    define_mod8_enums_and_n_types!();
    pub trait DiscriminantInRange {}
    pub struct True;
    impl True {
        pub fn new() -> Self {
            Self
        }
    }
    pub struct False;
    impl False {
        pub fn new() -> Self {
            Self
        }
    }
    impl DiscriminantInRange for True {}
    pub struct EvaluateDiscriminantInRange<const B: bool>;
    pub trait ConvertToCheck {
        type Result;
    }
    impl ConvertToCheck for EvaluateDiscriminantInRange<true> {
        type Result = True;
    }
    impl ConvertToCheck for EvaluateDiscriminantInRange<false> {
        type Result = False;
    }
}

impl Specifier for bool {
    const BITS: usize = 1;
    type SizeModEight = crate::checks::OneMod8;
    type ValueType = bool;
    fn from_usize(value: usize) -> Self::ValueType {
        value != 0
    }
}

impl crate::checks::TotalSizeIsMultipleOfEightBits for crate::checks::ZeroMod8 {}

define_specifiers!();
