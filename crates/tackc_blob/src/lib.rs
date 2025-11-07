include!(concat!(env!("OUT_DIR"), "/blob.rs"));

macro_rules! blob_impl_prelude {
    ($krate_name:ident) => {
        pub use $krate_name::prelude::*;
    };
}

pub mod prelude {
    blob_impl_prelude!(tackc_error);
}
