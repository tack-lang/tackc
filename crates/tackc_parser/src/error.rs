pub use tackc_ast::error::*;

pub fn collect_error(errors: &mut Option<ParseErrors>, e: ParseErrors) {
    if let Some(err) = errors {
        err.merge(e);
    } else {
        *errors = Some(e);
    }
}
