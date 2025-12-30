use tackc_ast::Program;

pub struct Resolver {}

pub fn resolve(progs: &[Program]) {
    println!("{progs:?}");
    _ = progs;
}
