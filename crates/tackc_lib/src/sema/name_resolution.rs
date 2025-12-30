use crate::ast::Program;

pub struct Resolver {}

pub fn resolve(progs: &[Program]) {
    println!("{progs:?}");
    _ = progs;
}
