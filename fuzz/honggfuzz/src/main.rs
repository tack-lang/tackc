use honggfuzz::fuzz;

fn main() {
    loop {
        fuzz!(|data: &[u8]| {
            tackc_fuzz::run(data);
        })
    }
}
