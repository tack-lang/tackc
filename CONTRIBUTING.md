# Contributing to Tack

Thank you for considering contributing to Tack! There are many ways you can help, and your help is greatly appreciated.

## Ways to Contribute

- Issue Reports
- Pull Requests
- Feature Feedback
- Documentation

While contributing, you may add your name to `CONTRIBUTORS.md`.

If you have any questions, you may create a discussion on GitHub, and documentation may be updated.

## Dependencies

To work on `tackc`'s code, or to run `tackc`, a rust toolchain is required. `tackc` doesn't have a specific MSRV (Minimum Supported Rust Version), and so the most recent stable version should work. However, the codebase should also work for beta/nightly channels as well.

## Verification

Every push/pull request will have the following verification steps ran:

```bash
cargo fmt --check --all # Verify code is formatted
cargo test # Run tests in code
cargo verify # Custom-made tool for code verification
```

## `cargo verify`

The previous section mentioned a tool called `cargo verify`. This is a tool that will scan the source code to ensure that special attention is payed to any code snippets deemed "dangerous." Currently, the dangerous snippets are as follows:

- Using `#[expect]`
  - In `tackc` usage of `#[allow]` is banned, and `#[expect]` may be used instead. `#[expect]` works similar to `#[allow]`, but emits a lint if the lint being expected stops being emitted.
- Anything that can possibly panic:
  - `unwrap()`
  - `unwrap_err()`
  - `expect()`
  - `expect_err()`
  - `panic!()`
  - `unreachable!()`
  - `unimplemented!()`

## Clippy

`tackc` uses extremely aggressive clippy lints. If you have any questions about these, feel free to create a discussion on GitHub.

## Rules on Generative AI

For usage of Generative AI, use it minimally. If AI wrote more than about 25% of the content you claim to be contributing, it's probably too much. If you're not sure whether you asked AI to write too much, feel free to create a discussion on GitHub.

Just to be clear, asking AI to read your code and get suggestions is completely fine, as long as it's **YOU** that is implementing those suggestions.
