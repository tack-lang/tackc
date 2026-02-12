# Contributing to Tack

Thank you for considering contributing to Tack! There are many ways you can help, and your help is greatly appreciated.

## Ways to Contribute

- Issue Reports
- Pull Requests
- Feature Feedback
- Documentation

While contributing, you may add your name to `CONTRIBUTORS.md`.

If you have any questions, you may open an issue, since it's probably an issue with the existing documentation.

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

The previous section mentioned a tool called `cargo verify`. This is a tool that will scan the source code to ensure that special attention is payed to any code snippets deemed "dangerous." Currently, the only two snippets deemed
