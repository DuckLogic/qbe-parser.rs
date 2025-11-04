check: && check-format
    cargo +nightly clippy --all-targets --all-features
    cargo +nightly doc --no-deps --all-features

test: check
    cargo +nightly nextest run --all-features --no-fail-fast

format:
    cargo fmt --all
    taplo format

check-format: && check-spelling
    cargo fmt --check --all
    taplo format --check

check-spelling: _typos

fix-spelling: (_typos "--write-changes")

_typos *flags:
    # use pinned version to avoid breaking build
    uvx typos@1.38 {{flags}}

