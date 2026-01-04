# Sipha Tools

Developer utilities for working with Sipha grammars.

## Features

- **Grammar Visualization**: Generate DOT/Graphviz graphs and HTML visualizations
- **Grammar Analysis**: Analyze grammar structure and dependencies
- **CLI Tools**: Command-line interface for grammar visualization

## Usage

### Library API

```rust
use sipha_tools::visualize::generate_dot;
use sipha::grammar::Grammar;

// Generate DOT format
let dot = generate_dot(&grammar, |t| format!("{:?}", t));
println!("{}", dot);

// Generate HTML visualization
let html = generate_html(&grammar, |t| format!("{:?}", t), Some("My Grammar"));
std::fs::write("grammar.html", html)?;
```

### CLI Tool

```bash
# Visualize a grammar (coming soon)
cargo run --bin sipha-viz -- --input grammar.rs --format dot --output grammar.dot
```

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
sipha-tools = { path = "../crates/sipha_tools" }
```

