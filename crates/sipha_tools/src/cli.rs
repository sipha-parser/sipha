//! CLI interface for sipha-tools

use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "sipha-viz")]
#[command(about = "Grammar visualization tool for Sipha")]
#[command(version = "1.0.0")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Visualize a grammar from Rust source code
    Viz {
        /// Input Rust file containing grammar definition
        #[arg(short, long)]
        input: PathBuf,
        
        /// Output file (default: stdout)
        #[arg(short, long)]
        output: Option<PathBuf>,
        
        /// Output format
        #[arg(short, long, default_value = "dot")]
        format: OutputFormat,
        
        /// Show conflicts and issues
        #[arg(short, long)]
        show_conflicts: bool,
    },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OutputFormat {
    Dot,
    Html,
    Json,
}

impl std::str::FromStr for OutputFormat {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "dot" | "graphviz" => Ok(OutputFormat::Dot),
            "html" => Ok(OutputFormat::Html),
            "json" => Ok(OutputFormat::Json),
            _ => Err(format!("Unknown format: {}. Supported: dot, html, json", s)),
        }
    }
}

