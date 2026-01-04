//! Sipha Tools CLI
//!
//! Command-line tools for working with Sipha grammars.

use clap::Parser;
use sipha_tools::cli::{Cli, Commands, OutputFormat};
use std::fs;

#[allow(clippy::too_many_lines)]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Viz {
            input,
            output,
            format,
            show_conflicts,
        } => {
            // Grammar visualization implementation
            // Note: Full grammar extraction from Rust source files requires Rust AST parsing
            // which is complex. This implementation provides a framework that can be extended.

            if !input.exists() {
                eprintln!("Error: Input file {} does not exist", input.display());
                return Err("Input file not found".into());
            }

            // Attempt to extract grammar from the Rust source file
            // Note: Full grammar extraction is complex and requires type information
            // This is a simplified implementation that detects grammar usage but cannot
            // extract the full grammar without runtime type information
            //
            // For now, we provide a placeholder message and framework for future extension
            // Note: Grammar extraction from Rust source files requires concrete types
            // which cannot be determined at compile time. The extract_grammar_from_source
            // function always returns None for this reason.
            // For now, we skip extraction and go straight to the placeholder visualization.
            eprintln!(
                "Note: Grammar extraction from Rust source files is not yet fully implemented."
            );
            eprintln!("The file may not contain GrammarBuilder usage or grammar! macro calls.");
            eprintln!("For now, use the library API directly to visualize grammars:");
            eprintln!();
            eprintln!("  use sipha_tools::visualize::generate_dot;");
            eprintln!("  let dot = generate_dot(&grammar, |t| format!(\"{{:?}}\", t));");
            eprintln!();

            // Generate a basic visualization using the file as context
            // This is used when grammar extraction fails or finds nothing
            let content = match format {
                OutputFormat::Dot => {
                    // Generate a basic DOT structure
                    // In full implementation, this would use generate_dot() with extracted grammar
                    let mut dot = String::from("digraph Grammar {\n");
                    dot.push_str("  rankdir=LR;\n");
                    dot.push_str("  node [shape=box];\n\n");
                    dot.push_str("  // Grammar extracted from: ");
                    dot.push_str(&input.to_string_lossy());
                    dot.push('\n');
                    dot.push_str("  // Note: Full grammar extraction not yet implemented\n");
                    dot.push_str("  // Use the library API directly for now\n");
                    dot.push_str("}\n");
                    dot
                }
                OutputFormat::Html => {
                    // Generate HTML using generate_html_with_conflicts() when grammar is available
                    // For now, provide a basic structure
                    let title = format!("Grammar Visualization - {}", input.file_name().unwrap_or_default().to_string_lossy());
                    let conflict_note = if show_conflicts {
                        "<p><strong>Note:</strong> Conflict highlighting requires full grammar extraction.</p>"
                    } else {
                        ""
                    };
                    format!(
                        r#"<!DOCTYPE html>
<html>
<head>
    <title>{}</title>
    <style>
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif;
            margin: 20px;
            background: #f5f5f5;
        }}
        .container {{
            max-width: 1200px;
            margin: 0 auto;
            background: white;
            padding: 30px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }}
        .info {{
            background: #e3f2fd;
            padding: 15px;
            border-radius: 5px;
            margin-bottom: 20px;
            border-left: 4px solid #2196f3;
        }}
        code {{
            background: #f5f5f5;
            padding: 2px 6px;
            border-radius: 3px;
            font-family: 'Courier New', monospace;
        }}
        pre {{
            background: #f5f5f5;
            padding: 10px;
            border-radius: 5px;
            overflow-x: auto;
        }}
    </style>
</head>
<body>
    <div class="container">
        <h1>{}</h1>
        <div class="info">
            <p><strong>Note:</strong> Full grammar extraction from Rust source files is not yet implemented.</p>
            {}
            <p>Use the library API directly:</p>
            <pre><code>use sipha_tools::visualize::generate_html_with_conflicts;
let html = generate_html_with_conflicts(
    &grammar, 
    |t| format!("{{:?}}", t), 
    Some("Grammar"),
    true  // show_conflicts
);</code></pre>
        </div>
        <p>Input file: <code>{}</code></p>
    </div>
</body>
</html>"#,
                        title, title, conflict_note, input.to_string_lossy()
                    )
                }
                OutputFormat::Json => {
                    serde_json::json!({
                        "status": "partial",
                        "message": "Grammar extraction from Rust source files requires Rust AST parsing",
                        "input_file": input.to_string_lossy(),
                        "note": "Use the library API directly: sipha_tools::visualize::generate_dot() or generate_html()"
                    })
                    .to_string()
                }
            };

            if let Some(output_path) = output {
                fs::write(&output_path, content)?;
                println!("Wrote visualization to {}", output_path.display());
                if show_conflicts {
                    println!("Note: Conflict highlighting requires full grammar extraction");
                }
            } else {
                // Write to stdout
                print!("{content}");
            }
        }
    }

    Ok(())
}
