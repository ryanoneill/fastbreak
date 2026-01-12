//! CLI commands implementation

use crate::codegen::{DiagramType, MarkdownGenerator, MermaidGenerator};
use crate::project::{Loader, Manifest, Project};
use clap::{Parser, Subcommand};
use std::fs;
use std::path::{Path, PathBuf};

/// Fastbreak specification language CLI
#[derive(Parser, Debug)]
#[command(name = "fstbrk")]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    /// Subcommand to run
    #[command(subcommand)]
    pub command: Commands,
}

/// Available commands
#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Initialize a new Fastbreak project
    Init {
        /// Project name
        name: String,
        /// Directory to create the project in (defaults to project name)
        #[arg(short, long)]
        path: Option<PathBuf>,
    },

    /// Check specifications for errors
    Check {
        /// Path to manifest or source file
        #[arg(default_value = ".")]
        path: PathBuf,
    },

    /// Generate Markdown documentation
    Doc {
        /// Path to manifest or source file
        #[arg(default_value = ".")]
        path: PathBuf,
        /// Output directory (overrides manifest setting)
        #[arg(short, long)]
        output: Option<PathBuf>,
    },

    /// Generate Mermaid diagrams
    Diagram {
        /// Path to manifest or source file
        #[arg(default_value = ".")]
        path: PathBuf,
        /// Output directory (overrides manifest setting)
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Diagram type to generate (erd, state, sequence, or all)
        #[arg(short = 't', long, default_value = "all")]
        diagram_type: String,
    },

    /// Build project (check + doc + diagram)
    Build {
        /// Path to manifest or source file
        #[arg(default_value = ".")]
        path: PathBuf,
        /// Output directory (overrides manifest setting)
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
}

/// Run the CLI with the given arguments
///
/// # Errors
///
/// Returns an error if the command fails.
pub fn run(cli: Cli) -> Result<(), CliError> {
    match cli.command {
        Commands::Init { name, path } => cmd_init(&name, path.as_deref()),
        Commands::Check { path } => cmd_check(&path),
        Commands::Doc { path, output } => cmd_doc(&path, output.as_deref()),
        Commands::Diagram {
            path,
            output,
            diagram_type,
        } => cmd_diagram(&path, output.as_deref(), &diagram_type),
        Commands::Build { path, output } => cmd_build(&path, output.as_deref()),
    }
}

fn cmd_init(name: &str, path: Option<&Path>) -> Result<(), CliError> {
    let project_dir = path.map_or_else(|| PathBuf::from(name), Path::to_path_buf);

    // Check if directory already exists and is not empty
    if project_dir.exists() {
        let is_empty = project_dir
            .read_dir()
            .map(|mut d| d.next().is_none())
            .unwrap_or(false);

        if !is_empty {
            return Err(CliError::DirectoryNotEmpty(project_dir));
        }
    }

    // Create project structure
    fs::create_dir_all(&project_dir).map_err(|e| CliError::Io {
        path: project_dir.clone(),
        source: e,
    })?;

    let specs_dir = project_dir.join("specs");
    fs::create_dir_all(&specs_dir).map_err(|e| CliError::Io {
        path: specs_dir.clone(),
        source: e,
    })?;

    let docs_dir = project_dir.join("docs");
    fs::create_dir_all(&docs_dir).map_err(|e| CliError::Io {
        path: docs_dir.clone(),
        source: e,
    })?;

    // Create manifest
    let manifest = Manifest::new(name);
    let manifest_path = project_dir.join("fastbreak.toml");
    manifest
        .save(&manifest_path)
        .map_err(|e| CliError::Manifest(e.to_string()))?;

    // Create example spec file
    let example_spec = format!(
        r#"// {name} specification
module {module_name}

// Define your types here
type Example {{
    id: Int,
    name: String,
}}

// Define your states here
state System {{
    examples: Set<Example>,

    invariant "examples have unique ids" {{
        forall e1, e2 in examples where e1 != e2 =>
            e1.id != e2.id
    }}
}}

// Define your actions here
action create_example(name: String) -> Example
    requires {{ name.len() > 0 }}
    ensures {{ result in examples' }}

// Define your scenarios here
scenario "create an example" {{
    given {{
        examples = {{}}
    }}
    when {{
        result = create_example("test")
    }}
    then {{
        examples.len() == 1
    }}
}}
"#,
        name = name,
        module_name = name.replace('-', "_")
    );

    let spec_path = specs_dir.join("main.fbrk");
    fs::write(&spec_path, example_spec).map_err(|e| CliError::Io {
        path: spec_path,
        source: e,
    })?;

    // Create .gitignore
    let gitignore = "# Generated files\ndocs/*.md\ndocs/*.mmd\n";
    let gitignore_path = project_dir.join(".gitignore");
    fs::write(&gitignore_path, gitignore).map_err(|e| CliError::Io {
        path: gitignore_path,
        source: e,
    })?;

    println!("Created new Fastbreak project: {name}");
    println!("  {}", manifest_path.display());
    println!("  {}/main.fbrk", specs_dir.display());
    println!("\nTo get started:");
    println!("  cd {}", project_dir.display());
    println!("  fstbrk check");
    println!("  fstbrk build");

    Ok(())
}

fn cmd_check(path: &Path) -> Result<(), CliError> {
    let project = load_project(path)?;

    println!("Checking {}...", project.name());

    if project.has_errors() {
        let errors = project.errors();
        println!("\nFound {} error(s):\n", errors.len());

        for error in errors {
            println!("  - {error}");
        }

        return Err(CliError::CheckFailed(errors.len()));
    }

    let def_count = project.spec.definition_count();
    let scenario_count = project.spec.scenarios.len();
    let property_count = project.spec.properties.len();

    println!("  {def_count} definition(s)");
    println!("  {scenario_count} scenario(s)");
    println!("  {property_count} property(ies)");
    println!("\nNo errors found.");

    Ok(())
}

fn cmd_doc(path: &Path, output: Option<&Path>) -> Result<(), CliError> {
    let project = load_project(path)?;

    let output_dir =
        output.map_or_else(|| project.root.join(&project.manifest.output.dir), Path::to_path_buf);

    fs::create_dir_all(&output_dir).map_err(|e| CliError::Io {
        path: output_dir.clone(),
        source: e,
    })?;

    println!("Generating documentation for {}...", project.name());

    let generator = MarkdownGenerator::new(&project.spec);
    let markdown = generator.generate();

    let output_file = output_dir.join(format!("{}.md", project.name()));
    fs::write(&output_file, markdown).map_err(|e| CliError::Io {
        path: output_file.clone(),
        source: e,
    })?;

    println!("  Generated: {}", output_file.display());

    Ok(())
}

fn cmd_diagram(path: &Path, output: Option<&Path>, diagram_type: &str) -> Result<(), CliError> {
    let project = load_project(path)?;

    let output_dir =
        output.map_or_else(|| project.root.join(&project.manifest.output.dir), Path::to_path_buf);

    fs::create_dir_all(&output_dir).map_err(|e| CliError::Io {
        path: output_dir.clone(),
        source: e,
    })?;

    println!("Generating diagrams for {}...", project.name());

    let generator = MermaidGenerator::new(&project.spec);

    let diagrams_to_generate: Vec<(&str, DiagramType)> = match diagram_type {
        "erd" => vec![("erd", DiagramType::Erd)],
        "state" => vec![("state", DiagramType::State)],
        "sequence" => vec![("sequence", DiagramType::Sequence)],
        "all" => vec![
            ("erd", DiagramType::Erd),
            ("state", DiagramType::State),
            ("sequence", DiagramType::Sequence),
        ],
        _ => {
            return Err(CliError::InvalidDiagramType(diagram_type.to_string()));
        }
    };

    for (name, dtype) in diagrams_to_generate {
        let diagram = generator.generate(dtype);
        let output_file = output_dir.join(format!("{}_{}.mmd", project.name(), name));

        fs::write(&output_file, diagram).map_err(|e| CliError::Io {
            path: output_file.clone(),
            source: e,
        })?;

        println!("  Generated: {}", output_file.display());
    }

    Ok(())
}

fn cmd_build(path: &Path, output: Option<&Path>) -> Result<(), CliError> {
    // Check first
    cmd_check(path)?;

    println!();

    // Generate docs
    cmd_doc(path, output)?;

    println!();

    // Generate diagrams
    cmd_diagram(path, output, "all")?;

    println!("\nBuild complete.");

    Ok(())
}

fn load_project(path: &Path) -> Result<Project, CliError> {
    // Try to find manifest
    if path.is_file() {
        // If path is a file, check if it's a manifest or source file
        if path.ends_with("fastbreak.toml") {
            return Loader::load_project(path).map_err(|e| CliError::Load(e.to_string()));
        }

        // Assume it's a source file
        return Loader::load_file(path).map_err(|e| CliError::Load(e.to_string()));
    }

    // Path is a directory, look for manifest
    let manifest_path = path.join("fastbreak.toml");
    if manifest_path.exists() {
        return Loader::load_project(&manifest_path).map_err(|e| CliError::Load(e.to_string()));
    }

    // Try to find manifest in parent directories
    if let Ok(manifest_path) = Manifest::find(path) {
        return Loader::load_project(&manifest_path).map_err(|e| CliError::Load(e.to_string()));
    }

    // No manifest found, look for .fbrk files in current directory
    let fbs_files: Vec<_> = path
        .read_dir()
        .ok()
        .into_iter()
        .flatten()
        .filter_map(Result::ok)
        .filter(|e| e.path().extension().and_then(|s| s.to_str()) == Some("fbrk"))
        .collect();

    if let Some(first) = fbs_files.first() {
        Loader::load_file(&first.path()).map_err(|e| CliError::Load(e.to_string()))
    } else {
        Err(CliError::NoProject(path.to_path_buf()))
    }
}

/// CLI errors
#[derive(Debug)]
pub enum CliError {
    /// IO error
    Io {
        /// Path that caused the error
        path: PathBuf,
        /// Underlying IO error
        source: std::io::Error,
    },
    /// Manifest error
    Manifest(String),
    /// Load error
    Load(String),
    /// Check failed
    CheckFailed(usize),
    /// No project found
    NoProject(PathBuf),
    /// Directory not empty
    DirectoryNotEmpty(PathBuf),
    /// Invalid diagram type
    InvalidDiagramType(String),
}

impl std::fmt::Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CliError::Io { path, source } => {
                write!(f, "IO error at {}: {}", path.display(), source)
            }
            CliError::Manifest(e) => write!(f, "Manifest error: {e}"),
            CliError::Load(e) => write!(f, "Load error: {e}"),
            CliError::CheckFailed(n) => write!(f, "Check failed with {n} error(s)"),
            CliError::NoProject(path) => {
                write!(
                    f,
                    "No Fastbreak project found at {}. Run 'fstbrk init' to create one.",
                    path.display()
                )
            }
            CliError::DirectoryNotEmpty(path) => {
                write!(f, "Directory {} is not empty", path.display())
            }
            CliError::InvalidDiagramType(t) => {
                write!(
                    f,
                    "Invalid diagram type: {t}. Valid types: erd, state, sequence, all"
                )
            }
        }
    }
}

impl std::error::Error for CliError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            CliError::Io { source, .. } => Some(source),
            _ => None,
        }
    }
}
