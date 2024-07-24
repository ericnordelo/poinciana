//! `poinciana`'s configuration.
use clap::{Parser, Subcommand};
use figment::{providers::Serialized, Figment};
use serde::{Deserialize, Serialize};

/// `poinciana`'s configuration.
#[derive(Parser, Debug, Clone, Default, Serialize, Deserialize)]
#[command(author, version, about, long_about = None)] // Read from `Cargo.toml`
pub struct Config {
    /// `poinciana`'s commands.
    #[clap(subcommand)]
    pub command: Commands,
}

impl Config {
    pub(crate) fn scaffold(&self) -> crate::scaffold::Scaffold {
        self.command.clone().into()
    }
}

/// `poinciana`'s commands.
#[derive(Debug, Clone, Subcommand, Serialize, Deserialize)]
pub enum Commands {
    /// `poinciana scaffold`.
    #[command(name = "scaffold")]
    Scaffold(crate::scaffold::Scaffold),

    /// `poinciana check`.
    #[command(name = "check")]
    Check(crate::scaffold::Scaffold),
}

impl Default for Commands {
    fn default() -> Self {
        Self::Scaffold(Default::default())
    }
}

/// Main entrypoint of `poinciana`'s execution.
pub fn run() -> anyhow::Result<()> {
    let config: Config = Figment::new()
        .merge(Serialized::defaults(Config::parse()))
        .extract()?;

    match &config.command {
        Commands::Scaffold(command) => command.run(&config),
        Commands::Check(_) => panic!("Check command not implemented yet."),
    }
}

impl From<Commands> for crate::scaffold::Scaffold {
    fn from(command: Commands) -> Self {
        match command {
            Commands::Scaffold(s) => s,
            _ => Default::default(),
        }
    }
}
