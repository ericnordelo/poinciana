use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_starknet::starknet_plugin_suite;

/// Get a new database with the StarkNet plugin configured.
pub fn get_database() -> RootDatabase {
    RootDatabase::builder()
        .with_plugin_suite(starknet_plugin_suite())
        .build()
        .unwrap()
}
