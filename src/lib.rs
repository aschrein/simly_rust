use wasm_bindgen::prelude::*;
use std::{any::Any, string};
mod tests;
mod utils;
mod dsl;

#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen(start)]
pub fn start() -> Result<(), JsValue> {
    utils::set_panic_hook();
    Ok(())
}

#[wasm_bindgen]
pub fn test() {
    let msg = "Hello World From Rust!";
    log!("{}", msg);
}