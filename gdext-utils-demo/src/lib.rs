mod ui_demo_1;

use godot::prelude::*;

struct ExtensionImpl;

#[gdextension]
unsafe impl ExtensionLibrary for ExtensionImpl {}
