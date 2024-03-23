use std::error;
use std::fmt;

use godot::engine::box_container::AlignmentMode;
use godot::engine::canvas_item::ExDrawString;
use godot::engine::control::{GrowDirection, LayoutPreset, MouseFilter, SizeFlags};
use godot::engine::global::{HorizontalAlignment, Side};
use godot::engine::multi_mesh::TransformFormat;
use godot::engine::BoxContainer;
use godot::engine::Material;
use godot::engine::Mesh;
use godot::engine::Texture2D;
use godot::engine::{Button, CanvasItem, Control, Font, Label};
use godot::engine::{MeshInstance3D, MultiMesh, StandardMaterial3D};
use godot::prelude::*;

// ----------------------------------------------------------------------------
// Error handling
// ----------------------------------------------------------------------------

pub type Result<T> = std::result::Result<T, GDError>;

#[derive(Debug)]
pub struct GDError {
    msg: String,
    underlying: Option<Box<dyn error::Error>>,
}

impl GDError {
    pub fn new<S: Into<String>>(msg: S) -> GDError {
        GDError {
            msg: msg.into(),
            underlying: None,
        }
    }

    #[allow(dead_code)]
    pub fn new_wrapped<S: Into<String>, E: 'static + error::Error>(
        msg: S,
        underlying: E,
    ) -> GDError {
        GDError {
            msg: msg.into(),
            underlying: Some(Box::new(underlying)),
        }
    }
}

impl fmt::Display for GDError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)?;
        if let Some(underlying) = &self.underlying {
            write!(f, " [{}]", underlying)?;
        }
        Ok(())
    }
}

impl error::Error for GDError {}

// ----------------------------------------------------------------------------
// Color utils
// ----------------------------------------------------------------------------

pub fn color_from_hex(hex: &str) -> Option<Color> {
    if hex.len() != 6 {
        return None;
    }
    let r = u8::from_str_radix(&hex[0..2], 16).ok()?;
    let g = u8::from_str_radix(&hex[2..4], 16).ok()?;
    let b = u8::from_str_radix(&hex[4..6], 16).ok()?;
    let r = r as f32 / 255.0;
    let g = g as f32 / 255.0;
    let b = b as f32 / 255.0;
    Some(Color::from_rgb(r, g, b))
}

// ----------------------------------------------------------------------------
// Vector utils
// ----------------------------------------------------------------------------

// It looks like Vectors don't support `<` or `<=` operators, most likely because
// deriving `Eq` isn't sound in general because floats have no total ordering.
// In cases where we don't care about NaN, we can use a little convenience...

#[allow(dead_code)]
pub fn is_larger(a: Vector2, b: Vector2) -> bool {
    a.x > b.x && a.y > b.y
}

pub fn is_larger_eq(a: Vector2, b: Vector2) -> bool {
    a.x >= b.x && a.y >= b.y
}

pub trait Vector2Ext {
    fn add_x(&self, delta: f32) -> Self;
    fn add_y(&self, delta: f32) -> Self;
}

impl Vector2Ext for Vector2 {
    fn add_x(&self, delta: f32) -> Self {
        Self::new(self.x + delta, self.y)
    }
    fn add_y(&self, delta: f32) -> Self {
        Self::new(self.x, self.y + delta)
    }
}

// ----------------------------------------------------------------------------
// Misc
// ----------------------------------------------------------------------------

// For now, we don't use any kind of pre-loading. Most likely this is fast enough.
// TODO: Decide if this should go into the "resources" module.

pub fn try_load_with_error<T>(name: &str) -> Result<Gd<T>>
where
    T: GodotClass + Inherits<Resource>,
{
    try_load::<T>(name).map_err(|_| GDError::new(format!("Failed to load resource: {}", name)))
}

pub fn try_load_material(name: &str) -> Result<Gd<StandardMaterial3D>> {
    try_load_with_error(name)
}

pub fn mutate_position<T>(control: Gd<T>, modify: impl FnOnce(&mut Vector2))
where
    T: GodotClass + Inherits<Control>,
{
    let mut control = control.upcast();
    let mut position = control.get_position();
    modify(&mut position);
    control.set_position(position);
}

pub fn mutate_position_3d<T>(control: Gd<T>, modify: impl FnOnce(&mut Vector3))
where
    T: GodotClass + Inherits<Node3D>,
{
    let mut node: Gd<Node3D> = control.upcast();
    let mut position = node.get_position();
    modify(&mut position);
    node.set_position(position);
}

pub fn duplicate_material_with_color(
    orig_material: &Gd<StandardMaterial3D>,
    color: Color,
) -> Gd<StandardMaterial3D> {
    // See discussion:
    // https://discordapp.com/channels/723850269347283004/781453984221102081/913440702434783232
    let material = orig_material.duplicate().unwrap();
    let mut material = material.cast::<StandardMaterial3D>();
    material.set_albedo(color);
    material
}

pub fn create_3d_multi_mesh(num_instances: i32) -> Gd<MultiMesh> {
    // Caution: There has been an order dependency, i.e., setting `transform_format` has to
    // be done before `instance_count`. https://github.com/godotengine/godot/issues/12595
    let mut multi_mesh = MultiMesh::new_gd();
    multi_mesh.set_transform_format(TransformFormat::TRANSFORM_3D);
    // Here, Godot 3 was using the following
    // multi_mesh.set_color_format(MultiMesh::COLOR_NONE);
    // I'm not 100% what the equivalent is in Godot 4, but I'd guess it is the "use colors" switch now.
    // Godot 3: https://docs.godotengine.org/de/stable/classes/class_multimesh.html#class-multimesh-property-color-format
    // Godot 4: https://docs.godotengine.org/en/stable/classes/class_multimesh.html#class-multimesh-property-use-colors
    multi_mesh.set_use_colors(false);
    multi_mesh.set_instance_count(num_instances);
    multi_mesh
}

pub fn create_mesh_instance_with_material_override<MeshClass, MaterialClass>(
    mesh: &Gd<MeshClass>,
    material: &Gd<MaterialClass>,
    name: &str,
) -> Gd<MeshInstance3D>
where
    MeshClass: GodotClass + Inherits<Mesh>,
    MaterialClass: GodotClass + Inherits<Material>,
{
    let mut mesh_inst = MeshInstance3D::new_alloc();
    mesh_inst.set_name(name.into());
    mesh_inst.set_mesh(mesh.clone().upcast());
    for i in 0..mesh_inst.get_surface_override_material_count() {
        mesh_inst.set_surface_override_material(i, material.clone().upcast());
    }
    mesh_inst
}

pub fn transform_from_raw_values(rows: &[real; 12]) -> Transform3D {
    let [ax, bx, cx, ay, by, cy, az, bz, cz, px, py, pz] = rows;
    Transform3D::new(
        Basis::from_rows(
            Vector3::new(*ax, *bx, *cx),
            Vector3::new(*ay, *by, *cy),
            Vector3::new(*az, *bz, *cz),
        ),
        Vector3 {
            x: *px,
            y: *py,
            z: *pz,
        },
    )
}

// TODO: I'm still a bit unsure which expressions should have a trailing semicolon, and which not.
// For now regular statements have a semicolon, but for the recursive macro calls it is largely
// a guess.
#[macro_export]
macro_rules! assemble_tree {
    ($base:expr => { $($other:tt)+ } $(,)?) => {
        {
            let base = $base;
            assemble_tree!( @iter_children, base, $($other)*);
            base
            // Here we could go for one of the following variants if we want to have "auto share"
            // semantics. However doing this would currently be inconsistent with the semantics
            // of "pure children", which are moved as well. It would basically mean that as soon
            // as a node is a parent (even if it is also a child of another node), it would get
            // shared -- only true leaves would be moved. The above pattern has the effect that
            // all nodes are moved, no matter if they are leaves or not, which feels more consistent.
            // If we want to go for "auto share" semantics it would make sense to do it for leaves
            // as well for consistency.
            // let base = &$base;
            // assemble_tree!( @iter_children, base, $($other)*);
            // base.clone()
            // let base = $base.clone();
            // assemble_tree!( @iter_children, base, $($other)*);
            // base
        }
    };

    // Patterns for '.. children' syntax
    (@iter_children, $base:expr, .. $child_iter:expr $(,)?) => {
        for child in $child_iter {
            $base.clone().upcast::<::godot::engine::Node>().add_child(child.upcast());
        }
    };
    (@iter_children, $base:expr, .. $child_iter:expr, $($other:tt)+) => {
        for child in $child_iter {
            $base.clone().upcast::<::godot::engine::Node>().add_child(child.upcast());
        }
        assemble_tree!( @iter_children, $base, $($other)*)
    };

    // Patterns for 'child' syntax
    (@iter_children, $base:expr, $child:expr $(,)?) => {
        $base.clone().upcast::<::godot::engine::Node>().add_child($child.upcast());
    };
    (@iter_children, $base:expr, $child:expr, $($other:tt)+) => {
        $base.clone().upcast::<::godot::engine::Node>().add_child($child.upcast());
        assemble_tree!( @iter_children, $base, $($other)*)
    };

    // Patterns for 'child => { ... }' syntax
    (@iter_children, $base:expr, $child:expr => { $($children:tt)+ } $(,)?) => {
        let child = assemble_tree!( $child => { $($children)* }); // True recursion
        $base.clone().upcast::<::godot::engine::Node>().add_child(child.upcast());
    };
    (@iter_children, $base:expr, $child:expr => { $($children:tt)+ }, $($other:tt)+) => {
        let child = assemble_tree!( $child => { $($children)* }); // True recursion
        $base.clone().upcast::<::godot::engine::Node>().add_child(child.upcast());
        assemble_tree!( @iter_children, $base, $($other)*)
    };

    // Support for empty braces
    (@iter_children, $base:expr, $child:expr => {} $(,)?) => {
        $base.clone().upcast::<::godot::engine::Node>().add_child($child.upcast());
    };
    (@iter_children, $base:expr, $child:expr => {}, $($other:tt)+) => {
        $base.clone().upcast::<::godot::engine::Node>().add_child($child.upcast());
        assemble_tree!( @iter_children, $base, $($other)*)
    };

    // Support for empty braces at top-level
    ($base:expr => {} $(,)?) => {
        $base
    };

    // Support for single expressions
    ($base:expr $(,)?) => {
        $base
    };
}

// ----------------------------------------------------------------------------
// Make utils
// ----------------------------------------------------------------------------

pub fn make_label<S: Into<GString>>(text: S) -> Gd<Label> {
    let mut label = Label::new_alloc();
    label.set_text(text.into());
    label
}

#[allow(dead_code)]
pub fn make_button<S: Into<GString>>(text: S) -> Gd<Button> {
    let mut button = Button::new_alloc();
    button.set_text(text.into());
    button
}

pub fn make_vspace(vspace: i32) -> Gd<Control> {
    let mut dummy = Control::new_alloc();
    dummy.set_custom_minimum_size(Vector2 {
        x: 0.0,
        y: vspace as f32,
    });
    dummy
}

// ----------------------------------------------------------------------------
// Misc utils (turn into extensions?)
// ----------------------------------------------------------------------------

pub fn remove_all_children<N>(node: Gd<N>)
where
    N: GodotClass + Inherits<Node>,
{
    let node: Gd<Node> = node.clone().upcast();
    for mut child in node.get_children().iter_shared() {
        child.queue_free();
    }
}

pub fn set_full_rect<C>(control: &mut Gd<C>)
where
    C: GodotClass + Inherits<Control>,
{
    let mut control = control.clone().upcast();
    control.set_anchors_preset(LayoutPreset::FULL_RECT);
    control.set_anchor(Side::RIGHT, 1.0);
    control.set_anchor(Side::BOTTOM, 1.0);
    control.set_h_grow_direction(GrowDirection::BOTH);
    control.set_v_grow_direction(GrowDirection::BOTH);
}

// ----------------------------------------------------------------------------
// Control extensions
// ----------------------------------------------------------------------------

pub trait ControlExtensions {
    fn set_full_rect(self) -> Self;

    // Initially I was offer "add flags" variants that were reading the original
    // flags and adding the specified flag. Since the size flags aren't orthogonal
    // treating them as actually independently settable bits is not really sensible.
    // In particular when trying to achieve any "shrink" behavior it is crucial to
    // unset the default bit "fill" (1), which an additive setter couldn't accomplish.
    fn ext_set_h_size_flags(self, size_flags: SizeFlags) -> Self;
    fn ext_set_v_size_flags(self, size_flags: SizeFlags) -> Self;

    // The most common thing to set is the "expand fill" combination.
    // "fill" is the default, so there is no need to set it explicitly.
    // The shrink settings, and "expand without fill / shrink" are more exotic
    // and therefore don't require shorthands... [xref:aekaiw8E]
    fn ext_set_h_expand_fill(self) -> Self;
    fn ext_set_v_expand_fill(self) -> Self;

    fn ext_set_mouse_filter(self, filter: MouseFilter) -> Self;
    fn ext_custom_minimum_size(self, size: Vector2) -> Self;
    fn ext_custom_minimum_width(self, width: f32) -> Self;
    fn ext_custom_minimum_height(self, height: f32) -> Self;

    fn ext_mod<F>(self, f: F) -> Self
    where
        F: FnOnce(&mut Self);
}

impl<C> ControlExtensions for Gd<C>
where
    C: GodotClass + Inherits<Control>,
{
    fn set_full_rect(mut self) -> Self {
        set_full_rect(&mut self);
        self
    }

    fn ext_set_h_size_flags(self, size_flags: SizeFlags) -> Self {
        let mut control = self.upcast::<Control>();
        control.set_h_size_flags(size_flags);
        control.cast::<C>()
    }
    fn ext_set_v_size_flags(self, size_flags: SizeFlags) -> Self {
        let mut control = self.upcast::<Control>();
        control.set_v_size_flags(size_flags);
        control.cast::<C>()
    }

    fn ext_set_h_expand_fill(self) -> Self {
        let mut control = self.upcast::<Control>();
        control.set_h_size_flags(SizeFlags::EXPAND_FILL);
        control.cast::<C>()
    }
    fn ext_set_v_expand_fill(self) -> Self {
        let mut control = self.upcast::<Control>();
        control.set_v_size_flags(SizeFlags::EXPAND_FILL);
        control.cast::<C>()
    }

    fn ext_set_mouse_filter(self, filter: MouseFilter) -> Self {
        let mut control = self.upcast::<Control>();
        control.set_mouse_filter(filter);
        control.cast::<C>()
    }

    fn ext_custom_minimum_size(self, size: Vector2) -> Self {
        let mut control = self.upcast::<Control>();
        control.set_custom_minimum_size(size);
        control.cast::<C>()
    }

    fn ext_custom_minimum_width(self, width: f32) -> Self {
        let mut control = self.upcast::<Control>();
        let mut size = control.get_custom_minimum_size();
        size.x = width;
        control.set_custom_minimum_size(size);
        control.cast::<C>()
    }

    fn ext_custom_minimum_height(self, height: f32) -> Self {
        let mut control = self.upcast::<Control>();
        let mut size = control.get_custom_minimum_size();
        size.y = height;
        control.set_custom_minimum_size(size);
        control.cast::<C>()
    }

    fn ext_mod<F>(mut self, f: F) -> Self
    where
        F: FnOnce(&mut Self),
    {
        let self_clone = self.clone();
        f(&mut self);
        self_clone
    }
}

// ----------------------------------------------------------------------------
// BoxContainer extensions
// ----------------------------------------------------------------------------

/// https://docs.godotengine.org/en/stable/classes/class_boxcontainer.html#class-boxcontainer
pub trait BoxContainerExtensions {
    fn ext_set_alignment(self, alignment: AlignmentMode) -> Self;
}

impl<C> BoxContainerExtensions for Gd<C>
where
    C: GodotClass + Inherits<BoxContainer>,
{
    fn ext_set_alignment(self, alignment: AlignmentMode) -> Self {
        let mut box_container = self.upcast::<BoxContainer>();
        box_container.set_alignment(alignment);
        box_container.cast::<C>()
    }
}

// ----------------------------------------------------------------------------
// CanvasItem extensions
// ----------------------------------------------------------------------------

pub trait CanvasItemExtensions {
    fn ext_draw_text<F>(
        &self,
        font: Gd<Font>,
        pos: Vector2,
        s: impl AsRef<str>,
        alignment: HorizontalAlignment,
        font_size: i32,
        post_instruction_func: F,
    ) where
        F: FnOnce(ExDrawString) -> ExDrawString;

    fn ext_draw_texture_centered(&self, texture: Gd<Texture2D>, pos: Vector2);
    fn ext_draw_texture_centered_rotated(
        &self,
        texture: Gd<Texture2D>,
        pos: Vector2,
        angle_radians: f32,
    );
}

impl<C> CanvasItemExtensions for Gd<C>
where
    C: GodotClass + Inherits<CanvasItem>,
{
    ///
    /// Improved version of `draw_string`, with a work-around for the unexpected horizontal
    /// alignment semantics
    ///
    /// https://github.com/godotengine/godot/issues/80163
    ///
    fn ext_draw_text<F>(
        &self,
        font: Gd<Font>,
        pos: Vector2,
        s: impl AsRef<str>,
        alignment: HorizontalAlignment,
        font_size: i32,
        post_instruction_func: F,
    ) where
        F: FnOnce(ExDrawString) -> ExDrawString,
    {
        let canvas_item = self.clone().upcast();

        let ex = match alignment {
            HorizontalAlignment::LEFT => canvas_item.draw_string_ex(font.clone(), pos, s.into()),
            HorizontalAlignment::CENTER => {
                let s: GString = s.into();
                let w = font
                    .get_string_size_ex(s.clone())
                    .font_size(font_size)
                    .done()
                    .x;
                let pos = Vector2 {
                    x: pos.x - w * 0.5,
                    y: pos.y,
                };
                canvas_item.draw_string_ex(font.clone(), pos, s)
            }
            HorizontalAlignment::RIGHT => {
                let s: GString = s.into();
                let w = font
                    .get_string_size_ex(s.clone())
                    .font_size(font_size)
                    .done()
                    .x;
                let pos = Vector2 {
                    x: pos.x - w,
                    y: pos.y,
                };
                canvas_item.draw_string_ex(font.clone(), pos, s)
            }
            _ => panic!("Unsupported alignment"),
        };
        let ex = post_instruction_func(ex);
        let ex = ex.font_size(font_size);
        ex.done()
    }

    fn ext_draw_texture_centered(&self, texture: Gd<Texture2D>, pos: Vector2) {
        let mut canvas_item = self.clone().upcast();
        let texture_size = texture.get_size();
        canvas_item.draw_texture(texture, pos - (texture_size * 0.5));
    }

    fn ext_draw_texture_centered_rotated(
        &self,
        texture: Gd<Texture2D>,
        pos: Vector2,
        angle_radians: f32,
    ) {
        let mut canvas_item = self.clone().upcast();
        let texture_size = texture.get_size();

        // Note that in order to rotate around the given position we have to do the usual
        // wrapping into a translation to make that point temporarily the origin.
        let transformation = Transform2D::IDENTITY
            .translated(-pos)
            .rotated(angle_radians)
            .translated(pos);

        canvas_item.draw_set_transform_matrix(transformation);
        canvas_item.draw_texture(texture, pos - (texture_size * 0.5));
        canvas_item.draw_set_transform_matrix(Transform2D::IDENTITY);
    }
}

#[cfg(test)]
pub mod test {
    use super::*;

    #[test]
    fn test_color_from_hex() {
        assert_eq!(
            color_from_hex("EC131E"),
            Some(Color::from_rgb(0.925_490_2, 0.074_509_81, 0.117_647_06))
        );
    }
}
