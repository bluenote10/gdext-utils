use gdext_utils::{assemble_tree, make_button, make_label, set_full_rect, ControlExtensions};
use godot::engine::{
    Button, Control, HBoxContainer, IControl, MarginContainer, PanelContainer, RenderingServer,
    Theme, VBoxContainer,
};
use godot::prelude::*;

#[derive(GodotClass)]
#[class(base=Control)]
#[allow(dead_code)]
pub struct UiDemo1 {
    base: Base<Control>,
    button: Gd<Button>,
}

#[godot_api]
impl IControl for UiDemo1 {
    fn init(base: Base<Self::Base>) -> Self {
        RenderingServer::singleton().set_default_clear_color(Color::from_html("#F0F0FA").unwrap());

        let mut theme = Theme::new_gd();
        theme.set_constant("margin_top".into(), "MarginContainer".into(), 20);
        theme.set_constant("margin_left".into(), "MarginContainer".into(), 20);
        theme.set_constant("margin_bottom".into(), "MarginContainer".into(), 20);
        theme.set_constant("margin_right".into(), "MarginContainer".into(), 20);
        base.to_gd().set_theme(theme);

        // Example of an explicitly named node (logic relevant)
        let button = make_button("Ok");

        // Example of dynamically generated children
        let children: Vec<_> = (1..=10)
            .map(|i| make_label(format!("Label: {i}")))
            .collect();

        assemble_tree!(
            base.to_gd().clone() => {
                MarginContainer::new_alloc().set_full_rect() => {
                    VBoxContainer::new_alloc() => {
                        PanelContainer::new_alloc() => {
                            MarginContainer::new_alloc() => {
                                HBoxContainer::new_alloc() => {
                                    make_label("left"),
                                    make_label("middle").ext_set_h_expand_fill(),
                                    make_label("right"),
                                }
                            }
                        },
                        PanelContainer::new_alloc() => {
                            MarginContainer::new_alloc() => {
                                VBoxContainer::new_alloc() => {
                                    .. children,
                                    button.clone(),
                                }
                            }
                        },
                    },
                },
            }
        );

        set_full_rect(&mut base.to_gd());
        Self { base, button }
    }
}
