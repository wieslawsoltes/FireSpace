use xilem::core::{MessageContext, MessageResult, ViewPathTracker, Mut, View, ViewId, ViewMarker};
use xilem::masonry::accesskit::{Node, Role};
use xilem::masonry::core::{
    AccessCtx, BoxConstraints, ChildrenIds, EventCtx, LayoutCtx, NewWidget, PaintCtx,
    PointerEvent, PropertiesMut, PropertiesRef, RegisterCtx, Widget, WidgetMut,
    WidgetPod,
};
use xilem::masonry::kurbo::{Point, Size};
use xilem::{Pod, ViewCtx, WidgetView};

const CHILD_ID: ViewId = ViewId::new(0);

pub fn hover_wrap<State, Action, V>(
    child: V,
    node_id: usize,
    on_action: impl Fn(&mut State, HoverWrapAction) -> Action + Send + Sync + 'static,
) -> HoverWrap<
    V,
    impl Fn(&mut State, HoverWrapAction) -> Action + Send + Sync + 'static,
>
where
    V: WidgetView<State, Action>,
{
    HoverWrap {
        child,
        node_id,
        on_action,
    }
}

#[derive(Clone, Copy, Debug)]
pub enum HoverWrapAction {
    Entered(usize),
    Exited(usize),
    Clicked(usize),
}

pub struct HoverWrap<V, F> {
    child: V,
    node_id: usize,
    on_action: F,
}

impl<V, F> ViewMarker for HoverWrap<V, F> {}
impl<V, F, State, Action> View<State, Action, ViewCtx> for HoverWrap<V, F>
where
    V: WidgetView<State, Action>,
    F: Fn(&mut State, HoverWrapAction) -> Action + Send + Sync + 'static,
{
    type Element = Pod<HoverWrapWidget>;
    type ViewState = V::ViewState;

    fn build(&self, ctx: &mut ViewCtx, app_state: &mut State) -> (Self::Element, Self::ViewState) {
        let (child, child_state) = ctx.with_id(CHILD_ID, |ctx| {
            View::<State, Action, _>::build(&self.child, ctx, app_state)
        });
        (
            ctx.with_action_widget(|ctx| {
                ctx.create_pod(HoverWrapWidget::new(child.new_widget, self.node_id))
            }),
            child_state,
        )
    }

    fn rebuild(
        &self,
        prev: &Self,
        view_state: &mut Self::ViewState,
        ctx: &mut ViewCtx,
        mut element: Mut<'_, Self::Element>,
        app_state: &mut State,
    ) {
        if prev.node_id != self.node_id {
            HoverWrapWidget::set_node_id(&mut element, self.node_id);
        }
        ctx.with_id(CHILD_ID, |ctx| {
            View::<State, Action, _>::rebuild(
                &self.child,
                &prev.child,
                view_state,
                ctx,
                HoverWrapWidget::child_mut(&mut element).downcast(),
                app_state,
            );
        });
    }

    fn teardown(
        &self,
        view_state: &mut Self::ViewState,
        ctx: &mut ViewCtx,
        mut element: Mut<'_, Self::Element>,
    ) {
        ctx.with_id(CHILD_ID, |ctx| {
            View::<State, Action, _>::teardown(
                &self.child,
                view_state,
                ctx,
                HoverWrapWidget::child_mut(&mut element).downcast(),
            );
        });
        ctx.teardown_leaf(element);
    }

    fn message(
        &self,
        view_state: &mut Self::ViewState,
        message: &mut MessageContext,
        mut element: Mut<'_, Self::Element>,
        app_state: &mut State,
    ) -> MessageResult<Action> {
        match message.take_first() {
            Some(CHILD_ID) => self.child.message(
                view_state,
                message,
                HoverWrapWidget::child_mut(&mut element).downcast(),
                app_state,
            ),
            None => match message.take_message::<HoverWrapAction>() {
                Some(action) => MessageResult::Action((self.on_action)(app_state, *action)),
                None => MessageResult::Stale,
            },
            _ => MessageResult::Stale,
        }
    }
}

pub struct HoverWrapWidget {
    child: WidgetPod<dyn Widget>,
    node_id: usize,
    hovered: bool,
    pressed: bool,
}

impl HoverWrapWidget {
    pub fn new(child: NewWidget<impl Widget + ?Sized>, node_id: usize) -> Self {
        Self {
            child: child.erased().to_pod(),
            node_id,
            hovered: false,
            pressed: false,
        }
    }

    pub fn set_node_id(this: &mut WidgetMut<'_, Self>, node_id: usize) {
        this.widget.node_id = node_id;
    }

    pub fn child_mut<'t>(this: &'t mut WidgetMut<'_, Self>) -> WidgetMut<'t, dyn Widget> {
        this.ctx.get_mut(&mut this.widget.child)
    }
}

impl Widget for HoverWrapWidget {
    type Action = HoverWrapAction;

    fn on_pointer_event(
        &mut self,
        ctx: &mut EventCtx<'_>,
        _props: &mut PropertiesMut<'_>,
        event: &PointerEvent,
    ) {
        match event {
            PointerEvent::Enter(_) | PointerEvent::Move(_) => {
                if !self.hovered {
                    self.hovered = true;
                    ctx.submit_action::<HoverWrapAction>(HoverWrapAction::Entered(self.node_id));
                }
            }
            PointerEvent::Down(event) => {
                if event.button.is_some() {
                    self.pressed = true;
                }
                ctx.set_handled();
            }
            PointerEvent::Up(event) => {
                if self.pressed {
                    self.pressed = false;
                    if event.button.is_some() {
                        ctx.submit_action::<HoverWrapAction>(HoverWrapAction::Clicked(
                            self.node_id,
                        ));
                    }
                }
                ctx.set_handled();
            }
            PointerEvent::Leave(_) | PointerEvent::Cancel(_) => {
                if self.hovered {
                    self.hovered = false;
                    ctx.submit_action::<HoverWrapAction>(HoverWrapAction::Exited(self.node_id));
                }
                self.pressed = false;
            }
            _ => {}
        }
    }

    fn register_children(&mut self, ctx: &mut RegisterCtx<'_>) {
        ctx.register_child(&mut self.child);
    }

    fn layout(
        &mut self,
        ctx: &mut LayoutCtx<'_>,
        _props: &mut PropertiesMut<'_>,
        bc: &BoxConstraints,
    ) -> Size {
        let size = ctx.run_layout(&mut self.child, bc);
        ctx.place_child(&mut self.child, Point::ORIGIN);
        size
    }

    fn paint(&mut self, _ctx: &mut PaintCtx<'_>, _props: &PropertiesRef<'_>, _scene: &mut vello::Scene) {
    }

    fn accessibility_role(&self) -> Role {
        Role::GenericContainer
    }

    fn accessibility(
        &mut self,
        _ctx: &mut AccessCtx<'_>,
        _props: &PropertiesRef<'_>,
        _node: &mut Node,
    ) {
    }

    fn children_ids(&self) -> ChildrenIds {
        ChildrenIds::from_slice(&[self.child.id()])
    }

}
