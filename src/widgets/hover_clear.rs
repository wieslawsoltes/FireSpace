use xilem::core::{MessageContext, MessageResult, Mut, View, ViewId, ViewMarker, ViewPathTracker};
use xilem::masonry::accesskit::{Node, Role};
use xilem::masonry::core::{
    AccessCtx, BoxConstraints, ChildrenIds, EventCtx, LayoutCtx, NewWidget, PaintCtx,
    PointerEvent, PropertiesMut, PropertiesRef, RegisterCtx, Widget, WidgetMut,
    WidgetPod,
};
use xilem::masonry::kurbo::{Point, Size};
use xilem::{Pod, ViewCtx, WidgetView};

const CHILD_ID: ViewId = ViewId::new(0);

pub fn hover_clear<State, Action, V>(
    child: V,
    on_clear: impl Fn(&mut State) -> Action + Send + Sync + 'static,
) -> HoverClear<V, impl Fn(&mut State) -> Action + Send + Sync + 'static>
where
    V: WidgetView<State, Action>,
{
    HoverClear { child, on_clear }
}

#[derive(Clone, Copy, Debug)]
pub enum HoverClearAction {
    Clear,
}

pub struct HoverClear<V, F> {
    child: V,
    on_clear: F,
}

impl<V, F> ViewMarker for HoverClear<V, F> {}

impl<V, F, State, Action> View<State, Action, ViewCtx> for HoverClear<V, F>
where
    V: WidgetView<State, Action>,
    F: Fn(&mut State) -> Action + Send + Sync + 'static,
{
    type Element = Pod<HoverClearWidget>;
    type ViewState = V::ViewState;

    fn build(&self, ctx: &mut ViewCtx, app_state: &mut State) -> (Self::Element, Self::ViewState) {
        let (child, child_state) = ctx.with_id(CHILD_ID, |ctx| {
            View::<State, Action, _>::build(&self.child, ctx, app_state)
        });
        (
            ctx.with_action_widget(|ctx| {
                ctx.create_pod(HoverClearWidget::new(child.new_widget))
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
        ctx.with_id(CHILD_ID, |ctx| {
            View::<State, Action, _>::rebuild(
                &self.child,
                &prev.child,
                view_state,
                ctx,
                HoverClearWidget::child_mut(&mut element).downcast(),
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
                HoverClearWidget::child_mut(&mut element).downcast(),
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
                HoverClearWidget::child_mut(&mut element).downcast(),
                app_state,
            ),
            None => match message.take_message::<HoverClearAction>() {
                Some(action) => match *action {
                    HoverClearAction::Clear => MessageResult::Action((self.on_clear)(app_state)),
                },
                None => MessageResult::Stale,
            },
            _ => MessageResult::Stale,
        }
    }
}

pub struct HoverClearWidget {
    child: WidgetPod<dyn Widget>,
    inside: bool,
}

impl HoverClearWidget {
    pub fn new(child: NewWidget<impl Widget + ?Sized>) -> Self {
        Self {
            child: child.erased().to_pod(),
            inside: false,
        }
    }

    pub fn child_mut<'t>(this: &'t mut WidgetMut<'_, Self>) -> WidgetMut<'t, dyn Widget> {
        this.ctx.get_mut(&mut this.widget.child)
    }
}

impl Widget for HoverClearWidget {
    type Action = HoverClearAction;

    fn on_pointer_event(
        &mut self,
        ctx: &mut EventCtx<'_>,
        _props: &mut PropertiesMut<'_>,
        event: &PointerEvent,
    ) {
        match event {
            PointerEvent::Enter(_) | PointerEvent::Move(_) => {
                self.inside = true;
                if ctx.target() == ctx.widget_id() {
                    ctx.submit_action::<HoverClearAction>(HoverClearAction::Clear);
                }
            }
            PointerEvent::Leave(_) | PointerEvent::Cancel(_) => {
                if self.inside {
                    self.inside = false;
                    ctx.submit_action::<HoverClearAction>(HoverClearAction::Clear);
                }
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
