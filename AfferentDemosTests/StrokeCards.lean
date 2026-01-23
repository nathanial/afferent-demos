/-
  Stroke Card Tests
  Validate that stroke-only demo cards emit render commands.
-/
import Crucible
import Afferent
import Afferent.Arbor
import Afferent.Widget
import Demos.Overview.Strokes
import Trellis

namespace AfferentDemosTests.StrokeCards

open Crucible
open Afferent
open Afferent.Arbor

private structure NodeInfo where
  id : WidgetId
  widget : Widget
  parent : Option WidgetId

private partial def collectNodes (w : Widget) (parent : Option WidgetId) : Array NodeInfo :=
  let node := { id := w.id, widget := w, parent := parent }
  #[node] ++ (w.children.flatMap (fun child => collectNodes child (some w.id)))

private def findTextNode (nodes : Array NodeInfo) (label : String) : Option NodeInfo :=
  nodes.find? (fun n =>
    match n.widget with
    | .text _ _ content _ _ _ _ _ => content == label
    | _ => false)

private def findWidgetById (nodes : Array NodeInfo) (id : WidgetId) : Option Widget :=
  nodes.find? (fun n => n.id == id) |>.map (·.widget)

private def findCustomChild (w : Widget) : Option Widget :=
  w.children.find? (fun child =>
    match child with
    | .custom .. => true
    | _ => false)

private def collectCardCommands (label : String) : IO RenderCommands := do
  let widget := Afferent.Arbor.build (Demos.strokesWidget FontId.default)
  let measureResult ← Afferent.runWithFonts FontRegistry.empty
    (Afferent.Arbor.measureWidget widget 1000 800)
  let layouts := Trellis.layout measureResult.node 1000 800
  let nodes := collectNodes measureResult.widget none
  match findTextNode nodes label with
  | none =>
      throw (IO.userError s!"Missing label widget: {label}")
  | some textNode =>
      match textNode.parent with
      | none => throw (IO.userError s!"Label has no parent: {label}")
      | some parentId =>
          match findWidgetById nodes parentId with
          | none => throw (IO.userError s!"Missing parent widget for label: {label}")
          | some parentWidget =>
              match findCustomChild parentWidget with
              | none => throw (IO.userError s!"Missing custom child for label: {label}")
              | some customWidget =>
                  match customWidget with
                  | .custom _ _ _ spec =>
                      match layouts.get customWidget.id with
                      | none => throw (IO.userError s!"Missing layout for label: {label}")
                      | some computed =>
                          pure (spec.collect computed)
                  | _ =>
                      throw (IO.userError s!"Expected custom widget for label: {label}")

private def widthsNear (widths : Array Float) (target : Float) : Bool :=
  widths.any (fun w => Float.abs (w - target) < 0.001)

private def strokeRectWidths (cmds : RenderCommands) : Array Float :=
  cmds.foldl (fun acc cmd =>
    match cmd with
    | .strokeRect _ _ w _ => acc.push w
    | _ => acc) #[]

private def strokePathWidths (cmds : RenderCommands) : Array (Afferent.Arbor.Path × Float) :=
  cmds.foldl (fun acc cmd =>
    match cmd with
    | .strokePath path _ w => acc.push (path, w)
    | _ => acc) #[]

private def bezierCount (path : Afferent.Arbor.Path) : Nat :=
  path.commands.foldl (fun acc cmd =>
    match cmd with
    | .bezierCurveTo .. => acc + 1
    | _ => acc) 0

private def lineStrokeWidths (cmds : RenderCommands) : Array Float :=
  (strokePathWidths cmds).foldl (fun acc (path, w) =>
    if path.commands.size == 2 then acc.push w else acc) #[]

private def bezierStrokeWidths (cmds : RenderCommands) : Array Float :=
  (strokePathWidths cmds).foldl (fun acc (path, w) =>
    if bezierCount path == 4 then acc.push w else acc) #[]

open Crucible

testSuite "Stroke Card Commands"

test "Rect Widths card emits strokeRect widths" := do
  let cmds ← collectCardCommands "Rect Widths"
  let widths := strokeRectWidths cmds
  ensure (widthsNear widths 1.0) "Missing line width 1.0"
  ensure (widthsNear widths 2.0) "Missing line width 2.0"
  ensure (widthsNear widths 4.0) "Missing line width 4.0"
  ensure (widthsNear widths 8.0) "Missing line width 8.0"

test "Circle Widths card emits bezier stroke paths" := do
  let cmds ← collectCardCommands "Circle Widths"
  let widths := bezierStrokeWidths cmds
  ensure (widthsNear widths 2.0) "Missing circle width 2.0"
  ensure (widthsNear widths 4.0) "Missing circle width 4.0"
  ensure (widthsNear widths 6.0) "Missing circle width 6.0"

test "Line Widths card emits line stroke paths" := do
  let cmds ← collectCardCommands "Line Widths"
  let widths := lineStrokeWidths cmds
  ensure (widthsNear widths 1.0) "Missing line width 1.0"
  ensure (widthsNear widths 2.0) "Missing line width 2.0"
  ensure (widthsNear widths 4.0) "Missing line width 4.0"
  ensure (widthsNear widths 8.0) "Missing line width 8.0"

test "Diagonals card emits line stroke paths" := do
  let cmds ← collectCardCommands "Diagonals"
  let widths := lineStrokeWidths cmds
  ensure (widthsNear widths 2.0) "Missing diagonal width 2.0"
  ensure (widthsNear widths 3.0) "Missing diagonal width 3.0"
  ensure (widthsNear widths 4.0) "Missing diagonal width 4.0"



end AfferentDemosTests.StrokeCards
