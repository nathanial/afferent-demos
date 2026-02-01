/-
  Demo Runner - Canopy app shell for demo tabs.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Core.DemoRegistry
import Demos.Overview.Card
import Demos.Overview.DemoGrid
import Demos.Overview.SpinningCubes
import Demos.Layout.Flexbox
import Demos.Layout.CssGrid
import Demos.Reactive.Showcase.App
import Demos.Perf.Circles
import Demos.Perf.Lines
import Demos.Perf.Sprites
import Demos.Perf.Widget.App
import Demos.Visuals.Seascape
import Demos.Visuals.ShapeGallery
import Demos.Visuals.DashedLines
import Demos.Visuals.LineCaps
import Demos.Visuals.TextureMatrix
import Demos.Visuals.Orbital
import Demos.Overview.Fonts
import Demos.Chat.App
import Demos.Visuals.Worldmap
import Demos.Linalg.VectorInterpolation
import Demos.Linalg.VectorArithmetic
import Demos.Linalg.VectorProjection
import Demos.Linalg.VectorField
import Demos.Linalg.CrossProduct3D
import Demos.Linalg.Matrix2DTransform
import Demos.Linalg.Matrix3DTransform
import Demos.Linalg.ProjectionExplorer
import Demos.Linalg.MatrixDecomposition
import Demos.Linalg.QuaternionVisualizer
import Demos.Linalg.SlerpInterpolation
import Demos.Linalg.EulerGimbalLock
import Demos.Linalg.DualQuaternionBlending
import Demos.Linalg.RayCastingPlayground
import Demos.Linalg.PrimitiveOverlapTester
import Demos.Linalg.BarycentricCoordinates
import Demos.Linalg.FrustumCullingDemo
import Tileset
import Worldmap
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

structure CanopyAppState where
  render : ComponentRender

private structure SeascapeInputState where
  w : Bool := false
  a : Bool := false
  s : Bool := false
  d : Bool := false
  q : Bool := false
  e : Bool := false
  deriving Inhabited

private def roundTo (v : Float) (places : Nat) : Float :=
  let factor := (10 : Float) ^ places.toFloat
  (v * factor).round / factor

private def formatFloat (v : Float) (places : Nat := 2) : String :=
  let s := toString (roundTo v places)
  if s.any (· == '.') then
    let s := s.dropRightWhile (· == '0')
    if s.endsWith "." then s.dropRight 1 else s
  else
    s

private def formatStatsLines (stats : RunnerStats) : Array String :=
  #[
    s!"layout {formatFloat stats.layoutMs}ms • collect {formatFloat stats.collectMs}ms • exec {formatFloat stats.executeMs}ms",
    s!"cmds {stats.commandCount} • widgets {stats.widgetCount} • layouts {stats.layoutCount}",
    s!"draws {stats.drawCalls} • batched {stats.batchedCalls} • single {stats.individualCalls}",
    s!"cache hits {stats.cacheHits} • misses {stats.cacheMisses}",
    s!"frame {formatFloat stats.frameMs}ms • {formatFloat stats.fps 1} fps"
  ]

private def demoFontsFromEnv (env : DemoEnv) : DemoFonts := {
  label := env.fontSmallId
  small := env.fontSmallId
  medium := env.fontMediumId
  large := env.fontLargeId
  huge := env.fontHugeId
}

private def overviewTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let demoFonts := demoFontsFromEnv env
  let cubes := spinningCubesInitialState
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (demoGridWidget env.screenScale t demoFonts cubes env.windowWidthF env.windowHeightF))
  pure ()

private def circlesTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (particlesRef : IO.Ref Render.Dynamic.ParticleState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    let particles ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let current ← particlesRef.get
      let next := current.updateBouncing dt env.circleRadius
      particlesRef.set next
      lastTimeRef.set t
      pure next
    emit (pure (circlesPerfWidget t env.fontMedium particles env.circleRadius))
  pure ()

private def spritesTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (particlesRef : IO.Ref Render.Dynamic.ParticleState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    let particles ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let current ← particlesRef.get
      let next := current.updateBouncing dt env.spriteHalfSize
      particlesRef.set next
      lastTimeRef.set t
      pure next
    emit (pure (spritesPerfWidget env.screenScale env.fontMedium env.spriteTexture particles env.spriteHalfSize))
  pure ()

private def linesPerfTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (linesPerfWidget t env.lineBuffer env.lineCount env.lineWidth
      env.fontMedium env.windowWidthF env.windowHeightF))
  pure ()

private def layoutTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (layoutWidgetFlex env.fontMediumId env.fontSmallId env.screenScale))
  pure ()

private def cssGridTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (cssGridWidget env.fontMediumId env.fontSmallId env.screenScale))
  pure ()

private def reactiveShowcaseTabContent (appState : ReactiveShowcase.AppState) : WidgetM Unit := do
  emit appState.render

private def widgetPerfTabContent (appState : WidgetPerf.AppState) : WidgetM Unit := do
  emit appState.render

private def fontShowcaseTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (fontShowcaseWidget env.showcaseFonts env.fontMediumId env.screenScale))
  pure ()

private def chatDemoTabContent (appState : ChatDemo.AppState) : WidgetM Unit := do
  emit appState.render

private def textureMatrixTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (textureMatrixWidget t env.screenScale env.windowWidthF env.windowHeightF
      env.fontMedium env.fontSmall env.spriteTexture))
  pure ()

private def orbitalInstancedTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (orbitalInstancedWidget t env.screenScale env.windowWidthF env.windowHeightF
      env.fontMedium env.orbitalCount env.orbitalParams env.orbitalBuffer))
  pure ()

private def vectorInterpolationTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.VectorInterpolationState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let interpName ← registerComponentW "vector-interpolation"

  let clickEvents ← useClickData interpName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? interpName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let origin := (rect.width / 2, rect.height / 2)
              let scale := 50.0 * env.screenScale
              let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
              let state ← stateRef.get
              if Demos.Linalg.nearPoint worldPos state.vectorA 0.5 then
                stateRef.set { state with dragging := some .vectorA }
              else if Demos.Linalg.nearPoint worldPos state.vectorB 0.5 then
                stateRef.set { state with dragging := some .vectorB }
              else
                pure ()
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | some target =>
        match data.nameMap.get? interpName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 50.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let next := match target with
                  | .vectorA => { state with vectorA := worldPos }
                  | .vectorB => { state with vectorB := worldPos }
                stateRef.set next
            | none => pure ()
        | none => pure ()
    | none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.key == .space && data.event.isPress then
      stateRef.modify fun s => { s with animating := !s.animating }
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun t => do
    let state ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let mut current ← stateRef.get
      if current.animating then
        let nextT := current.t + dt * 0.5
        let wrapped := if nextT >= 1.0 then nextT - 1.0 else nextT
        current := { current with t := wrapped }
        stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn interpName 0 containerStyle #[
      Demos.Linalg.vectorInterpolationWidget env state
    ]))
  pure ()

private def vectorArithmeticTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.VectorArithmeticState) : WidgetM Unit := do
  let arithName ← registerComponentW "vector-arithmetic"

  let clickEvents ← useClickData arithName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? arithName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let origin := (rect.width / 2, rect.height / 2)
              let scale := 50.0 * env.screenScale
              let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
              let state ← stateRef.get
              if Demos.Linalg.nearPoint worldPos state.vectorA 0.5 then
                stateRef.set { state with dragging := some .vectorA }
              else if Demos.Linalg.nearPoint worldPos state.vectorB 0.5 then
                stateRef.set { state with dragging := some .vectorB }
              else
                pure ()
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | some target =>
        match data.nameMap.get? arithName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 50.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let next := match target with
                  | .vectorA => { state with vectorA := worldPos }
                  | .vectorB => { state with vectorB := worldPos }
                stateRef.set next
            | none => pure ()
        | none => pure ()
    | none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char '1' =>
          stateRef.modify fun s => { s with operation := .add }
      | .char '2' =>
          stateRef.modify fun s => { s with operation := .sub }
      | .char '3' =>
          stateRef.modify fun s => { s with operation := .scale }
      | .char '=' | .char '+' =>
          stateRef.modify fun s => { s with scaleFactor := s.scaleFactor + 0.1 }
      | .char '-' =>
          stateRef.modify fun s =>
            let newScale := if s.scaleFactor > 0.2 then s.scaleFactor - 0.1 else 0.1
            { s with scaleFactor := newScale }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn arithName 0 containerStyle #[
      Demos.Linalg.vectorArithmeticWidget env state
    ]))
  pure ()

private def vectorProjectionTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.VectorProjectionState) : WidgetM Unit := do
  let projName ← registerComponentW "vector-projection"

  let clickEvents ← useClickData projName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? projName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let origin := (rect.width / 2, rect.height / 2)
              let scale := 50.0 * env.screenScale
              let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
              let state ← stateRef.get
              if Demos.Linalg.nearPoint worldPos state.vectorV 0.5 then
                stateRef.set { state with dragging := some .vectorV }
              else if Demos.Linalg.nearPoint worldPos state.vectorU 0.5 then
                stateRef.set { state with dragging := some .vectorU }
              else
                pure ()
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | some target =>
        match data.nameMap.get? projName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 50.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let next := match target with
                  | .vectorV => { state with vectorV := worldPos }
                  | .vectorU => { state with vectorU := worldPos }
                stateRef.set next
            | none => pure ()
        | none => pure ()
    | none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'p' =>
          stateRef.modify fun s => { s with showMode := .projection }
      | .char 'r' =>
          stateRef.modify fun s => { s with showMode := .reflection }
      | .char 'b' =>
          stateRef.modify fun s => { s with showMode := .both }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn projName 0 containerStyle #[
      Demos.Linalg.vectorProjectionWidget env state
    ]))
  pure ()

private def vectorFieldTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.VectorFieldState) : WidgetM Unit := do
  let fieldName ← registerComponentW "vector-field"
  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char '1' =>
          stateRef.modify fun s => { s with fieldType := .radial }
      | .char '2' =>
          stateRef.modify fun s => { s with fieldType := .rotational }
      | .char '3' =>
          stateRef.modify fun s => { s with fieldType := .gradient }
      | .char '4' =>
          stateRef.modify fun s => { s with fieldType := .saddle }
      | .char '=' | .char '+' =>
          stateRef.modify fun s =>
            { s with gridResolution := Nat.min 24 (s.gridResolution + 2) }
      | .char '-' =>
          stateRef.modify fun s =>
            { s with gridResolution := Nat.max 4 (s.gridResolution - 2) }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn fieldName 0 containerStyle #[
      Demos.Linalg.vectorFieldWidget env state
    ]))
  pure ()

private def crossProduct3DTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.CrossProduct3DState) : WidgetM Unit := do
  let crossName ← registerComponentW "cross-product-3d"

  let clickEvents ← useClickData crossName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s =>
        { s with dragging := .camera, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .camera =>
        let dx := data.x - state.lastMouseX
        let dy := data.y - state.lastMouseY
        let newYaw := state.cameraYaw + dx * 0.01
        let rawPitch := state.cameraPitch + dy * 0.01
        let newPitch := if rawPitch < -1.5 then -1.5 else if rawPitch > 1.5 then 1.5 else rawPitch
        stateRef.set { state with
          cameraYaw := newYaw
          cameraPitch := newPitch
          lastMouseX := data.x
          lastMouseY := data.y
        }
    | _ => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := .none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'p' =>
          stateRef.modify fun s => { s with showParallelogram := !s.showParallelogram }
      | .char 'r' =>
          stateRef.modify fun s => { s with cameraYaw := 0.6, cameraPitch := 0.4 }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn crossName 0 containerStyle #[
      Demos.Linalg.crossProduct3DWidget env state
    ]))
  pure ()

private def matrix2DTransformTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.Matrix2DTransformState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let mat2dName ← registerComponentW "matrix-2d-transform"
  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char '1' =>
          stateRef.modify fun s =>
            { s with preset := .identity, matrix := Demos.Linalg.presetToMatrix .identity }
      | .char '2' =>
          stateRef.modify fun s =>
            { s with preset := .rotation45, matrix := Demos.Linalg.presetToMatrix .rotation45 }
      | .char '3' =>
          stateRef.modify fun s =>
            { s with preset := .rotation90, matrix := Demos.Linalg.presetToMatrix .rotation90 }
      | .char '4' =>
          stateRef.modify fun s =>
            { s with preset := .scale2x, matrix := Demos.Linalg.presetToMatrix .scale2x }
      | .char '5' =>
          stateRef.modify fun s =>
            { s with preset := .scaleNonUniform, matrix := Demos.Linalg.presetToMatrix .scaleNonUniform }
      | .char '6' =>
          stateRef.modify fun s =>
            { s with preset := .shearX, matrix := Demos.Linalg.presetToMatrix .shearX }
      | .char '7' =>
          stateRef.modify fun s =>
            { s with preset := .shearY, matrix := Demos.Linalg.presetToMatrix .shearY }
      | .char '8' =>
          stateRef.modify fun s =>
            { s with preset := .reflectX, matrix := Demos.Linalg.presetToMatrix .reflectX }
      | .char '9' =>
          stateRef.modify fun s =>
            { s with preset := .reflectY, matrix := Demos.Linalg.presetToMatrix .reflectY }
      | .tab =>
          stateRef.modify fun s =>
            { s with editingCell := Demos.Linalg.nextMatrixCell s.editingCell }
      | .left =>
          stateRef.modify fun s =>
            { s with editingCell := Demos.Linalg.moveMatrixCell s.editingCell 0 (-1) }
      | .right =>
          stateRef.modify fun s =>
            { s with editingCell := Demos.Linalg.moveMatrixCell s.editingCell 0 1 }
      | .up =>
          stateRef.modify fun s =>
            { s with editingCell := Demos.Linalg.moveMatrixCell s.editingCell (-1) 0 }
      | .down =>
          stateRef.modify fun s =>
            { s with editingCell := Demos.Linalg.moveMatrixCell s.editingCell 1 0 }
      | .char '=' | .char '+' =>
          stateRef.modify fun s =>
            if s.editingCell != .none then
              let updated := Demos.Linalg.modifyMatrixCell s.matrix s.editingCell 0.1
              { s with matrix := updated, preset := .custom }
            else s
      | .char '-' =>
          stateRef.modify fun s =>
            if s.editingCell != .none then
              let updated := Demos.Linalg.modifyMatrixCell s.matrix s.editingCell (-0.1)
              { s with matrix := updated, preset := .custom }
            else s
      | .char 'i' =>
          stateRef.modify fun s =>
            let t := s.translation
            { s with translation := Linalg.Vec2.mk t.x (t.y + 0.1), preset := .custom }
      | .char 'k' =>
          stateRef.modify fun s =>
            let t := s.translation
            { s with translation := Linalg.Vec2.mk t.x (t.y - 0.1), preset := .custom }
      | .char 'j' =>
          stateRef.modify fun s =>
            let t := s.translation
            { s with translation := Linalg.Vec2.mk (t.x - 0.1) t.y, preset := .custom }
      | .char 'l' =>
          stateRef.modify fun s =>
            let t := s.translation
            { s with translation := Linalg.Vec2.mk (t.x + 0.1) t.y, preset := .custom }
      | .char 'g' =>
          stateRef.modify fun s => { s with showGrid := !s.showGrid }
      | .char 'v' =>
          stateRef.modify fun s => { s with showBasisVectors := !s.showBasisVectors }
      | .char 's' =>
          stateRef.modify fun s =>
            let newShape := match s.shape with
              | .square => .triangle
              | .triangle => .arrow
              | .arrow => .square
            { s with shape := newShape }
      | .space =>
          stateRef.modify fun s => { s with animating := !s.animating }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun t => do
    let state ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let mut current ← stateRef.get
      if current.animating then
        let newT := current.animT + dt * 0.5
        current := { current with animT := if newT >= 1.0 then 0.0 else newT }
        stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn mat2dName 0 containerStyle #[
      Demos.Linalg.matrix2DTransformWidget env state
    ]))
  pure ()

private def matrix3DTransformTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.Matrix3DTransformState) : WidgetM Unit := do
  let mat3dName ← registerComponentW "matrix-3d-transform"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char '1' =>
          stateRef.modify fun s => { s with selectedIndex := some 0 }
      | .char '2' =>
          stateRef.modify fun s => { s with selectedIndex := some 1 }
      | .char '3' =>
          stateRef.modify fun s => { s with selectedIndex := some 2 }
      | .up =>
          stateRef.modify fun s =>
            match s.selectedIndex with
            | some idx =>
                if idx > 0 then
                  let arr := s.transforms
                  let temp := arr.getD idx (.rotateX 0)
                  let arr := arr.set! idx (arr.getD (idx - 1) (.rotateX 0))
                  let arr := arr.set! (idx - 1) temp
                  { s with transforms := arr, selectedIndex := some (idx - 1) }
                else s
            | none => s
      | .down =>
          stateRef.modify fun s =>
            match s.selectedIndex with
            | some idx =>
                if idx + 1 < s.transforms.size then
                  let arr := s.transforms
                  let temp := arr.getD idx (.rotateX 0)
                  let arr := arr.set! idx (arr.getD (idx + 1) (.rotateX 0))
                  let arr := arr.set! (idx + 1) temp
                  { s with transforms := arr, selectedIndex := some (idx + 1) }
                else s
            | none => s
      | .char 'a' =>
          stateRef.modify fun s => { s with showAxes := !s.showAxes }
      | .char 'i' =>
          stateRef.modify fun s => { s with showIntermediateSteps := !s.showIntermediateSteps }
      | .char 'r' =>
          stateRef.modify fun s => { s with cameraYaw := 0.5, cameraPitch := 0.3 }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData mat3dName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s => { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    if state.dragging then
      let dx := data.x - state.lastMouseX
      let dy := data.y - state.lastMouseY
      let newYaw := state.cameraYaw + dx * 0.01
      let rawPitch := state.cameraPitch + dy * 0.01
      let newPitch := if rawPitch < -1.5 then -1.5 else if rawPitch > 1.5 then 1.5 else rawPitch
      stateRef.set { state with
        cameraYaw := newYaw
        cameraPitch := newPitch
        lastMouseX := data.x
        lastMouseY := data.y
      }
    else
      pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := false }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn mat3dName 0 containerStyle #[
      Demos.Linalg.matrix3DTransformWidget env state
    ]))
  pure ()

private def projectionExplorerTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.ProjectionExplorerState) : WidgetM Unit := do
  let projName ← registerComponentW "projection-explorer"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .tab =>
          stateRef.modify fun s =>
            { s with projType := match s.projType with
              | .perspective => .orthographic
              | .orthographic => .perspective
            }
      | .char 'f' =>
          stateRef.modify fun s =>
            let newFar := s.far + 0.5
            { s with far := if newFar > 10.0 then 10.0 else newFar }
      | .char 'n' =>
          stateRef.modify fun s =>
            let newNear := s.near + 0.1
            let maxNear := s.far - 0.5
            let clampedNear := if newNear > maxNear then maxNear else newNear
            { s with near := if clampedNear < 0.1 then 0.1 else clampedNear }
      | .char '=' | .char '+' =>
          stateRef.modify fun s =>
            match s.projType with
            | .perspective =>
                let newFov := s.fov + 0.1
                let maxFov := 2.513
                { s with fov := if newFov > maxFov then maxFov else newFov }
            | .orthographic =>
                let newSize := s.orthoSize + 0.2
                { s with orthoSize := if newSize > 5.0 then 5.0 else newSize }
      | .char '-' =>
          stateRef.modify fun s =>
            match s.projType with
            | .perspective =>
                let newFov := s.fov - 0.1
                { s with fov := if newFov < 0.3 then 0.3 else newFov }
            | .orthographic =>
                let newSize := s.orthoSize - 0.2
                { s with orthoSize := if newSize < 0.5 then 0.5 else newSize }
      | .char 'c' =>
          stateRef.modify fun s => { s with showClipSpace := !s.showClipSpace }
      | .char 'o' =>
          stateRef.modify fun s => { s with showTestObjects := !s.showTestObjects }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData projName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s => { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    if state.dragging then
      let dx := data.x - state.lastMouseX
      let dy := data.y - state.lastMouseY
      let newYaw := state.cameraYaw + dx * 0.01
      let rawPitch := state.cameraPitch + dy * 0.01
      let newPitch := if rawPitch < -1.5 then -1.5 else if rawPitch > 1.5 then 1.5 else rawPitch
      stateRef.set { state with
        cameraYaw := newYaw
        cameraPitch := newPitch
        lastMouseX := data.x
        lastMouseY := data.y
      }
    else
      pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := false }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn projName 0 containerStyle #[
      Demos.Linalg.projectionExplorerWidget env state
    ]))
  pure ()

private def matrixDecompositionTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.MatrixDecompositionState) : WidgetM Unit := do
  let decompName ← registerComponentW "matrix-decomposition"
  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char '1' =>
          stateRef.modify fun s =>
            if 0 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 0 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 0 }
            else s
      | .char '2' =>
          stateRef.modify fun s =>
            if 1 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 1 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 1 }
            else s
      | .char '3' =>
          stateRef.modify fun s =>
            if 2 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 2 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 2 }
            else s
      | .char '4' =>
          stateRef.modify fun s =>
            if 3 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 3 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 3 }
            else s
      | .char '5' =>
          stateRef.modify fun s =>
            if 4 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 4 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 4 }
            else s
      | .char '6' =>
          stateRef.modify fun s =>
            if 5 < Demos.Linalg.decompositionPresets.size then
              let (_, m) := Demos.Linalg.decompositionPresets.getD 5 ("", Linalg.Mat2.identity)
              { s with matrix := m, decomp := Demos.Linalg.decomposeMatrix2D m, presetIndex := 5 }
            else s
      | .tab =>
          stateRef.modify fun s =>
            { s with currentStep := match s.currentStep with
              | .original => .afterRotation1
              | .afterRotation1 => .afterScale
              | .afterScale => .afterRotation2
              | .afterRotation2 => .original
            }
      | .char 'c' =>
          stateRef.modify fun s => { s with showComponents := !s.showComponents }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn decompName 0 containerStyle #[
      Demos.Linalg.matrixDecompositionWidget env state
    ]))
  pure ()

private def quaternionVisualizerTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.QuaternionVisualizerState) : WidgetM Unit := do
  let quatName ← registerComponentW "quaternion-visualizer"

  let clickEvents ← useClickData quatName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 && data.click.button != 1 then
      pure ()
    else
      match data.nameMap.get? quatName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let labels : Array (Demos.Linalg.QuatComponent × Nat) := #[(.x, 0), (.y, 1), (.z, 2), (.w, 3)]
              let mut hitSlider : Option Demos.Linalg.QuatComponent := none
              for (comp, idx) in labels do
                let layout := Demos.Linalg.sliderLayoutFor rect.width rect.height env.screenScale idx
                let hit := localX >= layout.x && localX <= layout.x + layout.width
                  && localY >= layout.y - 8.0 && localY <= layout.y + layout.height + 8.0
                if hit then
                  hitSlider := some comp
              let state ← stateRef.get
              match hitSlider with
              | some comp =>
                  let idx := match comp with | .x => 0 | .y => 1 | .z => 2 | .w => 3
                  let layout := Demos.Linalg.sliderLayoutFor rect.width rect.height env.screenScale idx
                  let t := (localX - layout.x) / layout.width
                  let value := Demos.Linalg.clampUnit (t * 2.0 - 1.0)
                  let q := (Demos.Linalg.setQuatComponent state.quat comp value).normalize
                  stateRef.set { state with
                    quat := q
                    dragging := .slider comp
                    lastMouseX := data.click.x
                    lastMouseY := data.click.y
                  }
              | none =>
                  if data.click.button == 1 then
                    stateRef.set { state with dragging := .camera, lastMouseX := data.click.x, lastMouseY := data.click.y }
                  else
                    stateRef.set { state with dragging := .arcball, lastMouseX := data.click.x, lastMouseY := data.click.y }
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .none => pure ()
    | .camera =>
        let dx := data.x - state.lastMouseX
        let dy := data.y - state.lastMouseY
        let yaw := state.cameraYaw + dx * 0.005
        let pitch := state.cameraPitch + dy * 0.005
        stateRef.set { state with cameraYaw := yaw, cameraPitch := pitch, lastMouseX := data.x, lastMouseY := data.y }
    | .arcball =>
        let dx := data.x - state.lastMouseX
        let dy := data.y - state.lastMouseY
        let rotY := Linalg.Quat.fromAxisAngle Linalg.Vec3.unitY (dx * 0.008)
        let rotX := Linalg.Quat.fromAxisAngle Linalg.Vec3.unitX (dy * 0.008)
        let q := Linalg.Quat.multiply rotY (Linalg.Quat.multiply rotX state.quat) |>.normalize
        stateRef.set { state with quat := q, lastMouseX := data.x, lastMouseY := data.y }
    | .slider comp =>
        match data.nameMap.get? quatName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let idx := match comp with | .x => 0 | .y => 1 | .z => 2 | .w => 3
                let layout := Demos.Linalg.sliderLayoutFor rect.width rect.height env.screenScale idx
                let t := (localX - layout.x) / layout.width
                let value := Demos.Linalg.clampUnit (t * 2.0 - 1.0)
                let q := (Demos.Linalg.setQuatComponent state.quat comp value).normalize
                stateRef.set { state with quat := q, lastMouseX := data.x, lastMouseY := data.y }
            | none => pure ()
        | none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun _ => do
    stateRef.modify fun s => { s with dragging := .none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'r' =>
          stateRef.set Demos.Linalg.quaternionVisualizerInitialState
      | .char 'e' =>
          stateRef.modify fun s =>
            let e := s.eulerAngles
            let q := Linalg.Quat.fromEuler e.x e.y e.z
            { s with quat := q.normalize }
      | .char '1' =>
          stateRef.modify fun s => { s with selectedEuler := 0 }
      | .char '2' =>
          stateRef.modify fun s => { s with selectedEuler := 1 }
      | .char '3' =>
          stateRef.modify fun s => { s with selectedEuler := 2 }
      | .left =>
          stateRef.modify fun s =>
            let delta := -5.0 * Linalg.Float.pi / 180.0
            let e := s.eulerAngles
            let e' := match s.selectedEuler with
              | 0 => Linalg.Vec3.mk (e.x + delta) e.y e.z
              | 1 => Linalg.Vec3.mk e.x (e.y + delta) e.z
              | _ => Linalg.Vec3.mk e.x e.y (e.z + delta)
            { s with eulerAngles := e' }
      | .right =>
          stateRef.modify fun s =>
            let delta := 5.0 * Linalg.Float.pi / 180.0
            let e := s.eulerAngles
            let e' := match s.selectedEuler with
              | 0 => Linalg.Vec3.mk (e.x + delta) e.y e.z
              | 1 => Linalg.Vec3.mk e.x (e.y + delta) e.z
              | _ => Linalg.Vec3.mk e.x e.y (e.z + delta)
            { s with eulerAngles := e' }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn quatName 0 containerStyle #[
      Demos.Linalg.quaternionVisualizerWidget env state
    ]))
  pure ()

private def slerpInterpolationTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.SlerpInterpolationState)
    (lastTimeRef : IO.Ref Float) : WidgetM Unit := do
  let slerpName ← registerComponentW "slerp-interpolation"

  let clickEvents ← useClickData slerpName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s => { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    if !state.dragging then
      pure ()
    else
      let dx := data.x - state.lastMouseX
      let dy := data.y - state.lastMouseY
      let newYaw := state.cameraYaw + dx * 0.005
      let newPitch := state.cameraPitch + dy * 0.005
      stateRef.set { state with
        cameraYaw := newYaw
        cameraPitch := newPitch
        lastMouseX := data.x
        lastMouseY := data.y
      }
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := false }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.key == .space && data.event.isPress then
      stateRef.modify fun s => { s with animating := !s.animating }
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun t => do
    let state ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let mut current ← stateRef.get
      if current.animating then
        let newT := current.t + dt * 0.35
        current := { current with t := if newT > 1.0 then newT - 1.0 else newT }
        stateRef.set current
      lastTimeRef.set t
      pure current
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn slerpName 0 containerStyle #[
      Demos.Linalg.slerpInterpolationWidget env state
    ]))
  pure ()

private def eulerGimbalLockTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.EulerGimbalLockState) : WidgetM Unit := do
  let gimbalName ← registerComponentW "euler-gimbal-lock"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'r' =>
          stateRef.set Demos.Linalg.eulerGimbalLockInitialState
      | .char '1' =>
          stateRef.modify fun s => { s with selectedAxis := 0 }
      | .char '2' =>
          stateRef.modify fun s => { s with selectedAxis := 1 }
      | .char '3' =>
          stateRef.modify fun s => { s with selectedAxis := 2 }
      | .char 'o' =>
          stateRef.modify fun s =>
            let nextOrder := match s.euler.order with
              | .XYZ => .XZY
              | .XZY => .YXZ
              | .YXZ => .YZX
              | .YZX => .ZXY
              | .ZXY => .ZYX
              | .ZYX => .XYZ
            { s with euler := { s.euler with order := nextOrder } }
      | .left | .right =>
          stateRef.modify fun s =>
            let delta := if data.event.key == .left then -5.0 else 5.0
            let e := s.euler
            let radDelta := delta * Linalg.Float.pi / 180.0
            let e' := match s.selectedAxis with
              | 0 => { e with a1 := e.a1 + radDelta }
              | 1 => { e with a2 := e.a2 + radDelta }
              | _ => { e with a3 := e.a3 + radDelta }
            { s with euler := e' }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData gimbalName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s => { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    if !state.dragging then
      pure ()
    else
      let dx := data.x - state.lastMouseX
      let dy := data.y - state.lastMouseY
      let newYaw := state.cameraYaw + dx * 0.005
      let newPitch := state.cameraPitch + dy * 0.005
      stateRef.set { state with
        cameraYaw := newYaw
        cameraPitch := newPitch
        lastMouseX := data.x
        lastMouseY := data.y
      }
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := false }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn gimbalName 0 containerStyle #[
      Demos.Linalg.eulerGimbalLockWidget env state
    ]))
  pure ()

private def dualQuaternionBlendingTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.DualQuaternionBlendingState) : WidgetM Unit := do
  let dualName ← registerComponentW "dual-quaternion-blending"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'r' =>
          stateRef.set Demos.Linalg.dualQuaternionBlendingInitialState
      | .char 't' =>
          stateRef.modify fun s => { s with twist := s.twist + 0.1 }
      | .char 'g' =>
          stateRef.modify fun s => { s with twist := s.twist - 0.1 }
      | .char 'b' =>
          stateRef.modify fun s => { s with bend := s.bend + 0.1 }
      | .char 'v' =>
          stateRef.modify fun s => { s with bend := s.bend - 0.1 }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData dualName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s => { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    if !state.dragging then
      pure ()
    else
      let dx := data.x - state.lastMouseX
      let dy := data.y - state.lastMouseY
      let newYaw := state.cameraYaw + dx * 0.005
      let newPitch := state.cameraPitch + dy * 0.005
      stateRef.set { state with
        cameraYaw := newYaw
        cameraPitch := newPitch
        lastMouseX := data.x
        lastMouseY := data.y
      }
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := false }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn dualName 0 containerStyle #[
      Demos.Linalg.dualQuaternionBlendingWidget env state
    ]))
  pure ()

private def rayCastingPlaygroundTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.RayCastingPlaygroundState) : WidgetM Unit := do
  let rayName ← registerComponentW "ray-casting-playground"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.key == .char 'r' && data.event.isPress then
      stateRef.set Demos.Linalg.rayCastingPlaygroundInitialState
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData rayName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 && data.click.button != 1 then
      pure ()
    else
      match data.nameMap.get? rayName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let origin := (rect.width / 2, rect.height / 2)
              let scale := 70.0 * env.screenScale
              let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
              let state ← stateRef.get
              let origin2 := Linalg.Vec2.mk state.rayOrigin.x state.rayOrigin.z
              let target2 := Linalg.Vec2.mk state.rayTarget.x state.rayTarget.z
              if data.click.button == 1 then
                stateRef.set { state with dragging := .camera, lastMouseX := data.click.x, lastMouseY := data.click.y }
              else if Demos.Linalg.nearPoint worldPos origin2 0.5 then
                stateRef.set { state with dragging := .origin, lastMouseX := data.click.x, lastMouseY := data.click.y }
              else if Demos.Linalg.nearPoint worldPos target2 0.5 then
                stateRef.set { state with dragging := .direction, lastMouseX := data.click.x, lastMouseY := data.click.y }
              else
                pure ()
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .none => pure ()
    | .camera =>
        let dx := data.x - state.lastMouseX
        let dy := data.y - state.lastMouseY
        let newYaw := state.cameraYaw + dx * 0.005
        let newPitch := state.cameraPitch + dy * 0.005
        stateRef.set { state with
          cameraYaw := newYaw
          cameraPitch := newPitch
          lastMouseX := data.x
          lastMouseY := data.y
        }
    | .origin =>
        match data.nameMap.get? rayName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let newOrigin := Linalg.Vec3.mk worldPos.x 0.0 worldPos.y
                stateRef.set { state with rayOrigin := newOrigin, lastMouseX := data.x, lastMouseY := data.y }
            | none => pure ()
        | none => pure ()
    | .direction =>
        match data.nameMap.get? rayName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                let newTarget := Linalg.Vec3.mk worldPos.x 0.0 worldPos.y
                stateRef.set { state with rayTarget := newTarget, lastMouseX := data.x, lastMouseY := data.y }
            | none => pure ()
        | none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun _ => do
    stateRef.modify fun s => { s with dragging := .none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn rayName 0 containerStyle #[
      Demos.Linalg.rayCastingPlaygroundWidget env state
    ]))
  pure ()

private def primitiveOverlapTesterTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.PrimitiveOverlapTesterState) : WidgetM Unit := do
  let overlapName ← registerComponentW "primitive-overlap-tester"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'r' =>
          stateRef.set Demos.Linalg.primitiveOverlapTesterInitialState
      | .char '1' =>
          stateRef.modify fun s => { s with mode := .sphereSphere }
      | .char '2' =>
          stateRef.modify fun s => { s with mode := .aabbAabb }
      | .char '3' =>
          stateRef.modify fun s => { s with mode := .sphereAabb }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData overlapName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? overlapName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let origin := (rect.width / 2, rect.height / 2)
              let scale := 70.0 * env.screenScale
              let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
              let state ← stateRef.get
              if Demos.Linalg.nearPoint worldPos state.centerA 0.6 then
                stateRef.set { state with dragging := .shapeA }
              else if Demos.Linalg.nearPoint worldPos state.centerB 0.6 then
                stateRef.set { state with dragging := .shapeB }
              else
                pure ()
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    match state.dragging with
    | .shapeA | .shapeB =>
        match data.nameMap.get? overlapName with
        | some wid =>
            match data.layouts.get wid with
            | some layout =>
                let rect := layout.contentRect
                let localX := data.x - rect.x
                let localY := data.y - rect.y
                let origin := (rect.width / 2, rect.height / 2)
                let scale := 70.0 * env.screenScale
                let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
                match state.dragging with
                | .shapeA => stateRef.set { state with centerA := worldPos }
                | .shapeB => stateRef.set { state with centerB := worldPos }
                | .none => pure ()
            | none => pure ()
        | none => pure ()
    | .none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun _ => do
    stateRef.modify fun s => { s with dragging := .none }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn overlapName 0 containerStyle #[
      Demos.Linalg.primitiveOverlapTesterWidget env state
    ]))
  pure ()

private def barycentricCoordinatesTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.BarycentricCoordinatesState) : WidgetM Unit := do
  let baryName ← registerComponentW "barycentric-coordinates"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.key == .char 'r' && data.event.isPress then
      stateRef.set Demos.Linalg.barycentricCoordinatesInitialState
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData baryName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      match data.nameMap.get? baryName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              let origin := (rect.width / 2, rect.height / 2)
              let scale := 70.0 * env.screenScale
              let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
              let state ← stateRef.get
              if Demos.Linalg.nearPoint worldPos state.point 0.4 then
                stateRef.set { state with dragging := true }
              else
                pure ()
          | none => pure ()
      | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    if !state.dragging then
      pure ()
    else
      match data.nameMap.get? baryName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.x - rect.x
              let localY := data.y - rect.y
              let origin := (rect.width / 2, rect.height / 2)
              let scale := 70.0 * env.screenScale
              let worldPos := Demos.Linalg.screenToWorld (localX, localY) origin scale
              stateRef.set { state with point := worldPos }
          | none => pure ()
      | none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun _ => do
    stateRef.modify fun s => { s with dragging := false }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn baryName 0 containerStyle #[
      Demos.Linalg.barycentricCoordinatesWidget env state
    ]))
  pure ()

private def frustumCullingDemoTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Demos.Linalg.FrustumCullingDemoState) : WidgetM Unit := do
  let frustumName ← registerComponentW "frustum-culling-demo"

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    if data.event.isPress then
      match data.event.key with
      | .char 'r' =>
          stateRef.set Demos.Linalg.frustumCullingDemoInitialState
      | .char 'j' =>
          stateRef.modify fun s => { s with camYaw := s.camYaw - 0.08 }
      | .char 'l' =>
          stateRef.modify fun s => { s with camYaw := s.camYaw + 0.08 }
      | .char 'i' =>
          stateRef.modify fun s =>
            let newPitch := s.camPitch + 0.08
            { s with camPitch := if newPitch > 1.2 then 1.2 else newPitch }
      | .char 'k' =>
          stateRef.modify fun s =>
            let newPitch := s.camPitch - 0.08
            { s with camPitch := if newPitch < -1.2 then -1.2 else newPitch }
      | .char '=' | .char '+' =>
          stateRef.modify fun s =>
            let newDist := s.camDist - 0.3
            { s with camDist := if newDist < 2.0 then 2.0 else newDist }
      | .char '-' =>
          stateRef.modify fun s => { s with camDist := s.camDist + 0.3 }
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let clickEvents ← useClickData frustumName
  let clickAction ← Event.mapM (fun data => do
    if data.click.button != 0 then
      pure ()
    else
      stateRef.modify fun s => { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
    ) clickEvents
  performEvent_ clickAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    let state ← stateRef.get
    if !state.dragging then
      pure ()
    else
      let dx := data.x - state.lastMouseX
      let dy := data.y - state.lastMouseY
      let newYaw := state.viewYaw + dx * 0.005
      let newPitch := state.viewPitch + dy * 0.005
      stateRef.set { state with
        viewYaw := newYaw
        viewPitch := newPitch
        lastMouseX := data.x
        lastMouseY := data.y
      }
    ) hoverEvents
  performEvent_ hoverAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify fun s => { s with dragging := false }
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let _ ← dynWidget elapsedTime fun _ => do
    let state ← SpiderM.liftIO stateRef.get
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn frustumName 0 containerStyle #[
      Demos.Linalg.frustumCullingDemoWidget env state
    ]))
  pure ()

private def seascapeTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref SeascapeState) (lastTimeRef : IO.Ref Float)
    (keysRef : IO.Ref SeascapeInputState) (lockRef : IO.Ref Bool)
    (deltaRef : IO.Ref Afferent.Canopy.Reactive.MouseDeltaData) : WidgetM Unit := do
  let seascapeName ← registerComponentW "seascape"
  let clickEvents ← useClick seascapeName
  let clickAction ← Event.mapM (fun _ => do
    let locked ← lockRef.get
    if !locked then
      FFI.Window.setPointerLock env.window true
      lockRef.set true
    ) clickEvents
  performEvent_ clickAction

  let keyEvents ← useKeyboardAll
  let keyAction ← Event.mapM (fun data => do
    let key := data.event.key
    let isPress := data.event.isPress
    if key == .escape && isPress then
      let locked ← lockRef.get
      FFI.Window.setPointerLock env.window (!locked)
      lockRef.set (!locked)
    match key with
    | .char 'w' => keysRef.modify fun s => { s with w := isPress }
    | .char 'a' => keysRef.modify fun s => { s with a := isPress }
    | .char 's' => keysRef.modify fun s => { s with s := isPress }
    | .char 'd' => keysRef.modify fun s => { s with d := isPress }
    | .char 'q' => keysRef.modify fun s => { s with q := isPress }
    | .char 'e' => keysRef.modify fun s => { s with e := isPress }
    | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let mouseDeltas ← useMouseDelta
  let deltaAction ← Event.mapM (fun delta => deltaRef.set delta) mouseDeltas
  performEvent_ deltaAction

  let _ ← dynWidget elapsedTime fun t => do
    let state ← SpiderM.liftIO do
      let lastT ← lastTimeRef.get
      let dt := if lastT == 0.0 then 0.0 else max 0.0 (t - lastT)
      let current ← stateRef.get
      let locked ← lockRef.get
      let keys ← keysRef.get
      let delta ← deltaRef.get
      let dx := if locked then delta.dx else 0.0
      let dy := if locked then delta.dy else 0.0
      let camera := current.camera.update dt keys.w keys.s keys.a keys.d keys.e keys.q dx dy
      let next := { current with camera := camera, locked := locked }
      stateRef.set next
      lastTimeRef.set t
      pure next
    let (windowW, windowH) ← SpiderM.liftIO do
      let (w, h) ← FFI.Window.getSize env.window
      pure (w.toFloat, h.toFloat)
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn seascapeName 0 containerStyle #[
      seascapeWidget t env.screenScale windowW windowH env.fontMedium env.fontSmall state
    ]))
  pure ()

private def shapeGalleryTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (indexRef : IO.Ref Nat) : WidgetM Unit := do
  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    let total := shapeGalleryCount
    if total == 0 then
      pure ()
    else
      match data.event.key with
      | .right | .space =>
          indexRef.modify fun idx => (idx + 1) % total
      | .left =>
          indexRef.modify fun idx => if idx == 0 then total - 1 else idx - 1
      | _ => pure ()
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let idx ← SpiderM.liftIO indexRef.get
    emit (pure (shapeGalleryWidget idx env.screenScale env.fontLarge env.fontSmall env.fontMedium))
  pure ()

private def worldmapTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float)
    (stateRef : IO.Ref Worldmap.MapState) (tileManager : Tileset.TileManager) : WidgetM Unit := do
  let mapName ← registerComponentW "worldmap-map"
  let sidebarName ← registerComponentW "worldmap-sidebar" (isInteractive := false)
  let layoutRef ← SpiderM.liftIO (IO.mkRef (none : Option Trellis.ComputedLayout))

  let updateLayout := fun (nameMap : Std.HashMap String Afferent.Arbor.WidgetId)
      (layouts : Trellis.LayoutResult) => do
        match nameMap.get? mapName with
        | some wid =>
            match layouts.get wid with
            | some layout => layoutRef.set (some layout)
            | none => pure ()
        | none => pure ()

  let updateScreenSize := fun (state : Worldmap.MapState) (layout : Trellis.ComputedLayout) =>
    let w := max 1.0 layout.contentRect.width
    let h := max 1.0 layout.contentRect.height
    state.updateScreenSize w.toUInt32.toNat h.toUInt32.toNat

  let updateCursor := fun (state : Worldmap.MapState) (x y : Float) =>
    if Worldmap.isInsideViewport state x y then
      let (cursorLat, cursorLon) := Worldmap.Zoom.screenToGeo state.viewport x y
      { state with
        cursorLat := cursorLat
        cursorLon := cursorLon
        cursorScreenX := x
        cursorScreenY := y
      }
    else
      state

  let updateDrag := fun (state : Worldmap.MapState) (x y : Float) =>
    if state.isDragging then
      let dx := state.dragStartX - x
      let dy := state.dragStartY - y
      let (dLon, dLat) := state.viewport.pixelsToDegrees dx dy
      let newLat := state.mapBounds.clampLat (Tileset.clampLatitude (state.dragStartLat - dLat))
      let newLon := state.mapBounds.clampLon (state.dragStartLon + dLon)
      { state with viewport := { state.viewport with centerLat := newLat, centerLon := newLon } }
    else
      state

  let clickEvents ← useClickData mapName
  let clickAction ← Event.mapM (fun data => do
    updateLayout data.nameMap data.layouts
    match data.nameMap.get? mapName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let x := data.click.x - layout.contentRect.x
            let y := data.click.y - layout.contentRect.y
            let mut state ← stateRef.get
            state := updateScreenSize state layout
            if data.click.button == 0 && Worldmap.isInsideViewport state x y then
              state := state.startDrag x y
            stateRef.set state
        | none => pure ()
    | none => pure ()
    ) clickEvents
  performEvent_ clickAction

  let mouseUpEvents ← useAllMouseUp
  let mouseUpAction ← Event.mapM (fun data => do
    if data.button == 0 then
      stateRef.modify (·.stopDrag)
    ) mouseUpEvents
  performEvent_ mouseUpAction

  let hoverEvents ← useAllHovers
  let hoverAction ← Event.mapM (fun data => do
    updateLayout data.nameMap data.layouts
    match data.nameMap.get? mapName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let x := data.x - layout.contentRect.x
            let y := data.y - layout.contentRect.y
            let mut state ← stateRef.get
            state := updateScreenSize state layout
            state := updateCursor state x y
            state := updateDrag state x y
            stateRef.set state
        | none => pure ()
    | none => pure ()
    ) hoverEvents
  performEvent_ hoverAction

  let scrollEvents ← useScroll mapName
  let scrollAction ← Event.mapM (fun data => do
    updateLayout data.nameMap data.layouts
    match data.nameMap.get? mapName with
    | some wid =>
        match data.layouts.get wid with
        | some layout =>
            let wheelY := data.scroll.deltaY
            if wheelY != 0.0 then
              let x := data.scroll.x - layout.contentRect.x
              let y := data.scroll.y - layout.contentRect.y
              let mut state ← stateRef.get
              state := updateScreenSize state layout
              if Worldmap.isInsideViewport state x y then
                let delta := if wheelY > 0.0 then 1 else -1
                let newTarget := state.mapBounds.clampZoom (Tileset.clampZoom (state.targetZoom + delta))
                if state.isAnimatingZoom then
                  state := { state with
                    targetZoom := newTarget
                    lastZoomChangeFrame := state.frameCount
                  }
                else
                  let (anchorLat, anchorLon) := Worldmap.Zoom.screenToGeo state.viewport x y
                  state := { state with
                    targetZoom := newTarget
                    isAnimatingZoom := true
                    zoomAnchorScreenX := x
                    zoomAnchorScreenY := y
                    zoomAnchorLat := anchorLat
                    zoomAnchorLon := anchorLon
                    lastZoomChangeFrame := state.frameCount
                  }
              stateRef.set state
        | none => pure ()
    | none => pure ()
    ) scrollEvents
  performEvent_ scrollAction

  let keyEvents ← useKeyboard
  let keyAction ← Event.mapM (fun data => do
    let key := data.event.key
    let mut state ← stateRef.get
    match key with
    | .up =>
        let (_, dLat) := state.viewport.pixelsToDegrees 0.0 (-Worldmap.keyboardPanSpeed)
        let newLat := state.mapBounds.clampLat (Tileset.clampLatitude (state.viewport.centerLat - dLat))
        state := { state with viewport := { state.viewport with centerLat := newLat } }
    | .down =>
        let (_, dLat) := state.viewport.pixelsToDegrees 0.0 Worldmap.keyboardPanSpeed
        let newLat := state.mapBounds.clampLat (Tileset.clampLatitude (state.viewport.centerLat - dLat))
        state := { state with viewport := { state.viewport with centerLat := newLat } }
    | .left =>
        let (dLon, _) := state.viewport.pixelsToDegrees (-Worldmap.keyboardPanSpeed) 0.0
        let newLon := state.mapBounds.clampLon (state.viewport.centerLon + dLon)
        state := { state with viewport := { state.viewport with centerLon := newLon } }
    | .right =>
        let (dLon, _) := state.viewport.pixelsToDegrees Worldmap.keyboardPanSpeed 0.0
        let newLon := state.mapBounds.clampLon (state.viewport.centerLon + dLon)
        state := { state with viewport := { state.viewport with centerLon := newLon } }
    | .home =>
        state := state.resetToInitial
    | .char '=' =>
        let newZoom := state.mapBounds.clampZoom (Tileset.clampZoom (state.viewport.zoom + 1))
        state := state.setZoom newZoom
    | .char '-' =>
        let newZoom := state.mapBounds.clampZoom (Tileset.clampZoom (state.viewport.zoom - 1))
        state := state.setZoom newZoom
    | .char c =>
        if c.isDigit then
          let zoom := (c.toNat - '0'.toNat)
          let newZoom := state.mapBounds.clampZoom (Tileset.clampZoom (Int.ofNat zoom))
          state := state.setZoom newZoom
    | _ => pure ()
    stateRef.set state
    ) keyEvents
  performEvent_ keyAction

  let _ ← dynWidget elapsedTime fun _ => do
    let mut state ← SpiderM.liftIO stateRef.get
    if let some layout ← SpiderM.liftIO layoutRef.get then
      state := updateScreenSize state layout
    state ← requestWorldmapTiles state tileManager
    state ← SpiderM.liftIO (Worldmap.updateFrame state tileManager)
    SpiderM.liftIO (stateRef.set state)
    let (windowW, windowH) ← SpiderM.liftIO do
      let (w, h) ← FFI.Window.getSize env.window
      pure (w.toFloat, h.toFloat)
    emit (pure (worldmapWidgetNamed env.screenScale env.fontMedium env.fontSmall windowW windowH state
      mapName sidebarName))
  pure ()

private def lineCapsTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (lineCapsWidget env.screenScale env.fontSmall env.fontMedium))
  pure ()

private def dashedLinesTabContent (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (dashedLinesWidget env.screenScale env.fontSmall env.fontMedium))
  pure ()

private def statsFooter (env : DemoEnv) (elapsedTime : Dynamic Spider Float) : WidgetM Unit := do
  let footerHeight := 110.0 * env.screenScale
  let footerStyle : BoxStyle := {
    backgroundColor := some (Color.gray 0.08)
    padding := EdgeInsets.symmetric (6.0 * env.screenScale) (4.0 * env.screenScale)
    width := .percent 1.0
    height := .length footerHeight
    flexItem := some (FlexItem.fixed footerHeight)
  }
  column' (gap := 2.0 * env.screenScale) (style := footerStyle) do
    let _ ← dynWidget elapsedTime fun _ => do
      let stats ← SpiderM.liftIO env.statsRef.get
      let lines := formatStatsLines stats
      for line in lines do
        caption' line
      pure ()
    pure ()

private def demoStubContent (id : DemoId) : WidgetM Unit := do
  let inst := demoInstance id
  filledPanel' 24 do
    heading2' inst.name
    caption' s!"{inst.shortName} • stub"
    spacer' 0 8
    bodyText' "This demo has not been migrated to Canopy yet."
    bodyText' "Replace this panel with the real demo widget."

/-- Create the demo shell as a single Canopy widget tree. -/
def createCanopyApp (env : DemoEnv) : ReactiveM CanopyAppState := do
  let elapsedTime ← useElapsedTime
  let reactiveShowcaseApp ← ReactiveShowcase.createApp env
  let widgetPerfApp ← WidgetPerf.createApp env
  let chatDemoApp ← ChatDemo.createApp env
  let circlesRef ← SpiderM.liftIO do
    let particles := Render.Dynamic.ParticleState.create 1000000 env.physWidthF env.physHeightF 42
    IO.mkRef particles
  let circlesTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let spritesRef ← SpiderM.liftIO do
    let particles := Render.Dynamic.ParticleState.create 1000000 env.physWidthF env.physHeightF 123
    IO.mkRef particles
  let spritesTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let seascapeRef ← SpiderM.liftIO (IO.mkRef { camera := seascapeCamera })
  let seascapeTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let seascapeKeysRef ← SpiderM.liftIO (IO.mkRef ({} : SeascapeInputState))
  let seascapeLockRef ← SpiderM.liftIO (IO.mkRef false)
  let seascapeDeltaRef ← SpiderM.liftIO (IO.mkRef { dx := 0.0, dy := 0.0 })
  let shapeGalleryIndexRef ← SpiderM.liftIO (IO.mkRef 0)
  let vectorInterpolationRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.vectorInterpolationInitialState)
  let vectorInterpolationTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let vectorArithmeticRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.vectorArithmeticInitialState)
  let vectorProjectionRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.vectorProjectionInitialState)
  let vectorFieldRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.vectorFieldInitialState)
  let crossProduct3DRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.crossProduct3DInitialState)
  let matrix2DTransformRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.matrix2DTransformInitialState)
  let matrix2DTransformTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let matrix3DTransformRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.matrix3DTransformInitialState)
  let projectionExplorerRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.projectionExplorerInitialState)
  let matrixDecompositionRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.matrixDecompositionInitialState)
  let quaternionVisualizerRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.quaternionVisualizerInitialState)
  let slerpInterpolationRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.slerpInterpolationInitialState)
  let slerpInterpolationTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
  let eulerGimbalLockRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.eulerGimbalLockInitialState)
  let dualQuaternionBlendingRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.dualQuaternionBlendingInitialState)
  let rayCastingPlaygroundRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.rayCastingPlaygroundInitialState)
  let primitiveOverlapTesterRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.primitiveOverlapTesterInitialState)
  let barycentricCoordinatesRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.barycentricCoordinatesInitialState)
  let frustumCullingDemoRef ← SpiderM.liftIO (IO.mkRef Demos.Linalg.frustumCullingDemoInitialState)
  let worldmapStateRef ← SpiderM.liftIO do
    let config : Worldmap.MapStateConfig := {
      lat := 37.7749
      lon := -122.4194
      zoom := 12
      width := env.physWidth.toNat
      height := env.physHeight.toNat
      provider := Tileset.TileProvider.cartoDarkRetina
      fallbackParentDepth := 3
      fallbackChildDepth := 2
      fadeFrames := 12
      persistentFallbackZoom := some 1
    }
    let state ← Worldmap.MapState.init config
    IO.mkRef state
  let worldmapTileConfig : Tileset.TileManagerConfig := {
    provider := Tileset.TileProvider.cartoDarkRetina
    diskCacheDir := "./tile_cache"
    diskCacheMaxSize := 500 * 1024 * 1024
  }
  let worldmapManager ← Tileset.TileManager.new worldmapTileConfig
  let tabs : Array TabDef := demoIds.map fun id => {
    label := (demoInstance id).shortName
    content := match id with
      | .demoGrid => overviewTabContent env elapsedTime
      | .circlesPerf => circlesTabContent env elapsedTime circlesRef circlesTimeRef
      | .spritesPerf => spritesTabContent env elapsedTime spritesRef spritesTimeRef
      | .linesPerf => linesPerfTabContent env elapsedTime
      | .layout => layoutTabContent env elapsedTime
      | .cssGrid => cssGridTabContent env elapsedTime
      | .reactiveShowcase => reactiveShowcaseTabContent reactiveShowcaseApp
      | .widgetPerf => widgetPerfTabContent widgetPerfApp
      | .seascape => seascapeTabContent env elapsedTime seascapeRef seascapeTimeRef
          seascapeKeysRef seascapeLockRef seascapeDeltaRef
      | .shapeGallery => shapeGalleryTabContent env elapsedTime shapeGalleryIndexRef
      | .worldmap => worldmapTabContent env elapsedTime worldmapStateRef worldmapManager
      | .lineCaps => lineCapsTabContent env elapsedTime
      | .dashedLines => dashedLinesTabContent env elapsedTime
      | .textureMatrix => textureMatrixTabContent env elapsedTime
      | .orbitalInstanced => orbitalInstancedTabContent env elapsedTime
      | .fontShowcase => fontShowcaseTabContent env elapsedTime
      | .chatDemo => chatDemoTabContent chatDemoApp
      | .vectorInterpolation =>
          vectorInterpolationTabContent env elapsedTime vectorInterpolationRef vectorInterpolationTimeRef
      | .vectorArithmetic =>
          vectorArithmeticTabContent env elapsedTime vectorArithmeticRef
      | .vectorProjection =>
          vectorProjectionTabContent env elapsedTime vectorProjectionRef
      | .vectorField =>
          vectorFieldTabContent env elapsedTime vectorFieldRef
      | .crossProduct3D =>
          crossProduct3DTabContent env elapsedTime crossProduct3DRef
      | .matrix2DTransform =>
          matrix2DTransformTabContent env elapsedTime matrix2DTransformRef matrix2DTransformTimeRef
      | .matrix3DTransform =>
          matrix3DTransformTabContent env elapsedTime matrix3DTransformRef
      | .projectionExplorer =>
          projectionExplorerTabContent env elapsedTime projectionExplorerRef
      | .matrixDecomposition =>
          matrixDecompositionTabContent env elapsedTime matrixDecompositionRef
      | .quaternionVisualizer =>
          quaternionVisualizerTabContent env elapsedTime quaternionVisualizerRef
      | .slerpInterpolation =>
          slerpInterpolationTabContent env elapsedTime slerpInterpolationRef slerpInterpolationTimeRef
      | .eulerGimbalLock =>
          eulerGimbalLockTabContent env elapsedTime eulerGimbalLockRef
      | .dualQuaternionBlending =>
          dualQuaternionBlendingTabContent env elapsedTime dualQuaternionBlendingRef
      | .rayCastingPlayground =>
          rayCastingPlaygroundTabContent env elapsedTime rayCastingPlaygroundRef
      | .primitiveOverlapTester =>
          primitiveOverlapTesterTabContent env elapsedTime primitiveOverlapTesterRef
      | .barycentricCoordinates =>
          barycentricCoordinatesTabContent env elapsedTime barycentricCoordinatesRef
      | .frustumCullingDemo =>
          frustumCullingDemoTabContent env elapsedTime frustumCullingDemoRef
      | _ => demoStubContent id
  }

  let (_, render) ← runWidget do
    let rootStyle : BoxStyle := {
      backgroundColor := some (Color.gray 0.08)
      padding := EdgeInsets.uniform 16
      width := .percent 1.0
      height := .percent 1.0
      flexItem := some (FlexItem.growing 1)
    }

    column' (gap := 16) (style := rootStyle) do
      heading1' "Afferent Demos"

      let contentStyle : BoxStyle := {
        flexItem := some (FlexItem.growing 1)
        width := .percent 1.0
        height := .percent 1.0
      }

      column' (gap := 0) (style := contentStyle) do
        let _ ← tabView tabs 0
        pure ()
      statsFooter env elapsedTime

  pure { render := render }

end Demos
