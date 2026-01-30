/-
  Projection Matrix Explorer - Visualize perspective and orthographic projections.
  Shows frustum visualization with adjustable FOV, near/far planes, and aspect ratio.
  Demonstrates Mat4.perspective, Mat4.orthographic, Mat4.lookAt.
-/
import Afferent
import Afferent.Widget
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Linalg.Shared
import Trellis
import Linalg.Core
import Linalg.Vec2
import Linalg.Vec3
import Linalg.Vec4
import Linalg.Mat4

open Afferent CanvasM Linalg

namespace Demos.Linalg

/-- Projection type -/
inductive ProjectionType where
  | perspective
  | orthographic
  deriving BEq, Inhabited

/-- State for the projection explorer demo -/
structure ProjectionExplorerState where
  projType : ProjectionType := .perspective
  fov : Float := Float.pi / 3  -- 60 degrees
  near : Float := 0.5
  far : Float := 5.0
  aspect : Float := 1.5
  orthoSize : Float := 2.0  -- Half-height for orthographic
  cameraYaw : Float := 0.4
  cameraPitch : Float := 0.3
  dragging : Bool := false
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  showClipSpace : Bool := true
  showTestObjects : Bool := true
  deriving Inhabited

def projectionExplorerInitialState : ProjectionExplorerState := {}

/-- Test objects placed in world space for projection visualization. -/
def projectionTestObjects : Array (Vec3 × Float × Color) := #[
  (Vec3.mk 0.0 0.0 (-1.0), 0.2, Color.cyan),
  (Vec3.mk 0.5 0.3 (-2.5), 0.3, Color.magenta),
  (Vec3.mk (-0.4) (-0.2) (-4.0), 0.4, Color.orange)
]

/-- Build the projection matrix from the current state. -/
def projectionMatrix (state : ProjectionExplorerState) : Mat4 :=
  match state.projType with
  | .perspective =>
      Mat4.perspective state.fov state.aspect state.near state.far
  | .orthographic =>
      let halfH := state.orthoSize
      let halfW := halfH * state.aspect
      Mat4.orthographic (-halfW) halfW (-halfH) halfH state.near state.far

/-- Build the view matrix (camera at origin looking down -Z). -/
def viewMatrix : Mat4 :=
  Mat4.lookAt (Vec3.mk 0 0 0) (Vec3.mk 0 0 (-1)) (Vec3.mk 0 1 0)

/-- Project 3D point to 2D screen -/
def projectToScreen (p : Vec3) (yaw pitch : Float) (origin : Float × Float) (scale : Float) : Float × Float :=
  let cosY := Float.cos yaw
  let sinY := Float.sin yaw
  let x1 := p.x * cosY + p.z * sinY
  let z1 := -p.x * sinY + p.z * cosY

  let cosP := Float.cos pitch
  let sinP := Float.sin pitch
  let y2 := p.y * cosP - z1 * sinP

  (origin.1 + x1 * scale, origin.2 - y2 * scale)

/-- Draw a line in 3D -/
def drawLine3D (p1 p2 : Vec3) (yaw pitch : Float) (origin : Float × Float) (scale : Float)
    (color : Color) (lineWidth : Float := 1.0) : CanvasM Unit := do
  let s1 := projectToScreen p1 yaw pitch origin scale
  let s2 := projectToScreen p2 yaw pitch origin scale
  setStrokeColor color
  setLineWidth lineWidth
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk s1.1 s1.2)
    |>.lineTo (Point.mk s2.1 s2.2)
  strokePath path

/-- Get perspective frustum corners at a given distance -/
def getPerspectivePlaneCorners (fov aspect distance : Float) : Array Vec3 :=
  let halfH := distance * Float.tan (fov / 2)
  let halfW := halfH * aspect
  #[
    Vec3.mk (-halfW) (-halfH) (-distance),  -- Bottom-left
    Vec3.mk halfW (-halfH) (-distance),      -- Bottom-right
    Vec3.mk halfW halfH (-distance),         -- Top-right
    Vec3.mk (-halfW) halfH (-distance)       -- Top-left
  ]

/-- Get orthographic frustum corners at a given distance -/
def getOrthographicPlaneCorners (size aspect distance : Float) : Array Vec3 :=
  let halfH := size
  let halfW := halfH * aspect
  #[
    Vec3.mk (-halfW) (-halfH) (-distance),
    Vec3.mk halfW (-halfH) (-distance),
    Vec3.mk halfW halfH (-distance),
    Vec3.mk (-halfW) halfH (-distance)
  ]

/-- Draw a frustum plane (rectangle) -/
def drawFrustumPlane (corners : Array Vec3) (yaw pitch : Float) (origin : Float × Float)
    (scale : Float) (edgeColor : Color) (fillColor : Option Color := none) : CanvasM Unit := do
  if corners.size < 4 then return

  let projected := corners.map (projectToScreen · yaw pitch origin scale)

  -- Build path
  let p0 := projected.getD 0 (0.0, 0.0)
  let mut path := Afferent.Path.empty |>.moveTo (Point.mk p0.1 p0.2)
  for i in [1:4] do
    let p := projected.getD i (0.0, 0.0)
    path := path.lineTo (Point.mk p.1 p.2)
  path := path.closePath

  -- Fill if requested
  if let some fill := fillColor then
    setFillColor fill
    fillPath path

  -- Stroke edges
  setStrokeColor edgeColor
  setLineWidth 1.5
  strokePath path

/-- Draw a complete frustum visualization -/
def drawFrustum (state : ProjectionExplorerState) (yaw pitch : Float)
    (origin : Float × Float) (scale : Float) : CanvasM Unit := do
  let (nearCorners, farCorners) := match state.projType with
    | .perspective =>
      (getPerspectivePlaneCorners state.fov state.aspect state.near,
       getPerspectivePlaneCorners state.fov state.aspect state.far)
    | .orthographic =>
      (getOrthographicPlaneCorners state.orthoSize state.aspect state.near,
       getOrthographicPlaneCorners state.orthoSize state.aspect state.far)

  -- Draw near plane
  drawFrustumPlane nearCorners yaw pitch origin scale
    (Color.rgba 0.3 1.0 0.3 0.8) (some (Color.rgba 0.3 1.0 0.3 0.15))

  -- Draw far plane
  drawFrustumPlane farCorners yaw pitch origin scale
    (Color.rgba 1.0 0.3 0.3 0.8) (some (Color.rgba 1.0 0.3 0.3 0.1))

  -- Draw edges connecting near to far
  let edgeColor := Color.rgba 0.5 0.7 1.0 0.6
  for i in [:4] do
    let nearCorner := nearCorners.getD i Vec3.zero
    let farCorner := farCorners.getD i Vec3.zero
    drawLine3D nearCorner farCorner yaw pitch origin scale edgeColor 1.0

  -- Draw camera origin
  let camPos := projectToScreen Vec3.zero yaw pitch origin scale
  setFillColor Color.yellow
  fillPath (Afferent.Path.circle (Point.mk camPos.1 camPos.2) 6.0)

  -- Draw camera direction indicator
  let dirEnd := projectToScreen (Vec3.mk 0 0 (-0.3)) yaw pitch origin scale
  setStrokeColor Color.yellow
  setLineWidth 2.0
  let dirPath := Afferent.Path.empty
    |>.moveTo (Point.mk camPos.1 camPos.2)
    |>.lineTo (Point.mk dirEnd.1 dirEnd.2)
  strokePath dirPath

/-- Draw test objects in the scene -/
def drawTestObjects (_state : ProjectionExplorerState) (yaw pitch : Float)
    (origin : Float × Float) (scale : Float) : CanvasM Unit := do
  for (pos, radius, color) in projectionTestObjects do
    let screenPos := projectToScreen pos yaw pitch origin scale
    let screenRadius := radius * scale * 0.8  -- Approximate screen-space radius
    setFillColor (Color.rgba color.r color.g color.b 0.6)
    fillPath (Afferent.Path.circle (Point.mk screenPos.1 screenPos.2) screenRadius)
    setStrokeColor color
    setLineWidth 2.0
    strokePath (Afferent.Path.circle (Point.mk screenPos.1 screenPos.2) screenRadius)

/-- Draw clip space visualization (NDC cube) -/
def drawClipSpace (origin : Float × Float) (scale : Float) (yaw pitch : Float)
    (points : Array (Vec3 × Color)) : CanvasM Unit := do
  -- Draw the normalized device coordinate cube (-1 to 1 in all axes)
  let vertices := #[
    Vec3.mk (-1) (-1) (-1), Vec3.mk 1 (-1) (-1),
    Vec3.mk 1 1 (-1), Vec3.mk (-1) 1 (-1),
    Vec3.mk (-1) (-1) 1, Vec3.mk 1 (-1) 1,
    Vec3.mk 1 1 1, Vec3.mk (-1) 1 1
  ]

  let edges := #[
    (0, 1), (1, 2), (2, 3), (3, 0),
    (4, 5), (5, 6), (6, 7), (7, 4),
    (0, 4), (1, 5), (2, 6), (3, 7)
  ]

  let projected := vertices.map (projectToScreen · yaw pitch origin scale)

  setStrokeColor (Color.rgba 1.0 1.0 0.3 0.5)
  setLineWidth 1.0
  for (i, j) in edges do
    let p1 := projected.getD i (0.0, 0.0)
    let p2 := projected.getD j (0.0, 0.0)
    let path := Afferent.Path.empty
      |>.moveTo (Point.mk p1.1 p1.2)
      |>.lineTo (Point.mk p2.1 p2.2)
    strokePath path

  -- Draw projected points inside clip space
  for (pos, color) in points do
    let screenPos := projectToScreen pos yaw pitch origin scale
    setFillColor color
    fillPath (Afferent.Path.circle (Point.mk screenPos.1 screenPos.2) 4.0)

/-- Render the projection explorer visualization -/
def renderProjectionExplorer (state : ProjectionExplorerState)
    (w h : Float) (screenScale : Float) (fontMedium fontSmall : Font) : CanvasM Unit := do
  -- Split view: left side shows world space, right side shows clip space
  let worldOrigin := (w * 0.35, h / 2)
  let clipOrigin := (w * 0.75, h / 2)
  let worldScale := 50.0 * screenScale
  let clipScale := 40.0 * screenScale
  let vp := projectionMatrix state * viewMatrix
  let clipSamples := Id.run do
    let mut arr : Array (Vec3 × Color × Vec4) := #[]
    for (pos, _radius, color) in projectionTestObjects do
      let clip := vp.transformVec4 (Vec4.fromPoint pos)
      let ndc := clip.toVec3Normalized
      let alpha := if clip.w <= 0.0 then 0.2 else 0.85
      let drawColor := Color.rgba color.r color.g color.b alpha
      arr := arr.push (ndc, drawColor, clip)
    return arr

  -- Draw divider
  setStrokeColor (Color.gray 0.3)
  setLineWidth 1.0
  let dividerPath := Afferent.Path.empty
    |>.moveTo (Point.mk (w * 0.55) 80)
    |>.lineTo (Point.mk (w * 0.55) (h - 40))
  strokePath dividerPath

  -- World space view (left)
  setFillColor (Color.gray 0.7)
  fillTextXY "World Space" (w * 0.25) (90 * screenScale) fontSmall

  -- Draw ground grid
  setStrokeColor (Color.gray 0.15)
  for i in [:11] do
    let offset := (i.toFloat - 5.0) * 0.5
    let p1 := projectToScreen (Vec3.mk (-2.5) 0 (offset - 3)) state.cameraYaw state.cameraPitch worldOrigin worldScale
    let p2 := projectToScreen (Vec3.mk 2.5 0 (offset - 3)) state.cameraYaw state.cameraPitch worldOrigin worldScale
    let path := Afferent.Path.empty
      |>.moveTo (Point.mk p1.1 p1.2)
      |>.lineTo (Point.mk p2.1 p2.2)
    strokePath path

  -- Draw frustum
  drawFrustum state state.cameraYaw state.cameraPitch worldOrigin worldScale

  -- Draw test objects
  if state.showTestObjects then
    drawTestObjects state state.cameraYaw state.cameraPitch worldOrigin worldScale

  -- Clip space view (right)
  setFillColor (Color.gray 0.7)
  fillTextXY "Clip Space (NDC)" (w * 0.68) (90 * screenScale) fontSmall

  if state.showClipSpace then
    let clipPoints := if state.showTestObjects then
      clipSamples.map (fun sample => (sample.1, sample.2.1))
    else
      #[]
    drawClipSpace clipOrigin clipScale 0.4 0.3 clipPoints

  -- Info panel
  let infoY := h - 160.0 * screenScale
  let infoX := 20.0 * screenScale

  setFillColor VecColor.label
  let projName := match state.projType with
    | .perspective => "Perspective"
    | .orthographic => "Orthographic"
  fillTextXY s!"Projection: {projName}" infoX infoY fontSmall

  match state.projType with
  | .perspective =>
    let fovDeg := state.fov * 180 / Float.pi
    fillTextXY s!"FOV: {formatFloat fovDeg}" infoX (infoY + 22 * screenScale) fontSmall
  | .orthographic =>
    fillTextXY s!"Size: {formatFloat state.orthoSize}" infoX (infoY + 22 * screenScale) fontSmall

  fillTextXY s!"Near: {formatFloat state.near}" infoX (infoY + 44 * screenScale) fontSmall
  fillTextXY s!"Far: {formatFloat state.far}" infoX (infoY + 66 * screenScale) fontSmall
  fillTextXY s!"Aspect: {formatFloat state.aspect}" infoX (infoY + 88 * screenScale) fontSmall

  -- Color legend
  setFillColor (Color.rgba 0.3 1.0 0.3 0.8)
  fillPath (Afferent.Path.rectangleXYWH infoX (infoY + 110 * screenScale) 12 12)
  setFillColor (Color.gray 0.7)
  fillTextXY "Near plane" (infoX + 18) (infoY + 120 * screenScale) fontSmall

  setFillColor (Color.rgba 1.0 0.3 0.3 0.8)
  fillPath (Afferent.Path.rectangleXYWH (infoX + 100 * screenScale) (infoY + 110 * screenScale) 12 12)
  setFillColor (Color.gray 0.7)
  fillTextXY "Far plane" (infoX + 118 * screenScale) (infoY + 120 * screenScale) fontSmall

  -- Clip coordinate readout (right side)
  let clipInfoX := w * 0.58
  let clipInfoY := h - 120.0 * screenScale
  setFillColor (Color.gray 0.7)
  fillTextXY "Clip coords (x,y,z,w):" clipInfoX clipInfoY fontSmall
  if state.showTestObjects then
    for i in [:clipSamples.size] do
      let sample := clipSamples.getD i (Vec3.zero, Color.white, Vec4.zero)
      let clip := sample.2.2
      let line := s!"{i + 1}: {formatFloat clip.x}, {formatFloat clip.y}, {formatFloat clip.z}, {formatFloat clip.w}"
      fillTextXY line clipInfoX (clipInfoY + (i.toFloat + 1) * 18.0 * screenScale) fontSmall

  -- Title and instructions
  setFillColor VecColor.label
  fillTextXY "PROJECTION MATRIX EXPLORER" (20 * screenScale) (30 * screenScale) fontMedium
  setFillColor (Color.gray 0.6)
  fillTextXY "Tab: Switch projection | F/N: Near/Far | +/-: FOV/Size | C: Clip | O: Objects | Drag: Rotate"
    (20 * screenScale) (55 * screenScale) fontSmall

/-- Create the projection explorer widget -/
def projectionExplorerWidget (env : DemoEnv) (state : ProjectionExplorerState)
    : Afferent.Arbor.WidgetBuilder := do
  Afferent.Arbor.custom (spec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        renderProjectionExplorer state w h env.screenScale env.fontMedium env.fontSmall
    )
  }) (style := { flexItem := some (Trellis.FlexItem.growing 1) })

end Demos.Linalg
