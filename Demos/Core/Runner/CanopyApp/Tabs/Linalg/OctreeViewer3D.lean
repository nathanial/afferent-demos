/-
  Demo Runner - Canopy app linalg OctreeViewer3D tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
import Demos.Linalg.OctreeViewer3D
import Trellis
import AfferentMath.Widget.MathView3D

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis
open AfferentMath.Widget

namespace Demos

private def updateExtents (scale : Float) (state : Demos.Linalg.OctreeViewer3DState)
    : Demos.Linalg.OctreeViewer3DState :=
  let minE := 0.3
  let maxE := 4.5
  let ex := Linalg.Float.clamp (state.queryExtents.x * scale) minE maxE
  let ey := Linalg.Float.clamp (state.queryExtents.y * scale) minE maxE
  let ez := Linalg.Float.clamp (state.queryExtents.z * scale) minE maxE
  { state with queryExtents := Linalg.Vec3.mk ex ey ez }

private def updateRotation (yawDelta pitchDelta : Float) (state : Demos.Linalg.OctreeViewer3DState)
    : Demos.Linalg.OctreeViewer3DState :=
  let newPitch := Linalg.Float.clamp (state.cameraPitch + pitchDelta) (-1.2) 1.2
  { state with cameraYaw := state.cameraYaw + yawDelta, cameraPitch := newPitch }

private def updateCameraDistance (scale : Float) (state : Demos.Linalg.OctreeViewer3DState)
    : Demos.Linalg.OctreeViewer3DState :=
  let minD := 4.0
  let maxD := 25.0
  let newDist := Linalg.Float.clamp (state.cameraDistance * scale) minD maxD
  { state with cameraDistance := newDist }

def octreeViewer3DTabContent (env : DemoEnv) : WidgetM Unit := do
  let demoName ← registerComponentW "octree-viewer-3d"

  let keyEvents ← useKeyboard
  let keyUpdates ← Event.mapM (fun data =>
    fun (s : Demos.Linalg.OctreeViewer3DState) =>
      if data.event.isPress then
        match data.event.key with
        | .char 'r' => Demos.Linalg.octreeViewer3DInitialState
        | .char 'x' =>
            if s.items.isEmpty then s
            else { s with items := s.items.eraseIdxIfInBounds (s.items.size - 1) }
        | .char 'v' => { s with showNodes := !s.showNodes }
        | .char '-' => updateExtents 0.9 s
        | .char '+' => updateExtents 1.1 s
        | .char '=' => updateExtents 1.1 s
        | .char 'w' => { s with queryCenter := s.queryCenter.add (Linalg.Vec3.mk 0.0 0.3 0.0) }
        | .char 's' => { s with queryCenter := s.queryCenter.add (Linalg.Vec3.mk 0.0 (-0.3) 0.0) }
        | .char 'a' => { s with queryCenter := s.queryCenter.add (Linalg.Vec3.mk (-0.3) 0.0 0.0) }
        | .char 'd' => { s with queryCenter := s.queryCenter.add (Linalg.Vec3.mk 0.3 0.0 0.0) }
        | .char 'u' => { s with queryCenter := s.queryCenter.add (Linalg.Vec3.mk 0.0 0.0 0.3) }
        | .char 'j' => { s with queryCenter := s.queryCenter.add (Linalg.Vec3.mk 0.0 0.0 (-0.3)) }
        | .char '[' => updateCameraDistance 1.1 s
        | .char ']' => updateCameraDistance 0.9 s
        | .left => updateRotation (-0.1) 0.0 s
        | .right => updateRotation 0.1 0.0 s
        | .up => updateRotation 0.0 (-0.1) s
        | .down => updateRotation 0.0 0.1 s
        | _ => s
      else s
    ) keyEvents

  let clickEvents ← useClickData demoName
  let clickUpdates ← Event.mapM (fun data =>
    if data.click.button != 0 then
      id
    else
      match data.nameMap.get? demoName with
      | some wid =>
          match data.layouts.get wid with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.click.x - rect.x
              let localY := data.click.y - rect.y
              fun (state : Demos.Linalg.OctreeViewer3DState) =>
                let config := Demos.Linalg.octreeViewer3DMathViewConfig state env.screenScale
                let view := AfferentMath.Widget.MathView3D.viewForSize config rect.width rect.height
                match Demos.Linalg.screenToWorldOnPlane view localX localY 0.0 with
                | some worldPos =>
                    let z := Float.sin state.spawnPhase * 2.0
                    let center := Linalg.Vec3.mk worldPos.x worldPos.y z
                    let size := 0.25 + 0.1 * Float.cos (state.spawnPhase * 0.8)
                    let item := Linalg.AABB.fromCenterExtents center (Linalg.Vec3.mk size size size)
                    { state with
                      items := state.items.push item
                      spawnPhase := state.spawnPhase + 0.7 }
                | none => state
          | none => id
      | none => id
    ) clickEvents

  let allUpdates ← Event.mergeAllListM [keyUpdates, clickUpdates]
  let state ← foldDyn (fun f s => f s) Demos.Linalg.octreeViewer3DInitialState allUpdates

  let _ ← dynWidget state fun s => do
    let containerStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    emit (pure (namedColumn demoName 0 containerStyle #[
      Demos.Linalg.octreeViewer3DWidget env s
    ]))
  pure ()

end Demos
