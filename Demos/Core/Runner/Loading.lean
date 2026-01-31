/-
  Demo Runner - Asset loading and environment setup.
-/
import Afferent
import Afferent.Arbor
import Demos.Core.Demo
import Demos.Core.Runner.Types
import Std.Data.HashMap
import Init.Data.FloatArray

set_option maxRecDepth 1024

open Afferent CanvasM

namespace Demos

/-- Showcase font specifications: (key, fontPath, size) -/
private def showcaseFontSpecs : Array (String × String × Nat) :=
  let families := #[
    ("monaco", "/System/Library/Fonts/Monaco.ttf"),
    ("helvetica", "/System/Library/Fonts/Helvetica.ttc"),
    ("times", "/System/Library/Fonts/Times.ttc"),
    ("georgia", "/System/Library/Fonts/Supplemental/Georgia.ttf")
  ]
  let sizes := #[12, 18, 24, 36, 48, 72]
  families.foldl (init := #[]) fun acc (family, path) =>
    sizes.foldl (init := acc) fun acc' size =>
      acc'.push (s!"{family}-{size}", path, size)

private def showcaseFontCount : Nat := 24  -- 4 families × 6 sizes

private def loadingStepsTotal : Nat := 13 + showcaseFontCount  -- 37 total

private def loadingStepsDone (s : LoadingState) : Nat :=
  (if s.fontSmall.isSome then 1 else 0) +
  (if s.fontMedium.isSome then 1 else 0) +
  (if s.fontLarge.isSome then 1 else 0) +
  (if s.fontHuge.isSome then 1 else 0) +
  (if s.fontCanopy.isSome then 1 else 0) +
  (if s.fontCanopySmall.isSome then 1 else 0) +
  (if s.layoutFont.isSome then 1 else 0) +
  s.showcaseFontsLoaded +
  (if s.fontPack.isSome then 1 else 0) +
  (if s.spriteTexture.isSome then 1 else 0) +
  (if s.lineSegments.isSome then 1 else 0) +
  (if s.lineBuffer.isSome then 1 else 0) +
  (if s.orbitalParams.isSome then 1 else 0) +
  (if s.orbitalBuffer.isSome then 1 else 0)

def loadingProgress (s : LoadingState) : Float :=
  (loadingStepsDone s).toFloat / (loadingStepsTotal.toFloat)

def loadingStatus (s : LoadingState) : String :=
  if s.fontSmall.isNone || s.fontMedium.isNone || s.fontLarge.isNone || s.fontHuge.isNone ||
     s.fontCanopy.isNone || s.fontCanopySmall.isNone then
    "Loading fonts..."
  else if s.layoutFont.isNone then
    "Preparing layout font..."
  else if s.showcaseFontsLoaded < showcaseFontCount then
    "Loading showcase fonts..."
  else if s.fontPack.isNone then
    "Registering fonts..."
  else if s.spriteTexture.isNone then
    "Loading sprites..."
  else if s.lineSegments.isNone then
    "Generating lines..."
  else if s.lineBuffer.isNone then
    "Uploading line buffer..."
  else if s.orbitalParams.isNone then
    "Preparing orbitals..."
  else if s.orbitalBuffer.isNone then
    "Uploading orbitals..."
  else
    "Finalizing..."

def buildOrbitalParams (orbitalCount : Nat)
    (minRadius maxRadius speedMin speedMax sizeMin sizeMax : Float) : FloatArray := Id.run do
  let twoPi : Float := 6.283185307
  let mut arr := FloatArray.emptyWithCapacity (orbitalCount * 5)
  let mut s := 4242
  for i in [:orbitalCount] do
    s := (s * 1103515245 + 12345) % (2^31)
    let phase := (s.toFloat / 2147483648.0) * twoPi
    s := (s * 1103515245 + 12345) % (2^31)
    let radius := minRadius + (s.toFloat / 2147483648.0) * (maxRadius - minRadius)
    s := (s * 1103515245 + 12345) % (2^31)
    let baseSpeed := speedMin + (s.toFloat / 2147483648.0) * (speedMax - speedMin)
    s := (s * 1103515245 + 12345) % (2^31)
    let dir : Float := if s % 2 == 0 then 1.0 else -1.0
    let speed := baseSpeed * dir
    s := (s * 1103515245 + 12345) % (2^31)
    let size := sizeMin + (s.toFloat / 2147483648.0) * (sizeMax - sizeMin)
    let hue := i.toFloat / orbitalCount.toFloat
    arr := arr.push phase
    arr := arr.push radius
    arr := arr.push speed
    arr := arr.push hue
    arr := arr.push size
  arr

private def spriteHalfSizeFromTexture (texture : FFI.Texture) : IO Float := do
  let (w, h) ← FFI.Texture.getSize texture
  let side : UInt32 := if w ≤ h then w else h
  pure (side.toFloat / 2.0)

def renderLoading (c : Canvas) (t : Float) (screenScale : Float)
    (progress : Float) (label : String) (font? : Option Font) : IO Canvas := do
  let c' ← run' c do
    resetTransform
    let (w, h) ← getCurrentSize
    setFillColor (Color.gray 0.12)
    fillRectXYWH 0 0 w h
    let barW := min (w * 0.6) (420.0 * screenScale)
    let barH := max (h * 0.02) (12.0 * screenScale)
    let x := (w - barW) / 2.0
    let y := (h - barH) / 2.0
    setFillColor (Color.gray 0.22)
    fillRectXYWH x y barW barH
    let hue := (t * 0.08) - (t * 0.08).floor
    setFillColor (Color.hsva hue 0.55 0.9 1.0)
    fillRectXYWH x y (barW * progress) barH
    let radius := min w h * 0.08
    let angle := t * 2.2
    let dotSize := max (6.0 * screenScale) (barH * 0.6)
    let dotX := w * 0.5 + Float.cos angle * radius
    let dotY := h * 0.5 - Float.sin angle * radius
    setFillColor (Color.gray 0.8)
    fillRectXYWH (dotX - dotSize / 2) (dotY - dotSize / 2) dotSize dotSize
    if let some font := font? then
      let (textW, _) ← measureText label font
      setFillColor (Color.gray 0.75)
      fillTextXY label ((w - textW) / 2.0) (y - 12.0 * screenScale) font
  pure c'

def advanceLoading (s0 : LoadingState) (screenScale : Float) (canvas : Canvas)
    (lineRef : IO.Ref (Option (Array Float × Nat)))
    (orbitalRef : IO.Ref (Option FloatArray))
    (orbitalCount : Nat) : IO LoadingState := do
  let mut s := s0
  if s.lineSegments.isNone then
    if let some segs ← lineRef.get then
      s := { s with lineSegments := some segs }
  if s.orbitalParams.isNone then
    if let some params ← orbitalRef.get then
      s := { s with orbitalParams := some params }

  if s.fontSmall.isNone then
    let fontSmall ← Font.load "/System/Library/Fonts/Monaco.ttf" (16 * screenScale).toUInt32
    return { s with fontSmall := some fontSmall }
  if s.fontMedium.isNone then
    let fontMedium ← Font.load "/System/Library/Fonts/Monaco.ttf" (24 * screenScale).toUInt32
    return { s with fontMedium := some fontMedium }
  if s.fontLarge.isNone then
    let fontLarge ← Font.load "/System/Library/Fonts/Monaco.ttf" (36 * screenScale).toUInt32
    return { s with fontLarge := some fontLarge }
  if s.fontHuge.isNone then
    let fontHuge ← Font.load "/System/Library/Fonts/Monaco.ttf" (48 * screenScale).toUInt32
    return { s with fontHuge := some fontHuge }
  if s.fontCanopy.isNone then
    let fontCanopy ← Font.load "/System/Library/Fonts/Monaco.ttf" (14 * screenScale).toUInt32
    return { s with fontCanopy := some fontCanopy }
  if s.fontCanopySmall.isNone then
    let fontCanopySmall ← Font.load "/System/Library/Fonts/Monaco.ttf" (10 * screenScale).toUInt32
    return { s with fontCanopySmall := some fontCanopySmall }
  if s.layoutFont.isNone then
    let layoutLabelPt : Float := 12.0
    let layoutFontPx : UInt32 := (max 8.0 (layoutLabelPt * screenScale)).toUInt32
    let layoutFont ← Font.load "/System/Library/Fonts/Monaco.ttf" layoutFontPx
    return { s with layoutFont := some layoutFont }
  -- Load showcase fonts one at a time into the HashMap
  if s.showcaseFontsLoaded < showcaseFontCount then
    let specs := showcaseFontSpecs
    if h : s.showcaseFontsLoaded < specs.size then
      let (key, path, size) := specs[s.showcaseFontsLoaded]
      let font ← Font.load path (size.toFloat * screenScale).toUInt32
      let newFonts := s.showcaseFonts.insert key font
      return { s with showcaseFonts := newFonts, showcaseFontsLoaded := s.showcaseFontsLoaded + 1 }
    else
      return s
  if s.fontPack.isNone then
    match s.fontSmall, s.fontMedium, s.fontLarge, s.fontHuge, s.fontCanopy, s.fontCanopySmall with
    | some fontSmall, some fontMedium, some fontLarge, some fontHuge, some fontCanopy, some fontCanopySmall =>
        let (reg1, fontSmallId) := FontRegistry.empty.register fontSmall "small"
        let (reg2, fontMediumId) := reg1.register fontMedium "medium"
        let (reg3, fontLargeId) := reg2.register fontLarge "large"
        let (reg4, fontHugeId) := reg3.register fontHuge "huge"
        let (reg5, fontCanopyId) := reg4.register fontCanopy "canopy"
        let (reg6, fontCanopySmallId) := reg5.register fontCanopySmall "canopySmall"
        -- Register showcase fonts from the HashMap
        let (finalReg, showcaseFontIds) := s.showcaseFonts.fold (init := (reg6, ({} : Std.HashMap String Afferent.Arbor.FontId))) fun (reg, ids) key font =>
          let (newReg, fontId) := reg.register font key
          (newReg, ids.insert key fontId)
        let registry := finalReg.setDefault fontMedium
        return { s with fontPack := some {
          registry := registry
          smallId := fontSmallId
          mediumId := fontMediumId
          largeId := fontLargeId
          hugeId := fontHugeId
          canopyId := fontCanopyId
          canopySmallId := fontCanopySmallId
          showcaseFonts := showcaseFontIds
        } }
    | _, _, _, _, _, _ => return s
  if s.spriteTexture.isNone then
    let spriteTexture ← FFI.Texture.load "nibble-32.png"
    return { s with spriteTexture := some spriteTexture }
  if s.lineBuffer.isNone then
    match s.lineSegments with
    | some (segments, _) =>
        let lineBuffer ← FFI.Buffer.createStrokeSegmentPersistent canvas.ctx.renderer segments
        return { s with lineBuffer := some lineBuffer }
    | none => pure ()
  if s.orbitalBuffer.isNone then
    match s.orbitalParams with
    | some _ =>
        let orbitalBuffer ← FFI.FloatBuffer.create (orbitalCount.toUSize * 8)
        return { s with orbitalBuffer := some orbitalBuffer }
    | none => pure ()
  return s

def toLoadedAssets (s : LoadingState)
    (screenScale circleRadius : Float)
    (lineWidth : Float)
    (orbitalCount : Nat)
    (physWidthF physHeightF : Float)
    (physWidth physHeight : UInt32)
    (layoutOffsetX layoutOffsetY layoutScale : Float)
    : IO (Option LoadedAssets) := do
  match s.fontSmall, s.fontMedium, s.fontLarge, s.fontHuge, s.fontCanopy, s.fontCanopySmall,
        s.layoutFont, s.fontPack, s.spriteTexture, s.lineSegments,
        s.lineBuffer, s.orbitalParams, s.orbitalBuffer with
  | some fontSmall, some fontMedium, some fontLarge, some fontHuge, some fontCanopy, some fontCanopySmall,
    some layoutFont, some fontPack, some spriteTexture, some (_, lineCount),
    some lineBuffer, some orbitalParams, some orbitalBuffer =>
      let spriteHalfSize ← spriteHalfSizeFromTexture spriteTexture
      pure (some {
        screenScale
        fontSmall
        fontMedium
        fontLarge
        fontHuge
        fontCanopy
        fontCanopySmall
        layoutFont
        showcaseFonts := s.showcaseFonts
        fontPack
        spriteTexture
        circleRadius
        spriteHalfSize
        lineBuffer
        lineCount
        lineWidth
        orbitalCount
        orbitalParams
        orbitalBuffer
        physWidthF
        physHeightF
        physWidth
        physHeight
        layoutOffsetX
        layoutOffsetY
        layoutScale
      })
  | _, _, _, _, _, _, _, _, _, _, _, _, _ => pure none

def cleanupLoading (s : LoadingState) : IO Unit := do
  if let some font := s.fontSmall then font.destroy
  if let some font := s.fontMedium then font.destroy
  if let some font := s.fontLarge then font.destroy
  if let some font := s.fontHuge then font.destroy
  if let some font := s.fontCanopy then font.destroy
  if let some font := s.fontCanopySmall then font.destroy
  if let some font := s.layoutFont then font.destroy
  -- Cleanup showcase fonts from HashMap
  for (_, font) in s.showcaseFonts.toList do
    font.destroy
  if let some tex := s.spriteTexture then FFI.Texture.destroy tex
  if let some buf := s.lineBuffer then FFI.Buffer.destroy buf
  if let some buf := s.orbitalBuffer then FFI.FloatBuffer.destroy buf

def cleanupAssets (a : LoadedAssets) : IO Unit := do
  a.fontSmall.destroy
  a.fontMedium.destroy
  a.fontLarge.destroy
  a.fontHuge.destroy
  a.fontCanopy.destroy
  a.fontCanopySmall.destroy
  a.layoutFont.destroy
  -- Cleanup showcase fonts from HashMap
  for (_, font) in a.showcaseFonts.toList do
    font.destroy
  FFI.Texture.destroy a.spriteTexture
  FFI.Buffer.destroy a.lineBuffer
  FFI.FloatBuffer.destroy a.orbitalBuffer

def mkEnvFromAssets (a : LoadedAssets) (t dt : Float)
    (keyCode : UInt16) (clearKey : IO Unit) (window : FFI.Window)
    (statsRef : IO.Ref RunnerStats) : DemoEnv := {
  statsRef := statsRef
  screenScale := a.screenScale
  t := t
  dt := dt
  keyCode := keyCode
  clearKey := clearKey
  window := window
  fontSmall := a.fontSmall
  fontMedium := a.fontMedium
  fontLarge := a.fontLarge
  fontHuge := a.fontHuge
  fontCanopy := a.fontCanopy
  fontCanopySmall := a.fontCanopySmall
  layoutFont := a.layoutFont
  fontRegistry := a.fontPack.registry
  fontMediumId := a.fontPack.mediumId
  fontSmallId := a.fontPack.smallId
  fontLargeId := a.fontPack.largeId
  fontHugeId := a.fontPack.hugeId
  fontCanopyId := a.fontPack.canopyId
  fontCanopySmallId := a.fontPack.canopySmallId
  showcaseFonts := a.fontPack.showcaseFonts
  spriteTexture := a.spriteTexture
  circleRadius := a.circleRadius
  spriteHalfSize := a.spriteHalfSize
  lineBuffer := a.lineBuffer
  lineCount := a.lineCount
  lineWidth := a.lineWidth
  orbitalCount := a.orbitalCount
  orbitalParams := a.orbitalParams
  orbitalBuffer := a.orbitalBuffer
  windowWidthF := a.physWidthF
  windowHeightF := a.physHeightF
  physWidthF := a.physWidthF
  physHeightF := a.physHeightF
  physWidth := a.physWidth
  physHeight := a.physHeight
  contentOffsetX := 0.0
  contentOffsetY := 0.0
  layoutOffsetX := a.layoutOffsetX
  layoutOffsetY := a.layoutOffsetY
  layoutScale := a.layoutScale
}

end Demos
