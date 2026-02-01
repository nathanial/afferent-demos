/-
  Demo Runner - Canopy app core tab content.
-/
import Reactive
import Afferent
import Afferent.Canopy
import Afferent.Canopy.Reactive
import Demos.Core.Demo
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
import Demos.Overview.Fonts
import Demos.Chat.App
import Trellis

open Reactive Reactive.Host
open Afferent
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

namespace Demos

private def demoFontsFromEnv (env : DemoEnv) : DemoFonts := {
  label := env.fontSmallId
  small := env.fontSmallId
  medium := env.fontMediumId
  large := env.fontLargeId
  huge := env.fontHugeId
}

def overviewTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let demoFonts := demoFontsFromEnv env
  let cubes := spinningCubesInitialState
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (demoGridWidget env.screenScale t demoFonts cubes env.windowWidthF env.windowHeightF))
  pure ()

def circlesTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let particlesRef ← SpiderM.liftIO do
    let particles := Render.Dynamic.ParticleState.create 1000000 env.physWidthF env.physHeightF 42
    IO.mkRef particles
  let lastTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
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

def spritesTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let particlesRef ← SpiderM.liftIO do
    let particles := Render.Dynamic.ParticleState.create 1000000 env.physWidthF env.physHeightF 123
    IO.mkRef particles
  let lastTimeRef ← SpiderM.liftIO (IO.mkRef 0.0)
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

def linesPerfTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let _ ← dynWidget elapsedTime fun t => do
    emit (pure (linesPerfWidget t env.lineBuffer env.lineCount env.lineWidth
      env.fontMedium env.windowWidthF env.windowHeightF))
  pure ()

def layoutTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (layoutWidgetFlex env.fontMediumId env.fontSmallId env.screenScale))
  pure ()

def cssGridTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (cssGridWidget env.fontMediumId env.fontSmallId env.screenScale))
  pure ()

def reactiveShowcaseTabContent (appState : ReactiveShowcase.AppState) : WidgetM Unit := do
  emit appState.render

def widgetPerfTabContent (appState : WidgetPerf.AppState) : WidgetM Unit := do
  emit appState.render

def fontShowcaseTabContent (env : DemoEnv) : WidgetM Unit := do
  let elapsedTime ← useElapsedTime
  let _ ← dynWidget elapsedTime fun _ => do
    emit (pure (fontShowcaseWidget env.showcaseFonts env.fontMediumId env.screenScale))
  pure ()

def chatDemoTabContent (appState : ChatDemo.AppState) : WidgetM Unit := do
  emit appState.render

end Demos
