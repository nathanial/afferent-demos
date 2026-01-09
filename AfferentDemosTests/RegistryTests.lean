/-
  ComponentRegistry Unit Tests
-/
import Crucible
import Reactive
import Demos.ReactiveShowcase.Inputs

open Crucible
open Reactive Reactive.Host
open Demos.ReactiveShowcase

namespace AfferentDemosTests.Registry

testSuite "ComponentRegistry"

test "register generates unique names" := do
  let result ← SpiderM.runFresh do
    let reg ← ComponentRegistry.create
    let n1 ← SpiderM.liftIO <| reg.register "button"
    let n2 ← SpiderM.liftIO <| reg.register "button"
    let n3 ← SpiderM.liftIO <| reg.register "checkbox"
    pure (n1, n2, n3)
  result.1 ≡ "button-0"
  result.2.1 ≡ "button-1"
  result.2.2 ≡ "checkbox-2"

test "input tracking" := do
  let result ← SpiderM.runFresh do
    let reg ← ComponentRegistry.create
    let _ ← SpiderM.liftIO <| reg.register "button" (isInput := false)
    let _ ← SpiderM.liftIO <| reg.register "text-input" (isInput := true)
    let _ ← SpiderM.liftIO <| reg.register "text-input" (isInput := true)
    let inputs ← SpiderM.liftIO reg.inputNames.get
    let interactives ← SpiderM.liftIO reg.interactiveNames.get
    pure (inputs.size, interactives.size)
  result.1 ≡ 2
  result.2 ≡ 3

test "non-interactive widgets not tracked" := do
  let result ← SpiderM.runFresh do
    let reg ← ComponentRegistry.create
    let _ ← SpiderM.liftIO <| reg.register "label" (isInput := false) (isInteractive := false)
    let _ ← SpiderM.liftIO <| reg.register "button"
    let interactives ← SpiderM.liftIO reg.interactiveNames.get
    pure interactives.size
  result ≡ 1

test "counter increments globally" := do
  let result ← SpiderM.runFresh do
    let reg ← ComponentRegistry.create
    let n1 ← SpiderM.liftIO <| reg.register "a"
    let n2 ← SpiderM.liftIO <| reg.register "b"
    let n3 ← SpiderM.liftIO <| reg.register "a"
    pure (n1, n2, n3)
  result.1 ≡ "a-0"
  result.2.1 ≡ "b-1"
  result.2.2 ≡ "a-2"

test "initial focus is none" := do
  let initial ← SpiderM.runFresh do
    let reg ← ComponentRegistry.create
    reg.focusedInput.sample
  initial ≡ none

test "focus can be fired" := do
  SpiderM.runFresh do
    let reg ← ComponentRegistry.create
    let _ ← SpiderM.liftIO <| reg.register "text-input" (isInput := true)
    SpiderM.liftIO <| reg.fireFocus (some "text-input-0")

test "registries are independent" := do
  let result ← SpiderM.runFresh do
    let reg1 ← ComponentRegistry.create
    let reg2 ← ComponentRegistry.create
    let n1 ← SpiderM.liftIO <| reg1.register "button"
    let n2 ← SpiderM.liftIO <| reg2.register "button"
    pure (n1, n2)
  result.1 ≡ "button-0"
  result.2 ≡ "button-0"

test "mixed input and interactive flags" := do
  let result ← SpiderM.runFresh do
    let reg ← ComponentRegistry.create
    let _ ← SpiderM.liftIO <| reg.register "text-input" (isInput := true) (isInteractive := true)
    let _ ← SpiderM.liftIO <| reg.register "readonly" (isInput := true) (isInteractive := false)
    let inputs ← SpiderM.liftIO reg.inputNames.get
    let interactives ← SpiderM.liftIO reg.interactiveNames.get
    pure (inputs.size, interactives.size)
  result.1 ≡ 2
  result.2 ≡ 1

#generate_tests

end AfferentDemosTests.Registry
