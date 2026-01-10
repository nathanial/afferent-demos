/-
  ReactiveShowcase - FRP-Powered Widget Demo
  Demonstrates interactive widgets using the data/reactive FRP library.

  Uses a React-like component model where each widget is self-contained:
  - Creates its own internal state (dynamics)
  - Wires its own subscriptions
  - Returns IO WidgetBuilder that samples dynamics at render time
-/
import Demos.ReactiveShowcase.Types
import Demos.ReactiveShowcase.Inputs
import Demos.ReactiveShowcase.Component
import Demos.ReactiveShowcase.App

-- Everything is already in the Demos.ReactiveShowcase namespace via the imports.
-- Components are in Demos.ReactiveShowcase.Components namespace.
