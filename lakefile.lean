import Lake
open Lake DSL

package afferentDemos where
  version := v!"0.1.0"

-- require afferent from git "https://github.com/nathanial/afferent" @ "v0.0.3"
require afferent from "../afferent"
-- require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.9"
require crucible from "../../testing/crucible"
-- require wisp from git "https://github.com/nathanial/wisp" @ "v0.0.1"
require wisp from "../../network/wisp"
-- require cellar from git "https://github.com/nathanial/cellar" @ "v0.0.1"
require cellar from "../../data/cellar"
-- require reactive from git "https://github.com/nathanial/reactive" @ "v0.0.4"
require reactive from "../../data/reactive"
-- require tileset from git "https://github.com/nathanial/tileset" @ "v0.0.1"
require tileset from "../../data/tileset"

-- Link arguments for Metal/macOS (inherited from Afferent)
def commonLinkArgs : Array String := #[
  "-framework", "Metal",
  "-framework", "Cocoa",
  "-framework", "QuartzCore",
  "-framework", "Foundation",
  "-framework", "Security",
  "-framework", "SystemConfiguration",
  "-lobjc",
  "-L/opt/homebrew/lib",
  "-L/usr/local/lib",
  "-lfreetype",
  "-lassimp",
  "-lz",
  "-lc++",
  "-lcurl"
]

@[default_target]
lean_lib AfferentDemos where
  roots := #[`AfferentDemos]

lean_lib Demos where
  roots := #[`Demos]

lean_lib Worldmap where
  roots := #[`Worldmap]

lean_lib AfferentDemosTests where
  roots := #[`AfferentDemosTests]
  globs := #[.submodules `AfferentDemosTests]

lean_exe afferent_demos where
  root := `AfferentDemos.Main
  moreLinkArgs := commonLinkArgs

@[test_driver]
lean_exe afferent_demos_tests where
  root := `AfferentDemosTests.Main
  moreLinkArgs := commonLinkArgs
