# CLAUDE.md

Demo runner and visual showcase for the Afferent 2D graphics framework.

## Build & Run

```bash
./build.sh                         # Build the demo executable
./build.sh afferent_demos          # Same as above (explicit target)
.lake/build/bin/afferent_demos     # Run the demo app
```

The build script sets `LEAN_CC=/usr/bin/clang` for proper macOS framework linking.

## Testing

```bash
./build.sh afferent_demos_tests
lake test
```

## Project Structure

```
Demos/
├── Overview/      # Basic feature demos (shapes, transforms, strokes, gradients, text, animations)
├── Perf/          # Performance benchmarks (grid, triangles, circles, sprites, lines)
├── Layout/        # Layout demos (flexbox)
├── Visuals/       # Visual galleries (shape gallery, line caps, dashed lines)
├── Chat/          # Chat app demo
├── Core/          # Demo runner infrastructure (registry, tab bar, unified runner)
└── Reactive/      # FRP demos
```

## Dependencies

- **afferent** - GPU-accelerated 2D graphics (Metal)
- **crucible** - Test framework
- **wisp** - HTTP client
- **cellar** - Disk cache
- **reactive** - FRP library
- **tileset** - Map tile loading

## Adding a Demo

1. Create a new file in the appropriate `Demos/` subdirectory
2. Define a `WidgetBuilder` function that returns your demo widget
3. Register it in `Demos/Core/DemoRegistry.lean`
4. Add the import to `Demos.lean`

## Requirements

- macOS with Metal support
- Homebrew libraries: freetype, assimp, curl
