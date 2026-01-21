/-
  TextureCache - GPU Texture Management for Map Tiles

  This module provides a thin adapter layer between the GPU-agnostic tileset library
  and Afferent's GPU textures. It handles:
  - On-demand texture upload from PNG bytes to GPU
  - LRU eviction of distant tiles from GPU memory
  - Texture lifecycle management (create/destroy)
-/
import Std.Data.HashMap
import Std.Data.HashSet
import Tileset
import Afferent.FFI.Texture

namespace Worldmap

open Std (HashMap HashSet)
open Afferent.FFI (Texture)
open Tileset (TileCoord)

/-- Entry in the texture cache with LRU tracking -/
structure TextureEntry where
  /-- GPU texture handle -/
  texture : Texture
  /-- Frame when this texture was last used -/
  lastUsedFrame : Nat

/-- GPU texture cache for map tiles -/
structure TextureCache where
  /-- Map from tile coordinate to GPU texture -/
  texturesRef : IO.Ref (HashMap TileCoord TextureEntry)
  /-- Maximum number of textures to keep in GPU memory -/
  maxTextures : Nat

namespace TextureCache

/-- Create a new texture cache -/
def new (maxTextures : Nat := 256) : IO TextureCache := do
  let texturesRef ← IO.mkRef {}
  pure { texturesRef, maxTextures }

/-- Check if a texture exists for a tile coordinate -/
def has (cache : TextureCache) (coord : TileCoord) : IO Bool := do
  let textures ← cache.texturesRef.get
  pure (textures.contains coord)

/-- Get an existing texture, updating its LRU timestamp -/
def get? (cache : TextureCache) (coord : TileCoord) (frame : Nat)
    : IO (Option Texture) := do
  let textures ← cache.texturesRef.get
  match textures[coord]? with
  | some entry =>
    -- Update LRU timestamp
    cache.texturesRef.modify (·.insert coord { entry with lastUsedFrame := frame })
    pure (some entry.texture)
  | none => pure none

/-- Upload PNG bytes to GPU and cache the texture.
    If the texture already exists, just returns it (updating LRU). -/
def getOrUpload (cache : TextureCache) (coord : TileCoord) (pngData : ByteArray)
    (frame : Nat) : IO Texture := do
  let textures ← cache.texturesRef.get
  match textures[coord]? with
  | some entry =>
    -- Already have texture, update LRU and return
    cache.texturesRef.modify (·.insert coord { entry with lastUsedFrame := frame })
    pure entry.texture
  | none =>
    -- Upload to GPU
    let texture ← Texture.loadFromMemory pngData
    let entry : TextureEntry := { texture, lastUsedFrame := frame }
    cache.texturesRef.modify (·.insert coord entry)
    pure texture

/-- Evict textures not in the keep set (LRU eviction for distant tiles).
    Destroys the GPU textures to free memory. -/
def evictDistant (cache : TextureCache) (keepSet : HashSet TileCoord) : IO Unit := do
  let textures ← cache.texturesRef.get
  -- Find textures outside the keep set
  let toEvict := textures.toList.filter fun (coord, _) => !keepSet.contains coord
  -- Destroy and remove them
  for (coord, entry) in toEvict do
    Texture.destroy entry.texture
    cache.texturesRef.modify (·.erase coord)

/-- Evict oldest textures if cache exceeds max size.
    Keeps textures in the keepSet regardless of age. -/
def evictOldest (cache : TextureCache) (keepSet : HashSet TileCoord) : IO Unit := do
  let textures ← cache.texturesRef.get
  let currentSize := textures.size
  if currentSize <= cache.maxTextures then
    return ()  -- Under limit, nothing to do

  -- Sort by lastUsedFrame (oldest first), excluding keep set
  let evictable := textures.toList.filter fun (coord, _) => !keepSet.contains coord
  let sorted := evictable.toArray.qsort fun (_, a) (_, b) => a.lastUsedFrame < b.lastUsedFrame
  let numToEvict := currentSize - cache.maxTextures

  -- Evict the oldest entries
  for i in [:numToEvict] do
    if h : i < sorted.size then
      let (coord, entry) := sorted[i]
      Texture.destroy entry.texture
      cache.texturesRef.modify (·.erase coord)

/-- Get cache statistics: (total textures, total memory estimate in bytes) -/
def stats (cache : TextureCache) : IO (Nat × Nat) := do
  let textures ← cache.texturesRef.get
  let count := textures.size
  -- Estimate: 512x512 tiles with 4 bytes per pixel = ~1MB each
  let memoryEstimate := count * 512 * 512 * 4
  pure (count, memoryEstimate)

/-- Destroy all textures and clear the cache -/
def clear (cache : TextureCache) : IO Unit := do
  let textures ← cache.texturesRef.get
  for (_, entry) in textures.toList do
    Texture.destroy entry.texture
  cache.texturesRef.set {}

/-- Get the texture for a coordinate without updating LRU (for read-only access) -/
def peek? (cache : TextureCache) (coord : TileCoord) : IO (Option Texture) := do
  let textures ← cache.texturesRef.get
  pure (textures[coord]?.map (·.texture))

/-- Get all coordinates currently in the cache -/
def coordinates (cache : TextureCache) : IO (List TileCoord) := do
  let textures ← cache.texturesRef.get
  pure (textures.toList.map Prod.fst)

/-- Number of textures currently cached -/
def size (cache : TextureCache) : IO Nat := do
  let textures ← cache.texturesRef.get
  pure textures.size

end TextureCache

end Worldmap
