# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# ==========================================================================*/
import sys
import os
import itk
from pathlib import Path

if len(sys.argv) < 4:
  print(f"Usage: {sys.argv[0]} <directoryWithInputTiles> <outputDirectory> <outputFilename>")
  sys.exit(1)

input_path = Path(sys.argv[1])
output_path = Path(sys.argv[2])
out_file = Path(sys.argv[3])
if not out_file.is_absolute():
  out_file = (output_path / out_file).resolve()


dimension = 2

stage_tiles = itk.TileConfiguration[dimension]()
stage_tiles.Parse(str(input_path / "TileConfiguration.txt"))

color_images = [] # for mosaic creation
grayscale_images = [] # for registration
for t in range(stage_tiles.LinearSize()):
  origin = stage_tiles.GetTile(t).GetPosition()
  filename = str(input_path / stage_tiles.GetTile(t).GetFileName())
  image = itk.imread(filename)
  spacing = image.GetSpacing()

  # tile configurations are in pixel (index) coordinates
  # so we convert them into physical ones
  for d in range(dimension):
    origin[d] *= spacing[d]

  image.SetOrigin(origin)
  color_images.append(image)

  image = itk.imread(filename, itk.F) # read as grayscale
  image.SetOrigin(origin)
  grayscale_images.append(image)

# only float is wrapped as coordinate representation type in TileMontage
montage = itk.TileMontage[type(grayscale_images[0]), itk.F].New()
montage.SetMontageSize(stage_tiles.GetAxisSizes())
for t in range(stage_tiles.LinearSize()):
  montage.SetInputTile(t, grayscale_images[t])

print("Computing tile registration transforms")
montage.Update()

print("Writing tile transforms")
actual_tiles = stage_tiles # we will update it later
for t in range(stage_tiles.LinearSize()):
  index = stage_tiles.LinearIndexToNDIndex(t)
  regTr = montage.GetOutputTransform(index)
  tile = stage_tiles.GetTile(t)
  itk.transformwrite([regTr], str(output_path / (tile.GetFileName() + ".tfm")))

  # calculate updated positions - transform physical into index shift
  pos = tile.GetPosition()
  for d in range(dimension):
    pos[d] -= regTr.GetOffset()[d] / spacing[d]
  tile.SetPosition(pos)
  actual_tiles.SetTile(t, tile)
actual_tiles.Write(str(output_path / "TileConfiguration.registered.txt"))

print("Producing the mosaic")

color_image_type = type(color_images[0])
interpolater_function_type = itk.LinearInterpolateImageFunction[color_image_type, itk.D]
input_pixel_type = itk.template(color_images[0])[1][0]
try:
  input_rgb_type = itk.template(input_pixel_type)[0]
  accum_type = input_rgb_type[itk.F] # RGB or RGBA input/output images
except KeyError:
  accum_type = itk.D # scalar input / output images

resampleF = itk.TileMergeImageFilter[color_image_type, accum_type, interpolater_function_type].New()
resampleF.SetMontageSize(stage_tiles.GetAxisSizes())
for t in range(stage_tiles.LinearSize()):
  resampleF.SetInputTile(t, color_images[t])
  index = stage_tiles.LinearIndexToNDIndex(t)
  resampleF.SetTileTransform(index, montage.GetOutputTransform(index))
resampleF.Update()
itk.imwrite(resampleF.GetOutput(), str(out_file))
print("Resampling complete")
