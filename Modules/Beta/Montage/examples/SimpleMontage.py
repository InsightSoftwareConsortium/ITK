# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
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
import itkConfig
itkConfig.LazyLoading = False
import itk

itk.auto_progress(2)

if len(sys.argv) < 4:
  print(f"Usage: {sys.argv[0]} <directoryWithInputData> <outputDirectory> <outputFilename>")
  sys.exit(1)

inputPath = sys.argv[1]
outputPath = sys.argv[2]
outFile = sys.argv[3]

if not inputPath.endswith('/') and not inputPath.endswith('\\'):
  inputPath += '/'

if not outputPath.endswith('/') and not outputPath.endswith('\\'):
  outputPath += '/'

os.chdir(outputPath)
outFile = os.path.abspath(outFile)

dimension = 2

stageTiles = itk.TileConfiguration[dimension]()
stageTiles.Parse(inputPath + "TileConfiguration.txt")

ScalarPixelType = itk.UC
OriginalPixelType = itk.RGBPixel[itk.UC]
ScalarImageType = itk.Image[ScalarPixelType,dimension]
OriginalImageType = itk.Image[OriginalPixelType,dimension]

oImages = [] # RGB images, for mosaic creation
sImages = [] # grayscale images, for registration
for t in range(stageTiles.LinearSize()):
  origin = stageTiles.GetTile(t).GetPosition()
  filename = inputPath + stageTiles.GetTile(t).GetFileName()
  image = itk.imread(filename, OriginalPixelType)
  sp = image.GetSpacing()
  # tile configurations are in pixel (index) coordinates
  # so we convert them into physical ones
  for d in range(dimension):
    origin[d] *= sp[d]

  image.SetOrigin(origin)
  oImages.append(image)

  image = itk.imread(filename, ScalarPixelType)
  image.SetOrigin(origin)
  sImages.append(image)

# only float is wrapped as coordinate representation type in TileMontage
montage = itk.TileMontage[ScalarImageType, itk.F].New()
montage.SetMontageSize(stageTiles.GetAxisSizes())
for t in range(stageTiles.LinearSize()):
  montage.SetInputTile(t, sImages[t])

print("Computing tile registration transforms")
montage.Update()

print("Writing tile transforms")
actualTiles = stageTiles # we will update it later
for t in range(stageTiles.LinearSize()):
  ind = stageTiles.LinearIndexToNDIndex(t)
  regTr = montage.GetOutputTransform(ind)
  tile = stageTiles.GetTile(t)
  itk.transformwrite([regTr], outputPath + tile.GetFileName() + ".tfm")

  # calculate updated positions - transform physical into index shift
  pos = tile.GetPosition()
  for d in range(dimension):
    pos[d] -= regTr.GetOffset()[d] / sp[d]
  tile.SetPosition(pos)
  actualTiles.SetTile(t, tile)
actualTiles.Write(outputPath + "TileConfiguration.registered.txt")

print("Producing the mosaic")
resampleF = itk.TileMergeImageFilter[OriginalImageType].New()
resampleF.SetMontageSize(stageTiles.GetAxisSizes())
for t in range(stageTiles.LinearSize()):
  resampleF.SetInputTile(t, oImages[t])
  ind = stageTiles.LinearIndexToNDIndex(t)
  resampleF.SetTileTransform(ind, montage.GetOutputTransform(ind))
resampleF.Update()
itk.imwrite(resampleF.GetOutput(), outFile)
print("Resampling complete")
