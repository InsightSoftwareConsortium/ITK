/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkTileConfiguration.h"

namespace itk
{
TileLayout2D
ParseTileConfiguration2D(const std::string pathToFile)
{
  TileConfiguration<2> tc;
  tc.Parse(pathToFile);

  TileLayout2D tiles(tc.AxisSizes[1]);
  for (unsigned y = 0; y < tc.AxisSizes[1]; y++)
  {
    tiles[y].resize(tc.AxisSizes[0]);
    for (unsigned x = 0; x < tc.AxisSizes[0]; x++)
    {
      size_t linearIndex = x + y * tc.AxisSizes[0];
      tiles[y][x] = tc.Tiles[linearIndex];
    }
  }

  return tiles;
}

void
WriteTileConfiguration2D(const std::string pathToFile, const TileLayout2D & tileConfiguration2D)
{
  TileConfiguration<2> tc;
  tc.AxisSizes[1] = tileConfiguration2D.size();
  if (tc.AxisSizes[1] > 0)
  {
    tc.AxisSizes[0] = tileConfiguration2D[0].size();
  }
  tc.Tiles.resize(tc.LinearSize());

  for (unsigned y = 0; y < tileConfiguration2D.size(); y++)
  {
    for (unsigned x = 0; x < tileConfiguration2D[y].size(); x++)
    {
      size_t linearIndex = x + y * tc.AxisSizes[0];
      tc.Tiles[linearIndex].Position = tileConfiguration2D[y][x].Position;
      tc.Tiles[linearIndex].FileName = tileConfiguration2D[y][x].FileName;
    }
  }

  tc.Write(pathToFile); // delegate writing to nD templated method
}

} // namespace itk
