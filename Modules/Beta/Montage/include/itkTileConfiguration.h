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

#ifndef itkTileConfiguration_h
#define itkTileConfiguration_h

#include "MontageExport.h"

#include <string>
#include <vector>

#include "itkPoint.h"

namespace itk
{
template <unsigned Dimension>
struct Tile
{
  using PointType = Point<double, Dimension>;

  PointType Position; // x, y... coordinates

  std::string FileName;
};

using Tile2D = Tile<2>;
using TileRow2D = std::vector<Tile2D>;
using TileLayout2D = std::vector<TileRow2D>;

/** The tile filenames are taken directly from the configuration file.
 * Path is NOT prepended to them, and they are not otherwise modified. */
Montage_EXPORT TileLayout2D
               ParseTileConfiguration2D(const std::string pathToFile);

/** The path is NOT prepended to tile filenames. */
Montage_EXPORT void
WriteTileConfiguration2D(const std::string pathToFile, const TileLayout2D & tileConfiguration2D);

} // namespace itk

#endif // itkTileConfiguration_h
