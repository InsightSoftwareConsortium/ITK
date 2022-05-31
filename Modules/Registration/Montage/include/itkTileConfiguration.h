/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

#include <string>
#include <vector>

#include "itkPoint.h"
#include "itkSize.h"

#include "double-conversion/double-conversion.h"

#include <cassert>
#include <fstream>
#include <limits>
#include <sstream>

namespace itk
{
template <unsigned Dimension>
struct ITK_TEMPLATE_EXPORT Tile
{
  using PointType = Point<double, Dimension>;

  PointType Position; // x, y... coordinates

  std::string FileName;

  /* Primarily for access via SWIG wrapping */
  PointType
  GetPosition() const
  {
    return Position;
  }
  void
  SetPosition(PointType position)
  {
    Position = position;
  }

  /* Primarily for access via SWIG wrapping */
  std::string
  GetFileName() const
  {
    return FileName;
  }
  void
  SetFileName(std::string fileName)
  {
    FileName = fileName;
  }
};


template <unsigned Dimension>
struct ITK_TEMPLATE_EXPORT TileConfiguration
{
  using PointType = typename Tile<Dimension>::PointType;
  using TileIndexType = Size<Dimension>;
  using TileND = Tile<Dimension>;

  TileIndexType AxisSizes;

  std::vector<TileND> Tiles;

  /* Primarily for access via SWIG wrapping */
  TileIndexType
  GetAxisSizes() const
  {
    return AxisSizes;
  }
  void
  SetAxisSizes(TileIndexType axisSizes)
  {
    AxisSizes = axisSizes;
  }

  /* Primarily for access via SWIG wrapping */
  TileND
  GetTile(size_t linearIndex) const
  {
    return Tiles[linearIndex];
  }
  void
  SetTile(size_t linearIndex, TileND tile)
  {
    Tiles[linearIndex] = tile;
  }

  size_t
  LinearSize() const
  {
    size_t linearSize = 1u;
    for (unsigned d = 0; d < Dimension; d++)
    {
      linearSize *= AxisSizes[d];
    }
    return linearSize;
  }

  size_t
  nDIndexToLinearIndex(TileIndexType nDIndex) const
  {
    size_t        ind = 0;
    SizeValueType stride = 1u;
    for (unsigned d = 0; d < Dimension; d++)
    {
      itkAssertOrThrowMacro(nDIndex[d] < AxisSizes[d],
                            "Tile index " << nDIndex << " exceeds axis size " << AxisSizes << " at dimension " << d);
      ind += nDIndex[d] * stride;
      stride *= AxisSizes[d];
    }
    return ind;
  }

  TileIndexType
  LinearIndexToNDIndex(size_t linearIndex) const
  {
    TileIndexType ind;
    SizeValueType stride = 1u;
    for (unsigned d = 0; d < Dimension; d++)
    {
      stride *= AxisSizes[d];
      ind[d] = linearIndex % AxisSizes[d];
      linearIndex /= AxisSizes[d];
    }
    itkAssertOrThrowMacro(linearIndex < stride,
                          "Linear tile index " << linearIndex << " exceeds total montage size " << stride);
    return ind;
  }

  // tries parsing the file, return first file name and set dimension
  static std::string
  TryParse(const std::string & pathToFile, unsigned & dimension)
  {
    std::ifstream tileFile(pathToFile);
    if (!tileFile)
    {
      throw std::runtime_error("Could not open for reading: " + pathToFile);
    }

    std::string temp = getNextNonCommentLine(tileFile);
    if (temp.substr(0, 6) == "dim = ")
    {
      dimension = std::stoul(temp.substr(6));
      temp = TileConfiguration<Dimension>::getNextNonCommentLine(tileFile); // get next line
    }

    std::string     timePointID;
    Tile<Dimension> tile = parseLine(temp, timePointID);
    return tile.FileName;
  }

  void
  Parse(const std::string & pathToFile)
  {
    std::ifstream tileFile(pathToFile);
    if (!tileFile)
    {
      throw std::runtime_error("Could not open for reading: " + pathToFile);
    }
    std::string line = getNextNonCommentLine(tileFile);
    if (line.substr(0, 6) == "dim = ")
    {
      unsigned dim = std::stoul(line.substr(6));
      if (dim != Dimension)
      {
        throw std::runtime_error("Expected dimension " + std::to_string(Dimension) + ", but got " +
                                 std::to_string(dim) + " from string:\n\n" + line);
      }
      line = TileConfiguration<Dimension>::getNextNonCommentLine(tileFile); // get next line
    }

    AxisSizes.Fill(1);
    Tiles.clear();
    TileIndexType cInd;
    cInd.Fill(0);
    unsigned initializedDimensions = 0; // no dimension has been initialized

    std::string          timePoint;
    itk::Tile<Dimension> tile = parseLine(line, timePoint);
    Tiles.push_back(tile);
    line = getNextNonCommentLine(tileFile);

    while (tileFile)
    {
      tile = parseLine(line, timePoint);
      // determine dominant axis change
      unsigned maxAxis = 0; // (0=x, 1=y, 2=z etc)
      double   maxDiff = tile.Position[0] - Tiles.back().Position[0];
      for (unsigned d = 1; d < Dimension; d++)
      {
        double diff = tile.Position[d] - Tiles.back().Position[d];
        if (diff > maxDiff)
        {
          maxDiff = diff;
          maxAxis = d;
        }
      }

      if (maxAxis > initializedDimensions) // we now know the size along this dimension
      {
        AxisSizes[maxAxis - 1] = cInd[maxAxis - 1] + 1;
        initializedDimensions = maxAxis;
      }

      // check consistency with previously established size
      for (unsigned d = 0; d < maxAxis; d++)
      {
        itkAssertOrThrowMacro(cInd[d] == AxisSizes[d] - 1,
                              "Axis sizes: " << AxisSizes << " current index: " << cInd
                                             << ". We have reached the end along axis " << maxAxis
                                             << "\nIndex along axis " << d << " is " << cInd[d] << ", but it should be "
                                             << AxisSizes[d] - 1);
      }

      // update current tile index
      for (unsigned d = 0; d < maxAxis; d++)
      {
        cInd[d] = 0;
      }
      ++cInd[maxAxis];


      if (maxAxis < initializedDimensions) // check bounds, if bounds are established
      {
        itkAssertOrThrowMacro(cInd[maxAxis] < AxisSizes[maxAxis],
                              "Axis sizes: " << AxisSizes << ", but we reached index " << cInd[maxAxis]
                                             << ". Violation along axis " << maxAxis);
      }

      Tiles.push_back(tile);
      line = getNextNonCommentLine(tileFile);
    }

    for (unsigned d = 0; d < Dimension; ++d)
    {
      AxisSizes[d] = cInd[d] + 1;
    }

    size_t expectedSize = this->LinearSize();
    itkAssertOrThrowMacro(expectedSize == Tiles.size(),
                          "Incorrect number of tiles: " << Tiles.size() << ". Expected: " << expectedSize);
  }

  void
  Write(const std::string & pathToFile)
  {
    std::ofstream tileFile(pathToFile);
    if (!tileFile)
    {
      throw std::runtime_error("Could not open for writing: " + pathToFile);
    }

    tileFile << "# Tile coordinates are in index space, not physical space\n";
    tileFile << "dim = " << Dimension << "\n\n";
    char                             buffer[25];
    double_conversion::StringBuilder conversionResult(buffer, 25);

    size_t totalTiles = this->LinearSize();
    for (SizeValueType linearIndex = 0; linearIndex < totalTiles; linearIndex++)
    {
      tileFile << Tiles[linearIndex].FileName << ";;(";

      for (unsigned d = 0; d < Dimension; d++)
      {
        if (d > 0)
        {
          tileFile << ", ";
        }

        doubleConverter.ToShortest(Tiles[linearIndex].Position[d], &conversionResult);
        tileFile << conversionResult.Finalize();
        conversionResult.Reset();
      }
      tileFile << ')' << std::endl;
    }

    if (!tileFile)
    {
      throw std::runtime_error("Writing not successful to: " + pathToFile);
    }
  }

  static double_conversion::StringToDoubleConverter stringConverter;
  static double_conversion::DoubleToStringConverter doubleConverter;

  static std::string
  getNextNonCommentLine(std::istream & in)
  {
    std::string temp;
    while (std::getline(in, temp))
    {
      if (temp.empty() || temp[0] == '#')
      {
        continue; // this is either an empty line or a comment
      }
      if (temp.size() == 1 && temp[0] == '\r')
      {
        continue; // empty line ending in CRLF
      }
      if (temp[temp.size() - 1] == '\r')
      {
        temp.erase(temp.size() - 1, 1);
      }
      break; // temp has interesting content
    }
    return temp;
  }

  static Tile<Dimension>
  parseLine(const std::string line, std::string & timePointID)
  {
    itk::Tile<Dimension> tile;
    std::stringstream    ss(line);
    std::string          temp;

    std::getline(ss, temp, ';');
    tile.FileName = temp;
    std::getline(ss, temp, ';');
    if (timePointID.empty())
    {
      timePointID = temp;
    }
    else
    {
      itkAssertOrThrowMacro(temp == timePointID,
                            "Only a single time point is supported. " << timePointID << " != " << temp);
    }
    std::getline(ss, temp, '(');

    using PointTypelocal = itk::Point<double, Dimension>;
    PointTypelocal p;
    for (unsigned d = 0; d < Dimension; d++)
    {
      std::getline(ss, temp, ',');
      int processed = 0;
      p[d] = stringConverter.StringToDouble(temp.c_str(), temp.length(), &processed);
    }
    tile.Position = p;

    return tile;
  }
};

template <unsigned Dimension>
double_conversion::StringToDoubleConverter TileConfiguration<Dimension>::stringConverter(
  double_conversion::StringToDoubleConverter::ALLOW_TRAILING_JUNK |
    double_conversion::StringToDoubleConverter::ALLOW_LEADING_SPACES |
    double_conversion::StringToDoubleConverter::ALLOW_TRAILING_SPACES,
  0.0,
  std::numeric_limits<double>::quiet_NaN(),
  nullptr,
  nullptr);

template <unsigned Dimension>
double_conversion::DoubleToStringConverter TileConfiguration<
  Dimension>::doubleConverter(double_conversion::DoubleToStringConverter::NO_FLAGS, nullptr, nullptr, 'e', 0, 17, 1, 0);

} // namespace itk

#endif // itkTileConfiguration_h
