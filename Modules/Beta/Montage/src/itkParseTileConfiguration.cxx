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

#include "itkParseTileConfiguration.h"

#include <cassert>
#include <fstream>
#include <limits>
#include <sstream>

#include "double-conversion/double-conversion.h"

namespace // annonymous namespace
{
std::string
getNextNonCommentLine( std::istream& in )
{
  std::string temp;
  while ( std::getline( in, temp ) )
    {
    // this is neither an empty line nor a comment
    if ( !temp.empty() && temp[0] != '#' )
      {
      break;
      }
    }

  if ( temp[temp.size() - 1] == '\r' )
  {
    temp.erase(temp.size() - 1, 1);
  }
  return temp;
}

static double_conversion::StringToDoubleConverter stringConverter(
  double_conversion::StringToDoubleConverter::ALLOW_TRAILING_JUNK |
  double_conversion::StringToDoubleConverter::ALLOW_LEADING_SPACES |
  double_conversion::StringToDoubleConverter::ALLOW_TRAILING_SPACES,
  0.0, std::numeric_limits<double>::quiet_NaN(), nullptr, nullptr);

template< unsigned Dimension >
itk::Tile< Dimension >
parseLine( const std::string line, std::string& timePointID )
{
  itk::Tile< Dimension > tile;
  std::stringstream ss( line );
  std::string temp;

  std::getline( ss, temp, ';' );
  tile.FileName = temp;
  std::getline( ss, temp, ';' );
  if (timePointID.empty())
    {
    timePointID = temp;
    }
  else
    {
    itkAssertOrThrowMacro( temp == timePointID,
      "Only a single time point is supported. " << timePointID << " != " << temp );
    }
  std::getline( ss, temp, '(' );

  using PointType = itk::Point< double, Dimension >;
  PointType p;
  for (unsigned d = 0; d < Dimension; d++)
    {
    std::getline( ss, temp, ',' );
    int processed = 0;
    p[d] = stringConverter.StringToDouble( temp.c_str(), temp.length(), &processed );
    }
  tile.Position = p;

  return tile;
}

template< unsigned Dimension >
std::vector< itk::Tile< Dimension > >
parseRow( std::string& line, std::istream& in, std::string& timePointID )
{
  std::vector< itk::Tile< Dimension > > row;

  std::string timePoint;
  itk::Tile< Dimension > tile = parseLine< Dimension >( line, timePoint );
  row.push_back( tile );
  line = getNextNonCommentLine( in );

  while (in)
    {
    tile = parseLine< Dimension >( line, timePointID );
    if ( tile.Position[0] < row.back().Position[0] ) // this is start of a new row
      {
      return row;
      }
    row.push_back( tile );
    line = getNextNonCommentLine( in );
    }

  return row;
}

static double_conversion::DoubleToStringConverter doubleConverter(
  double_conversion::DoubleToStringConverter::NO_FLAGS,
  nullptr, nullptr, 'e', 0, 17, 1, 0 );

} // annonymous namespace


namespace itk
{
TileLayout2D
ParseTileConfiguration2D( const std::string pathToFile )
{
  constexpr unsigned Dimension = 2;

  unsigned xMontageSize = 0;
  TileLayout2D tiles;
  std::string timePointID; // just to make sure all lines specify the same time point

  std::ifstream tileFile( pathToFile );
  std::string temp = getNextNonCommentLine( tileFile );
  if (temp.substr(0, 6) == "dim = ")
    {
    unsigned dim = std::stoul( temp.substr( 6 ) );
    if (dim != Dimension)
      {
      throw std::runtime_error( "Expected dimension 2, but got " + std::to_string( dim ) + " from string:\n\n" + temp );
      }
    temp = getNextNonCommentLine( tileFile ); //get next line
    }

  // read coordinates from files
  while ( tileFile )
    {
    TileRow2D tileRow = parseRow< Dimension >( temp, tileFile, timePointID );
    if (xMontageSize == 0)
      {
      xMontageSize = tileRow.size(); // we get size from the first row
      }
    else // check it is the same size as the first row
      {
      assert( xMontageSize == tileRow.size() );
      }
    tiles.push_back( tileRow );
    }

  return tiles;
}

void
WriteTileConfiguration2D( const std::string pathToFile, const TileLayout2D& tileConfiguration2D )
{
  std::ofstream tileFile( pathToFile );
  if (!tileFile)
    {
    throw std::runtime_error( "Could not open for writing: " + pathToFile );
    }
  else
    {
    tileFile << "# Define the number of dimensions we are working on\ndim = 2\n\n";
    tileFile << "# Define the image coordinates\n";
    char buffer[20];
    double_conversion::StringBuilder conversionResult(buffer, 20);

    for (unsigned y = 0; y < tileConfiguration2D.size(); y++)
      {
      for (unsigned x = 0; x < tileConfiguration2D[y].size(); x++)
        {
        tileFile << tileConfiguration2D[y][x].FileName << ";;(";
        auto pos = tileConfiguration2D[y][x].Position;

        doubleConverter.ToShortest( pos[0], &conversionResult );
        tileFile << conversionResult.Finalize();
        tileFile << ", ";
        conversionResult.Reset();

        doubleConverter.ToShortest( pos[1], &conversionResult );
        tileFile << conversionResult.Finalize();
        tileFile << ")\n";
        conversionResult.Reset();
        }
      }

    if (!tileFile)
      {
      throw std::runtime_error( "Writing not successful to: " + pathToFile );
      }
    }
}

} // namespace itk
