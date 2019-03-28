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

#include "itkImageFileWriter.h"
#include "itkParseTileConfiguration.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkTileMontage.h"

#include "itksys/SystemTools.hxx"


//taken from test/itkMontageTestHelper.hxx and simplified
template< typename PixelType, typename AccumulatePixelType >
void
montage2D( const itk::TileLayout2D& stageTiles, itk::TileLayout2D& actualTiles,
           const std::string& inputPath )
{
  using ScalarPixelType = typename itk::NumericTraits< PixelType >::ValueType;
  constexpr unsigned Dimension = 2;
  using TransformType = itk::TranslationTransform< double, Dimension >;
  using ScalarImageType = itk::Image< ScalarPixelType, Dimension >;
  typename ScalarImageType::SpacingType sp;
  sp.Fill( 1.0 ); // most data assumes unit spacing, even if the files themselves have something else (72 DPI, 96 DPI, 300 DPI etc)
  const size_t yMontageSize = stageTiles.size();
  const size_t xMontageSize = stageTiles[0].size();
  const unsigned origin1x = xMontageSize > 1 ? 1 : 0; // support 1xN montages
  const unsigned origin1y = yMontageSize > 1 ? 1 : 0; // support Nx1 montages
  const itk::Point< double, Dimension > stageStrideXY = stageTiles[origin1y][origin1x].Position;

  using MontageType = itk::TileMontage< ScalarImageType >;
  typename MontageType::Pointer montage = MontageType::New();
  montage->SetMontageSize( { xMontageSize, yMontageSize } );
  montage->SetOriginAdjustment( stageStrideXY );
  montage->SetForcedSpacing( sp );

  typename MontageType::TileIndexType ind;
  for ( unsigned y = 0; y < yMontageSize; y++ )
    {
    ind[1] = y;
    for ( unsigned x = 0; x < xMontageSize; x++ )
      {
      ind[0] = x;
      montage->SetInputTile( ind, inputPath + stageTiles[y][x].FileName );
      }
    }

  montage->Update(); // calculate registration transforms

  for ( unsigned y = 0; y < yMontageSize; y++ )
    {
    ind[1] = y;
    for ( unsigned x = 0; x < xMontageSize; x++ )
      {
      ind[0] = x;
      const TransformType* regTr = montage->GetOutputTransform( ind );
      itk::Point< double, Dimension > stagePos;
      // we need to compensate for origin adjustment
      stagePos[0] = x * stageStrideXY[0];
      stagePos[1] = y * stageStrideXY[1];
      actualTiles[y][x].Position[0] = stagePos[0] - regTr->GetParameters()[0];
      actualTiles[y][x].Position[1] = stagePos[1] - regTr->GetParameters()[1];
      }
    }

}


int main( int argc, char *argv[] )
{
  if ( argc < 3 )
    {
    std::cout << "Usage: " << std::endl;
    std::cout << argv[0] << " <inputTileConfiguration> <outputTileConfiguration>" << std::endl;
    return EXIT_FAILURE;
    }

  constexpr unsigned Dimension = 2;

  std::string inputPath = itksys::SystemTools::GetFilenamePath( argv[1] );
  if ( !inputPath.empty() ) // a path was given in addition to file name
    {
    inputPath += '/';
    }

  itk::TileLayout2D stageTiles = itk::ParseTileConfiguration2D( argv[1] );
  itk::TileLayout2D actualTiles = stageTiles; // result - only positions need to be updated

  try
    {
    itk::ImageIOBase::Pointer imageIO = itk::ImageIOFactory::CreateImageIO(
        (inputPath + stageTiles[0][0].FileName).c_str(), itk::ImageIOFactory::ReadMode );
    imageIO->SetFileName( inputPath + stageTiles[0][0].FileName );
    imageIO->ReadImageInformation();

    const unsigned numDimensions = imageIO->GetNumberOfDimensions();
    if ( numDimensions != Dimension )
      {
      itkGenericExceptionMacro( "Only 2D images are supported!" );
      }
    //const itk::ImageIOBase::IOPixelType pixelType = imageIO->GetPixelType(); // always instantiate scalar for registrations
    const itk::ImageIOBase::IOComponentType componentType = imageIO->GetComponentType();

    switch ( componentType )
      {
        case itk::ImageIOBase::IOComponentType::UCHAR:
          montage2D< unsigned char, unsigned int >( stageTiles, actualTiles, inputPath);
          break;
        case itk::ImageIOBase::IOComponentType::USHORT:
          montage2D< unsigned short, double >( stageTiles, actualTiles, inputPath);
          break;
        default: // instantiating too many types leads to long compileation time and big executable
          itkGenericExceptionMacro( "Only unsigned char and unsigned short are supported!" );
      }

    itk::WriteTileConfiguration2D( argv[2], actualTiles );
    }
  catch ( itk::ExceptionObject& e )
    {
    std::cout << "Error during montaging:" << std::endl;
    std::cout << e << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
