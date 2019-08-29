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
#include "itkTileMergeImageFilter.h"

#include "itksys/SystemTools.hxx"


//taken from test/itkMontageTestHelper.hxx and simplified
template< typename PixelType, typename AccumulatePixelType >
void
montage2D( const itk::TileLayout2D& actualTiles, const std::string& inputPath,
           const std::string& outFilename, unsigned streamSubdivisions )
{
  constexpr unsigned Dimension = 2;
  using TransformType = itk::TranslationTransform< double, Dimension >;
  using OriginalImageType = itk::Image< PixelType, Dimension >; // possibly RGB instead of scalar
  using ReaderType = itk::ImageFileReader< OriginalImageType >;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( inputPath + actualTiles[0][0].FileName );
  reader->UpdateOutputInformation();
  typename OriginalImageType::SpacingType sp = reader->GetOutput()->GetSpacing();
  const unsigned yMontageSize = actualTiles.size();
  const unsigned xMontageSize = actualTiles[0].size();

  // write generated mosaic
  using Resampler = itk::TileMergeImageFilter< OriginalImageType, AccumulatePixelType >;
  typename Resampler::Pointer resampleF = Resampler::New();
  resampleF->SetMontageSize( { xMontageSize, yMontageSize } );
  typename Resampler::TileIndexType ind;
  for ( unsigned y = 0; y < yMontageSize; y++ )
    {
    ind[1] = y;
    for ( unsigned x = 0; x < xMontageSize; x++ )
      {
      ind[0] = x;
      resampleF->SetInputTile( ind, inputPath + actualTiles[y][x].FileName );
      TransformType::ParametersType params( Dimension );
      typename TransformType::Pointer regTr = TransformType::New();
      params[0] = -actualTiles[y][x].Position[0] * sp[0];
      params[1] = -actualTiles[y][x].Position[1] * sp[1];
      regTr->SetParametersByValue( params );
      resampleF->SetTileTransform( ind, regTr );
      }
    }

  // resampleF->Update(); //implicitly called by the writer
  using WriterType = itk::ImageFileWriter< OriginalImageType >;
  typename WriterType::Pointer w = WriterType::New();
  w->SetInput( resampleF->GetOutput() );
  // resampleF->DebugOn(); //generate an image of contributing regions
  // MetaImage and HDF5 formats support streaming
  w->SetFileName( outFilename );
  // w->UseCompressionOn();
  // streamSubdivisions of 1 disables streaming (higher memory useage)
  w->SetNumberOfStreamDivisions( streamSubdivisions );
  w->Update();
}


// dispatches to main implementation based on pixel type
template< typename ComponentType, typename AccumulatePixelType >
void
montage2D( const itk::TileLayout2D& actualTiles, const std::string& inputPath,
           const std::string& outFilename, unsigned streamSubdivisions, itk::ImageIOBase::IOPixelType pixelType )
{
  switch ( pixelType )
    {
      case itk::ImageIOBase::IOPixelType::SCALAR:
        montage2D< ComponentType, AccumulatePixelType >(
          actualTiles, inputPath, outFilename, streamSubdivisions );
        break;
      case itk::ImageIOBase::IOPixelType::RGB:
        montage2D< itk::RGBPixel< ComponentType >, itk::RGBPixel< AccumulatePixelType > >(
          actualTiles, inputPath, outFilename, streamSubdivisions );
        break;
      case itk::ImageIOBase::IOPixelType::RGBA:
        montage2D< itk::RGBAPixel< ComponentType >, itk::RGBAPixel< AccumulatePixelType > >(
          actualTiles, inputPath, outFilename, streamSubdivisions );
        break;
      default:
        itkGenericExceptionMacro( "Only sclar, RGB and RGBA images are supported!" );
        break;
    }
}

int main( int argc, char *argv[] )
{
  if ( argc < 3 )
    {
    std::cout << "Usage: " << std::endl;
    std::cout << argv[0] << " <tileConfiguration> <outputFilename>" << std::endl;
    return EXIT_FAILURE;
    }

  std::string inputPath = itksys::SystemTools::GetFilenamePath( argv[1] );
  if ( !inputPath.empty() ) // a path was given in addition to file name
    {
    inputPath += '/';
    }
  itk::TileLayout2D actualTiles = itk::ParseTileConfiguration2D( argv[1] );

  try
    {
    itk::ImageIOBase::Pointer imageIO = itk::ImageIOFactory::CreateImageIO(
      ( inputPath + actualTiles[0][0].FileName ).c_str(), itk::ImageIOFactory::FileModeType::ReadMode );
    imageIO->SetFileName( inputPath + actualTiles[0][0].FileName );
    imageIO->ReadImageInformation();

    const unsigned numDimensions = imageIO->GetNumberOfDimensions();
    if ( numDimensions != 2 )
      {
      itkGenericExceptionMacro( "Only 2D images are supported!" );
      }
    const itk::ImageIOBase::IOPixelType pixelType = imageIO->GetPixelType();
    const itk::ImageIOBase::IOComponentType componentType = imageIO->GetComponentType();

    switch ( componentType )
      {
        case itk::ImageIOBase::IOComponentType::UCHAR:
          montage2D< unsigned char, unsigned int >( actualTiles, inputPath, argv[2], 1, pixelType );
          break;
        case itk::ImageIOBase::IOComponentType::USHORT:
          montage2D< unsigned short, double >( actualTiles, inputPath, argv[2], 1, pixelType );
          break;
        default: // instantiating too many types leads to long compileation time and big executable
          itkGenericExceptionMacro( "Only unsigned char and unsigned short are supported!" );
          break;
      }
    }
  catch ( itk::ExceptionObject& e )
    {
    std::cout << "Error during montaging:" << std::endl;
    std::cout << e << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
