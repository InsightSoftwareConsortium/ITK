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

#include "itkAffineTransform.h"
#include "itkImageFileWriter.h"
#include "itkParseTileConfiguration.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkTileMergeImageFilter.h"
#include "itkTileMontage.h"
#include "itkTransformFileWriter.h"
#include "itkTxtTransformIOFactory.h"

template< typename TransformType >
void
WriteTransform( const TransformType* transform, std::string filename )
{
  using AffineType = itk::AffineTransform< double, 3 >;
  using TransformWriterType = itk::TransformFileWriterTemplate< double >;
  TransformWriterType::Pointer tWriter = TransformWriterType::New();
  tWriter->SetFileName( filename );

  if ( TransformType::SpaceDimension >= 2 || TransformType::SpaceDimension <= 3 )
    { // convert into affine which Slicer can read
    AffineType::Pointer aTr = AffineType::New();
    AffineType::TranslationType t;
    t.Fill( 0 );
    for ( unsigned i = 0; i < TransformType::SpaceDimension; i++ )
      {
      t[i] = transform->GetOffset()[i];
      }
    aTr->SetTranslation( t );
    tWriter->SetInput( aTr );
    }
  else
    {
    tWriter->SetInput( transform );
    }
  tWriter->Update();
}


//taken from test/itkMontageTestHelper.hxx and simplified
template< typename PixelType, typename AccumulatePixelType >
void
montage2D( const itk::TileLayout2D& stageTiles,
           const std::string& inputPath,
           const std::string& outputPath,
           const std::string& outFilename )
{
  using ScalarPixelType = typename itk::NumericTraits< PixelType >::ValueType;
  constexpr unsigned Dimension = 2;
  using TransformType = itk::TranslationTransform< double, Dimension >;
  using ScalarImageType = itk::Image< ScalarPixelType, Dimension >;
  using OriginalImageType = itk::Image< PixelType, Dimension >; // possibly RGB instead of scalar
  typename ScalarImageType::SpacingType sp;
  sp.Fill( 1.0 ); // most data assumes unit spacing, even if the files themselves have something else (72 DPI, 96 DPI, 300 DPI etc)
  itk::ObjectFactoryBase::RegisterFactory( itk::TxtTransformIOFactory::New() );
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

      WriteTransform( regTr, outputPath + stageTiles[y][x].FileName + ".tfm" );
      }
    }

  // write generated mosaic
  using Resampler = itk::TileMergeImageFilter< OriginalImageType, AccumulatePixelType >;
  typename Resampler::Pointer resampleF = Resampler::New();
  resampleF->SetMontageSize( { xMontageSize, yMontageSize } );
  resampleF->SetOriginAdjustment( stageStrideXY );
  resampleF->SetForcedSpacing( sp );
  for ( unsigned y = 0; y < yMontageSize; y++ )
    {
    ind[1] = y;
    for ( unsigned x = 0; x < xMontageSize; x++ )
      {
      ind[0] = x;
      resampleF->SetInputTile( ind, inputPath + stageTiles[y][x].FileName );
      resampleF->SetTileTransform( ind, montage->GetOutputTransform( ind ) );
      }
    }

  // resampleF->Update(); //implicitly called by the writer
  using WriterType = itk::ImageFileWriter< OriginalImageType >;
  typename WriterType::Pointer w = WriterType::New();
  w->SetInput( resampleF->GetOutput() );
  // resampleF->DebugOn(); //generate an image of contributing regions
  // MetaImage and HDF5 formats support streaming
  w->SetFileName( outputPath + outFilename );
  // w->UseCompressionOn();
  w->Update();
}


// dispatches to main implementation based on pixel type
template< typename ComponentType, typename AccumulatePixelType >
void
montage2D( const itk::TileLayout2D& stageTiles,
           const std::string& inputPath, const std::string& outputPath,
           const std::string& outFilename, itk::ImageIOBase::IOPixelType pixelType )
{
  switch ( pixelType )
    {
      case itk::ImageIOBase::IOPixelType::SCALAR:
        montage2D< ComponentType, AccumulatePixelType >(
          stageTiles, inputPath, outputPath, outFilename );
        break;
      case itk::ImageIOBase::IOPixelType::RGB:
        montage2D< itk::RGBPixel< ComponentType >, itk::RGBPixel< AccumulatePixelType > >(
          stageTiles, inputPath, outputPath, outFilename );
        break;
      case itk::ImageIOBase::IOPixelType::RGBA:
        montage2D< itk::RGBAPixel< ComponentType >, itk::RGBAPixel< AccumulatePixelType > >(
          stageTiles, inputPath, outputPath, outFilename );
        break;
      default:
        itkGenericExceptionMacro( "Only sclar, RGB and RGBA images are supported!" );
        break;
    }
}

int main( int argc, char *argv[] )
{
  if ( argc < 4 )
    {
    std::cout << "Usage: " << std::endl;
    std::cout << argv[0] << " <directoryWtihInputData> <outputDirectory> <outputFilename>" << std::endl;
    return EXIT_FAILURE;
    }

  constexpr unsigned Dimension = 2;
  std::string inputPath = argv[1];
  if ( inputPath.back() != '/' && inputPath.back() != '\\' )
    {
    inputPath += '/';
    }
  std::string outputPath = argv[2];
  if ( outputPath.back() != '/' && outputPath.back() != '\\' )
    {
    outputPath += '/';
    }

  itk::TileLayout2D stageTiles = itk::ParseTileConfiguration2D( inputPath + "TileConfiguration.txt" );

  try
    {
    // simplest case would be just calling this:
    // montage2D< unsigned short >( stageTiles, actualTiles, inputPath, outputPath, argv[3] );
    // but we examine what is the input type of the first tile and instantiate that

    itk::ImageIOBase::Pointer imageIO = itk::ImageIOFactory::CreateImageIO(
        (inputPath + stageTiles[0][0].FileName).c_str(), itk::ImageIOFactory::ReadMode );
    imageIO->SetFileName( inputPath + stageTiles[0][0].FileName );
    imageIO->ReadImageInformation();

    const unsigned numDimensions = imageIO->GetNumberOfDimensions();
    if ( numDimensions != Dimension )
      {
      itkGenericExceptionMacro( "Only 2D images are supported!" );
      }
    const itk::ImageIOBase::IOPixelType pixelType = imageIO->GetPixelType();
    const itk::ImageIOBase::IOComponentType componentType = imageIO->GetComponentType();

    switch ( componentType )
      {
        case itk::ImageIOBase::IOComponentType::UCHAR:
          montage2D< unsigned char, unsigned int >( stageTiles, inputPath, outputPath, argv[3], pixelType );
          break;
        case itk::ImageIOBase::IOComponentType::USHORT:
          montage2D< unsigned short, double >( stageTiles, inputPath, outputPath, argv[3], pixelType );
          break;
        default: // instantiating too many types leads to long compileation time and big executable
          itkGenericExceptionMacro( "Only unsigned char and unsigned short are supported!" );
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
