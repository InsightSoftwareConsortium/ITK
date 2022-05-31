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

#include "itkImageFileWriter.h"
#include "itkTileConfiguration.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkTileMergeImageFilter.h"

#include "itksys/SystemTools.hxx"

template <typename TImage>
typename TImage::Pointer
ReadImage(const char * filename)
{
  using ReaderType = itk::ImageFileReader<TImage>;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(filename);
  reader->Update();
  return reader->GetOutput();
}

// taken from test/itkMontageTestHelper.hxx and simplified
template <unsigned Dimension, typename PixelType, typename AccumulatePixelType>
void
resampleMontage(const itk::TileConfiguration<Dimension> & actualTiles,
                const std::string &                       inputPath,
                const std::string &                       outFilename)
{
  using TileConfig = itk::TileConfiguration<Dimension>;
  using TransformType = itk::TranslationTransform<double, Dimension>;
  using OriginalImageType = itk::Image<PixelType, Dimension>; // possibly RGB instead of scalar
  typename OriginalImageType::SpacingType sp;
  typename TransformType::ConstPointer    identity = TransformType::New();

  // instantiate and invoke the resampling class
  using Resampler = itk::TileMergeImageFilter<OriginalImageType, AccumulatePixelType>;
  typename Resampler::Pointer resampleF = Resampler::New();
  resampleF->SetMontageSize(actualTiles.AxisSizes);
  for (size_t t = 0; t < actualTiles.LinearSize(); t++)
  {
    std::string                         filename = inputPath + actualTiles.Tiles[t].FileName;
    typename OriginalImageType::Pointer image = ReadImage<OriginalImageType>(filename.c_str());
    typename TileConfig::PointType      origin = actualTiles.Tiles[t].Position;

    // tile configurations are in pixel (index) coordinates
    // so we convert them into physical ones
    sp = image->GetSpacing();
    for (unsigned d = 0; d < Dimension; d++)
    {
      origin[d] *= sp[d];
    }
    image->SetOrigin(origin);

    resampleF->SetInputTile(t, image);
    resampleF->SetTileTransform(actualTiles.LinearIndexToNDIndex(t), identity);
  }

  // resampleF->Update(); //implicitly called by the writer
  using WriterType = itk::ImageFileWriter<OriginalImageType>;
  typename WriterType::Pointer w = WriterType::New();
  w->SetInput(resampleF->GetOutput());
  // resampleF->DebugOn(); // generate an image of contributing regions
  w->SetFileName(outFilename);
  w->UseCompressionOn();
  w->Update();
}


// dispatches to main implementation based on pixel type
template <unsigned Dimension, typename ComponentType, typename AccumulatePixelType>
void
resampleMontage(const itk::TileConfiguration<Dimension> & actualTiles,
                const std::string &                       inputPath,
                const std::string &                       outFilename,
                itk::IOPixelEnum                          pixelType)
{
  switch (pixelType)
  {
    case itk::IOPixelEnum ::SCALAR:
      resampleMontage<Dimension, ComponentType, AccumulatePixelType>(actualTiles, inputPath, outFilename);
      break;
    case itk::IOPixelEnum ::RGB:
      resampleMontage<Dimension, itk::RGBPixel<ComponentType>, itk::RGBPixel<AccumulatePixelType>>(
        actualTiles, inputPath, outFilename);
      break;
    case itk::IOPixelEnum ::RGBA:
      resampleMontage<Dimension, itk::RGBAPixel<ComponentType>, itk::RGBAPixel<AccumulatePixelType>>(
        actualTiles, inputPath, outFilename);
      break;
    default:
      itkGenericExceptionMacro("Only sclar, RGB and RGBA images are supported! Actual pixel type: "
                               << itk::ImageIOBase::GetPixelTypeAsString(pixelType));
      break;
  }
}

template <unsigned Dimension>
int
mainHelper(char * argv[])
{
  std::string inputPath = itksys::SystemTools::GetFilenamePath(argv[1]);
  if (!inputPath.empty()) // a path was given in addition to file name
  {
    inputPath += '/';
  }

  itk::TileConfiguration<Dimension> actualTiles;
  actualTiles.Parse(argv[1]);

  std::string               firstFilename = inputPath + actualTiles.Tiles[0].FileName;
  itk::ImageIOBase::Pointer imageIO =
    itk::ImageIOFactory::CreateImageIO(firstFilename.c_str(), itk::IOFileModeEnum::ReadMode);
  imageIO->SetFileName(firstFilename);
  imageIO->ReadImageInformation();

  const unsigned numDimensions = imageIO->GetNumberOfDimensions();
  if (numDimensions != Dimension)
  {
    itkGenericExceptionMacro("Tile configuration has dimension " << Dimension << ", but image\n"
                                                                 << firstFilename << "\nhas dimension "
                                                                 << numDimensions);
  }

  const itk::IOPixelEnum     pixelType = imageIO->GetPixelType();
  const itk::IOComponentEnum componentType = imageIO->GetComponentType();
  switch (componentType)
  {
    case itk::IOComponentEnum::UCHAR:
      resampleMontage<Dimension, unsigned char, unsigned int>(actualTiles, inputPath, argv[2], pixelType);
      break;
    case itk::IOComponentEnum::USHORT:
      resampleMontage<Dimension, unsigned short, double>(actualTiles, inputPath, argv[2], pixelType);
      break;
    case itk::IOComponentEnum::SHORT:
      resampleMontage<Dimension, short, double>(actualTiles, inputPath, argv[2], pixelType);
      break;
    default: // instantiating too many types leads to long compilation time and big executable
      itkGenericExceptionMacro(
        "Only unsigned char, unsigned short and short are supported as pixel component types! Trying to montage "
        << itk::ImageIOBase::GetComponentTypeAsString(componentType));
      break;
  }

  return EXIT_SUCCESS;
}

int
main(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cout << "Usage: " << std::endl;
    std::cout << argv[0] << " <tileConfiguration> <outputFilename>" << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    unsigned dim;
    itk::TileConfiguration<2>::TryParse(argv[1], dim);

    switch (dim)
    {
      case 2:
        return mainHelper<2>(argv);
      case 3:
        return mainHelper<3>(argv);
      default:
        std::cerr << "Only dimensions 2 and 3 are supported. You are attempting to resample dimension " << dim;
        return EXIT_FAILURE;
    }
  }
  catch (itk::ExceptionObject & exc)
  {
    std::cerr << exc;
  }
  catch (std::runtime_error & exc)
  {
    std::cerr << exc.what();
  }
  catch (...)
  {
    std::cerr << "Unknown error has occurred" << std::endl;
  }
  return EXIT_FAILURE;
}
