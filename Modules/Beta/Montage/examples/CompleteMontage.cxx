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
#include "itkTileConfiguration.h"
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkRGBToLuminanceImageFilter.h"
#include "itkTileMergeImageFilter.h"
#include "itkTileMontage.h"
#include "itkTransformFileWriter.h"
#include "itkTxtTransformIOFactory.h"

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

template <typename TransformType>
void
WriteTransform(const TransformType * transform, std::string filename)
{
  using AffineType = itk::AffineTransform<double, 3>;
  using TransformWriterType = itk::TransformFileWriterTemplate<double>;
  TransformWriterType::Pointer tWriter = TransformWriterType::New();
  tWriter->SetFileName(filename);

  if (TransformType::SpaceDimension >= 2 || TransformType::SpaceDimension <= 3)
  { // convert into affine which Slicer can read
    AffineType::Pointer         aTr = AffineType::New();
    AffineType::TranslationType t;
    t.Fill(0);
    for (unsigned i = 0; i < TransformType::SpaceDimension; i++)
    {
      t[i] = transform->GetOffset()[i];
    }
    aTr->SetTranslation(t);
    tWriter->SetInput(aTr);
  }
  else
  {
    tWriter->SetInput(transform);
  }
  tWriter->Update();
}

// use SFINAE to select whether to do simple assignment or RGB to Luminance conversion
template <typename RGBImage, typename ScalarImage>
typename std::enable_if<std::is_same<RGBImage, ScalarImage>::value, void>::type
assignRGBtoScalar(typename RGBImage::Pointer rgbImage, typename ScalarImage::Pointer & scalarImage)
{
  scalarImage = rgbImage;
}
template <typename RGBImage, typename ScalarImage>
typename std::enable_if<!std::is_same<RGBImage, ScalarImage>::value, void>::type
assignRGBtoScalar(typename RGBImage::Pointer rgbImage, typename ScalarImage::Pointer & scalarImage)
{
  using CastType = itk::RGBToLuminanceImageFilter<RGBImage, ScalarImage>;
  typename CastType::Pointer caster = CastType::New();
  caster->SetInput(rgbImage);
  caster->Update();
  scalarImage = caster->GetOutput();
  scalarImage->DisconnectPipeline();
}

// taken from test/itkMontageTestHelper.hxx and simplified
template <unsigned Dimension, typename PixelType, typename AccumulatePixelType>
void
completeMontage(const itk::TileConfiguration<Dimension> & stageTiles,
                const std::string &                       inputPath,
                const std::string &                       outputPath,
                const std::string &                       outFilename)
{
  using TileConfig = itk::TileConfiguration<Dimension>;
  using ScalarPixelType = typename itk::NumericTraits<PixelType>::ValueType;
  using TransformType = itk::TranslationTransform<double, Dimension>;
  using ScalarImageType = itk::Image<ScalarPixelType, Dimension>;
  using OriginalImageType = itk::Image<PixelType, Dimension>; // possibly RGB instead of scalar
  typename ScalarImageType::SpacingType sp;

  std::vector<typename OriginalImageType::Pointer> oImages(stageTiles.LinearSize());
  std::vector<typename ScalarImageType::Pointer>   sImages(stageTiles.LinearSize());
  std::cout << "Loading images...\n";
  typename TileConfig::TileIndexType ind;
  for (size_t t = 0; t < stageTiles.LinearSize(); t++)
  {
    std::string                         filename = inputPath + stageTiles.Tiles[t].FileName;
    typename OriginalImageType::Pointer image = ReadImage<OriginalImageType>(filename.c_str());
    typename TileConfig::PointType      origin = stageTiles.Tiles[t].Position;

    // tile configurations are in pixel (index) coordinates
    // so we convert them into physical ones
    sp = image->GetSpacing();
    for (unsigned d = 0; d < Dimension; d++)
    {
      origin[d] *= sp[d];
    }
    image->SetOrigin(origin);
    oImages[t] = image;
    assignRGBtoScalar<OriginalImageType, ScalarImageType>(image, sImages[t]);

    // show image loading progress
    ind = stageTiles.LinearIndexToNDIndex(t);
    char digit = '0';
    for (unsigned d = 0; d < Dimension; d++)
    {
      if (ind[d] < stageTiles.AxisSizes[d] - 1)
      {
        break;
      }
      ++digit;
    }
    std::cout << digit << std::flush;
  }
  std::cout << std::endl;

  std::cout << "Doing tile pair registrations and position optimization...";
  using MontageType = itk::TileMontage<ScalarImageType>;
  typename MontageType::Pointer montage = MontageType::New();
  montage->SetMontageSize(stageTiles.AxisSizes);
  for (size_t t = 0; t < stageTiles.LinearSize(); t++)
  {
    montage->SetInputTile(t, sImages[t]);
  }
  montage->Update(); // calculate registration transforms
  std::cout << std::endl;

  // instantiate the resampling class
  using Resampler = itk::TileMergeImageFilter<OriginalImageType, AccumulatePixelType>;
  typename Resampler::Pointer resampleF = Resampler::New();
  resampleF->SetMontageSize(stageTiles.AxisSizes);

  std::cout << "Writing transform for each input tile...";
  for (size_t t = 0; t < stageTiles.LinearSize(); t++)
  {
    const TransformType * regTr = montage->GetOutputTransform(stageTiles.LinearIndexToNDIndex(t));
    WriteTransform(regTr, outputPath + stageTiles.Tiles[t].FileName + ".tfm");

    resampleF->SetInputTile(t, oImages[t]);
    resampleF->SetTileTransform(stageTiles.LinearIndexToNDIndex(t), regTr);
  }
  std::cout << std::endl;

  std::cout << "Resampling the tiles into the final single image...";
  resampleF->Update(); // invoke this explicitly, because writing can take a while do to compression
  std::cout << std::endl;

  std::cout << "Writing the final image...";
  using WriterType = itk::ImageFileWriter<OriginalImageType>;
  typename WriterType::Pointer w = WriterType::New();
  w->SetInput(resampleF->GetOutput());
  // resampleF->DebugOn(); // generate an image of contributing regions
  w->SetFileName(outFilename);
  w->UseCompressionOn();
  w->Update();
  std::cout << "Done!" << std::endl;
}


// dispatches to main implementation based on pixel type
template <unsigned Dimension, typename ComponentType, typename AccumulatePixelType>
void
completeMontage(const itk::TileConfiguration<Dimension> & stageTiles,
                const std::string &                       inputPath,
                const std::string &                       outputPath,
                const std::string &                       outFilename,
                itk::ImageIOBase::IOPixelType             pixelType)
{
  switch (pixelType)
  {
    case itk::ImageIOBase::IOPixelType::SCALAR:
      completeMontage<Dimension, ComponentType, AccumulatePixelType>(stageTiles, inputPath, outputPath, outFilename);
      break;
    case itk::ImageIOBase::IOPixelType::RGB:
      completeMontage<Dimension, itk::RGBPixel<ComponentType>, itk::RGBPixel<AccumulatePixelType>>(
        stageTiles, inputPath, outputPath, outFilename);
      break;
    case itk::ImageIOBase::IOPixelType::RGBA:
      completeMontage<Dimension, itk::RGBAPixel<ComponentType>, itk::RGBAPixel<AccumulatePixelType>>(
        stageTiles, inputPath, outputPath, outFilename);
      break;
    default:
      itkGenericExceptionMacro("Only sclar, RGB and RGBA images are supported!");
      break;
  }
}

template <unsigned Dimension>
int
mainHelper(int argc, char * argv[])
{
  std::string inputPath = argv[1];
  if (inputPath.back() != '/' && inputPath.back() != '\\')
  {
    inputPath += '/';
  }
  std::string outputPath = argv[2];
  if (outputPath.back() != '/' && outputPath.back() != '\\')
  {
    outputPath += '/';
  }
  std::string outFile = argv[3];
  if (itksys::SystemTools::GetFilenamePath(outFile).empty()) // just file name, no path
  {
    outFile = outputPath + outFile;
  }

  itk::TileConfiguration<Dimension> stageTiles;
  stageTiles.Parse(inputPath + "TileConfiguration.txt");

  std::string               firstFilename = inputPath + stageTiles.Tiles[0].FileName;
  itk::ImageIOBase::Pointer imageIO =
    itk::ImageIOFactory::CreateImageIO(firstFilename.c_str(), itk::ImageIOFactory::FileModeType::ReadMode);
  imageIO->SetFileName(firstFilename);
  imageIO->ReadImageInformation();

  const unsigned numDimensions = imageIO->GetNumberOfDimensions();
  if (numDimensions != Dimension)
  {
    itkGenericExceptionMacro("Tile configuration has dimension " << Dimension << ", but image\n"
                                                                 << firstFilename << "\nhas dimension "
                                                                 << numDimensions);
  }

  const itk::ImageIOBase::IOPixelType     pixelType = imageIO->GetPixelType();
  const itk::ImageIOBase::IOComponentType componentType = imageIO->GetComponentType();
  switch (componentType)
  {
    case itk::ImageIOBase::IOComponentType::UCHAR:
      completeMontage<Dimension, unsigned char, unsigned int>(stageTiles, inputPath, outputPath, outFile, pixelType);
      break;
    case itk::ImageIOBase::IOComponentType::USHORT:
      completeMontage<Dimension, unsigned short, double>(stageTiles, inputPath, outputPath, outFile, pixelType);
      break;
    case itk::ImageIOBase::IOComponentType::SHORT:
      completeMontage<Dimension, short, double>(stageTiles, inputPath, outputPath, outFile, pixelType);
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
  if (argc < 4)
  {
    std::cout << "Usage: " << std::endl;
    std::cout << argv[0] << " <directoryWtihInputData> <outputDirectory> <outputFilename>" << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    unsigned dim;
    itk::TileConfiguration<2>::TryParse(argv[1], dim);

    switch (dim)
    {
      case 2:
        return mainHelper<2>(argc, argv);
      case 3:
        return mainHelper<3>(argc, argv);
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
