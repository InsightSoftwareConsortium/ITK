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
#include "itkN4BiasFieldCorrectionImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkImageDuplicator.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkBilateralImageFilter.h"
#include "itkComposeImageFilter.h"

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

// use SFINAE to select whether to do simple denoising or per RGB(A) component denoising
template <typename PixelType, unsigned Dimension>
typename std::enable_if<std::is_arithmetic<PixelType>::value, typename itk::Image<PixelType, Dimension>::Pointer>::type
denoiseImage(typename itk::Image<PixelType, Dimension>::Pointer image, double sigma)
{
  using ImageType = itk::Image<PixelType, Dimension>;
  using DensoingType = itk::BilateralImageFilter<ImageType, ImageType>;
  typename DensoingType::Pointer denoiser = DensoingType::New();
  denoiser->SetInput(image);
  denoiser->SetRangeSigma(itk::NumericTraits<PixelType>::max() / 20.0);
  denoiser->SetDomainSigma(sigma);
  denoiser->Update();
  return denoiser->GetOutput();
}
template <typename PixelType, unsigned Dimension>
typename std::enable_if<!std::is_arithmetic<PixelType>::value, typename itk::Image<PixelType, Dimension>::Pointer>::type
denoiseImage(typename itk::Image<PixelType, Dimension>::Pointer image, double sigma)
{
  // we assume RGB or RGBA pixel type
  using ComponentType = typename itk::NumericTraits<PixelType>::ValueType;
  using ColorImageType = itk::Image<PixelType, Dimension>;
  using ScalarImageType = itk::Image<ComponentType, Dimension>;
  using ImageAdaptorType = itk::VectorIndexSelectionCastImageFilter<ColorImageType, ScalarImageType>;
  using DensoingType = itk::BilateralImageFilter<ScalarImageType, ScalarImageType>;
  using ComposeType = itk::ComposeImageFilter<ScalarImageType, ColorImageType>;

  typename ImageAdaptorType::Pointer cAdaptor = ImageAdaptorType::New();
  cAdaptor->SetInput(image);
  typename DensoingType::Pointer denoiser = DensoingType::New();
  denoiser->SetInput(cAdaptor->GetOutput());
  denoiser->SetRangeSigma(itk::NumericTraits<ComponentType>::max() / 20.0);
  denoiser->SetDomainSigma(sigma);

  std::vector<typename ScalarImageType::Pointer> cImages(PixelType::Length);
  for (unsigned c = 0; c < PixelType::Length; c++)
  {
    cAdaptor->SetIndex(c);
    denoiser->Update();
    cImages[c] = denoiser->GetOutput();
    cImages[c]->DisconnectPipeline();
  }

  // compose an RGB(A) image from the filtered component images
  typename ComposeType::Pointer compose = ComposeType::New();
  for (unsigned c = 0; c < PixelType::Length; c++)
  {
    compose->SetInput(c, cImages[c]);
  }
  compose->Update();
  return compose->GetOutput();
}

template <typename TFilter>
class CommandIterationUpdate : public itk::Command
{
public:
  using Self = CommandIterationUpdate;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

protected:
  CommandIterationUpdate() = default;

public:
  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute(static_cast<const itk::Object *>(caller), event);
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    const auto * filter = dynamic_cast<const TFilter *>(object);

    if (typeid(event) != typeid(itk::IterationEvent))
    {
      return;
    }

    std::cout << ' ' << filter->GetElapsedIterations() << std::flush;
  }
};

template <unsigned Dimension>
using N4Filter = typename itk::N4BiasFieldCorrectionImageFilter<itk::Image<float, Dimension>>;
template <unsigned Dimension>
using LogBiasFieldType = typename N4Filter<Dimension>::BiasFieldControlPointLatticeType;
constexpr unsigned splineOrder = 3;

template <typename PixelType, unsigned Dimension>
typename LogBiasFieldType<Dimension>::Pointer
GetLogBiasField(typename itk::Image<PixelType, Dimension>::Pointer scalarImage,
                std::vector<unsigned>                              iterations = { 20, 10, 5 },
                unsigned                                           shrinkFactor = 4)
{
  using ImageType = itk::Image<PixelType, Dimension>;
  using RealImageType = itk::Image<float, Dimension>;

  typename ImageType::RegionType region = scalarImage->GetLargestPossibleRegion();

  // instantiate N4 and assign variables not exposed to the user
  using CorrecterType = itk::N4BiasFieldCorrectionImageFilter<RealImageType>;
  typename CorrecterType::Pointer correcter = CorrecterType::New();
  correcter->SetSplineOrder(splineOrder);
  correcter->SetWienerFilterNoise(0.01f);
  correcter->SetBiasFieldFullWidthAtHalfMaximum(0.15f);
  correcter->SetConvergenceThreshold(0.00001f);

  typename CorrecterType::VariableSizeArrayType maximumNumberOfIterations(
    static_cast<typename CorrecterType::VariableSizeArrayType::SizeValueType>(iterations.size()));
  for (unsigned int d = 0; d < iterations.size(); d++)
  {
    maximumNumberOfIterations[d] = iterations[d];
  }
  correcter->SetMaximumNumberOfIterations(maximumNumberOfIterations);

  typename CorrecterType::ArrayType numberOfFittingLevels;
  numberOfFittingLevels.Fill(iterations.size());
  correcter->SetNumberOfFittingLevels(numberOfFittingLevels);

  typename CorrecterType::ArrayType numberOfControlPoints;
  numberOfControlPoints.Fill(1 + correcter->GetSplineOrder());
  correcter->SetNumberOfControlPoints(numberOfControlPoints);

  using ShrinkerType = itk::ShrinkImageFilter<ImageType, RealImageType>;
  typename ShrinkerType::Pointer shrinker = ShrinkerType::New();
  shrinker->SetInput(scalarImage);
  shrinker->SetShrinkFactors(shrinkFactor); // this might be too aggresive for some dimension
  for (unsigned int d = 0; d < Dimension; d++)
  {
    unsigned shrinkF = shrinkFactor;
    while ((region.GetSize(d) / shrinkF <= 1) && (shrinkF > 1))
    {
      shrinkF--;
    }
    shrinker->SetShrinkFactor(d, shrinkF);
  }
  shrinker->Update();

  correcter->SetInput(shrinker->GetOutput());

  using CommandType = CommandIterationUpdate<CorrecterType>;
  typename CommandType::Pointer observer = CommandType::New();
  correcter->AddObserver(itk::IterationEvent(), observer);

  correcter->Update();

  using DuplicatorType = itk::ImageDuplicator<LogBiasFieldType<Dimension>>;
  typename DuplicatorType::Pointer dup = DuplicatorType::New();
  dup->SetInputImage(correcter->GetLogBiasFieldControlPointLattice());
  dup->Update();

  return dup->GetOutput();
}


template <typename PixelType, unsigned Dimension>
typename itk::Image<PixelType, Dimension>::Pointer
CorrectBias(typename itk::Image<PixelType, Dimension>::Pointer image,
            typename LogBiasFieldType<Dimension>::Pointer      bsplineLattice)
{
  using ImageType = itk::Image<PixelType, Dimension>;
  using RealImageType = itk::Image<float, Dimension>;

  typename ImageType::RegionType region = image->GetLargestPossibleRegion();

  using ScalarImageType = typename N4Filter<Dimension>::ScalarImageType;
  using BSplinerType = itk::BSplineControlPointImageFilter<LogBiasFieldType<Dimension>, ScalarImageType>;
  typename BSplinerType::Pointer bspliner = BSplinerType::New();
  bspliner->SetInput(bsplineLattice);
  bspliner->SetSplineOrder(splineOrder);
  bspliner->SetSize(region.GetSize());
  bspliner->SetOrigin(image->GetOrigin());
  bspliner->SetDirection(image->GetDirection());
  bspliner->SetSpacing(image->GetSpacing());
  bspliner->Update();
  typename ScalarImageType::Pointer logBiasField = bspliner->GetOutput();

  // we need this to get rid of the ScalarImageType which uses VariableVector type
  using CustomExpType = itk::UnaryGeneratorImageFilter<ScalarImageType, RealImageType>;
  typename CustomExpType::Pointer expFilter = CustomExpType::New();

  auto expLambda = [](const typename ScalarImageType::PixelType & logBias) {
    return static_cast<float>(std::exp(logBias[0]));
  };
  expFilter->SetFunctor(expLambda);
  expFilter->SetInput(logBiasField);
  expFilter->Update();
  typename RealImageType::Pointer mulField = expFilter->GetOutput();

  std::mutex sumMutex;
  double     sum = 0.0;
  // normalize the multiplicative bias field to have an average of 1.0
  // so it is neutral to overall image intensity
  itk::MultiThreaderBase::Pointer mt = itk::MultiThreaderBase::New();
  mt->ParallelizeImageRegion<Dimension>(
    region,
    [mulField, &sum, &sumMutex](const typename ImageType::RegionType & subRegion) {
      double threadSum = 0.0;

      itk::ImageRegionConstIterator<RealImageType> ItM(mulField, subRegion);
      for (ItM.GoToBegin(); !ItM.IsAtEnd(); ++ItM)
      {
        threadSum += ItM.Get();
      }

      std::lock_guard<std::mutex> lock(sumMutex);
      sum += threadSum;
    },
    nullptr);

  float diff = 1.0 - sum / region.GetNumberOfPixels();

  mt->ParallelizeImageRegion<Dimension>(
    region,
    [mulField, diff](const typename ImageType::RegionType & subRegion) {
      itk::ImageRegionIterator<RealImageType> ItM(mulField, subRegion);
      for (ItM.GoToBegin(); !ItM.IsAtEnd(); ++ItM)
      {
        ItM.Set(ItM.Get() + diff);
      }
    },
    nullptr);

  using CustomBinaryFilter = itk::BinaryGeneratorImageFilter<ImageType, RealImageType, ImageType>;
  typename CustomBinaryFilter::Pointer expAndDivFilter = CustomBinaryFilter::New();

  auto expAndDivLambda = [](PixelType input, float biasField) {
    if constexpr (std::is_arithmetic_v<PixelType>)
    {
      return input / biasField;
    }
    else
    {
      PixelType result;
      // we assume RGB or RGBA pixel type
      unsigned count = input.GetNumberOfComponents();
      for (unsigned c = 0; c < count; c++)
      {
        result.SetNthComponent(c, input.GetNthComponent(c) / biasField);
      }
      return result;
    }
  }; // end expAndDivLambda

  expAndDivFilter->SetFunctor(expAndDivLambda);
  expAndDivFilter->SetInput1(image);
  expAndDivFilter->SetInput2(mulField);
  expAndDivFilter->Update();

  return expAndDivFilter->GetOutput();
}


template <typename RGBImage, typename ScalarImage>
void
assignRGBtoScalar(typename RGBImage::Pointer rgbImage, typename ScalarImage::Pointer & scalarImage)
{
  if constexpr (std::is_same_v<RGBImage, ScalarImage>)
  {
    scalarImage = rgbImage;
  }
  else
  {
    using CastType = itk::RGBToLuminanceImageFilter<RGBImage, ScalarImage>;
    typename CastType::Pointer caster = CastType::New();
    caster->SetInput(rgbImage);
    caster->Update();
    scalarImage = caster->GetOutput();
    scalarImage->DisconnectPipeline();
  }
}


// taken from test/itkMontageTestHelper.hxx and simplified
template <unsigned Dimension, typename PixelType, typename AccumulatePixelType>
void
completeMontage(const itk::TileConfiguration<Dimension> & stageTiles,
                const std::string &                       inputPath,
                const std::string &                       outputPath,
                const std::string &                       outFilename,
                bool                                      correctBias,
                bool                                      denoiseTiles)
{
  using TileConfig = itk::TileConfiguration<Dimension>;
  using ScalarPixelType = typename itk::NumericTraits<PixelType>::ValueType;
  using TransformType = itk::TranslationTransform<itk::SpacePrecisionType, Dimension>;
  using ScalarImageType = itk::Image<ScalarPixelType, Dimension>;
  using OriginalImageType = itk::Image<PixelType, Dimension>; // possibly RGB instead of scalar
  using BiasFieldType = LogBiasFieldType<Dimension>;
  typename ScalarImageType::SpacingType sp;
  sp.Fill(1.0);

  TileConfig actualTiles = stageTiles; // we will update it later

  std::vector<typename OriginalImageType::Pointer> oImages(stageTiles.LinearSize());
  std::vector<typename ScalarImageType::Pointer>   sImages(stageTiles.LinearSize());
  std::vector<typename BiasFieldType::Pointer>     bImages(stageTiles.LinearSize());
  std::cout << "Reading";
  if (denoiseTiles)
  {
    std::cout << ", denoising";
  }
  if (correctBias)
  {
    std::cout << ", bias-correcting";
  }
  std::cout << " input tiles...\n";
  typename TileConfig::TileIndexType ind;
  for (size_t t = 0; t < stageTiles.LinearSize(); t++)
  {
    std::string                         filename = inputPath + stageTiles.Tiles[t].FileName;
    typename OriginalImageType::Pointer image = itk::ReadImage<OriginalImageType>(filename.c_str());
    typename TileConfig::PointType      origin = stageTiles.Tiles[t].Position;
    std::cout << 'R' << std::flush;

    // tile configurations are in pixel (index) coordinates
    // so we convert them into physical ones
    sp = image->GetSpacing();
    for (unsigned d = 0; d < Dimension; d++)
    {
      origin[d] *= sp[d];
    }
    image->SetOrigin(origin);

    if (denoiseTiles)
    {
      // geometric average perserves equivalent voxel volume
      double avgSpacing = 1.0;
      for (unsigned d = 0; d < Dimension; d++)
      {
        avgSpacing *= sp[d];
      }
      avgSpacing = std::pow(avgSpacing, 1.0 / Dimension);

      image = denoiseImage<PixelType, Dimension>(image, avgSpacing);
      itk::WriteImage(image.GetPointer(), (outputPath + stageTiles.Tiles[t].FileName + "-bil.nrrd").c_str(), true);
      std::cout << 'D' << std::flush;
    }

    assignRGBtoScalar<OriginalImageType, ScalarImageType>(image, sImages[t]);

    if (correctBias)
    {
      bImages[t] = GetLogBiasField<ScalarPixelType, Dimension>(sImages[t]); // input image must be scalar
      image = CorrectBias<PixelType, Dimension>(image, bImages[t]);

      assignRGBtoScalar<OriginalImageType, ScalarImageType>(image, sImages[t]);

      // write bias-corrected image
      std::string fileNameExt = itksys::SystemTools::GetFilenameLastExtension(stageTiles.Tiles[t].FileName);
      std::string baseFileName = itksys::SystemTools::GetFilenameWithoutLastExtension(stageTiles.Tiles[t].FileName);
      std::string flatFileName = baseFileName + "-flat" + fileNameExt;
      actualTiles.Tiles[t].FileName = flatFileName;
      itk::WriteImage(image.GetPointer(), (outputPath + flatFileName).c_str(), true);
    }
    oImages[t] = image;

    // show image loading progress
    ind = stageTiles.LinearIndexToNDIndex(t);
    std::cout << " " << ind << "  " << t + 1 << "/" << stageTiles.LinearSize() << std::endl;
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
    ind = stageTiles.LinearIndexToNDIndex(t);
    const TransformType * regTr = montage->GetOutputTransform(ind);
    WriteTransform(regTr, outputPath + stageTiles.Tiles[t].FileName + ".tfm");

    // set inputs to resampler class
    resampleF->SetInputTile(t, oImages[t]);
    resampleF->SetTileTransform(ind, regTr);

    // calculate updated positions - transform physical into index shift
    const itk::Vector<double, Dimension> regPos = regTr->GetOffset();
    for (unsigned d = 0; d < Dimension; d++)
    {
      actualTiles.Tiles[t].Position[d] = stageTiles.Tiles[t].Position[d] - regPos[d] / sp[d];
    }
  }
  std::cout << std::endl;

  std::cout << "Writing registered tile configuration file...";
  actualTiles.Write(outputPath + "TileConfiguration.registered.txt");
  std::cout << std::endl;

  std::cout << "Resampling the tiles into the final single image...";
  // resampleF->DebugOn(); // generate an image of contributing regions
  resampleF->Update(); // invoke this explicitly, because writing itself can take a while due to compression
  std::cout << std::endl;

  std::cout << "Writing the final image...";
  itk::WriteImage(resampleF->GetOutput(), outFilename.c_str(), true);
  std::cout << "Done!" << std::endl;
}


// dispatches to main implementation based on pixel type
template <unsigned Dimension, typename ComponentType, typename AccumulatePixelType>
void
completeMontage(const itk::TileConfiguration<Dimension> & stageTiles,
                const std::string &                       inputPath,
                const std::string &                       outputPath,
                const std::string &                       outFilename,
                itk::IOPixelEnum                          pixelType,
                bool                                      correctBias,
                bool                                      denoise)
{
  switch (pixelType)
  {
    case itk::IOPixelEnum::SCALAR:
      completeMontage<Dimension, ComponentType, AccumulatePixelType>(
        stageTiles, inputPath, outputPath, outFilename, correctBias, denoise);
      break;
    case itk::IOPixelEnum::RGB:
      completeMontage<Dimension, itk::RGBPixel<ComponentType>, itk::RGBPixel<AccumulatePixelType>>(
        stageTiles, inputPath, outputPath, outFilename, correctBias, denoise);
      break;
    case itk::IOPixelEnum::RGBA:
      completeMontage<Dimension, itk::RGBAPixel<ComponentType>, itk::RGBAPixel<AccumulatePixelType>>(
        stageTiles, inputPath, outputPath, outFilename, correctBias, denoise);
      break;
    default:
      itkGenericExceptionMacro("Only sclar, RGB and RGBA images are supported!");
      break;
  }
}

template <unsigned Dimension>
int
mainHelper(int argc, char * argv[], std::string inputPath)
{
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

  bool doBiasCorrection = true;
  if (argc > 4)
  {
    doBiasCorrection = std::stoi(argv[4]);
  }
  bool doDenoising = true;
  if (argc > 5)
  {
    doDenoising = std::stoi(argv[5]);
  }

  itk::TileConfiguration<Dimension> stageTiles;
  stageTiles.Parse(inputPath + "TileConfiguration.txt");

  std::string               firstFilename = inputPath + stageTiles.Tiles[0].FileName;
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
      completeMontage<Dimension, unsigned char, unsigned int>(
        stageTiles, inputPath, outputPath, outFile, pixelType, doBiasCorrection, doDenoising);
      break;
    case itk::IOComponentEnum::USHORT:
      completeMontage<Dimension, unsigned short, double>(
        stageTiles, inputPath, outputPath, outFile, pixelType, doBiasCorrection, doDenoising);
      break;
    case itk::IOComponentEnum::SHORT:
      completeMontage<Dimension, short, double>(
        stageTiles, inputPath, outputPath, outFile, pixelType, doBiasCorrection, doDenoising);
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
    std::cout << argv[0]
              << " <directoryWithInputData> <outputDirectory> <outputFilename> [biasCorrection] [tileDenoising]"
              << std::endl;
    return EXIT_FAILURE;
  }

  std::string inputPath = argv[1];
  if (inputPath.back() != '/' && inputPath.back() != '\\')
  {
    inputPath += '/';
  }

  try
  {
    unsigned dim;
    itk::TileConfiguration<2>::TryParse(inputPath + "TileConfiguration.txt", dim);

    switch (dim)
    {
      case 2:
        return mainHelper<2>(argc, argv, inputPath);
      case 3:
        return mainHelper<3>(argc, argv, inputPath);
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
