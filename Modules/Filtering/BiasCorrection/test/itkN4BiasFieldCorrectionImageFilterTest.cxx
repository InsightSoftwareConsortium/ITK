/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkConstantPadImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkN4BiasFieldCorrectionImageFilter.h"
#include "itkOtsuThresholdImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkTestingMacros.h"

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
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    const auto * filter = dynamic_cast<const TFilter *>(object);

    if (typeid(event) != typeid(itk::IterationEvent))
    {
      return;
    }
    if (filter->GetElapsedIterations() == 1)
    {
      std::cout << "Current level = " << filter->GetCurrentLevel() + 1 << std::endl;
    }
    std::cout << "  Iteration " << filter->GetElapsedIterations() << " (of "
              << filter->GetMaximumNumberOfIterations()[filter->GetCurrentLevel()] << ").  ";
    std::cout << " Current convergence value = " << filter->GetCurrentConvergenceMeasurement()
              << " (threshold = " << filter->GetConvergenceThreshold() << ")" << std::endl;
  }
};

template <typename TImage>
void
WriteImage(const TImage * out, const char * filename)
{
  using WriterType = itk::ImageFileWriter<TImage>;
  typename WriterType::Pointer w = WriterType::New();
  w->SetInput(out);
  w->SetFileName(filename);
  try
  {
    w->Update();
  }
  catch (const itk::ExceptionObject & error)
  {
    std::cerr << error << std::endl;
  }
}

template <typename TValue>
TValue
Convert(std::string optionString)
{
  TValue             value;
  std::istringstream iss(optionString);

  iss >> value;
  return value;
}

template <typename TValue>
std::vector<TValue>
ConvertVector(std::string optionString)
{
  std::vector<TValue>    values;
  std::string::size_type crosspos = optionString.find('x', 0);

  if (crosspos == std::string::npos)
  {
    values.push_back(Convert<TValue>(optionString));
  }
  else
  {
    std::string        element = optionString.substr(0, crosspos);
    TValue             value;
    std::istringstream iss(element);
    iss >> value;
    values.push_back(value);
    while (crosspos != std::string::npos)
    {
      std::string::size_type crossposfrom = crosspos;
      crosspos = optionString.find('x', crossposfrom + 1);
      if (crosspos == std::string::npos)
      {
        element = optionString.substr(crossposfrom + 1, optionString.length());
      }
      else
      {
        element = optionString.substr(crossposfrom + 1, crosspos);
      }
      std::istringstream iss2(element);
      iss2 >> value;
      values.push_back(value);
    }
  }
  return values;
}

template <unsigned int ImageDimension>
int
N4(int argc, char * argv[])
{
  using RealType = float;

  using ImageType = itk::Image<RealType, ImageDimension>;
  using ImagePointer = typename ImageType::Pointer;

  using ReaderType = itk::ImageFileReader<ImageType>;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[2]);
  reader->Update();

  ImagePointer inputImage = reader->GetOutput();
  inputImage->DisconnectPipeline();

  // handle the mask image
  using MaskImageType = itk::Image<unsigned char, ImageDimension>;
  typename MaskImageType::Pointer maskImage = nullptr;

  if (argc > 6)
  {
    using MaskReaderType = itk::ImageFileReader<MaskImageType>;
    typename MaskReaderType::Pointer maskreader = MaskReaderType::New();
    maskreader->SetFileName(argv[6]);
    try
    {
      maskreader->Update();
      maskImage = maskreader->GetOutput();
      maskImage->DisconnectPipeline();
    }
    catch (...)
    {
      maskImage = nullptr;
    }
  }

  if (!maskImage)
  {
    std::cout << "Mask not read.  Creating Otsu mask." << std::endl;
    using ThresholderType = itk::OtsuThresholdImageFilter<ImageType, MaskImageType>;
    typename ThresholderType::Pointer otsu = ThresholderType::New();
    otsu->SetInput(inputImage);
    // otsu->SetNumberOfHistogramBins( 200 );
    otsu->SetInsideValue(0);
    otsu->SetOutsideValue(1);

    otsu->Update();
    maskImage = otsu->GetOutput();
    maskImage->DisconnectPipeline();
  }

  // instantiate N4 and assign variables not exposed to the user in this test.
  using CorrecterType = itk::N4BiasFieldCorrectionImageFilter<ImageType, MaskImageType, ImageType>;
  typename CorrecterType::Pointer correcter = CorrecterType::New();
  correcter->SetSplineOrder(3);
  correcter->SetWienerFilterNoise(0.01);
  correcter->SetBiasFieldFullWidthAtHalfMaximum(0.15);
  correcter->SetConvergenceThreshold(0.0000001);

  // handle the number of iterations
  std::vector<unsigned int> numIters = ConvertVector<unsigned int>(std::string("100x50x50"));
  if (argc > 5)
  {
    numIters = ConvertVector<unsigned int>(argv[5]);
  }
  typename CorrecterType::VariableSizeArrayType maximumNumberOfIterations(
    static_cast<typename CorrecterType::VariableSizeArrayType::SizeValueType>(numIters.size()));
  for (unsigned int d = 0; d < numIters.size(); d++)
  {
    maximumNumberOfIterations[d] = numIters[d];
  }
  correcter->SetMaximumNumberOfIterations(maximumNumberOfIterations);

  typename CorrecterType::ArrayType numberOfFittingLevels;
  numberOfFittingLevels.Fill(
    static_cast<typename CorrecterType::VariableSizeArrayType::SizeValueType>(numIters.size()));
  correcter->SetNumberOfFittingLevels(numberOfFittingLevels);

  /* B-spline options -- we place this here to take care of the case where
   * the user wants to specify things in terms of the spline distance.
   *  1. need to pad the images to get as close to possible to the
   *     requested domain size.
   */
  typename ImageType::PointType newOrigin = inputImage->GetOrigin();

  typename CorrecterType::ArrayType numberOfControlPoints;

  float splineDistance = 200;
  if (argc > 7)
  {
    splineDistance = std::stod(argv[7]);
  }

  itk::SizeValueType lowerBound[ImageDimension];
  itk::SizeValueType upperBound[ImageDimension];

  for (unsigned int d = 0; d < ImageDimension; d++)
  {
    float domain =
      static_cast<RealType>(inputImage->GetLargestPossibleRegion().GetSize()[d] - 1) * inputImage->GetSpacing()[d];
    auto numberOfSpans = static_cast<unsigned int>(std::ceil(domain / splineDistance));
    auto extraPadding =
      static_cast<unsigned long>((numberOfSpans * splineDistance - domain) / inputImage->GetSpacing()[d] + 0.5);
    lowerBound[d] = static_cast<unsigned long>(0.5 * extraPadding);
    upperBound[d] = extraPadding - lowerBound[d];
    newOrigin[d] -= (static_cast<RealType>(lowerBound[d]) * inputImage->GetSpacing()[d]);
    numberOfControlPoints[d] = numberOfSpans + correcter->GetSplineOrder();
  }

  using PadderType = itk::ConstantPadImageFilter<ImageType, ImageType>;
  typename PadderType::Pointer padder = PadderType::New();
  padder->SetInput(inputImage);
  padder->SetPadLowerBound(lowerBound);
  padder->SetPadUpperBound(upperBound);
  padder->SetConstant(0);
  padder->Update();

  inputImage = padder->GetOutput();
  inputImage->DisconnectPipeline();

  using MaskPadderType = itk::ConstantPadImageFilter<MaskImageType, MaskImageType>;
  typename MaskPadderType::Pointer maskPadder = MaskPadderType::New();
  maskPadder->SetInput(maskImage);
  maskPadder->SetPadLowerBound(lowerBound);
  maskPadder->SetPadUpperBound(upperBound);
  maskPadder->SetConstant(0);
  maskPadder->Update();

  maskImage = maskPadder->GetOutput();
  maskImage->DisconnectPipeline();

  correcter->SetNumberOfControlPoints(numberOfControlPoints);

  // handle the shrink factor
  using ShrinkerType = itk::ShrinkImageFilter<ImageType, ImageType>;
  typename ShrinkerType::Pointer shrinker = ShrinkerType::New();
  shrinker->SetInput(inputImage);
  shrinker->SetShrinkFactors(1);

  using MaskShrinkerType = itk::ShrinkImageFilter<MaskImageType, MaskImageType>;
  typename MaskShrinkerType::Pointer maskshrinker = MaskShrinkerType::New();
  maskshrinker->SetInput(maskImage);
  maskshrinker->SetShrinkFactors(1);

  if (argc > 4)
  {
    shrinker->SetShrinkFactors(std::stoi(argv[4]));
    maskshrinker->SetShrinkFactors(std::stoi(argv[4]));
  }
  shrinker->Update();
  inputImage = shrinker->GetOutput();
  inputImage->DisconnectPipeline();

  maskshrinker->Update();
  maskImage = maskshrinker->GetOutput();
  maskImage->DisconnectPipeline();

  // set the input image and mask image
  correcter->SetInput(inputImage);
  correcter->SetMaskImage(maskImage);

  using CommandType = CommandIterationUpdate<CorrecterType>;
  typename CommandType::Pointer observer = CommandType::New();
  correcter->AddObserver(itk::IterationEvent(), observer);

  try
  {
    correcter->Update();
  }
  catch (const itk::ExceptionObject & excep)
  {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
  }

  correcter->Print(std::cout, 3);

  // Test the reconstruction of the log bias field
  ImagePointer originalInputImage = reader->GetOutput();
  reader->UpdateOutputInformation();
  correcter->SetMaskImage(nullptr);
  correcter->SetInput(originalInputImage);
  typename CorrecterType::RealImagePointer biasField =
    correcter->ReconstructBiasField(correcter->GetLogBiasFieldControlPointLattice());
  WriteImage(biasField.GetPointer(), (std::string(argv[3]) + "-LogBiasField.nrrd").c_str());

  // output the log bias field control point lattice
  WriteImage(correcter->GetLogBiasFieldControlPointLattice(), argv[3]);

  return EXIT_SUCCESS;
}

int
itkN4BiasFieldCorrectionImageFilterTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " imageDimension inputImage "
              << "outputLogControlPointLattice [shrinkFactor,default=1] "
              << "[numberOfIterations,default=100x50x50] "
              << " [maskImageWithLabelEqualTo1] [splineDistance,default=200]" << std::endl;
    exit(EXIT_FAILURE);
  }

  switch (std::stoi(argv[1]))
  {
    case 2:
      return N4<2>(argc, argv);

    case 3:
      return N4<3>(argc, argv);

    default:
      std::cerr << "Unsupported dimension" << std::endl;
      exit(EXIT_FAILURE);
  }
}
