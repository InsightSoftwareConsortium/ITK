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

#include "itkCastImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkSymmetricEigenAnalysisImageFilter.h"
#include "itkTestingMacros.h"
#include "itkImageRegionIterator.h"

/**
 * Function to convert image of double precision vector pixels to a
 * representation of scalar values that are easy to view and test.
 * NOTE: Similar function in itkBSplineScatteredDataPointSetToImageFilterTest5.cxx
 */
template <typename InternalImageType>
static void
makeTestableScalarImage(typename InternalImageType::Pointer internalImage, std::string outputFilename)
{ // using OutputPixelType = unsigned char;
  using OutputPixelType = uint8_t;
  using OutputImageType = itk::Image<OutputPixelType, 3>;

  const OutputImageType::Pointer outputImage = OutputImageType::New();
  outputImage->CopyInformation(internalImage);
  outputImage->SetRegions(internalImage->GetBufferedRegion());
  outputImage->AllocateInitialized();

  auto myiterator = itk::ImageRegionConstIterator<InternalImageType>(internalImage, internalImage->GetBufferedRegion());
  auto myOutiterator = itk::ImageRegionIterator<OutputImageType>(outputImage, outputImage->GetBufferedRegion());

  // Convert vector image to magnitude and scale to use range of png values
  float max_magnitude_value = 0.0;
  while (!myiterator.IsAtEnd())
  {
    const auto arr = myiterator.Get();
    const auto magvalue = std::sqrt(arr[0] * arr[0] + arr[1] * arr[1] + arr[2] * arr[2]);
    max_magnitude_value = std::max<float>(max_magnitude_value, magvalue);
    ++myiterator;
  }
  const float scale_factor = 255.0 / ceil(max_magnitude_value);
  myOutiterator.GoToBegin();
  myiterator.GoToBegin();
  while (!myOutiterator.IsAtEnd())
  {
    // Convert vector image to magnitude and scale to use range of png values
    const auto arr = myiterator.Get();
    const auto magvalue = std::sqrt(arr[0] * arr[0] + arr[1] * arr[1] + arr[2] * arr[2]);
    myOutiterator.Set(magvalue * scale_factor);
    ++myiterator;
    ++myOutiterator;
  }

  // Write the result image
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(outputFilename);
  writer->SetInput(outputImage);
  writer->Update();
}

namespace itk
{


template <typename TInputImage, typename TInternalImage, typename TOutputImage>
class SymmetricEigenAnalysisImageFilterHelper : public SymmetricEigenAnalysisImageFilter<TInputImage, TInternalImage>
{
public:
  using Self = SymmetricEigenAnalysisImageFilterHelper;
  using Superclass = SymmetricEigenAnalysisImageFilter<TInputImage, TInternalImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;


  using InputImageType = TInputImage;
  using InternalImageType = TInternalImage;

  itkOverrideGetNameOfClassMacro(SymmetricEigenAnalysisImageFilterHelper);

  itkNewMacro(Self);

  static int
  Exercise(itk::EigenValueOrderEnum order, const std::string & outputFilename)
  {

    using SymmetricEigenAnalysisImageFilterType = SymmetricEigenAnalysisImageFilter<InputImageType, InternalImageType>;

    // Declare the type of the index to access images
    using IndexType = itk::Index<InputImageType::ImageDimension>;

    // Declare the type of the size
    using SizeType = itk::Size<InputImageType::ImageDimension>;

    // Declare the type of the Region
    using RegionType = itk::ImageRegion<InputImageType::ImageDimension>;

    // Create the input image
    auto inputImage = InputImageType::New();

    // Define its size, and start index
    SizeType size;
    size[0] = 8;
    size[1] = 8;
    size[2] = 8;

    constexpr IndexType start{};

    RegionType region;
    region.SetIndex(start);
    region.SetSize(size);

    // Initialize the input image
    inputImage->SetRegions(region);
    inputImage->Allocate();

    // Declare Iterator type for the input image
    using IteratorType = itk::ImageRegionIteratorWithIndex<InputImageType>;

    typename InputImageType::PixelType tensorValue;

    tensorValue(0, 0) = 19.0;
    tensorValue(0, 1) = 23.0;
    tensorValue(0, 2) = 29.0;
    tensorValue(1, 1) = 31.0;
    tensorValue(1, 2) = 37.0;
    tensorValue(2, 2) = 39.0;

    // Create one iterator for the input image (this is a light object)
    IteratorType it(inputImage, inputImage->GetRequestedRegion());
    // Initialize the content of the input image
    while (!it.IsAtEnd())
    {
      it.Set(tensorValue);
      ++it;
    }

    // Create the filter
    auto filter = SymmetricEigenAnalysisImageFilterType::New();

    // Dimension should be initialized to the input image dimension
    ITK_TEST_EXPECT_EQUAL(filter->GetDimension(), InputImageType::ImageDimension);
    ITK_TEST_SET_GET_VALUE(InputImageType::ImageDimension, filter->GetDimension());

    // Set the input image
    filter->SetInput(inputImage);

    filter->SetFunctor(filter->GetFunctor());

    filter->OrderEigenValuesBy(order);
    filter->SetOrderEigenValuesBy(order);
    ITK_TEST_SET_GET_VALUE(order, filter->GetOrderEigenValuesBy());

    // Execute the filter
    ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

    // Get the filter output
    // It is important to do it AFTER the filter is Updated
    // Because the object connected to the output may be changed
    // by another during GenerateData() call
    const typename InternalImageType::Pointer internalImage = filter->GetOutput();
    ITK_TRY_EXPECT_NO_EXCEPTION(makeTestableScalarImage<InternalImageType>(internalImage, outputFilename));


    std::cout << "Test succeeded." << std::endl;
    return EXIT_SUCCESS;
  }
};


template <unsigned int TMatrixDimension, typename TInputImage, typename TInternalImage, typename TOutputImage>
class SymmetricEigenAnalysisFixedDimensionImageFilterHelper
  : public SymmetricEigenAnalysisFixedDimensionImageFilter<TMatrixDimension, TInputImage, TInternalImage>
{
public:
  using Self = SymmetricEigenAnalysisFixedDimensionImageFilterHelper;
  using Superclass = SymmetricEigenAnalysisFixedDimensionImageFilter<TMatrixDimension, TInputImage, TInternalImage>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputImageType = TInputImage;
  using InternalImageType = TInternalImage;
  using OutputImageType = TOutputImage;

  itkOverrideGetNameOfClassMacro(SymmetricEigenAnalysisFixedDimensionImageFilterHelper);
  itkNewMacro(Self);

  static int
  Exercise(itk::EigenValueOrderEnum order, const std::string & outputFilename)
  {

    using SymmetricEigenAnalysisFixedDimensionImageFilterType =
      SymmetricEigenAnalysisFixedDimensionImageFilter<TMatrixDimension, InputImageType, InternalImageType>;

    // Declare the type of the index to access images
    using IndexType = itk::Index<InputImageType::ImageDimension>;

    // Declare the type of the size
    using SizeType = itk::Size<InputImageType::ImageDimension>;

    // Declare the type of the Region
    using RegionType = itk::ImageRegion<InputImageType::ImageDimension>;

    // Create the input image
    auto inputImage = InputImageType::New();

    // Define its size, and start index
    SizeType size;
    size[0] = 8;
    size[1] = 8;
    size[2] = 8;

    constexpr IndexType start{};

    RegionType region;
    region.SetIndex(start);
    region.SetSize(size);

    // Initialize the input image
    inputImage->SetRegions(region);
    inputImage->Allocate();

    // Declare Iterator type for the input image
    using IteratorType = itk::ImageRegionIteratorWithIndex<InputImageType>;

    typename InputImageType::PixelType tensorValue;

    tensorValue(0, 0) = 19.0;
    tensorValue(0, 1) = 23.0;
    tensorValue(0, 2) = 29.0;
    tensorValue(1, 1) = 31.0;
    tensorValue(1, 2) = 37.0;
    tensorValue(2, 2) = 39.0;

    // Create one iterator for the input image (this is a light object)
    IteratorType it(inputImage, inputImage->GetRequestedRegion());
    // Initialize the content of the input image
    while (!it.IsAtEnd())
    {
      it.Set(tensorValue);
      ++it;
    }

    // Create the filter
    const typename SymmetricEigenAnalysisFixedDimensionImageFilterType::Pointer filter =
      SymmetricEigenAnalysisFixedDimensionImageFilterType::New();

    // Set the input image
    filter->SetInput(inputImage);
    filter->SetFunctor(filter->GetFunctor());
    filter->OrderEigenValuesBy(order);

    // Execute the filter
    ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

    // Get the filter output
    // It is important to do it AFTER the filter is Updated
    // Because the object connected to the output may be changed
    // by another during GenerateData() call
    const typename InternalImageType::Pointer internalImage = filter->GetOutput();
    ITK_TRY_EXPECT_NO_EXCEPTION(makeTestableScalarImage<InternalImageType>(internalImage, outputFilename));

    std::cout << "Test succeeded." << std::endl;
    return EXIT_SUCCESS;
  }
};
} // end namespace itk


int
itkSymmetricEigenAnalysisImageFilterTest(int argc, char * argv[])
{

  if (argc < 4)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << "outputImage order outputImageFixedDimension"
              << std::endl;
    return EXIT_FAILURE;
  }
  bool testPassed = true;

  // Define the dimension of the images
  constexpr unsigned int Dimension = 3;

  // Declare the pixel type
  using InputPixelType = float;
  using InternalPixelType = double;
  using OutputPixelType = unsigned char;

  // Define the symmetric tensor pixel type
  using TensorType = itk::SymmetricSecondRankTensor<InputPixelType, Dimension>;

  // Declare the types of the images
  using InputImageType = itk::Image<TensorType, Dimension>;

  // Define the type for storing the eigen-value
  using InternalValueArray = itk::FixedArray<InternalPixelType, Dimension>;
  using OutputValueArray = itk::FixedArray<OutputPixelType, Dimension>;

  // Declare the types of the output images
  using InternalImageType = itk::Image<InternalValueArray, Dimension>;
  using OutputImageType = itk::Image<OutputValueArray, Dimension>;

  // Declare the type for the filter
  using FilterType = itk::SymmetricEigenAnalysisImageFilter<InputImageType, InternalImageType>;

  // Create an instance to exercise basic object methods
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, SymmetricEigenAnalysisImageFilter, UnaryFunctorImageFilter);


  // Get the input arguments
  auto order = static_cast<itk::EigenValueOrderEnum>(std::stoi(argv[2]));

  const std::string outputFilename = argv[1];


  // Test the filter
  const int testResult =
    itk::SymmetricEigenAnalysisImageFilterHelper<InputImageType, InternalImageType, OutputImageType>::Exercise(
      order, outputFilename);

  if (testResult != EXIT_SUCCESS)
  {
    std::cout << "test SymmetricEigenAnalysisImageFilter failed" << std::endl;
    testPassed = false;
  }

  // Test the fixed dimension filter
  using FilterFixedDimensionType =
    itk::SymmetricEigenAnalysisFixedDimensionImageFilter<Dimension, InputImageType, InternalImageType>;

  auto              orderFixedDimension = static_cast<itk::EigenValueOrderEnum>(std::stoi(argv[2]));
  const std::string outputFilenameFixedDimension = argv[3];
  // Create an instance to exercise basic object methods
  auto filterFixedDimension = FilterFixedDimensionType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    filterFixedDimension, SymmetricEigenAnalysisFixedDimensionImageFilter, UnaryFunctorImageFilter);

  const int testFixedDimensionResult =
    itk::SymmetricEigenAnalysisFixedDimensionImageFilterHelper<Dimension,
                                                               InputImageType,
                                                               InternalImageType,
                                                               OutputImageType>::Exercise(orderFixedDimension,
                                                                                          outputFilenameFixedDimension);

  if (testFixedDimensionResult != EXIT_SUCCESS)
  {
    std::cout << "test SymmetricEigenAnalysisFixedImageImageFilter failed" << std::endl;
    testPassed = false;
  }

  // All objects should be automatically destroyed at this point
  if (testPassed)
  {
    return EXIT_SUCCESS;
  }

  return EXIT_FAILURE;
}
