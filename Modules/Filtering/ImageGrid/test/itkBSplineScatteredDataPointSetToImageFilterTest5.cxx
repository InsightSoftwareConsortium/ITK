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
#include "itkPointSet.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkTestingMacros.h"
#include "itkImageRegionIterator.h"


/**
 * Function to convert image of double precison vector pixels to a
 * representation of scalar values that are easy to view and test.
 * NOTE: Similar function in itkSymmetricEigenAnalysisImageFilterTest.cxx
 */
template <typename InternalImageType>
static void
makeTestableScalarImage(typename InternalImageType::Pointer internalImage, std::string outputFilename)
{ // using OutputPixelType = unsigned char;
  using OutputPixelType = uint8_t;
  using OutputImageType = itk::Image<OutputPixelType, 2>;

  OutputImageType::Pointer outputImage = OutputImageType::New();
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


/**
 * In this test, we approximate a sequence of 3D points with a
 * parametric curve described by B-Splines. Specifically, we
 * create 3-D trefoil knot parametric surface:
 *  https://en.wikipedia.org/wiki/Trefoil_knot
 * which is closed in both parametric dimensions.
 */
int
itkBSplineScatteredDataPointSetToImageFilterTest5(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << "outputImage" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int ParametricDimension = 2;
  constexpr unsigned int DataDimension = 3;

  using RealType = double;

  using VectorType = itk::Vector<RealType, DataDimension>;
  using ImageType = itk::Image<VectorType, ParametricDimension>;

  using PointSetType = itk::PointSet<VectorType, ParametricDimension>;

  auto pointSet = PointSetType::New();

  // Sample the trefoil knot.
  // The first parametric dimension, u,  is doing the knot part
  // whereas the second dimension, v, is going around in a simple circle.
  for (RealType u = -2.0 * itk::Math::pi; u <= 2.0 * itk::Math::pi; u += 0.1)
  {
    for (RealType v = -itk::Math::pi; v <= itk::Math::pi; v += 0.1)
    {
      PointSetType::PointType point;
      point[0] = (u + 2.0 * itk::Math::pi) / (4.0 * itk::Math::pi);
      point[1] = (v + itk::Math::pi) / (2.0 * itk::Math::pi);
      const unsigned long i = pointSet->GetNumberOfPoints();
      pointSet->SetPoint(i, point);

      VectorType V;
      V[0] = std::cos(u) * std::cos(v) + 3.0 * std::cos(u) * (1.5 + 0.5 * std::sin(1.5 * u));
      V[1] = std::sin(u) * std::cos(v) + 3.0 * std::sin(u) * (1.5 + 0.5 * std::sin(1.5 * u));
      V[2] = std::sin(v) + 2.0 * std::cos(1.5 * u);

      pointSet->SetPointData(i, V);
    }
  }

  // Instantiate the filter and set the parameters
  using FilterType = itk::BSplineScatteredDataPointSetToImageFilter<PointSetType, ImageType>;

  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, BSplineScatteredDataPointSetToImageFilter, PointSetToImageFilter);

  // Define the parametric domain
  ImageType::SpacingType spacing;
  spacing[0] = 0.001;
  spacing[1] = 0.01;
  filter->SetSpacing(spacing);

  ImageType::SizeType size;
  size[0] = 1000;
  size[1] = 100;

  ImageType::PointType origin;
  origin.Fill(0.0);

  filter->SetSize(size);
  filter->SetOrigin(origin);
  filter->SetSpacing(spacing);
  filter->SetInput(pointSet);
  filter->SetSplineOrder(3);

  FilterType::ArrayType ncps;
  ncps[0] = 7;
  ncps[1] = 10;

  filter->SetNumberOfControlPoints(ncps);
  filter->SetNumberOfLevels(4);
  filter->SetGenerateOutputImage(false);

  FilterType::ArrayType close;
  close.Fill(1);
  filter->SetCloseDimension(close);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  const std::string outputFilename = argv[1];
  auto              phiLatticeImage = filter->GetPhiLattice();
  ITK_TRY_EXPECT_NO_EXCEPTION(makeTestableScalarImage<FilterType::PointDataImageType>(phiLatticeImage, outputFilename));

  return EXIT_SUCCESS;
}
