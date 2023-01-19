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

#include "itkTestingMacros.h"
#include "itkImage.h"
#include "itkWindowedSincInterpolateImageFunction.h"
#include "itkConstantBoundaryCondition.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkRGBPixel.h"

namespace SincInterpolate
{

enum
{
  ImageDimension = 3
};
enum
{
  WindowRadius = 2
};

using PixelType = unsigned char;
using ImageType = itk::Image<PixelType, ImageDimension>;
using CoordRepType = itk::SpacePrecisionType;
using WindowFunctionType = itk::Function::HammingWindowFunction<2>;
using BoundaryConditionType = itk::ConstantBoundaryCondition<ImageType>;

using InterpolatorType =
  itk::WindowedSincInterpolateImageFunction<ImageType, 2, WindowFunctionType, BoundaryConditionType, CoordRepType>;

using IndexType = InterpolatorType::IndexType;
using PointType = InterpolatorType::PointType;
using ContinuousIndexType = InterpolatorType::ContinuousIndexType;
using OutputType = InterpolatorType::OutputType;


/**
 * Test a geometric point. Returns true if test has passed,
 * returns false otherwise
 */
bool
TestGeometricPoint(const InterpolatorType * interp, const PointType & point, bool isInside, OutputType trueValue)
{

  std::cout << " Point: " << point;

  bool bvalue = interp->IsInsideBuffer(point);
  std::cout << " Inside: " << bvalue << std::endl;

  if (bvalue != isInside)
  {
    std::cout << "*** Error: inside is " << bvalue << " but should be " << isInside << std::endl;
    return false;
  }

  if (isInside)
  {
    OutputType value = interp->Evaluate(point);
    std::cout << " Value: " << value << std::endl;

    if (itk::Math::abs(value - trueValue) > 1e-9)
    {
      std::cout << " *** Error: Value is " << value << " but should be: ";
      std::cout << trueValue << std::endl;
    }
  }

  return true;
}


/**
 * Test a continuous index. Returns true if test has passed,
 * returns false otherwise
 */
bool
TestContinuousIndex(const InterpolatorType *    interp,
                    const ContinuousIndexType & index,
                    bool                        isInside,
                    OutputType                  trueValue)
{

  std::cout << " Index: " << index;

  bool bvalue = interp->IsInsideBuffer(index);
  std::cout << " Inside: " << bvalue << std::endl;

  if (bvalue != isInside)
  {
    std::cout << "*** Error: inside is " << bvalue << " but should be " << isInside << std::endl;
    return false;
  }

  if (isInside)
  {
    OutputType value = interp->EvaluateAtContinuousIndex(index);
    std::cout << " Value: " << value << std::endl;

    if (itk::Math::abs(value - trueValue) > 1e-9)
    {
      std::cout << " *** Error: Value is " << value << " but should be: " << trueValue << std::endl;
    }
  }

  return true;
}

// Test instantiation with RGB pixel type
using RGBInterpolatorType = itk::WindowedSincInterpolateImageFunction<itk::Image<itk::RGBPixel<short>>, 3>;

} // namespace SincInterpolate

int
itkWindowedSincInterpolateImageFunctionTest(int, char *[])
{
  int flag = 0;

  std::cout << "Testing vector image interpolation: " << std::endl;

  using ImageType = SincInterpolate::ImageType;
  using IndexType = SincInterpolate::IndexType;
  using PixelType = SincInterpolate::PixelType;
  using PointType = SincInterpolate::PointType;
  using OutputType = SincInterpolate::OutputType;
  using ContinuousIndexType = SincInterpolate::ContinuousIndexType;
  using CoordRepType = SincInterpolate::CoordRepType;

  using InterpolatorType = SincInterpolate::InterpolatorType;

  const unsigned int ImageDimension = SincInterpolate::ImageDimension;

  ImageType::SizeType size = { { 20, 40, 80 } };
  double              origin[3] = { 0.5, 0.5, 0.5 };
  double              spacing[3] = { 0.1, 0.05, 0.025 };


  // Create a test image
  auto                  image = ImageType::New();
  ImageType::RegionType region;
  region.SetSize(size);

  image->SetRegions(region);
  image->Allocate();

  image->SetOrigin(origin);
  image->SetSpacing(spacing);

  // Write in a simple linear pattern
  using Iterator = itk::ImageRegionIteratorWithIndex<ImageType>;
  Iterator iter(image, region);

  IndexType      index;
  unsigned short value;
  PixelType      pixel;

  for (; !iter.IsAtEnd(); ++iter)
  {
    index = iter.GetIndex();
    value = 0;

    for (unsigned int j = 0; j < ImageDimension; ++j)
    {
      value += index[j];
    }

    pixel = value;

    iter.Set(pixel);
  }

  // Create the interpolator
  auto interp = InterpolatorType::New();
  interp->SetInputImage(image);
  interp->Print(std::cout);

  for (unsigned int d = 0; d < ImageDimension; ++d)
  {
    ITK_TEST_SET_GET_VALUE(2, interp->GetRadius()[d]);
  }

  /* Test evaluation at continuous indices and corresponding
     gemetric points */
  std::cout << "Evaluate at: " << std::endl;
  OutputType          output;
  ContinuousIndexType cindex;
  PointType           point;
  bool                passed;

  // an integer position inside the image
  {
    CoordRepType darray[3] = { 10, 20, 40 };
    output = OutputType(70);
    cindex = ContinuousIndexType(darray);
    passed = SincInterpolate::TestContinuousIndex(interp, cindex, true, output);
  }

  if (!passed)
  {
    flag = 1;
  }

  image->TransformContinuousIndexToPhysicalPoint(cindex, point);
  passed = SincInterpolate::TestGeometricPoint(interp, point, true, output);

  if (!passed)
  {
    flag = 1;
  }

  // position at the image border
  {
    CoordRepType darray[3] = { 0, 20, 40 };
    output = OutputType(60);
    cindex = ContinuousIndexType(darray);
    passed = SincInterpolate::TestContinuousIndex(interp, cindex, true, output);
  }

  if (!passed)
  {
    flag = 1;
  }

  image->TransformContinuousIndexToPhysicalPoint(cindex, point);
  passed = SincInterpolate::TestGeometricPoint(interp, point, true, output);

  if (!passed)
  {
    flag = 1;
  }

  // position near image border
  {
    double       epsilon = 1.0e-10;
    CoordRepType darray[3] = { 19 - epsilon, 20, 40 };
    output = OutputType(79);
    cindex = ContinuousIndexType(darray);
    passed = SincInterpolate::TestContinuousIndex(interp, cindex, true, output);
  }

  if (!passed)
  {
    flag = 1;
  }

  image->TransformContinuousIndexToPhysicalPoint(cindex, point);
  passed = SincInterpolate::TestGeometricPoint(interp, point, true, output);

  if (!passed)
  {
    flag = 1;
  }

  // position outside the image
  {
    CoordRepType darray[3] = { 20, 20, 40 };
    output = OutputType(1);
    cindex = ContinuousIndexType(darray);
    passed = SincInterpolate::TestContinuousIndex(interp, cindex, false, output);
  }

  if (!passed)
  {
    flag = 1;
  }

  image->TransformContinuousIndexToPhysicalPoint(cindex, point);
  passed = SincInterpolate::TestGeometricPoint(interp, point, false, output);

  if (!passed)
  {
    flag = 1;
  }

  // at non-integer position
  {
    CoordRepType darray[3] = { 5.25, 12.5, 42.0 };
    output = OutputType(59.75);
    cindex = ContinuousIndexType(darray);
    passed = SincInterpolate::TestContinuousIndex(interp, cindex, true, output);
  }

  if (!passed)
  {
    flag = 1;
  }

  image->TransformContinuousIndexToPhysicalPoint(cindex, point);
  passed = SincInterpolate::TestGeometricPoint(interp, point, true, output);

  if (!passed)
  {
    flag = 1;
  }

  /* Return results of test */
  if (flag != 0)
  {
    std::cout << "*** Some test failed" << std::endl;
    return flag;
  }


  std::cout << "All tests successfully passed" << std::endl;
  return EXIT_SUCCESS;
}
