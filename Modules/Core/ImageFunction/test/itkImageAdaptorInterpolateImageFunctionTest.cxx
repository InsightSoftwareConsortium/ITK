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

#include <iostream>
#include "itkImageAdaptor.h"
#include "itkRGBPixel.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace ImageAdaptorInterpolate
{

class RedChannelPixelAccessor
{
public:
  using InternalType = itk::RGBPixel<float>;
  using ExternalType = float;

  static ExternalType
  Get(const InternalType & input)
  {
    return static_cast<ExternalType>(input.GetRed());
  }
};

using InputPixelType = RedChannelPixelAccessor::InternalType;
using PixelType = RedChannelPixelAccessor::ExternalType;

enum
{
  ImageDimension = 3
};

using ImageType = itk::Image<InputPixelType, ImageDimension>;
using ImageAdaptorType = itk::ImageAdaptor<ImageType, RedChannelPixelAccessor>;

using CoordRepType = double;

using InterpolatorType = itk::LinearInterpolateImageFunction<ImageAdaptorType, CoordRepType>;
using IndexType = InterpolatorType::IndexType;
using PointType = ImageType::PointType;
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

  const bool bvalue = interp->IsInsideBuffer(point);
  std::cout << " Inside: " << bvalue;

  if (bvalue != isInside)
  {
    std::cout << "*** Error: inside should be " << isInside << std::endl;
    return false;
  }

  if (isInside)
  {
    OutputType value = interp->Evaluate(point);
    std::cout << " Value: " << value << std::endl;

    if (itk::Math::abs(value - trueValue) > 1e-9)
    {
      std::cout << " *** Error: Value should be: ";
      std::cout << trueValue << std::endl;
      return false;
    }
  }

  std::cout << std::endl;
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

  const bool bvalue = interp->IsInsideBuffer(index);
  std::cout << " Inside: " << bvalue;

  if (bvalue != isInside)
  {
    std::cout << "*** Error: inside should be " << isInside << std::endl;
    return false;
  }

  if (isInside)
  {
    OutputType value = interp->EvaluateAtContinuousIndex(index);
    std::cout << " Value: " << value << std::endl;

    if (itk::Math::abs(value - trueValue) > 1e-9)
    {
      std::cout << " *** Error: Value should be: ";
      std::cout << trueValue << std::endl;
      return false;
    }
  }

  std::cout << std::endl;
  return true;
}

} // namespace ImageAdaptorInterpolate

int
itkImageAdaptorInterpolateImageFunctionTest(int, char *[])
{
  int flag = 0;

  std::cout << "Testing image adaptor interpolation: " << std::endl;

  using ImageType = ImageAdaptorInterpolate::ImageType;
  using IndexType = ImageAdaptorInterpolate::IndexType;
  using PointType = ImageAdaptorInterpolate::PointType;
  using InputPixelType = ImageAdaptorInterpolate::InputPixelType;
  using OutputType = ImageAdaptorInterpolate::OutputType;
  using ContinuousIndexType = ImageAdaptorInterpolate::ContinuousIndexType;

  using InterpolatorType = ImageAdaptorInterpolate::InterpolatorType;

  const unsigned int ImageDimension = ImageAdaptorInterpolate::ImageDimension;

  ImageType::SizeType size = { { 20, 40, 80 } };
  double              origin[3] = { 0.5, 0.5, 0.5 };
  double              spacing[3] = { 0.1, 0.05, 0.025 };


  // Create a test image
  ImageType::Pointer    image = ImageType::New();
  ImageType::RegionType region;
  region.SetSize(size);

  image->SetLargestPossibleRegion(region);
  image->SetBufferedRegion(region);
  image->Allocate();

  image->SetOrigin(origin);
  image->SetSpacing(spacing);

  // Write in a simple linear pattern
  using Iterator = itk::ImageRegionIteratorWithIndex<ImageType>;
  Iterator iter(image, region);

  IndexType      index;
  unsigned short value;
  InputPixelType pixel;

  for (; !iter.IsAtEnd(); ++iter)
  {
    index = iter.GetIndex();
    value = 0;

    for (unsigned int j = 0; j < ImageDimension; j++)
    {
      value += index[j];
    }

    for (unsigned int k = 0; k < ImageDimension; k++)
    {
      pixel[k] = (k + 1) * value;
    }

    iter.Set(pixel);
  }

  // Create the image adaptor
  using ImageAdaptorType = ImageAdaptorInterpolate::ImageAdaptorType;
  ImageAdaptorType::Pointer adaptor = ImageAdaptorType::New();

  adaptor->SetImage(image);

  // Create the interpolator
  InterpolatorType::Pointer interp = InterpolatorType::New();
  interp->SetInputImage(adaptor);
  interp->Print(std::cout);

  /* Test evaluation at continuous indices and corresponding
     gemetric points */
  std::cout << "Evaluate at: " << std::endl;
  OutputType          output;
  ContinuousIndexType cindex;
  PointType           point;
  bool                passed;

  // an integer position inside the image
  {
    itk::SpacePrecisionType darray[3] = { 10, 20, 40 };
    double                  temp = 70.0;
    output = OutputType(temp);
    cindex = ContinuousIndexType(darray);
    passed = ImageAdaptorInterpolate::TestContinuousIndex(interp, cindex, true, output);
  }

  if (!passed)
  {
    flag = 1;
  }

  adaptor->TransformContinuousIndexToPhysicalPoint(cindex, point);
  passed = ImageAdaptorInterpolate::TestGeometricPoint(interp, point, true, output);

  if (!passed)
  {
    flag = 1;
  }

  // position at the image border
  {
    itk::SpacePrecisionType darray[3] = { 0, 20, 40 };
    double                  temp = 60.0;
    output = OutputType(temp);
    cindex = ContinuousIndexType(darray);
    passed = ImageAdaptorInterpolate::TestContinuousIndex(interp, cindex, true, output);
  }

  if (!passed)
  {
    flag = 1;
  }

  adaptor->TransformContinuousIndexToPhysicalPoint(cindex, point);
  passed = ImageAdaptorInterpolate::TestGeometricPoint(interp, point, true, output);

  if (!passed)
  {
    flag = 1;
  }

  // position near image border
  {
    itk::SpacePrecisionType epsilon = 1.0e-10;
    itk::SpacePrecisionType darray[3] = { 19 - epsilon, 20, 40 };
    double                  temp = 79.0;
    output = OutputType(temp);
    cindex = ContinuousIndexType(darray);
    passed = ImageAdaptorInterpolate::TestContinuousIndex(interp, cindex, true, output);
  }

  if (!passed)
  {
    flag = 1;
  }

  adaptor->TransformContinuousIndexToPhysicalPoint(cindex, point);
  passed = ImageAdaptorInterpolate::TestGeometricPoint(interp, point, true, output);

  if (!passed)
  {
    flag = 1;
  }

  // position outside the image
  {
    itk::SpacePrecisionType darray[3] = { 20, 20, 40 };
    double                  temp = 1.0;
    output = OutputType(temp);
    cindex = ContinuousIndexType(darray);
    passed = ImageAdaptorInterpolate::TestContinuousIndex(interp, cindex, false, output);
  }

  if (!passed)
  {
    flag = 1;
  }

  adaptor->TransformContinuousIndexToPhysicalPoint(cindex, point);
  passed = ImageAdaptorInterpolate::TestGeometricPoint(interp, point, false, output);

  if (!passed)
  {
    flag = 1;
  }

  // at non-integer position
  {
    itk::SpacePrecisionType darray[3] = { 5.25, 12.5, 42.0 };
    double                  temp = 59.75;
    output = OutputType(temp);
    cindex = ContinuousIndexType(darray);
    passed = ImageAdaptorInterpolate::TestContinuousIndex(interp, cindex, true, output);
  }

  if (!passed)
  {
    flag = 1;
  }

  adaptor->TransformContinuousIndexToPhysicalPoint(cindex, point);
  passed = ImageAdaptorInterpolate::TestGeometricPoint(interp, point, true, output);

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
  else
  {
    std::cout << "All tests successfully passed" << std::endl;
  }

  return EXIT_SUCCESS;
}
