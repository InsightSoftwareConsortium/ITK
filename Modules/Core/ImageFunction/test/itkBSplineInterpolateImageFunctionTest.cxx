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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/

#include <iostream>

#include "itkBSplineInterpolateImageFunction.h"

#include "makeRandomImageBsplineInterpolator.h"
#include "itkTestingMacros.h"


using InputPixelType = double;
using CoordinateType = itk::SpacePrecisionType;

// Set up for 1D Images
enum
{
  ImageDimension1D = 1
};

using ImageType1D = itk::Image<InputPixelType, ImageDimension1D>;
using ImageTypePtr1D = ImageType1D::Pointer;
using SizeType1D = ImageType1D::SizeType;
using InterpolatorType1D = itk::BSplineInterpolateImageFunction<ImageType1D, CoordinateType>;
//  using IndexType1D = InterpolatorType1D::IndexType;
using PointType1D = InterpolatorType1D::PointType;
using ContinuousIndexType1D = InterpolatorType1D::ContinuousIndexType;

void set1DInterpData(ImageType1D::Pointer);

// Set up for 2D Images
enum
{
  ImageDimension2D = 2
};

using ImageType2D = itk::Image<InputPixelType, ImageDimension2D>;
using ImageTypePtr2D = ImageType2D::Pointer;
using SizeType2D = ImageType2D::SizeType;
using InterpolatorType2D = itk::BSplineInterpolateImageFunction<ImageType2D, CoordinateType>;
//  using IndexType2D = InterpolatorType2D::IndexType;
using PointType2D = InterpolatorType2D::PointType;
using ContinuousIndexType2D = InterpolatorType2D::ContinuousIndexType;

void set2DInterpData(ImageType2D::Pointer);

// Set up for 3D Images
enum
{
  ImageDimension3D = 3
};

using ImageType3D = itk::Image<InputPixelType, ImageDimension3D>;
using ImageTypePtr3D = ImageType3D::Pointer;
using SizeType3D = ImageType3D::SizeType;
using InterpolatorType3D = itk::BSplineInterpolateImageFunction<ImageType3D, CoordinateType>;
using IndexType3D = InterpolatorType3D::IndexType;
using PointType3D = InterpolatorType3D::PointType;
using ContinuousIndexType3D = InterpolatorType3D::ContinuousIndexType;

using ImageIntegerType3D = itk::Image<unsigned int, ImageDimension3D>;
using InterpolatorIntegerType3D = itk::BSplineInterpolateImageFunction<ImageIntegerType3D, CoordinateType>;
using IndexIntegerType3D = InterpolatorIntegerType3D::IndexType;
using PointIntegerType3D = InterpolatorIntegerType3D::PointType;
using ContinuousIntegerIndexType3D = InterpolatorIntegerType3D::ContinuousIndexType;

void set3DDerivativeData(ImageType3D::Pointer);

template <typename TImage>
void
set3DInterpData(typename TImage::Pointer imgPtr)
{
  SizeType3D size = { { 80, 40, 30 } };

  /* Allocate a simple test image */
  typename TImage::RegionType region;
  region.SetSize(size);

  imgPtr->SetLargestPossibleRegion(region);
  imgPtr->SetBufferedRegion(region);
  imgPtr->Allocate();

  /* Set origin and spacing of physical coordinates */

  /* Initialize the image contents */
  IndexType3D index;
  for (unsigned int slice = 0; slice < size[2]; ++slice)
  {
    index[2] = slice;
    for (unsigned int row = 0; row < size[1]; ++row)
    {
      index[1] = row;
      for (unsigned int col = 0; col < size[0]; ++col)
      {
        index[0] = col;
        imgPtr->SetPixel(index, slice + row + col);
      }
    }
  }
}

/**
 * Test a geometric point. Returns true if test has passed,
 * returns false otherwise
 */
template <typename TInterpolator, typename PointType>
bool
TestGeometricPoint(const TInterpolator * interp, const PointType & point, bool isInside, double trueValue)
{

  std::cout << " Point: " << point;

  const bool bvalue = interp->IsInsideBuffer(point);
  std::cout << " Inside: " << bvalue << ' ';

  if (bvalue != isInside)
  {
    std::cout << "*** Error: inside should be " << isInside << std::endl;
    return false;
  }

  if (isInside)
  {
    const double value = interp->Evaluate(point);
    std::cout << " Value: " << value;

    if (itk::Math::abs(value - trueValue) > 1e-9)
    {
      std::cout << "*** Error: value should be " << trueValue << std::endl;
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
template <typename TInterpolator, typename ContinuousIndexType>
bool
TestContinuousIndex(const TInterpolator * interp, const ContinuousIndexType & index, bool isInside, double trueValue)
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
    const double value = interp->EvaluateAtContinuousIndex(index);
    std::cout << " Value: " << value;

    if (itk::Math::abs(value - trueValue) > 1e-4)
    {
      std::cout << "*** Error: value should be " << trueValue << std::endl;
      return false;
    }
  }

  std::cout << std::endl;
  return true;
}
/**
 * Test a continuous index Derivative. Returns true if test has passed,
 * returns false otherwise
 */
template <typename TInterpolator, typename ContinuousIndexType>
bool
TestContinuousIndexDerivative(const TInterpolator *       interp,
                              const ContinuousIndexType & index,
                              bool                        isInside,
                              double *                    trueValue)
{

  std::cout << " Index: " << index;

  const bool bvalue = interp->IsInsideBuffer(index);
  std::cout << " Inside: " << bvalue << '\n';

  if (bvalue != isInside)
  {
    std::cout << "*** Error: inside should be " << isInside << std::endl;
    return false;
  }

  if (isInside)
  {
    typename TInterpolator::CovariantVectorType value;
    const double                                value2 = interp->EvaluateAtContinuousIndex(index);
    std::cout << "Interpolated Value: " << value2 << '\n';
    value = interp->EvaluateDerivativeAtContinuousIndex(index);
    std::cout << " Value: ";
    for (int i = 0; i < ImageDimension3D; ++i)
    {
      if (i != 0)
      {
        std::cout << ", ";
      }
      std::cout << value[i];
      if (itk::Math::abs(value[i] - trueValue[i]) > 1e-4)
      {
        std::cout << "*** Error: value should be " << trueValue[i] << std::endl;
        return false;
      }
    }
  }

  std::cout << std::endl;
  return true;
}

// Run a series of tests to validate the 1D
// cubic spline implementation.
int
test1DCubicSpline()
{
  int flag = 0;

  // Allocate a simple test image
  const ImageTypePtr1D image = ImageType1D::New();

  set1DInterpData(image);

  // Set origin and spacing of physical coordinates
  double origin[] = { 0.5 };
  double spacing[] = { 0.1 };
  image->SetOrigin(origin);
  image->SetSpacing(spacing);

  // Create and initialize the interpolator
  auto interp = InterpolatorType1D::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(interp, BSplineInterpolateImageFunction, InterpolateImageFunction);


  constexpr itk::ThreadIdType numberOfWorkUnits = 1;
  interp->SetNumberOfWorkUnits(numberOfWorkUnits);
  ITK_TEST_SET_GET_VALUE(numberOfWorkUnits, interp->GetNumberOfWorkUnits());

  constexpr bool useImageDirection = true;
  ITK_TEST_SET_GET_BOOLEAN(interp, UseImageDirection, useImageDirection);

  interp->SetInputImage(image);
  interp->Print(std::cout);

  // Test evaluation at continuous indices and corresponding
  // gemetric points
  std::cout << "Testing 1D Cubic B-Spline:\n";
  std::cout << "Evaluate at: " << std::endl;


  // These values test 1) near border,
  //    2) inside
  //    3) integer value
  //    4) outside image
#define NPOINTS 5 // number of points
  constexpr itk::SpacePrecisionType darray1[NPOINTS] = { 1.4, 8.9, 10.0, 40.0, -0.3 };
  constexpr double truth[NPOINTS] = { 334.41265437584, 18.158173426944, 4.0000, 0, 442.24157192006658 };
  constexpr bool   b_Inside[NPOINTS] = { true, true, true, false, true };

  // an integer position inside the image
  for (int ii = 0; ii < NPOINTS; ++ii)
  {

    auto cindex = ContinuousIndexType1D(&darray1[ii]);
    bool passed =
      TestContinuousIndex<InterpolatorType1D, ContinuousIndexType1D>(interp, cindex, b_Inside[ii], truth[ii]);

    if (!passed)
    {
      flag += 1;
    }
    PointType1D point;
    image->TransformContinuousIndexToPhysicalPoint(cindex, point);
    passed = TestGeometricPoint<InterpolatorType1D, PointType1D>(interp, point, b_Inside[ii], truth[ii]);

    if (!passed)
    {
      flag += 1;
    }
  }

  return (flag);
}

int
test2DSpline()
{
  int flag = 0;

  /* Allocate a simple test image */
  const ImageTypePtr2D image = ImageType2D::New();

  set2DInterpData(image);

  /* Set origin and spacing of physical coordinates */
  constexpr double origin[] = { 0.5, 1.0 };
  constexpr double spacing[] = { 0.1, 0.5 };
  image->SetOrigin(origin);
  image->SetSpacing(spacing);

  ImageType2D::IndexType startIndex = image->GetRequestedRegion().GetIndex();

  /* Create and initialize the interpolator */
  for (unsigned int splineOrder = 0; splineOrder <= 5; ++splineOrder)
  {
    auto interp = InterpolatorType2D::New();
    interp->SetSplineOrder(splineOrder);
    ITK_TEST_SET_GET_VALUE(splineOrder, interp->GetSplineOrder());

    std::cout << "SplineOrder: " << interp->GetSplineOrder() << std::endl;

    interp->SetInputImage(image);
    interp->Print(std::cout);

    /* Test evaluation at continuous indices and corresponding
    gemetric points */
    std::cout << "Testing 2D B-Spline of Order " << splineOrder << ":\n";
    std::cout << "Evaluate at: " << std::endl;

    // These values test 1) near border,
    //    2) inside
    //    3) integer value
    //    4) outside image
#define NPOINTS2 4 // number of points

    constexpr itk::SpacePrecisionType darray1[NPOINTS2][2] = { { 0.1, 0.2 }, { 3.4, 5.8 }, { 4.0, 6.0 }, { 2.1, 8.0 } };
    constexpr double                  truth[NPOINTS2][6] = {
      { 154.5, 140.14, 151.86429192392, 151.650316034, 151.865916515, 151.882483111 },
      { 0, 13.84, 22.688125812495, 22.411473093, 22.606968306, 22.908345604 },
      { 36.2, 36.2, 36.2, 36.2, 36.2, 36.2 },
      { 0, 0, 0, 0, 0, 0 }
    };
    constexpr bool b_Inside[NPOINTS2] = { true, true, true, false };

    // an integer position inside the image
    for (int ii = 0; ii < NPOINTS2; ++ii)
    {
      auto cindex = ContinuousIndexType2D(&darray1[ii][0]);
      cindex[0] += startIndex[0];
      cindex[1] += startIndex[1];

      bool passed = TestContinuousIndex<InterpolatorType2D, ContinuousIndexType2D>(
        interp, cindex, b_Inside[ii], truth[ii][splineOrder]);

      if (!passed)
      {
        flag += 1;
      }
      PointType2D point;
      image->TransformContinuousIndexToPhysicalPoint(cindex, point);
      passed = TestGeometricPoint<InterpolatorType2D, PointType2D>(interp, point, b_Inside[ii], truth[ii][splineOrder]);

      if (!passed)
      {
        flag += 1;
      }
    }
  } // end of splineOrder

  return (flag);
}

int
test3DSpline()
{
  int flag = 0;

  /* Allocate a simple test image */
  const ImageTypePtr3D image = ImageType3D::New();

  set3DInterpData<ImageType3D>(image);

  /* Set origin and spacing of physical coordinates */
  constexpr double origin[] = { 0.5, 1.0, 1.333 };
  constexpr double spacing[] = { 0.1, 0.5, 0.75 };
  image->SetOrigin(origin);
  image->SetSpacing(spacing);

  /* Create and initialize the interpolator */
  for (int splineOrder = 2; splineOrder <= 5; ++splineOrder)
  {
    auto interp = InterpolatorType3D::New();
    interp->SetSplineOrder(splineOrder);
    interp->SetInputImage(image);
    interp->Print(std::cout);

    /* Test evaluation at continuous indices and corresponding
    gemetric points */
    std::cout << "Testing 3D B-Spline of Order " << splineOrder << ":\n";
    std::cout << "Evaluate at: " << std::endl;

    // These values test
    //    1) near border,
    //    2) inside
    //    3) integer value
    //    4) outside image
#define NPOINTS3 5 // number of points

    constexpr itk::SpacePrecisionType darray1[NPOINTS3][ImageDimension3D] = {
      { 0.1, 20.1, 28.4 }, { 21.58, 34.5, 17.2 }, { 10, 20, 12 }, { 15, 20.2, 31 }, { 2, 0.3, -0.3 }
    };
    constexpr double truth[NPOINTS3][4] = {
      { 48.621593795, 48.651173138, 48.656914878, 48.662256571 },
      { 73.280126903, 73.280816965, 73.282780615, 73.285315943 },
      { 42.0, 42.0, 42.0, 42.0 },
      { 0, 0, 0, 0 },
      { 2.2545584407825165, 2.2722384004239382, 2.2533523347849744, 2.2516795363567588 }
    };
    constexpr bool b_Inside[NPOINTS3] = { true, true, true, false, true };

    // an integer position inside the image
    for (int ii = 0; ii < NPOINTS3; ++ii)
    {
      auto cindex = ContinuousIndexType3D(&darray1[ii][0]);
      bool passed = TestContinuousIndex<InterpolatorType3D, ContinuousIndexType3D>(
        interp, cindex, b_Inside[ii], truth[ii][splineOrder - 2]);

      if (!passed)
      {
        flag += 1;
      }

      PointType3D point;
      image->TransformContinuousIndexToPhysicalPoint(cindex, point);
      passed =
        TestGeometricPoint<InterpolatorType3D, PointType3D>(interp, point, b_Inside[ii], truth[ii][splineOrder - 2]);

      if (!passed)
      {
        flag += 1;
      }
    }
  } // end of splineOrder

  return (flag);
}

int
test3DSplineDerivative()
{
  int flag = 0;

  /* Allocate a simple test image */
  const ImageTypePtr3D image = ImageType3D::New();

  set3DDerivativeData(image);

  /* Set origin and spacing of physical coordinates */
  constexpr double origin[] = { 0.0, 0.0, 0.0 };
  constexpr double spacing[] = { 1, 1, 1 };
  image->SetOrigin(origin);
  image->SetSpacing(spacing);

  /* Create and initialize the interpolator */
  for (int splineOrder = 1; splineOrder <= 5; ++splineOrder)
  {
    auto interp = InterpolatorType3D::New();
    interp->SetSplineOrder(splineOrder);
    interp->SetInputImage(image);
    interp->Print(std::cout);

    /* Test evaluation at continuous indices and corresponding
    gemetric points */
    std::cout << "Testing Derivatives of 3D B-Spline of Order " << splineOrder << ":\n";
    std::cout << "Evaluate at: " << std::endl;

    // These values test
    //    1) near border,
    //    2) inside
    //    3) integer value
    //    4) outside image
#define NPOINTS4 4 // number of points

    constexpr itk::SpacePrecisionType darray1[NPOINTS4][ImageDimension3D] = {
      { 25.3, 26.8, 24.5 }, { 21.0, 1.4, 0.6 }, { 18, 31, 10 }, { 4.3, 17.9, 42 }
    };
    // Calculated Truth is: {19.4158,5,-24}, {0.9,5,71.6}, {-7.2, 5, 34}, {0,0,0}
    // TODO: Value near border is way off, is this an algorithm problem?  Also,
    //       Is error for 1st order splines in the expected range?
    double truth[5][NPOINTS4][ImageDimension3D] = {
      { { 23.6, 5, -24 }, { 0, 5, 72.0 }, { -3.0, 5, 32 }, { 0, 0, 0 } },
      { { 19.345, 5, -24 }, { 0.875, 4.8873, 98.6607 }, { -7.525, 5, 34 }, { 0, 0, 0 } },
      { { 19.399, 5, -24 }, { 0.9, 4.95411, 92.9006 }, { -7.2, 5, 33.9999 }, { 0, 0, 0 } },
      { { 19.4164, 5, -24 }, { 0.9, 4.9925, 94.5082 }, { -7.2, 5.00044, 33.9976 }, { 0, 0, 0 } },
      { { 19.4223, 5, -24 }, { 0.900157, 5.0544, 93.8607 }, { -7.19929, 5.00189, 33.9879 }, { 0, 0, 0 } }
    };
    constexpr bool b_Inside[NPOINTS4] = { true, true, true, false };

    // an integer position inside the image
    for (int ii = 0; ii < NPOINTS4; ++ii)
    {
      auto       cindex = ContinuousIndexType3D(&darray1[ii][0]);
      const bool passed = TestContinuousIndexDerivative<InterpolatorType3D, ContinuousIndexType3D>(
        interp, cindex, b_Inside[ii], &truth[splineOrder - 1][ii][0]);
      if (!passed)
      {
        flag += 1;
      }
    }
  } // end of splineOrder

  return (flag);
}

int
testInteger3DSpline()
{
  int flag = 0;

  /* Allocate a simple test image */
  auto image = ImageIntegerType3D::New();

  set3DInterpData<ImageIntegerType3D>(image);

  /* Set origin and spacing of physical coordinates */
  constexpr double origin[] = { 0.5, 1.0, 1.333 };
  constexpr double spacing[] = { 0.1, 0.5, 0.75 };
  image->SetOrigin(origin);
  image->SetSpacing(spacing);

  /* Create and initialize the interpolator */
  for (int splineOrder = 2; splineOrder <= 5; ++splineOrder)
  {
    auto interp = InterpolatorIntegerType3D::New();
    interp->SetSplineOrder(splineOrder);
    interp->SetInputImage(image);
    interp->Print(std::cout);

    /* Test evaluation at continuous indices and corresponding
    gemetric points */
    std::cout << "Testing 3D Integer B-Spline of Order " << splineOrder << ":\n";
    std::cout << "Evaluate at: " << std::endl;

    // These values test
    //    1) near border,
    //    2) inside
    //    3) integer value
    //    4) outside image
#define NPOINTS4b 4 // number of points

    // Note: the answers should be the same as for the test3DSpline
    constexpr itk::SpacePrecisionType darray1[NPOINTS4b][ImageDimension3D] = {
      { 0.1, 20.1, 28.4 }, { 21.58, 34.5, 17.2 }, { 10, 20, 12 }, { 15, 20.2, 31 }
    };
    constexpr double truth[NPOINTS4b][4] = { { 48.621593795, 48.651173138, 48.656914878, 48.662256571 },
                                             { 73.280126903, 73.280816965, 73.282780615, 73.285315943 },
                                             { 42.0, 42.0, 42.0, 42.0 },
                                             { 0, 0, 0, 0 } };
    constexpr bool   b_Inside[NPOINTS4b] = { true, true, true, false };

    // an integer position inside the image
    for (int ii = 0; ii < NPOINTS4b; ++ii)
    {
      auto cindex = ContinuousIntegerIndexType3D(&darray1[ii][0]);
      bool passed = TestContinuousIndex<InterpolatorIntegerType3D, ContinuousIntegerIndexType3D>(
        interp, cindex, b_Inside[ii], truth[ii][splineOrder - 2]);

      if (!passed)
      {
        flag += 1;
      }
      PointIntegerType3D point;
      image->TransformContinuousIndexToPhysicalPoint(cindex, point);
      passed = TestGeometricPoint<InterpolatorIntegerType3D, PointIntegerType3D>(
        interp, point, b_Inside[ii], truth[ii][splineOrder - 2]);

      if (!passed)
      {
        flag += 1;
      }
    }
  } // end of splineOrder

  return (flag);
}

// Test to verify that EvaluateDerivativeAtContinuousIndex and EvaluateValueAndDerivativeAtContinuousIndex
// produce identical results.
int
testEvaluateValueAndDerivative()
{
  constexpr unsigned int ImageDimension = 2;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using BSplineInterpolatorFunctionType = itk::BSplineInterpolateImageFunction<ImageType, double, double>;

  constexpr unsigned int                         SplineOrder = 3;
  const BSplineInterpolatorFunctionType::Pointer interpolator =
    makeRandomImageInterpolator<BSplineInterpolatorFunctionType>(SplineOrder);

  /** Test the EvaluateDerivative and EvaluateValueAndDerivative functions **/
  using ContinuousIndexType = BSplineInterpolatorFunctionType::ContinuousIndexType;
  ContinuousIndexType x;
  x[0] = 15.1;
  x[1] = 15.2;

  using CovariantVectorType = BSplineInterpolatorFunctionType::CovariantVectorType;
  const CovariantVectorType dx_1 = interpolator->EvaluateDerivativeAtContinuousIndex(x);
  CovariantVectorType       dx_2;

  BSplineInterpolatorFunctionType::OutputType value;
  interpolator->EvaluateValueAndDerivativeAtContinuousIndex(x, value, dx_2);

  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    std::cout << std::scientific << value << std::endl;
    std::cout << std::scientific << "EvaluateDerivative:         " << dx_1 << std::endl;
    std::cout << std::scientific << "EvaluateValueAndDerivative: " << dx_2 << std::endl;
    if (itk::Math::abs(dx_1[i] - dx_2[i]) > 1e-5)
    {
      std::cout << "[ERROR]" << dx_1[i] << " != " << dx_2[i] << std::endl;
      return EXIT_FAILURE;
    }
  }
  return EXIT_SUCCESS;
}

int
itkBSplineInterpolateImageFunctionTest(int itkNotUsed(argc), char * itkNotUsed(argv)[])
{
  int flag = 0; /* Did this test program work? */

  std::cout << "Testing B Spline interpolation methods:\n";

  flag += test1DCubicSpline();

  flag += test2DSpline();

  flag += test3DSpline();

  flag += test3DSplineDerivative();

  flag += testInteger3DSpline();

  flag += testEvaluateValueAndDerivative();

  /* Return results of test */
  if (flag != 0)
  {
    std::cout << "*** " << flag << " tests failed" << std::endl;

    return EXIT_FAILURE;
  }

  std::cout << "All tests successfully passed" << std::endl;
  return EXIT_SUCCESS;
}

void
set1DInterpData(ImageType1D::Pointer imgPtr)
{
  constexpr SizeType1D size = { { 36 } };
  constexpr double     mydata[36] = { 454.0000, 369.4000,  295.2000,  230.8000, 175.6000, 129.0000, 90.4000, 59.2000,
                                      34.8000,  16.6000,   4.0000,    -3.6000,  -6.8000,  -6.2000,  -2.4000, 4.0000,
                                      12.4000,  22.2000,   32.8000,   43.6000,  54.0000,  63.4000,  71.2000, 76.8000,
                                      79.6000,  79.0000,   74.4000,   65.2000,  50.8000,  30.6000,  4.0000,  -29.6000,
                                      -70.8000, -120.2000, -178.4000, -246.0000 };

  ImageType1D::RegionType region;
  region.SetSize(size);

  imgPtr->SetLargestPossibleRegion(region);
  imgPtr->SetBufferedRegion(region);
  imgPtr->Allocate();

  using InputIterator = itk::ImageRegionIterator<ImageType1D>;

  InputIterator inIter(imgPtr, region);

  int j = 0;
  while (!inIter.IsAtEnd())
  {
    inIter.Set(mydata[j]);
    ++inIter;
    ++j;
  }
}

void
set2DInterpData(ImageType2D::Pointer imgPtr)
{
  constexpr SizeType2D size = { { 7, 7 } };
  constexpr double     mydata[49] = {
    154.5000,  82.4000,   30.9000,   0, -10.3000, 0, 30.9000,  117.0000,  62.4000,   23.4000,  0, -7.8000, 0, 23.4000,
    18.0000,   9.6000,    3.6000,    0, -1.2000,  0, 3.6000,   -120.0000, -64.0000,  -24.0000, 0, 8.0000,  0, -24.0000,
    -274.5000, -146.4000, -54.9000,  0, 18.3000,  0, -54.9000, -423.0000, -225.6000, -84.6000, 0, 28.2000, 0, -84.6000,
    -543.0000, -289.6000, -108.6000, 0, 36.2000,  0, -108.6000
  };

  auto index = ImageType2D::IndexType::Filled(10);

  const ImageType2D::RegionType region{ index, size };

  imgPtr->SetRegions(region);
  imgPtr->Allocate();

  using InputIterator = itk::ImageRegionIterator<ImageType2D>;

  InputIterator inIter(imgPtr, region);

  int j = 0;
  while (!inIter.IsAtEnd())
  {
    inIter.Set(mydata[j]);
    ++inIter;
    ++j;
  }
}

void
set3DDerivativeData(ImageType3D::Pointer imgPtr)
{
  SizeType3D size = { { 41, 41, 41 } };

  /* Allocate a simple test image */
  ImageType3D::RegionType region;
  region.SetSize(size);

  imgPtr->SetLargestPossibleRegion(region);
  imgPtr->SetBufferedRegion(region);
  imgPtr->Allocate();

  /* Set origin and spacing of physical coordinates */

  /* Initialize the image contents */
  // Note [x,y,z] = [x,y,z] - 20 so that image ranges from -20 + 20;
  // f(x) = 0.1x^4 - 0.5x^3 + 2x - 43
  // f(y) = 5y + 7
  // f(z) = -2z^2 - 6z + 10
  // df(x)/dx = 0.4x^3 - 1.5x^2 + 2
  // df(y)/dy = 5
  // df(z)/dz = -4z - 6

  IndexType3D index;
  for (unsigned int slice = 0; slice < size[2]; ++slice)
  {
    index[2] = slice;
    const double slice1 = slice - 20.0; // Center offset
    for (unsigned int row = 0; row < size[1]; ++row)
    {
      index[1] = row;
      const double row1 = row - 20.0; // Center
      for (unsigned int col = 0; col < size[0]; ++col)
      {
        index[0] = col;
        const double col1 = col - 20.0; // Center
        double       value = 0.1 * col1 * col1 * col1 * col1 - 0.5 * col1 * col1 * col1 + 2.0 * col1 - 43.0;
        value += 5.0 * row1 + 7.0;
        value += -2.0 * slice1 * slice1 - 6.0 * slice1 + 10.0;
        imgPtr->SetPixel(index, value);
      }
    }
  }
}
