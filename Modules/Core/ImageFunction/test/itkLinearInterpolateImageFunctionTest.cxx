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

#include "itkTestingMacros.h"
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkLinearInterpolateImageFunction.h"

/* VS 2015 has a bug when building release with the heavily nested for
 * loops iterating too many times.  This turns off optimization to
 * allow the tests to pass.
 */
#if defined(_MSC_VER) && (_MSC_VER == 1900)
#  pragma optimize("", off)
#endif

/* Allows testing up to TDimension=4 */
template <unsigned int TDimension>
int
RunLinearInterpolateTest()
{
  using PixelType = float;
  const unsigned int     Dimensions = TDimension;
  constexpr unsigned int VectorDimension = 4;
  using VectorPixelType = itk::Vector<PixelType, VectorDimension>;
  using ImageType = itk::Image<PixelType, Dimensions>;
  using VectorImageType = itk::Image<VectorPixelType, Dimensions>;
  using VariableVectorImageType = itk::VectorImage<PixelType, Dimensions>;
  using VariablePixelType = typename VariableVectorImageType::PixelType;
  using RegionType = typename ImageType::RegionType;
  using SizeType = typename RegionType::SizeType;
  using IndexType = typename ImageType::IndexType;

  using CoordRepType = float;
  using ContinuousIndexType = typename itk::ContinuousIndex<CoordRepType, Dimensions>;

  using AccumulatorType = typename ContinuousIndexType::ValueType;

  using PointType = typename itk::Point<CoordRepType, Dimensions>;

  using InterpolatorType = typename itk::LinearInterpolateImageFunction<ImageType, CoordRepType>;
  using VectorInterpolatorType = typename itk::LinearInterpolateImageFunction<VectorImageType, CoordRepType>;
  using VariableVectorInterpolatorType =
    typename itk::LinearInterpolateImageFunction<VariableVectorImageType, CoordRepType>;

  using InterpolatedVectorType = typename VectorInterpolatorType::OutputType;
  using InterpolatedVariableVectorType = typename VariableVectorInterpolatorType::OutputType;

  typename ImageType::Pointer               image = ImageType::New();
  typename VectorImageType::Pointer         vectorimage = VectorImageType::New();
  typename VariableVectorImageType::Pointer variablevectorimage = VariableVectorImageType::New();
  variablevectorimage->SetVectorLength(VectorDimension);

  IndexType start;
  start.Fill(0);

  SizeType      size;
  constexpr int dimMaxLength = 3;
  size.Fill(dimMaxLength);

  RegionType region;
  region.SetSize(size);
  region.SetIndex(start);

  image->SetRegions(region);
  image->Allocate();

  vectorimage->SetRegions(region);
  vectorimage->Allocate();

  variablevectorimage->SetRegions(region);
  variablevectorimage->Allocate();

  typename ImageType::PointType   origin;
  typename ImageType::SpacingType spacing;

  origin.Fill(0.0);
  spacing.Fill(1.0);

  image->SetOrigin(origin);
  image->SetSpacing(spacing);

  vectorimage->SetOrigin(origin);
  vectorimage->SetSpacing(spacing);

  variablevectorimage->SetOrigin(origin);
  variablevectorimage->SetSpacing(spacing);

  image->Print(std::cout);

  // Setup for testing up to Dimension=4
  unsigned int dimLengths[4] = { 1, 1, 1, 1 };
  for (unsigned int ind = 0; ind < Dimensions; ind++)
  {
    dimLengths[ind] = dimMaxLength;
  }

  //
  // Fill up the image values with the function
  //
  //   Intensity = f(d1[,d2[,d3[,d4]]]) = 3*d1 [+ d2 [+ d3 [+ d4] ] ]
  //
  //
  IndexType    index;
  unsigned int dimIt[4];
  std::cout << "Image Data: " << std::endl;
  for (dimIt[3] = 0; dimIt[3] < dimLengths[3]; dimIt[3]++)
  {
    for (dimIt[2] = 0; dimIt[2] < dimLengths[2]; dimIt[2]++)
    {
      std::cout << "* dimIt[3], dimIt[2]: " << dimIt[3] << ", " << dimIt[2] << std::endl;
      for (dimIt[1] = 0; dimIt[1] < dimLengths[1]; dimIt[1]++)
      {
        for (dimIt[0] = 0; dimIt[0] < dimLengths[0]; dimIt[0]++)
        {
          PixelType value = 3 * dimIt[0];
          index[0] = dimIt[0];
          for (unsigned int ind = 1; ind < Dimensions; ind++)
          {
            value += dimIt[ind];
            index[ind] = dimIt[ind];
          }
          image->SetPixel(index, value);

          VectorPixelType & vectorpixel = vectorimage->GetPixel(index);
          vectorpixel.Fill(value);

          VariablePixelType variablevectorpixel = variablevectorimage->GetPixel(index);
          variablevectorpixel.Fill(value);

          std::cout << value << " ";
        }
        std::cout << std::endl;
      }
    }
  }

  typename InterpolatorType::Pointer interpolator = InterpolatorType::New();
  interpolator->SetInputImage(image);

  typename VectorInterpolatorType::Pointer vectorinterpolator = VectorInterpolatorType::New();
  vectorinterpolator->SetInputImage(vectorimage);

  typename VariableVectorInterpolatorType::Pointer variablevectorinterpolator = VariableVectorInterpolatorType::New();
  variablevectorinterpolator->SetInputImage(variablevectorimage);

  typename ImageType::SizeType radius;
  radius.Fill(1);
  for (unsigned int d = 0; d < Dimensions; ++d)
  {
    ITK_TEST_SET_GET_VALUE(radius[d], interpolator->GetRadius()[d]);
  }

  constexpr AccumulatorType incr = 0.2;

  const AccumulatorType tolerance = 5e-6;
  // The tolerance of the norm must be greater than the tolerance for individual items.
  const AccumulatorType normTolerance = std::sqrt(4.0f * tolerance * tolerance);

  PointType       point;
  AccumulatorType testLengths[4] = { 1, 1, 1, 1 };
  for (unsigned int ind = 0; ind < Dimensions; ind++)
  {
    testLengths[ind] = dimMaxLength - 1;
  }
  AccumulatorType steps[4];
  AccumulatorType dimItf[4];
  for (dimItf[3] = 0; dimItf[3] < testLengths[3]; dimItf[3]++)
  {
    for (dimItf[2] = 0; dimItf[2] < testLengths[2]; dimItf[2]++)
    {
      for (dimItf[1] = 0; dimItf[1] < testLengths[1]; dimItf[1]++)
      {
        for (dimItf[0] = 0; dimItf[0] < testLengths[0]; dimItf[0]++)
        {
          for (steps[3] = 0; steps[3] < dimItf[3] + 1.01; steps[3] += incr)
          {
            for (steps[2] = 0; steps[2] < dimItf[2] + 1.01; steps[2] += incr)
            {
              for (steps[1] = 0; steps[1] < dimItf[1] + 1.01; steps[1] += incr)
              {
                for (steps[0] = 0; steps[0] < dimItf[0] + 1.01; steps[0] += incr)
                {
                  AccumulatorType expectedValue = 3 * steps[0];
                  point[0] = steps[0];
                  for (unsigned int ind = 1; ind < Dimensions; ind++)
                  {
                    expectedValue += steps[ind];
                    point[ind] = steps[ind];
                  }

                  if (interpolator->IsInsideBuffer(point))
                  {
                    const AccumulatorType computedValue = interpolator->Evaluate(point);
                    const AccumulatorType difference = expectedValue - computedValue;

                    if (std::fabs(difference) > tolerance)
                    {
                      std::cerr << "Error found while computing interpolation " << std::endl;
                      std::cerr << "Point = " << point << std::endl;
                      std::cerr << "Expected value = " << expectedValue << std::endl;
                      std::cerr << "Computed value = " << computedValue << std::endl;
                      std::cerr << "Difference     = " << difference << std::endl;
                      return EXIT_FAILURE;
                    }

                    const InterpolatedVectorType & vectorpixel = vectorinterpolator->Evaluate(point);

                    const InterpolatedVectorType expectedvector(expectedValue);

                    const AccumulatorType & errornorm = (expectedvector - vectorpixel).GetNorm();

                    if (errornorm > normTolerance)
                    {
                      std::cerr << "Error found computing vector interpolation " << std::endl;
                      std::cerr << "Point = " << point << std::endl;
                      std::cerr << "Expected vector = " << expectedvector << std::endl;
                      std::cerr << "Computed vector = " << vectorpixel << std::endl;
                      std::cerr << "Difference     = " << (expectedvector - vectorpixel) << " --> " << errornorm
                                << " > " << normTolerance << std::endl;
                      return EXIT_FAILURE;
                    }

                    const InterpolatedVariableVectorType variablevectorpixel =
                      variablevectorinterpolator->Evaluate(point);

                    InterpolatedVariableVectorType expectedvariablevector;
                    expectedvariablevector.SetSize(VectorDimension);
                    expectedvariablevector.Fill(expectedValue);

                    const AccumulatorType varerrornorm = (expectedvariablevector - variablevectorpixel).GetNorm();

                    if (varerrornorm > normTolerance)
                    {
                      std::cerr << "Error found while computing variable "
                                << " vector interpolation " << std::endl;
                      std::cerr << "Point = " << point << std::endl;
                      std::cerr << "Expected variablevector = " << expectedvariablevector << std::endl;
                      std::cerr << "Computed variablevector = " << variablevectorpixel << std::endl;
                      std::cerr << "Difference     = " << (expectedvariablevector - variablevectorpixel) << " --> "
                                << varerrornorm << " > " << normTolerance << std::endl;
                      return EXIT_FAILURE;
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  } // for dims[3]...
  return EXIT_SUCCESS;
} // RunTest()

int
itkLinearInterpolateImageFunctionTest(int, char *[])
{
  /* Test separately for images of 1 through 4 dimensions because this function
   * has optimized implementations for dimensionality of 1-3, and unoptimized
   * implementation for 4 and greater. */
  int result = EXIT_SUCCESS;

  std::cout << "***** Testing dimensionality of 1 *****" << std::endl;
  if (RunLinearInterpolateTest<1>() == EXIT_FAILURE)
  {
    result = EXIT_FAILURE;
    std::cout << "Failed for dimensionality 1." << std::endl;
  }
  std::cout << "***** Testing dimensionality of 2 *****" << std::endl;
  if (RunLinearInterpolateTest<2>() == EXIT_FAILURE)
  {
    result = EXIT_FAILURE;
    std::cout << "Failed for dimensionality 2." << std::endl;
  }
  std::cout << "***** Testing dimensionality of 3 *****" << std::endl;
  if (RunLinearInterpolateTest<3>() == EXIT_FAILURE)
  {
    result = EXIT_FAILURE;
    std::cout << "Failed for dimensionality 3." << std::endl;
  }
  std::cout << "***** Testing dimensionality of 4 *****" << std::endl;
  if (RunLinearInterpolateTest<4>() == EXIT_FAILURE)
  {
    result = EXIT_FAILURE;
    std::cout << "Failed for dimensionality 4." << std::endl;
  }
  return result;
}
