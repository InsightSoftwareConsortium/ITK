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
#include "itkMath.h"
#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkNearestNeighborInterpolateImageFunction.h"


int
itkNearestNeighborInterpolateImageFunctionTest(int, char *[])
{
  int result = EXIT_SUCCESS;

  constexpr unsigned int Dimension = 2;
  using PixelType = float;
  constexpr unsigned int VectorDimension = 4;
  using VectorPixelType = itk::Vector<PixelType, VectorDimension>;
  using ImageType = itk::Image<PixelType, Dimension>;
  using VectorImageType = itk::Image<VectorPixelType, Dimension>;
  using VariableVectorImageType = itk::VectorImage<PixelType, Dimension>;
  using VariablePixelType = VariableVectorImageType::PixelType;
  using RegionType = ImageType::RegionType;
  using SizeType = RegionType::SizeType;
  using IndexType = ImageType::IndexType;

  using PointType = itk::Point<float, 2>;

  using CoordRepType = float;
  using InterpolatorType = itk::NearestNeighborInterpolateImageFunction<ImageType, CoordRepType>;
  using VectorInterpolatorType = itk::NearestNeighborInterpolateImageFunction<VectorImageType, CoordRepType>;
  using VariableVectorInterpolatorType =
    itk::NearestNeighborInterpolateImageFunction<VariableVectorImageType, CoordRepType>;

  using InterpolatedVectorType = VectorInterpolatorType::OutputType;
  using InterpolatedVariableVectorType = VariableVectorInterpolatorType::OutputType;

  ImageType::Pointer               image = ImageType::New();
  VectorImageType::Pointer         vectorimage = VectorImageType::New();
  VariableVectorImageType::Pointer variablevectorimage = VariableVectorImageType::New();
  variablevectorimage->SetVectorLength(VectorDimension);

  IndexType start;
  start.Fill(0);

  SizeType size;
  size.Fill(3);

  RegionType region;
  region.SetSize(size);
  region.SetIndex(start);

  image->SetRegions(region);
  image->Allocate();

  vectorimage->SetRegions(region);
  vectorimage->Allocate();

  variablevectorimage->SetRegions(region);
  variablevectorimage->Allocate();

  ImageType::PointType   origin;
  ImageType::SpacingType spacing;

  origin.Fill(0.0);
  spacing.Fill(1.0);

  image->SetOrigin(origin);
  image->SetSpacing(spacing);

  vectorimage->SetOrigin(origin);
  vectorimage->SetSpacing(spacing);

  variablevectorimage->SetOrigin(origin);
  variablevectorimage->SetSpacing(spacing);

  image->Print(std::cout);

  unsigned int maxx = 3;
  unsigned int maxy = 3;

  //
  // Fill up the image values with the function
  //
  //   Intensity = f(x,y) = x + 3 * y
  //
  //
  for (unsigned int y = 0; y < maxy; y++)
  {
    for (unsigned int x = 0; x < maxx; x++)
    {
      IndexType index;
      index[0] = x;
      index[1] = y;

      const PixelType value = x + y * maxx;
      image->SetPixel(index, value);

      VectorPixelType & vectorpixel = vectorimage->GetPixel(index);
      vectorpixel.Fill(value);

      VariablePixelType variablevectorpixel = variablevectorimage->GetPixel(index);
      variablevectorpixel.Fill(value);

      std::cout << value << " ";
    }
    std::cout << std::endl;
  }

  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  interpolator->SetInputImage(image);

  typename ImageType::SizeType radius;
  radius.Fill(0);
  for (unsigned int d = 0; d < Dimension; ++d)
  {
    ITK_TEST_SET_GET_VALUE(radius[d], interpolator->GetRadius()[d]);
  }

  VectorInterpolatorType::Pointer vectorinterpolator = VectorInterpolatorType::New();
  vectorinterpolator->SetInputImage(vectorimage);

  VariableVectorInterpolatorType::Pointer variablevectorinterpolator = VariableVectorInterpolatorType::New();
  variablevectorinterpolator->SetInputImage(variablevectorimage);

  constexpr double incr = 0.1;
  PointType        point;

  for (double yy = 0; yy < static_cast<double>(maxy - 1); yy++)
  {
    for (double xx = 0; xx < static_cast<double>(maxx - 1); xx++)
    {
      for (double yyy = yy; yyy < yy + 1.01; yyy += incr)
      {
        for (double xxx = xx; xxx < xx + 1.01; xxx += incr)
        {
          point[0] = xxx;
          point[1] = yyy;

          if (interpolator->IsInsideBuffer(point))
          {
            auto         expectedX = itk::Math::Round<long, double>(xxx);
            auto         expectedY = itk::Math::Round<long, double>(yyy);
            const double expectedValue = static_cast<double>(expectedX) + 3.0 * static_cast<double>(expectedY);

            // test scalar image
            const double computedValue = interpolator->Evaluate(point);

            if (itk::Math::NotAlmostEquals(expectedValue, computedValue))
            {
              std::cerr << "Error found while computing interpolation " << std::endl;
              std::cerr << "Point = " << point << std::endl;
              std::cerr << "Expected value = " << expectedValue << std::endl;
              std::cerr << "Computed value = " << computedValue << std::endl;
              result = EXIT_FAILURE;
            }

            // test image of vectors
            const InterpolatedVectorType vectorpixel = vectorinterpolator->Evaluate(point);
            const InterpolatedVectorType expectedvector(expectedValue);
            const double                 errornorm = (expectedvector - vectorpixel).GetNorm();

            if (errornorm > 0)
            {
              std::cerr << "Error found while computing vector interpolation " << std::endl;
              std::cerr << "Point = " << point << std::endl;
              std::cerr << "Expected vector = " << expectedvector << std::endl;
              std::cerr << "Computed vector = " << vectorpixel << std::endl;
              result = EXIT_FAILURE;
            }

            // test variable-length-vector image
            const InterpolatedVariableVectorType variablevectorpixel = variablevectorinterpolator->Evaluate(point);

            InterpolatedVariableVectorType expectedvariablevector;
            expectedvariablevector.SetSize(VectorDimension);
            expectedvariablevector.Fill(expectedValue);

            const double varerrornorm = (expectedvariablevector - variablevectorpixel).GetNorm();

            if (varerrornorm > 0)
            {
              std::cerr << "Error found while computing variable vector interpolation " << std::endl;
              std::cerr << "Point = " << point << std::endl;
              std::cerr << "Expected variablevector = " << expectedvariablevector << std::endl;
              std::cerr << "Computed variablevector = " << variablevectorpixel << std::endl;
              result = EXIT_FAILURE;
            }
          }
        }
      }
    }
  }

  return result;
}
