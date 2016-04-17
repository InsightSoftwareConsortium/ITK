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


#include "itkMedianImageFunction.h"

int itkMedianImageFunctionTest(int, char* [] )
{

  const unsigned int Dimension = 3;
  typedef unsigned char   PixelType;

  typedef itk::Image< PixelType, Dimension >    ImageType;
  typedef itk::MedianImageFunction< ImageType > FunctionType;

  // Create and allocate the image
  ImageType::Pointer      image = ImageType::New();
  ImageType::SizeType     size;
  ImageType::IndexType    start;
  ImageType::RegionType   region;
  const int sizeDim(50);
  const int centerIndex(sizeDim/2);
  size[0] = 50;
  size[1] = 50;
  size[2] = 50;
  start.Fill( 0 );

  region.SetIndex( start );
  region.SetSize( size );

  image->SetRegions( region );
  image->Allocate();

  ImageType::PixelType initialValue = 27;

  image->FillBuffer( initialValue );

  FunctionType::Pointer function = FunctionType::New();

  function->SetInputImage( image );

  ImageType::IndexType    index;

  index[0] = centerIndex;
  index[1] = centerIndex;
  index[2] = centerIndex;

  FunctionType::OutputType  median;

  median = function->EvaluateAtIndex( index );
  std::cout << "function->EvaluateAtIndex( index ): "
            << static_cast<itk::NumericTraits<FunctionType::OutputType>::PrintType>(median)
            << std::endl;

  // Test Evaluate
  FunctionType::PointType point;
  point[0] = centerIndex;
  point[1] = centerIndex;
  point[2] = centerIndex;
  FunctionType::OutputType median2;
  median2 = function->Evaluate(point);
  std::cout << "function->Evaluate(point): "
            << static_cast<itk::NumericTraits<FunctionType::OutputType>::PrintType>(median2)
            << std::endl;

  // Test EvaluateAtContinuousIndex
  FunctionType::ContinuousIndexType cindex;
  cindex[0] = centerIndex;
  cindex[1] = centerIndex;
  cindex[2] = centerIndex;
  FunctionType::OutputType median3;
  median3 = function->EvaluateAtContinuousIndex(cindex);
  std::cout << "function->EvaluateAtContinuousIndex(cindex): "
            << static_cast<itk::NumericTraits<FunctionType::OutputType>::PrintType>(median3)
            << std::endl;

  // since the input image is constant
  // the should be equal to the initial value
  if( itk::Math::abs( initialValue - median ) > 10e-7 )
    {
    std::cerr << "Error in mean computation" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED ! " << std::endl;
  //
  std::cout << "Test setting the neighborhood radius" << std::endl;

  // first, put something in the neighborhood outside the current
  // neighborhood that will change the median result
  unsigned char voxelval(28);
  ImageType::IndexType    index2;
  for(index2[0] = centerIndex-2; index2[0] < centerIndex+3; index2[0]++)
    {
    for(index2[1] = centerIndex-2; index2[1] < centerIndex+3; index2[1]++)
      {
      for(index2[2] = centerIndex-2; index2[2] < centerIndex+3; index2[2]++)
        {
        // don't change voxels inside default neighborhood
        if((index2[0] < centerIndex - 1 || index2[0] > centerIndex + 1) ||
           (index2[1] < centerIndex - 1 || index2[1] > centerIndex + 1) ||
           (index2[2] < centerIndex - 1 || index2[2] > centerIndex + 1))
          {
          image->SetPixel(index2,voxelval);
          voxelval++;
          }
        }
      }
    }

  // check median with default neighborhood
  index[2] = index[1] = index[0] = centerIndex;
  median = function->EvaluateAtIndex( index );
  std::cout << "function->EvaluateAtIndex( index ): "
            << static_cast<itk::NumericTraits<FunctionType::OutputType>::PrintType>(median)
            << std::endl;

  // since the input image is constant
  // the should be equal to the initial value
  if( itk::Math::abs( initialValue - median ) > 10e-7 )
    {
    std::cerr << "Error in mean computation" << std::endl;
    return EXIT_FAILURE;
    }

  // now set the radius
  function->SetNeighborhoodRadius(2);
  median = function->EvaluateAtIndex( index );
  std::cout << "function->EvaluateAtIndex( index ), neighborhood radius 2: "
            << static_cast<itk::NumericTraits<FunctionType::OutputType>::PrintType>(median)
            << std::endl;
  // Since we've changed the image outside the default neighborhood
  // for the MedianImageFunction, it would be an error for the median
  // to be the same
  if( itk::Math::abs( initialValue - median ) < 10e-7 )
    {
    std::cerr << "Error in mean computation" << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}
