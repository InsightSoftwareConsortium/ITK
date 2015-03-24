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

#include "itkConstantBoundaryCondition.h"
#include "itkPadImageFilter.h"

int itkPadImageFilterTest( int, char* [] )
{
  typedef itk::Image< short, 2 >    ShortImage;
  typedef ShortImage::SizeType      SizeType;
  typedef ShortImage::SizeValueType SizeValueType;

  typedef itk::PadImageFilter< ShortImage, ShortImage > FilterType;
  FilterType::Pointer padFilter = FilterType::New();

  SizeType lowerBound = {{1, 2}};
  SizeType upperBound = {{3, 4}};
  SizeValueType lowerBoundArray[2] = {1, 2};
  SizeValueType upperBoundArray[2] = {3, 4};

  padFilter->SetPadLowerBound( lowerBound );
  if ( padFilter->GetPadLowerBound() != lowerBound )
    {
    std::cerr << "Lower bound not recovered while testing SetLowerBound(SizeType). Got "
              << padFilter->GetPadLowerBound() << ", expected " << lowerBound << std::endl;
    return EXIT_FAILURE;
    }

  padFilter->SetPadLowerBound( lowerBoundArray );
  if ( padFilter->GetPadLowerBound() != lowerBound )
    {
    std::cerr << "Lower bound not recovered while testing SetLowerBound(SizeValueType[]). Got "
              << padFilter->GetPadLowerBound() << ", expected " << lowerBound << std::endl;
    return EXIT_FAILURE;
    }

  padFilter->SetPadUpperBound( upperBound );
  if ( padFilter->GetPadUpperBound() != upperBound )
    {
    std::cerr << "Upper bound not recovered while testing SetUpperBound(SizeType). Got "
              << padFilter->GetPadUpperBound() << ", expected " << upperBound << std::endl;
    return EXIT_FAILURE;
    }

  padFilter->SetPadUpperBound( upperBoundArray );
  if ( padFilter->GetPadUpperBound() != upperBound )
    {
    std::cerr << "Upper bound not recovered while testing SetUpperBound(SizeValueType[]). Got "
              << padFilter->GetPadUpperBound() << ", expected " << upperBound << std::endl;
    return EXIT_FAILURE;
    }

  padFilter->SetPadBound( lowerBound );
  if ( padFilter->GetPadLowerBound() != lowerBound )
    {
    std::cerr << "Lower bound not recovered while testing SetPadBound(). Got "
              << padFilter->GetPadLowerBound() << ", expected " << lowerBound << std::endl;
    return EXIT_FAILURE;
    }

  if ( padFilter->GetPadUpperBound() != lowerBound )
    {
    std::cerr << "Upper bound not recovered while testing SetPadBound(). Got "
              << padFilter->GetPadUpperBound() << ", expected " << lowerBound << std::endl;
    return EXIT_FAILURE;
    }

  if ( padFilter->GetBoundaryCondition() != ITK_NULLPTR )
    {
    std::cerr << "Default BoundaryCondition was not ITK_NULLPTR." << std::endl;
    return EXIT_FAILURE;
    }

  itk::ConstantBoundaryCondition< ShortImage > bc;
  bc.SetConstant( 13 );

  padFilter->SetBoundaryCondition( &bc );
  if ( padFilter->GetBoundaryCondition() != &bc )
    {
    std::cerr << "Set/GetBoundaryCondition test failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Fill in a test image
  ShortImage::Pointer    inputImage = ShortImage::New();
  ShortImage::SizeType   inputSize  = {{1, 1}};
  ShortImage::RegionType inputRegion( inputSize );
  inputImage->SetRegions( inputRegion );
  inputImage->Allocate();
  inputImage->FillBuffer( 1 );

  lowerBound.Fill( 2 );
  padFilter->SetPadBound( lowerBound );

  padFilter->SetInput( inputImage );
  padFilter->UpdateLargestPossibleRegion();

  // Checkout output values
  ShortImage::Pointer output = padFilter->GetOutput();
  for (int j = -2; j <= 2; j++)
    {
    for (int i = -2; i <= 2; i++)
      {
      ShortImage::IndexType index;
      index[0] = i; index[1] = j;

      short pixel = output->GetPixel( index );
      std::cout << pixel << " ";
      if ( index[0] == 0 && index[1] == 0 )
        {
        if ( pixel != 1 )
          {
          std::cerr << "Center pixel has unexpected value " << pixel
                    << ", expected " << 1 << std::endl;
          return EXIT_FAILURE;
          }
        }
      else
        {
        if ( pixel != 13 )
          {
          std::cerr << "Padded pixel at index " << index << " has unexpected value "
                    << pixel << ", expected " << 13 << std::endl;
          return EXIT_FAILURE;
          }
        }
      }
    std::cout << std::endl;
    }

  // Set the boundary condition back to ITK_NULLPTR and check that exception
  // is thrown.
  std::cout << "Testing that exception is thrown when boundary condition is ITK_NULLPTR." << std::endl;
  padFilter->SetBoundaryCondition( ITK_NULLPTR );
  try
    {
    padFilter->Update();
    std::cerr << "Failed to catch expected exception when boundary condition is ITK_NULLPTR." << std::endl;
    return EXIT_FAILURE;
    }
  catch ( itk::ExceptionObject & e )
    {
    std::cout << "Caught expected exception when boundary condition is ITK_NULLPTR." << std::endl;
    std::cout << e << std::endl;
    }
  catch (...)
    {
    std::cerr << "Exception caught, but not the expected kind." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
