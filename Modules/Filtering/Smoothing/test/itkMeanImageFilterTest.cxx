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

#include "itkRandomImageSource.h"
#include "itkMeanImageFilter.h"
#include "itkTextOutput.h"
#include "itkTestingMacros.h"

int itkMeanImageFilterTest( int, char* [] )
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  const unsigned int Dimension = 2;
  typedef float PixelType;

  typedef itk::Image< PixelType, Dimension > FloatImage2DType;

  itk::RandomImageSource<FloatImage2DType>::Pointer random;
  random = itk::RandomImageSource< FloatImage2DType >::New();
  random->SetMin( 0.0 );
  random->SetMax( 1000.0 );

  FloatImage2DType::SizeValueType randomSize[2];
  randomSize[0] = randomSize[1] = 8;
  random->SetSize( randomSize );

  FloatImage2DType::SpacingValueType spacing[2] = {0.7, 2.1};
  random->SetSpacing( spacing );

  FloatImage2DType::PointValueType origin[2] = {15, 400};
  random->SetOrigin( origin );

  // Create a mean image
  itk::MeanImageFilter< FloatImage2DType, FloatImage2DType >::Pointer mean =
    itk::MeanImageFilter< FloatImage2DType, FloatImage2DType >::New();

  EXERCISE_BASIC_OBJECT_METHODS( mean, MeanImageFilter, BoxImageFilter );

  TRY_EXPECT_NO_EXCEPTION( random->Update() );

  mean->SetInput( random->GetOutput() );

  // Define the neighborhood size used for the mean filter (5x5)
  FloatImage2DType::SizeType neighRadius;
  neighRadius[0] = 1;
  neighRadius[1] = 1;
  mean->SetRadius( neighRadius );
  TEST_SET_GET_VALUE( neighRadius, mean->GetRadius() );

  TRY_EXPECT_NO_EXCEPTION( mean->Update() );

  itk::ImageRegionIterator< FloatImage2DType > it;
  it = itk::ImageRegionIterator< FloatImage2DType >( random->GetOutput(),
                               random->GetOutput()->GetBufferedRegion() );
  std::cout << "Input image" << std::endl;
  unsigned int i;
  for ( i = 1; !it.IsAtEnd(); ++i, ++it )
    {
    std::cout << "\t" << it.Get();
    if ((i % 8) == 0)
      {
      std::cout << std::endl;
      }
    }

  std::cout << "Output image" << std::endl;
  it = itk::ImageRegionIterator< FloatImage2DType >( mean->GetOutput(),
                               mean->GetOutput()->GetBufferedRegion() );
  for ( i = 1; !it.IsAtEnd(); ++i, ++it )
    {
    std::cout << "\t" << it.Get();
    if ((i % 8) == 0)
      {
      std::cout << std::endl;
      }
    }

  return EXIT_SUCCESS;
}
