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

#include <iostream>

#include "itkNormalizeToConstantImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkFilterWatcher.h"

int itkNormalizeToConstantImageFilterTest( int, char* [] )
{
  std::cout << "itkNormalizeToConstantImageFilterTest Start" << std::endl;

  typedef itk::Image< int, 3 >    IntImage;
  typedef itk::Image< double, 3 > DoubleImage;

  // Generate a real image
  typedef itk::RandomImageSource< IntImage > SourceType;
  SourceType::Pointer source = SourceType::New();

  IntImage::SizeValueType randomSize[3] = {18, 17, 67};

  source->SetSize( randomSize );

  IntImage::PixelType minValue = 0;
  IntImage::PixelType maxValue = 1000;
  source->SetMin( minValue );
  source->SetMax( maxValue );

  typedef itk::NormalizeToConstantImageFilter< IntImage, DoubleImage >
    NormalizeType;
  NormalizeType::Pointer normalize = NormalizeType::New();

  DoubleImage::PixelType constant = 1.0;
  normalize->SetConstant( constant );

  FilterWatcher watch(normalize, "NormalizeToConstant");

  normalize->SetInput( source->GetOutput() );
  normalize->Update();

  typedef itk::ImageRegionConstIterator< DoubleImage > IteratorType;
  IteratorType it( normalize->GetOutput(),
                   normalize->GetOutput()->GetLargestPossibleRegion() );

  float sum = 0.0;
  for ( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    sum += it.Value();
    }

  if ( vnl_math_abs( constant - sum ) > 1e-5 )
    {
    std::cout << "First sum (" << sum << ") does not equal constant ("
              << constant << ")" << std::endl;
    return EXIT_FAILURE;
    }

  constant = 134.2;
  normalize->SetConstant( constant );
  normalize->Update();

  sum = 0.0;
  for ( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    sum += it.Value();
    }

  if ( vnl_math_abs( constant - sum ) > 1e-3 )
    {
    std::cout << "Second sum (" << sum << ") does not equal constant ("
              << constant << ")" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
