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

#include "itkPolyLineParametricPath.h"
#include "itkPathToImageFilter.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


int itkPathToImageFilterTest( int, char* [] )
{
  const unsigned int Dimension = 2;
  typedef double PixelType;

  typedef itk::Image< PixelType, Dimension >                              ImageType;
  typedef itk::PolyLineParametricPath< Dimension >                        PolyLineParametricPathType;
  typedef PolyLineParametricPathType::VertexType                          VertexType;
  typedef itk::PathToImageFilter< PolyLineParametricPathType, ImageType >
    PathToImageFilterType;


  // Set up the path
  PolyLineParametricPathType::Pointer path = PolyLineParametricPathType::New();

  EXERCISE_BASIC_OBJECT_METHODS( path, PolyLineParametricPath, ParametricPath );

  std::cout << "Making a square Path with v0 at (30,30) and v2 at (33,33)" << std::endl;
  VertexType v;
  v.Fill( 30 );
  path->AddVertex( v );
  v[0] = 33;
  v[1] = 30;
  path->AddVertex( v );
  v.Fill( 33 );
  path->AddVertex( v );
  v[0] = 30;
  v[1] = 33;
  path->AddVertex( v );
  v.Fill( 30 );
  path->AddVertex( v );


  PathToImageFilterType::Pointer pathToImageFilter =
    PathToImageFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( pathToImageFilter, PathToImageFilter,
    ImageSource );

  pathToImageFilter->SetInput( path );

  PathToImageFilterType::ValueType pathValue = 1;
  pathToImageFilter->SetPathValue( pathValue );
  TEST_SET_GET_VALUE( pathValue, pathToImageFilter->GetPathValue() );

  PathToImageFilterType::ValueType backgroundValue = 0;
  pathToImageFilter->SetBackgroundValue( backgroundValue );
  TEST_SET_GET_VALUE( backgroundValue, pathToImageFilter->GetBackgroundValue() );

  ImageType::SizeType size;
  size[0] = 256;
  size[1] = 256;
  pathToImageFilter->SetSize( size );
  TEST_SET_GET_VALUE( size, pathToImageFilter->GetSize() );

  // Test spacing
  //
  double pathImageSpacing[2];
  for( unsigned int i = 0; i < ImageType::ImageDimension; ++i )
  {
    pathImageSpacing[i] = 1.0;
  }
  pathToImageFilter->SetSpacing( pathImageSpacing );
  const double* spacing_result = pathToImageFilter->GetSpacing();

  for( unsigned int i = 0; i < ImageType::ImageDimension; ++i )
  {
    if( spacing_result[i] != 1.0 )
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }

  // Test origin
  //
  double origin_double[2];
  for( unsigned int i = 0; i < 2; ++i )
  {
    origin_double[i] = 0.0;
  }
  pathToImageFilter->SetOrigin(origin_double);
  const double* origin_result = pathToImageFilter->GetOrigin();

  for( unsigned int i = 0; i < ImageType::ImageDimension; ++i )
  {
    if( origin_result[i] != 0.0 )
    {
      std::cout << "[FAILURE]" << std::endl;
      return EXIT_FAILURE;
    }
  }

  // Update the filter
  TRY_EXPECT_NO_EXCEPTION( pathToImageFilter->Update() );

  ImageType::Pointer image = pathToImageFilter->GetOutput();

  // Test the output image
  //

  ImageType::IndexType index;

  // Test only pixels on or in the path
  for( int i = 0; i <= 3; ++i )
    {
    for( int j = 0; j <= 3; ++j )
      {
      double targetValue;

      index[0] = 30 + i;
      index[1] = 30 + j;

      if( 0 < i && i < 3 && 0 < j && j < 3 )
        {
        // Inside the closed path, but not on it
        targetValue = 0;
        }
      else
        {
        // On the path
        targetValue=1;
        }
      if( itk::Math::NotAlmostEquals( image->GetPixel(index), targetValue) )
        {
        std::cout << "[FAILURE]" << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  std::cout << "Test finished" << std::endl;

  return EXIT_SUCCESS;
}
