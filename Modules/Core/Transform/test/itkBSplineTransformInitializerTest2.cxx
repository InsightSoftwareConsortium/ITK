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

#include "itkImageFileReader.h"

#include "itkBSplineTransform.h"
#include "itkBSplineTransformInitializer.h"

#include "itkPermuteAxesImageFilter.h"

#include "itkObject.h"
#include "itkTestingMacros.h"

#include <fstream>

// This test is meant to demonstrate the transform domain defining issues
// associated with the current B-spline deformable transform initializer
// class. Hans Johnson pointed out that the current initializer class
// uses the image information to construct the B-spline grid in contrast
// to how the image domain resides in physical space. So, for example, if
// a user initilializes a control point grid from a given image and then
// constructs a second control point grid from a permuted version of the
// given image (using the itkPermuteAxesImageFilter class) the second control
// point grid will be oriented completely different from the first image
// even though the image and it's permuted counterpart are situated in physical
// domain precisely the same way.

int itkBSplineTransformInitializerTest2( int argc, char * argv[] )
{

  if( argc < 2 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0] << " fixedImage ";
    return EXIT_FAILURE;
    }

  const   unsigned int ImageDimension = 2;

  typedef unsigned char                             PixelType;
  typedef itk::Image< PixelType, ImageDimension >   FixedImageType;

  typedef itk::ImageFileReader< FixedImageType > FixedReaderType;
  FixedReaderType::Pointer fixedReader = FixedReaderType::New();
  fixedReader->SetFileName( argv[1] );

  try
    {
    fixedReader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // We first use the passed fixed image to construct the control point
  // grid and save the control point locations.

  FixedImageType::ConstPointer fixedImage = fixedReader->GetOutput();

  const unsigned int SpaceDimension = ImageDimension;
  const unsigned int SplineOrder = 3;
  typedef double CoordinateRepType;

  typedef itk::BSplineTransform< CoordinateRepType, SpaceDimension,
    SplineOrder > TransformType;

  TransformType::Pointer bsplineTransform = TransformType::New();

  typedef itk::BSplineTransformInitializer< TransformType, FixedImageType >
    InitializerType;

  InitializerType::Pointer transformInitializer = InitializerType::New();

  EXERCISE_BASIC_OBJECT_METHODS( transformInitializer, BSplineTransformInitializer,
    Object );

  transformInitializer->SetTransform( bsplineTransform );
  TEST_SET_GET_VALUE( bsplineTransform, transformInitializer->GetTransform() );

  transformInitializer->SetImage( fixedImage );
  TEST_SET_GET_VALUE( fixedImage, transformInitializer->GetImage() );

  TransformType::CoefficientImageArray coefficientImages;

  transformInitializer->InitializeTransform();

  TransformType::MeshSizeType meshSize;
  meshSize[0] = 5;
  meshSize[1] = 6;

  bsplineTransform->SetTransformDomainMeshSize( meshSize );

  coefficientImages = bsplineTransform->GetCoefficientImages();

  std::vector<FixedImageType::PointType> controlPointLocations;

  typedef TransformType::ImageType CoefficientImageType;
  itk::ImageRegionIteratorWithIndex<CoefficientImageType> it(
    coefficientImages[0], coefficientImages[0]->GetLargestPossibleRegion() );
  for( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    FixedImageType::PointType point;
    coefficientImages[0]->TransformIndexToPhysicalPoint( it.GetIndex(),
      point );
    controlPointLocations.push_back( point );
    }

  // We permute the fixed image and construct the control point grid using
  // the same grid size. The idea is that since the two images reside in
  // physical space precisely the same way, the two sets of control points should
  // be the same.

  typedef itk::PermuteAxesImageFilter<FixedImageType> PermuterType;
  PermuterType::Pointer permuter = PermuterType::New();
  PermuterType::PermuteOrderArrayType array;

  array[0] = 1;
  array[1] = 0;

  permuter->SetInput( fixedImage );
  permuter->SetOrder( array );
  permuter->Update();

  TransformType::Pointer bsplineTransform2 = TransformType::New();
  InitializerType::Pointer transformInitializer2 = InitializerType::New();

  EXERCISE_BASIC_OBJECT_METHODS( transformInitializer2, BSplineTransformInitializer,
    Object );

  transformInitializer2->SetTransform( bsplineTransform2 );
  transformInitializer2->SetImage( permuter->GetOutput() );

  transformInitializer2->InitializeTransform();

  bsplineTransform2->SetTransformDomainMeshSize( meshSize );
  coefficientImages = bsplineTransform2->GetCoefficientImages();

  std::vector<FixedImageType::PointType> controlPointLocations2;

  itk::ImageRegionIteratorWithIndex<CoefficientImageType> it2(
    coefficientImages[0], coefficientImages[0]->GetLargestPossibleRegion() );
  for( it2.GoToBegin(); !it2.IsAtEnd(); ++it2 )
    {
    FixedImageType::PointType point;
    coefficientImages[0]->TransformIndexToPhysicalPoint( it2.GetIndex(),
      point );
    controlPointLocations2.push_back( point );
    }

  std::vector<FixedImageType::PointType>::const_iterator it3;
  std::vector<FixedImageType::PointType>::const_iterator it4;
  for( it3 = controlPointLocations.begin(), it4 = controlPointLocations2.begin();
    it3 != controlPointLocations.end(); ++it3, ++it4 )
    {
    if( *it3 != *it4 )
      {
      std::cerr << "Control point locations are different." << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}
