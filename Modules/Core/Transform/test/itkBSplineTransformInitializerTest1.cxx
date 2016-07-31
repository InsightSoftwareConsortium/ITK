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
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"

#include "itkBSplineTransform.h"
#include "itkBSplineTransformInitializer.h"

#include "itkObject.h"
#include "itkTestingMacros.h"

#include <fstream>

int itkBSplineTransformInitializerTest1( int argc, char * argv[] )
{

  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " coefficientsFile fixedImage";
    std::cerr << " movingImage deformedMovingImage" << std::endl;
    std::cerr << " [deformationField]" << std::endl;
    return EXIT_FAILURE;
    }

  const   unsigned int ImageDimension = 2;

  typedef unsigned char                         PixelType;
  typedef itk::Image<PixelType, ImageDimension> FixedImageType;
  typedef itk::Image<PixelType, ImageDimension> MovingImageType;

  typedef itk::ImageFileReader<FixedImageType>  FixedReaderType;
  typedef itk::ImageFileReader<MovingImageType> MovingReaderType;

  typedef itk::ImageFileWriter<MovingImageType> MovingWriterType;

  FixedReaderType::Pointer fixedReader = FixedReaderType::New();
  fixedReader->SetFileName( argv[2] );

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

  MovingReaderType::Pointer movingReader = MovingReaderType::New();
  MovingWriterType::Pointer movingWriter = MovingWriterType::New();

  movingReader->SetFileName( argv[3] );
  movingWriter->SetFileName( argv[4] );

  FixedImageType::ConstPointer fixedImage = fixedReader->GetOutput();

  typedef itk::ResampleImageFilter< MovingImageType, FixedImageType >
    FilterType;

  FilterType::Pointer resampler = FilterType::New();

  typedef itk::LinearInterpolateImageFunction<
    MovingImageType, double>  InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  resampler->SetInterpolator( interpolator );

  FixedImageType::SpacingType   fixedSpacing   = fixedImage->GetSpacing();
  FixedImageType::PointType     fixedOrigin    = fixedImage->GetOrigin();
  FixedImageType::DirectionType fixedDirection = fixedImage->GetDirection();

  resampler->SetOutputSpacing( fixedSpacing );
  resampler->SetOutputOrigin( fixedOrigin );
  resampler->SetOutputDirection( fixedDirection );

  FixedImageType::RegionType fixedRegion = fixedImage->GetBufferedRegion();
  FixedImageType::SizeType   fixedSize   = fixedRegion.GetSize();
  resampler->SetSize( fixedSize );
  resampler->SetOutputStartIndex(  fixedRegion.GetIndex() );

  resampler->SetInput( movingReader->GetOutput() );

  movingWriter->SetInput( resampler->GetOutput() );

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

  TransformType::MeshSizeType meshSize;
  meshSize.Fill( 4 );

  transformInitializer->SetTransform( bsplineTransform );
  TEST_SET_GET_VALUE( bsplineTransform, transformInitializer->GetTransform() );

  transformInitializer->SetImage( fixedImage );
  TEST_SET_GET_VALUE( fixedImage, transformInitializer->GetImage() );

  transformInitializer->SetTransformDomainMeshSize( meshSize );
  TEST_SET_GET_VALUE( meshSize, transformInitializer->GetTransformDomainMeshSize() );

  transformInitializer->InitializeTransform();

  typedef TransformType::ParametersType ParametersType;

  const unsigned int numberOfParameters =
    bsplineTransform->GetNumberOfParameters();

  const unsigned int numberOfNodes = numberOfParameters / SpaceDimension;

  ParametersType parameters( numberOfParameters );

  std::ifstream infile;

  infile.open( argv[1] );
  for( unsigned int n = 0; n < numberOfNodes; ++n )
    {
    infile >> parameters[n];
    infile >> parameters[n + numberOfNodes];
    }
  infile.close();

  bsplineTransform->SetParameters( parameters );

  resampler->SetTransform( bsplineTransform );

  try
    {
    movingWriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Vector<float, ImageDimension>     VectorType;
  typedef itk::Image<VectorType, ImageDimension> DeformationFieldType;

  DeformationFieldType::Pointer field = DeformationFieldType::New();
  field->SetRegions( fixedRegion );
  field->SetOrigin( fixedOrigin );
  field->SetSpacing( fixedSpacing );
  field->SetDirection( fixedDirection );
  field->Allocate();

  typedef itk::ImageRegionIterator<DeformationFieldType> FieldIterator;
  FieldIterator fi( field, fixedRegion );

  fi.GoToBegin();

  TransformType::InputPointType   fixedPoint;
  TransformType::OutputPointType  movingPoint;
  TransformType::JacobianType     jacobian;
  DeformationFieldType::IndexType index;

  VectorType displacement;

  while( !fi.IsAtEnd() )
    {
    index = fi.GetIndex();
    field->TransformIndexToPhysicalPoint( index, fixedPoint );
    movingPoint = bsplineTransform->TransformPoint( fixedPoint );
    bsplineTransform->ComputeJacobianWithRespectToParameters( fixedPoint, jacobian );
    displacement = movingPoint - fixedPoint;
    fi.Set( displacement );
    ++fi;
    }

  typedef itk::ImageFileWriter<DeformationFieldType> FieldWriterType;
  FieldWriterType::Pointer fieldWriter = FieldWriterType::New();

  fieldWriter->SetInput( field );

  if( argc >= 6 )
    {
    fieldWriter->SetFileName( argv[5] );
    try
      {
      fieldWriter->Update();
      }
    catch( itk::ExceptionObject & excp )
      {
      std::cerr << "Exception thrown " << std::endl;
      std::cerr << excp << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}
