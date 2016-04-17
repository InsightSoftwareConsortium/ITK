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

#include "itkAffineTransform.h"
#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor.h"
#include "itkBSplineSyNImageRegistrationMethod.h"
#include "itkEuclideanDistancePointSetToPointSetMetricv4.h"

int itkBSplineSyNPointSetRegistrationTest( int itkNotUsed( argc ), char * itkNotUsed( argv )[] )
{
  const unsigned int Dimension = 2;

  typedef itk::PointSet<unsigned int, Dimension> PointSetType;

  typedef itk::EuclideanDistancePointSetToPointSetMetricv4<PointSetType> PointSetMetricType;
  PointSetMetricType::Pointer metric = PointSetMetricType::New();

  typedef PointSetMetricType::FixedPointSetType    PointSetType;
  typedef PointSetType::PointType                  PointType;

  typedef double                           PixelType;
  typedef itk::Image<PixelType, Dimension> FixedImageType;
  typedef itk::Image<PixelType, Dimension> MovingImageType;


  PointSetType::Pointer fixedPoints = PointSetType::New();
  fixedPoints->Initialize();

  PointSetType::Pointer movingPoints = PointSetType::New();
  movingPoints->Initialize();

  // two circles with a small offset
  PointType offset;
  for( unsigned int d=0; d < PointSetType::PointDimension; d++ )
    {
    offset[d] = 2.0;
    }
  unsigned long count = 0;
  for( float theta = 0; theta < 2.0 * itk::Math::pi; theta += 0.1 )
    {
    unsigned int label = static_cast<unsigned int>( 1.5 + count / 100 );

    PointType fixedPoint;
    float radius = 100.0;
    fixedPoint[0] = radius * std::cos( theta );
    fixedPoint[1] = radius * std::sin( theta );
    if( PointSetType::PointDimension > 2 )
      {
      fixedPoint[2] = radius * std::sin( theta );
      }
    fixedPoints->SetPoint( count, fixedPoint );
    fixedPoints->SetPointData( count, label );

    PointType movingPoint;
    movingPoint[0] = fixedPoint[0] + offset[0];
    movingPoint[1] = fixedPoint[1] + offset[1];
    if( PointSetType::PointDimension > 2 )
      {
      movingPoint[2] = fixedPoint[2] + offset[2];
      }
    movingPoints->SetPoint( count, movingPoint );
    movingPoints->SetPointData( count, label );

    count++;
    }

  // virtual image domain is [-110,-110]  [110,110]

  FixedImageType::SizeType fixedImageSize;
  FixedImageType::PointType fixedImageOrigin;
  FixedImageType::DirectionType fixedImageDirection;
  FixedImageType::SpacingType fixedImageSpacing;

  fixedImageSize.Fill( 221 );
  fixedImageOrigin.Fill( -110 );
  fixedImageDirection.SetIdentity();
  fixedImageSpacing.Fill( 1 );

  FixedImageType::Pointer fixedImage = FixedImageType::New();
  fixedImage->SetRegions( fixedImageSize );
  fixedImage->SetOrigin( fixedImageOrigin );
  fixedImage->SetDirection( fixedImageDirection );
  fixedImage->SetSpacing( fixedImageSpacing );
  fixedImage->Allocate();

  typedef itk::AffineTransform<double, PointSetType::PointDimension> AffineTransformType;
  AffineTransformType::Pointer transform = AffineTransformType::New();
  transform->SetIdentity();

  metric->SetFixedPointSet( fixedPoints );
  metric->SetMovingPointSet( movingPoints );
  metric->SetVirtualDomainFromImage( fixedImage );
  metric->SetMovingTransform( transform );
  metric->Initialize();

  // Create the SyN deformable registration method

  typedef itk::Vector<double, Dimension> VectorType;
  VectorType zeroVector( 0.0 );

  typedef itk::Image<VectorType, Dimension> DisplacementFieldType;
  DisplacementFieldType::Pointer displacementField = DisplacementFieldType::New();
  displacementField->CopyInformation( fixedImage );
  displacementField->SetRegions( fixedImage->GetBufferedRegion() );
  displacementField->Allocate();
  displacementField->FillBuffer( zeroVector );

  DisplacementFieldType::Pointer inverseDisplacementField = DisplacementFieldType::New();
  inverseDisplacementField->CopyInformation( fixedImage );
  inverseDisplacementField->SetRegions( fixedImage->GetBufferedRegion() );
  inverseDisplacementField->Allocate();
  inverseDisplacementField->FillBuffer( zeroVector );

  typedef itk::BSplineSyNImageRegistrationMethod<FixedImageType, MovingImageType> DisplacementFieldRegistrationType;
  DisplacementFieldRegistrationType::Pointer displacementFieldRegistration = DisplacementFieldRegistrationType::New();

  typedef DisplacementFieldRegistrationType::OutputTransformType OutputTransformType;
  OutputTransformType::Pointer outputTransform = OutputTransformType::New();
  outputTransform->SetDisplacementField( displacementField );
  outputTransform->SetInverseDisplacementField( inverseDisplacementField );

  displacementFieldRegistration->SetInitialTransform( outputTransform );
  displacementFieldRegistration->InPlaceOn();

  typedef itk::BSplineSmoothingOnUpdateDisplacementFieldTransformParametersAdaptor<OutputTransformType> DisplacementFieldTransformAdaptorType;
  DisplacementFieldRegistrationType::TransformParametersAdaptorsContainerType adaptors;

  OutputTransformType::ArrayType updateMeshSize;
  OutputTransformType::ArrayType totalMeshSize;
  for( unsigned int d = 0; d < Dimension; d++ )
    {
    updateMeshSize[d] = 10;
    totalMeshSize[d] = 0;
    }

  // Create the transform adaptors
  // For the gaussian displacement field, the specified variances are in image spacing terms
  // and, in normal practice, we typically don't change these values at each level.  However,
  // if the user wishes to add that option, they can use the class
  // GaussianSmoothingOnUpdateDisplacementFieldTransformAdaptor

  unsigned int numberOfLevels = 3;

  DisplacementFieldRegistrationType::NumberOfIterationsArrayType numberOfIterationsPerLevel;
  numberOfIterationsPerLevel.SetSize( 3 );
  numberOfIterationsPerLevel[0] = 1;
  numberOfIterationsPerLevel[1] = 1;
  numberOfIterationsPerLevel[2] = 50;

  DisplacementFieldRegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize( 3 );
  shrinkFactorsPerLevel.Fill( 1 );

  DisplacementFieldRegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize( 3 );
  smoothingSigmasPerLevel.Fill( 0 );

  for( unsigned int level = 0; level < numberOfLevels; level++ )
    {
    // We use the shrink image filter to calculate the fixed parameters of the virtual
    // domain at each level.  To speed up calculation and avoid unnecessary memory
    // usage, we could calculate these fixed parameters directly.

    typedef itk::ShrinkImageFilter<DisplacementFieldType, DisplacementFieldType> ShrinkFilterType;
    ShrinkFilterType::Pointer shrinkFilter = ShrinkFilterType::New();
    shrinkFilter->SetShrinkFactors( shrinkFactorsPerLevel[level] );
    shrinkFilter->SetInput( displacementField );
    shrinkFilter->Update();

    DisplacementFieldTransformAdaptorType::Pointer fieldTransformAdaptor = DisplacementFieldTransformAdaptorType::New();
    fieldTransformAdaptor->SetRequiredSpacing( shrinkFilter->GetOutput()->GetSpacing() );
    fieldTransformAdaptor->SetRequiredSize( shrinkFilter->GetOutput()->GetBufferedRegion().GetSize() );
    fieldTransformAdaptor->SetRequiredDirection( shrinkFilter->GetOutput()->GetDirection() );
    fieldTransformAdaptor->SetRequiredOrigin( shrinkFilter->GetOutput()->GetOrigin() );
    fieldTransformAdaptor->SetTransform( outputTransform );

    // A good heuristic is to double the b-spline mesh resolution at each level
    OutputTransformType::ArrayType newUpdateMeshSize = updateMeshSize;
    OutputTransformType::ArrayType newTotalMeshSize = totalMeshSize;
    for( unsigned int d = 0; d < Dimension; d++ )
      {
      newUpdateMeshSize[d] = newUpdateMeshSize[d] << ( level );
      newTotalMeshSize[d] = newTotalMeshSize[d] << ( level );
      }
    fieldTransformAdaptor->SetMeshSizeForTheUpdateField( newUpdateMeshSize );
    fieldTransformAdaptor->SetMeshSizeForTheTotalField( newTotalMeshSize );

    adaptors.push_back( fieldTransformAdaptor.GetPointer() );
    }

  displacementFieldRegistration->SetFixedPointSet( fixedPoints );
  displacementFieldRegistration->SetMovingPointSet( movingPoints );
  displacementFieldRegistration->SetNumberOfLevels( 3 );
  displacementFieldRegistration->SetMovingInitialTransform( transform );
  displacementFieldRegistration->SetShrinkFactorsPerLevel( shrinkFactorsPerLevel );
  displacementFieldRegistration->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel );
  displacementFieldRegistration->SetMetric( metric );
  displacementFieldRegistration->SetLearningRate( 0.25 );
  displacementFieldRegistration->SetNumberOfIterationsPerLevel( numberOfIterationsPerLevel );
  displacementFieldRegistration->SetTransformParametersAdaptorsPerLevel( adaptors );

  outputTransform->SetDisplacementField( displacementField );
  outputTransform->SetInverseDisplacementField( inverseDisplacementField );
  displacementFieldRegistration->SetInitialTransform( outputTransform );
  displacementFieldRegistration->InPlaceOn();

  try
    {
    std::cout << "B-spline SyN point set registration" << std::endl;
    displacementFieldRegistration->Update();
    }
  catch( itk::ExceptionObject &e )
    {
    std::cerr << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
    }

  // applying the resultant transform to moving points and verify result
  std::cout << "Fixed\tMoving\tMovingTransformed\tFixedTransformed\tDiff" << std::endl;
  PointType::ValueType tolerance = 0.01;

  float averageError = 0.0;
  for( unsigned int n = 0; n < movingPoints->GetNumberOfPoints(); n++ )
    {
    // compare the points in virtual domain
    PointType transformedMovingPoint =
      displacementFieldRegistration->GetModifiableTransform()->GetInverseTransform()->TransformPoint( movingPoints->GetPoint( n ) );
    PointType fixedPoint = fixedPoints->GetPoint( n );
    PointType transformedFixedPoint = displacementFieldRegistration->GetModifiableTransform()->TransformPoint( fixedPoints->GetPoint( n ) );
    PointType difference;
    difference[0] = transformedMovingPoint[0] - fixedPoint[0];
    difference[1] = transformedMovingPoint[1] - fixedPoint[1];
    std::cout << fixedPoints->GetPoint( n ) << "\t" << movingPoints->GetPoint( n )
          << "\t" << transformedMovingPoint << "\t" << transformedFixedPoint << "\t" << difference << std::endl;

    averageError += ( ( difference.GetVectorFromOrigin() ).GetSquaredNorm() );
    }

  unsigned int numberOfPoints = movingPoints->GetNumberOfPoints();
  if( numberOfPoints > 0 )
    {
    averageError /= static_cast<float>( numberOfPoints );
    std::cout << "Average error: " << averageError << std::endl;
    if( averageError > tolerance )
      {
      std::cerr << "Results do not match truth within tolerance." << std::endl;
      return EXIT_FAILURE;
      }
    }
  else
    {
    std::cerr << "No points." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
