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

#include "itkTimeVaryingBSplineVelocityFieldImageRegistrationMethod.h"

#include "itkAffineTransform.h"
#include "itkEuclideanDistancePointSetToPointSetMetricv4.h"
#include "itkTimeVaryingBSplineVelocityFieldTransformParametersAdaptor.h"
#include "itkVector.h"

int itkTimeVaryingBSplineVelocityFieldPointSetRegistrationTest( int itkNotUsed( argc ), char * itkNotUsed( argv )[] )
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
  for( unsigned int d = 0; d < Dimension; d++ )
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

  typedef itk::AffineTransform<double, Dimension> AffineTransformType;
  AffineTransformType::Pointer transform = AffineTransformType::New();
  transform->SetIdentity();

  metric->SetFixedPointSet( fixedPoints );
  metric->SetMovingPointSet( movingPoints );
  metric->SetVirtualDomainFromImage( fixedImage );
  metric->SetMovingTransform( transform );
  metric->Initialize();

  // Create the deformable registration method

  typedef itk::TimeVaryingBSplineVelocityFieldImageRegistrationMethod<FixedImageType, MovingImageType> VelocityFieldRegistrationType;
  VelocityFieldRegistrationType::Pointer velocityFieldRegistration = VelocityFieldRegistrationType::New();

  typedef VelocityFieldRegistrationType::OutputTransformType OutputTransformType;
  OutputTransformType::Pointer outputTransform = OutputTransformType::New();
  velocityFieldRegistration->SetInitialTransform( outputTransform );
  velocityFieldRegistration->InPlaceOn();

  velocityFieldRegistration->SetFixedPointSet( fixedPoints );
  velocityFieldRegistration->SetMovingPointSet( movingPoints );
  velocityFieldRegistration->SetNumberOfLevels( 3 );
  velocityFieldRegistration->SetMovingInitialTransform( transform );
  velocityFieldRegistration->SetMetric( metric );
  velocityFieldRegistration->SetLearningRate( 0.25 );
  velocityFieldRegistration->SetNumberOfTimePointSamples(3);
  outputTransform->SetSplineOrder( 3 );
  outputTransform->SetLowerTimeBound( 0.0 );
  outputTransform->SetUpperTimeBound( 1.0 );

  VelocityFieldRegistrationType::ShrinkFactorsArrayType numberOfIterationsPerLevel;
  numberOfIterationsPerLevel.SetSize( 3 );
  numberOfIterationsPerLevel[0] = 1;
  numberOfIterationsPerLevel[1] = 1;
  numberOfIterationsPerLevel[2] = 50;
  velocityFieldRegistration->SetNumberOfIterationsPerLevel( numberOfIterationsPerLevel );

  VelocityFieldRegistrationType::ShrinkFactorsArrayType shrinkFactorsPerLevel;
  shrinkFactorsPerLevel.SetSize( 3 );
  shrinkFactorsPerLevel.Fill( 1 );
  velocityFieldRegistration->SetShrinkFactorsPerLevel( shrinkFactorsPerLevel );

  VelocityFieldRegistrationType::SmoothingSigmasArrayType smoothingSigmasPerLevel;
  smoothingSigmasPerLevel.SetSize( 3 );
  smoothingSigmasPerLevel.Fill( 0 );
  velocityFieldRegistration->SetSmoothingSigmasPerLevel( smoothingSigmasPerLevel );

  typedef itk::Vector<PixelType, Dimension> VectorType;
  typedef itk::Image<VectorType, Dimension+1> TimeVaryingVelocityFieldControlPointLatticeType;
  TimeVaryingVelocityFieldControlPointLatticeType::Pointer velocityFieldLattice = TimeVaryingVelocityFieldControlPointLatticeType::New();

  // Determine the parameters (size, spacing, etc) for the time-varying velocity field

  TimeVaryingVelocityFieldControlPointLatticeType::SizeType transformDomainMeshSize;
  TimeVaryingVelocityFieldControlPointLatticeType::PointType transformDomainOrigin;
  TimeVaryingVelocityFieldControlPointLatticeType::SpacingType transformDomainSpacing;
  TimeVaryingVelocityFieldControlPointLatticeType::SizeType transformDomainSize;
  TimeVaryingVelocityFieldControlPointLatticeType::DirectionType transformDomainDirection;

  transformDomainDirection.SetIdentity();
  transformDomainOrigin.Fill( 0.0 );
  transformDomainMeshSize.Fill( 4 );
  transformDomainSpacing.Fill( 1.0 );
  transformDomainSize.Fill( 10 );

  for( unsigned int i = 0; i < Dimension; i++ )
    {
    transformDomainOrigin[i] = fixedImageOrigin[i];
    transformDomainMeshSize[i] = 8;
    transformDomainSpacing[i] = fixedImageSpacing[i];
    transformDomainSize[i] = fixedImageSize[i];
    for( unsigned int j = 0; j < Dimension; j++ )
      {
      transformDomainDirection[i][j] = fixedImageDirection[i][j];
      }
    }

  typedef VelocityFieldRegistrationType::OutputTransformType TransformType;

  typedef itk::TimeVaryingBSplineVelocityFieldTransformParametersAdaptor<TransformType> VelocityFieldTransformAdaptorType;
  VelocityFieldTransformAdaptorType::Pointer initialFieldTransformAdaptor = VelocityFieldTransformAdaptorType::New();
  initialFieldTransformAdaptor->SetSplineOrder( outputTransform->GetSplineOrder() );
  initialFieldTransformAdaptor->SetRequiredTransformDomainOrigin( transformDomainOrigin );
  initialFieldTransformAdaptor->SetRequiredTransformDomainSpacing( transformDomainSpacing );
  initialFieldTransformAdaptor->SetRequiredTransformDomainSize( transformDomainSize );
  initialFieldTransformAdaptor->SetRequiredTransformDomainMeshSize( transformDomainMeshSize );
  initialFieldTransformAdaptor->SetRequiredTransformDomainDirection( transformDomainDirection );

  VectorType zeroVector( 0.0 );

  velocityFieldLattice->SetOrigin( initialFieldTransformAdaptor->GetRequiredControlPointLatticeOrigin() );
  velocityFieldLattice->SetSpacing( initialFieldTransformAdaptor->GetRequiredControlPointLatticeSpacing() );
  velocityFieldLattice->SetDirection( initialFieldTransformAdaptor->GetRequiredControlPointLatticeDirection() );
  velocityFieldLattice->SetRegions( initialFieldTransformAdaptor->GetRequiredControlPointLatticeSize() );
  velocityFieldLattice->Allocate();
  velocityFieldLattice->FillBuffer( zeroVector );

  TransformType::VelocityFieldPointType        velocityFieldOrigin;
  TransformType::VelocityFieldSpacingType      velocityFieldSpacing;
  TransformType::VelocityFieldSizeType         velocityFieldSize;
  TransformType::VelocityFieldDirectionType    velocityFieldDirection;

  velocityFieldOrigin.Fill( 0.0 );
  velocityFieldSpacing.Fill( 1.0 );
  velocityFieldSize.Fill( velocityFieldRegistration->GetNumberOfTimePointSamples() );
  velocityFieldDirection.SetIdentity();

  for( unsigned int i = 0; i < Dimension; i++ )
    {
    velocityFieldOrigin[i] = fixedImage->GetOrigin()[i];
    velocityFieldSpacing[i] = fixedImage->GetSpacing()[i];
    velocityFieldSize[i] = fixedImage->GetRequestedRegion().GetSize()[i];
    for( unsigned int j = 0; j < Dimension; j++ )
      {
      velocityFieldDirection[i][j] = fixedImage->GetDirection()[i][j];
      }
    }

  outputTransform->SetTimeVaryingVelocityFieldControlPointLattice( velocityFieldLattice );
  outputTransform->SetVelocityFieldOrigin( velocityFieldOrigin );
  outputTransform->SetVelocityFieldDirection( velocityFieldDirection );
  outputTransform->SetVelocityFieldSpacing( velocityFieldSpacing );
  outputTransform->SetVelocityFieldSize( velocityFieldSize );
  outputTransform->IntegrateVelocityField();

  VelocityFieldRegistrationType::TransformParametersAdaptorsContainerType adaptors;

  for( unsigned int level = 0; level < shrinkFactorsPerLevel.Size(); level++ )
    {
    typedef itk::ShrinkImageFilter<FixedImageType, FixedImageType> ShrinkFilterType;
    ShrinkFilterType::Pointer shrinkFilter = ShrinkFilterType::New();
    shrinkFilter->SetShrinkFactors( shrinkFactorsPerLevel[level] );
    shrinkFilter->SetInput( fixedImage );
    shrinkFilter->Update();

    // Although we shrink the images for the given levels,
    // we keep the size in time the same

    velocityFieldSize.Fill( 10 );
    velocityFieldOrigin.Fill( 0.0 );
    velocityFieldSpacing.Fill( 1.0 );
    velocityFieldDirection.SetIdentity();

    fixedImageSize = shrinkFilter->GetOutput()->GetBufferedRegion().GetSize();
    fixedImageOrigin = shrinkFilter->GetOutput()->GetOrigin();
    fixedImageSpacing = shrinkFilter->GetOutput()->GetSpacing();
    fixedImageDirection = shrinkFilter->GetOutput()->GetDirection();

    for( unsigned int i = 0; i < Dimension; i++ )
      {
      velocityFieldSize[i] = fixedImageSize[i];
      velocityFieldOrigin[i] = fixedImageOrigin[i];
      velocityFieldSpacing[i] = fixedImageSpacing[i];

      transformDomainMeshSize[i] <<= 1;

      for( unsigned int j = 0; j < Dimension; j++ )
        {
        velocityFieldDirection[i][j] = fixedImageDirection[i][j];
        }
      }

    VelocityFieldTransformAdaptorType::Pointer fieldTransformAdaptor = VelocityFieldTransformAdaptorType::New();
    fieldTransformAdaptor->SetSplineOrder( outputTransform->GetSplineOrder() );
    fieldTransformAdaptor->SetRequiredTransformDomainSpacing( velocityFieldSpacing );
    fieldTransformAdaptor->SetRequiredTransformDomainSize( velocityFieldSize );
    fieldTransformAdaptor->SetRequiredTransformDomainDirection( velocityFieldDirection );
    fieldTransformAdaptor->SetRequiredTransformDomainOrigin( velocityFieldOrigin );
    fieldTransformAdaptor->SetRequiredTransformDomainMeshSize( transformDomainMeshSize );

    adaptors.push_back( fieldTransformAdaptor.GetPointer() );
    }
  velocityFieldRegistration->SetTransformParametersAdaptorsPerLevel( adaptors );

  try
    {
    std::cout << "Time-varying B-spline velocity field point set registration" << std::endl;
    velocityFieldRegistration->Update();
    }
  catch( itk::ExceptionObject &e )
    {
    std::cerr << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
    }

  // applying the resultant transform to moving points and verify result
  std::cout << "Fixed\tMoving\tMovingTransformed\tFixedTransformed\tDiff" << std::endl;
  PointType::ValueType tolerance = 0.1;

  float averageError = 0.0;
  for( unsigned int n = 0; n < movingPoints->GetNumberOfPoints(); n++ )
    {
    // compare the points in virtual domain
    PointType transformedMovingPoint =
      velocityFieldRegistration->GetModifiableTransform()->GetInverseTransform()->TransformPoint( movingPoints->GetPoint( n ) );
    PointType fixedPoint = fixedPoints->GetPoint( n );
    PointType transformedFixedPoint = velocityFieldRegistration->GetModifiableTransform()->TransformPoint( fixedPoints->GetPoint( n ) );
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
