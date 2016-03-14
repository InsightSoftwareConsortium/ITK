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

#include "itkTimeVaryingBSplineVelocityFieldTransform.h"

int itkTimeVaryingBSplineVelocityFieldTransformTest( int, char* [] )
{
  typedef itk::Vector<double, 3>      VectorType;
  typedef itk::Image<VectorType, 3>   DisplacementFieldType;
  typedef itk::Image<VectorType, 4>   TimeVaryingVelocityFieldControlPointLatticeType;
  typedef itk::Image<VectorType, 4>   TimeVaryingVelocityFieldType;

  const unsigned int splineOrder = 3;

  TimeVaryingVelocityFieldControlPointLatticeType::PointType origin;
  origin.Fill( -2.0 );

  TimeVaryingVelocityFieldControlPointLatticeType::SpacingType spacing;
  spacing.Fill( 2.0 );

  TimeVaryingVelocityFieldControlPointLatticeType::SizeType size;
  size.Fill( 25 );

  VectorType displacement1;
  displacement1.Fill( 0.1 );

  TimeVaryingVelocityFieldControlPointLatticeType::Pointer timeVaryingVelocityFieldControlPointLattice =
    TimeVaryingVelocityFieldControlPointLatticeType::New();

  timeVaryingVelocityFieldControlPointLattice->SetOrigin( origin );
  timeVaryingVelocityFieldControlPointLattice->SetSpacing( spacing );
  timeVaryingVelocityFieldControlPointLattice->SetRegions( size );
  timeVaryingVelocityFieldControlPointLattice->Allocate();
  timeVaryingVelocityFieldControlPointLattice->FillBuffer( displacement1 );

  typedef itk::TimeVaryingVelocityFieldIntegrationImageFilter
    <TimeVaryingVelocityFieldControlPointLatticeType, DisplacementFieldType> IntegratorType;

  IntegratorType::Pointer integrator = IntegratorType::New();
  integrator->SetInput( timeVaryingVelocityFieldControlPointLattice );
  integrator->SetLowerTimeBound( 0.3 );
  integrator->SetUpperTimeBound( 0.75 );
  integrator->Update();

  integrator->Print( std::cout, 3 );

  DisplacementFieldType::IndexType index;
  index.Fill( 0 );
  VectorType displacementPixel;

  // This integration should result in a constant image of value
  // 0.75 * 0.1 - 0.3 * 0.1 = 0.045 with ~epsilon deviation
  // due to numerical computations
  const DisplacementFieldType * displacementField = integrator->GetOutput();

  displacementPixel = displacementField->GetPixel( index );

  std::cout << "Estimated forward displacement vector: " << displacementPixel << std::endl;
  if( itk::Math::abs( displacementPixel[0] - 0.045 ) > 0.01 )
    {
    std::cerr << "Failed to produce the correct forward integration." << std::endl;
    return EXIT_FAILURE;
    }

  IntegratorType::Pointer inverseIntegrator = IntegratorType::New();
  inverseIntegrator->SetInput( timeVaryingVelocityFieldControlPointLattice );
  inverseIntegrator->SetLowerTimeBound( 1.0 );
  inverseIntegrator->SetUpperTimeBound( 0.0 );
  inverseIntegrator->Update();

  // This integration should result in a constant image of value
  // -( 0.1 * 1.0 - ( 0.1 * 0.0 ) ) = -0.1 with ~epsilon deviation
  // due to numerical computations
  const DisplacementFieldType * inverseField = inverseIntegrator->GetOutput();
  displacementPixel = inverseField->GetPixel( index );
  if( itk::Math::abs( displacementPixel[0] + 0.1 ) > 0.01 )
    {
    std::cerr << "Failed to produce the correct inverse integration." << std::endl;
    return EXIT_FAILURE;
    }

  // Now test the transform

  TimeVaryingVelocityFieldType::PointType timeVaryingVelocityFieldOrigin;
  TimeVaryingVelocityFieldType::SpacingType timeVaryingVelocityFieldSpacing;
  TimeVaryingVelocityFieldType::SizeType timeVaryingVelocityFieldSize;
  TimeVaryingVelocityFieldType::DirectionType timeVaryingVelocityFieldDirection;

  timeVaryingVelocityFieldDirection.SetIdentity();
  timeVaryingVelocityFieldSpacing.Fill( 1.0 );
  for( unsigned int d = 0; d < 4; d++ )
    {
    float physicalDimensions = ( size[d] - splineOrder ) * spacing[d];
    timeVaryingVelocityFieldSize[d] = static_cast<unsigned int>( physicalDimensions / timeVaryingVelocityFieldSpacing[d] + 1 );
    timeVaryingVelocityFieldSpacing[d] = physicalDimensions / ( timeVaryingVelocityFieldSize[d] - 1 );
    timeVaryingVelocityFieldOrigin[d] = origin[d] + spacing[d] * ( splineOrder - 1 ) * 0.5;
    }
  timeVaryingVelocityFieldSize[3] = 5;

  typedef itk::TimeVaryingBSplineVelocityFieldTransform<double, 3> TransformType;
  TransformType::Pointer transform = TransformType::New();
  transform->SetLowerTimeBound( 0.0 );
  transform->SetUpperTimeBound( 1.0 );
  transform->SetSplineOrder( splineOrder );
  transform->SetVelocityFieldOrigin( timeVaryingVelocityFieldOrigin );
  transform->SetVelocityFieldDirection( timeVaryingVelocityFieldDirection );
  transform->SetVelocityFieldSize( timeVaryingVelocityFieldSize );
  transform->SetVelocityFieldSpacing( timeVaryingVelocityFieldSpacing );
  transform->SetNumberOfIntegrationSteps( 10 );
  transform->SetTimeVaryingVelocityFieldControlPointLattice( timeVaryingVelocityFieldControlPointLattice );
  transform->IntegrateVelocityField();

  transform->Print( std::cout, 3 );

  TransformType::InputPointType point;
  point.Fill( 1.3 );

  typedef TransformType::OutputPointType OutputPointType;
  OutputPointType transformedPoint = transform->TransformPoint( point );

  std::cout << point << ", " << transformedPoint << transform->TransformPoint( point ) << std::endl;

  VectorType displacement;
  displacement.Fill( 0.1 );

  point += displacement;
  if( point.EuclideanDistanceTo( transformedPoint ) > 0.1 )
    {
    std::cerr << "Failed to produce the expected transformed point." << std::endl;
    return EXIT_FAILURE;
    }
  point -= displacement;

  TransformType::InputPointType point2;
  point2.CastFrom( transformedPoint );

  typedef TransformType::InverseTransformBasePointer InverseTransformBasePointer;
  InverseTransformBasePointer inverseTransform = transform->GetInverseTransform();

  transformedPoint = inverseTransform->TransformPoint( point2 );

  if( point.EuclideanDistanceTo( transformedPoint ) > 0.1 )
    {
    std::cerr << "Failed to produce the expected inverse transformed point." << std::endl;
    return EXIT_FAILURE;
    }

  transform->Print( std::cout, 3 );

  return EXIT_SUCCESS;
}
