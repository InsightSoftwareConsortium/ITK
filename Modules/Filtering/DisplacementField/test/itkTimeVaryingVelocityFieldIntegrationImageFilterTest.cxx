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

#include "itkTimeVaryingVelocityFieldIntegrationImageFilter.h"

int itkTimeVaryingVelocityFieldIntegrationImageFilterTest( int, char* [] )
{
  typedef itk::Vector<double, 3>      VectorType;
  typedef itk::Image<VectorType, 3>   DisplacementFieldType;
  typedef itk::Image<VectorType, 4>   TimeVaryingVelocityFieldType;

  TimeVaryingVelocityFieldType::PointType origin;
  origin.Fill( 0.0 );

  TimeVaryingVelocityFieldType::SpacingType spacing;
  spacing.Fill( 2.0 );

  TimeVaryingVelocityFieldType::SizeType size;
  size.Fill( 25 );

  VectorType displacement1;
  displacement1.Fill( 0.1 );

  TimeVaryingVelocityFieldType::Pointer timeVaryingVelocityField =
    TimeVaryingVelocityFieldType::New();

  timeVaryingVelocityField->SetOrigin( origin );
  timeVaryingVelocityField->SetSpacing( spacing );
  timeVaryingVelocityField->SetRegions( size );
  timeVaryingVelocityField->Allocate();
  timeVaryingVelocityField->FillBuffer( displacement1 );


  typedef itk::TimeVaryingVelocityFieldIntegrationImageFilter
    <TimeVaryingVelocityFieldType, DisplacementFieldType> IntegratorType;
  IntegratorType::Pointer integrator = IntegratorType::New();
  integrator->SetInput( timeVaryingVelocityField );
  integrator->SetLowerTimeBound( 0.3 );
  integrator->SetUpperTimeBound( 0.75 );
  integrator->SetNumberOfIntegrationSteps( 10 );
  integrator->Update();

  integrator->Print( std::cout, 3 );

  DisplacementFieldType::IndexType index;
  index.Fill( 0 );
  VectorType displacement;

  IntegratorType::Pointer inverseIntegrator = IntegratorType::New();
  inverseIntegrator->SetInput( timeVaryingVelocityField );
  inverseIntegrator->SetLowerTimeBound( 1.0 );
  inverseIntegrator->SetUpperTimeBound( 0.0 );
  inverseIntegrator->SetNumberOfIntegrationSteps( 10 );
  inverseIntegrator->Update();

  // This integration should result in a constant image of value
  // -( 0.1 * 1.0 - ( 0.1 * 0.0 ) ) = -0.1 with ~epsilon deviation
  // due to numerical computations
  const DisplacementFieldType * inverseField = inverseIntegrator->GetOutput();
  displacement = inverseField->GetPixel( index );

  std::cout << "Estimated inverse displacement vector: " << displacement << std::endl;
  if( itk::Math::abs( displacement[0] + 0.101852 ) > 0.01 )
    {
    std::cerr << "Failed to produce the correct inverse integration." << std::endl;
    return EXIT_FAILURE;
    }

  inverseIntegrator->Print( std::cout, 3 );

  std::cout << inverseIntegrator->GetNameOfClass() << std::endl;

  // This integration should result in a constant image of value
  // 0.75 * 0.1 - 0.3 * 0.1 = 0.045 with ~epsilon deviation
  // due to numerical computations
  const DisplacementFieldType * displacementField = integrator->GetOutput();

  displacement = displacementField->GetPixel( index );

  std::cout << "Estimated forward displacement vector: " << displacement << std::endl;
  if( itk::Math::abs( displacement[0] - 0.045 ) > 0.0001 )
    {
    std::cerr << "Failed to produce the correct forward integration." << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
