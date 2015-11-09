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
#include "itkListSample.h"
#include "itkNeighborhoodSampler.h"
#include "itkMath.h"

int itkNeighborhoodSamplerTest1(int, char* [] )
{

  const unsigned int MeasurementVectorSize = 17;

  typedef itk::FixedArray<
    float, MeasurementVectorSize >  MeasurementVectorType;

  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;

  typedef itk::Statistics::NeighborhoodSampler< SampleType > FilterType;

  typedef FilterType::RadiusType              RadiusType;
  typedef FilterType::InputRadiusObjectType   InputRadiusObjectType;

  SampleType::Pointer sample = SampleType::New();

  FilterType::Pointer filter = FilterType::New();

  // Test GetInput() before setting the input
  if( filter->GetInput() != ITK_NULLPTR )
    {
    std::cerr << "GetInput() should have returned ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }

  // Test GetOutput() before creating the output
  if( filter->GetOutput() == ITK_NULLPTR )
    {
    std::cerr << "GetOutput() should have returned NON-ITK_NULLPTR" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetInput( sample );

  if( filter->GetInput() != sample.GetPointer() )
    {
    std::cerr << "GetInput() didn't matched SetInput()" << std::endl;
    return EXIT_FAILURE;
    }


  // Testing the settings of the Radius.
  const RadiusType radius1 = 237;
  const RadiusType radius2 = 179;

  filter->SetRadius( radius1 );

  const InputRadiusObjectType * recoveredRadiusObject =
    filter->GetRadiusInput();

  if( recoveredRadiusObject == ITK_NULLPTR )
    {
    std::cerr << "GetRadiusInput() returned ITK_NULLPTR object." << std::endl;
    return EXIT_FAILURE;
    }

  if( itk::Math::NotExactlyEquals(recoveredRadiusObject->Get(), radius1) )
    {
    std::cerr << "GetRadiusInput() test for value consistency 1 failed." << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetRadius( radius2 );

  recoveredRadiusObject = filter->GetRadiusInput();

  if( recoveredRadiusObject == ITK_NULLPTR )
    {
    std::cerr << "GetRadiusInput() returned ITK_NULLPTR object." << std::endl;
    return EXIT_FAILURE;
    }

  if( itk::Math::NotExactlyEquals(recoveredRadiusObject->Get(), radius2) )
    {
    std::cerr << "GetRadiusInput() test for value consistency 2 failed." << std::endl;
    return EXIT_FAILURE;
    }


  InputRadiusObjectType::Pointer radiusObject1 =
    InputRadiusObjectType::New();

  radiusObject1->Set( radius1 );

  filter->SetRadiusInput( radiusObject1 );

  recoveredRadiusObject = filter->GetRadiusInput();

  if( recoveredRadiusObject != radiusObject1 )
    {
    std::cerr << "GetRadiusInput() test for pointer consistency 1 failed." << std::endl;
    return EXIT_FAILURE;
    }

  if( itk::Math::NotExactlyEquals(recoveredRadiusObject->Get(), radius1) )
    {
    std::cerr << "GetRadiusInput() test for value consistency 3 failed." << std::endl;
    return EXIT_FAILURE;
    }

  InputRadiusObjectType::Pointer radiusObject2 =
    InputRadiusObjectType::New();

  radiusObject2->Set( radius2 );

  filter->SetRadiusInput( radiusObject2 );

  recoveredRadiusObject = filter->GetRadiusInput();

  if( recoveredRadiusObject != radiusObject2 )
    {
    std::cerr << "GetRadiusInput() test for pointer consistency 2 failed." << std::endl;
    return EXIT_FAILURE;
    }

  if( itk::Math::NotExactlyEquals(recoveredRadiusObject->Get(), radius2) )
    {
    std::cerr << "GetRadiusInput() test for value consistency 4 failed." << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Testing exception cases in the GenerateData() method.
  //
  filter->SetRadiusInput( ITK_NULLPTR );

  std::cout << "GetRadiusInput() =  " <<  filter->GetRadiusInput() << std::endl;

  try
    {
    filter->Update();
    std::cerr << "Failure to throw expected exception ";
    std::cerr << " due to ITK_NULLPTR SetRadiusInput()";
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & )
    {
    std::cout << "Expected exception received" << std::endl;
    }

  radiusObject1->Set( 100 );
  filter->SetRadiusInput( radiusObject1 );


  //
  // Exercise the Print() method
  //
  filter->Print( std::cout );


  filter->Update();

  std::cout << "Classname " << filter->GetNameOfClass() << std::endl;

  std::cout << "Test Passed !" << std::endl;
  return EXIT_SUCCESS;
}
