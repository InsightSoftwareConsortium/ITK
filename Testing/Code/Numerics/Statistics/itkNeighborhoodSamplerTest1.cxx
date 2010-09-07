/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodSamplerTest1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>
#include "itkListSample.h"
#include "itkNeighborhoodSampler.h"

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
  if( filter->GetInput() != NULL )
    {
    std::cerr << "GetInput() should have returned NULL" << std::endl;
    return EXIT_FAILURE;
    }

  // Test GetOutput() before creating the output
  if( filter->GetOutput() == NULL )
    {
    std::cerr << "GetOutput() should have returned NON-NULL" << std::endl;
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

  if( recoveredRadiusObject == NULL )
    {
    std::cerr << "GetRadiusInput() returned NULL object." << std::endl;
    return EXIT_FAILURE;
    }

  if( recoveredRadiusObject->Get() != radius1 )
    {
    std::cerr << "GetRadiusInput() test for value consistency 1 failed." << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetRadius( radius2 );

  recoveredRadiusObject = filter->GetRadiusInput();

  if( recoveredRadiusObject == NULL )
    {
    std::cerr << "GetRadiusInput() returned NULL object." << std::endl;
    return EXIT_FAILURE;
    }

  if( recoveredRadiusObject->Get() != radius2 )
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

  if( recoveredRadiusObject->Get() != radius1 )
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

  if( recoveredRadiusObject->Get() != radius2 )
    {
    std::cerr << "GetRadiusInput() test for value consistency 4 failed." << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetInput1( radiusObject1 );

  recoveredRadiusObject = filter->GetInput1();

  if( recoveredRadiusObject != radiusObject1 )
    {
    std::cerr << "GetRadiusInput() test for pointer consistency 3 failed." << std::endl;
    return EXIT_FAILURE;
    }

  if( recoveredRadiusObject->Get() != radius1 )
    {
    std::cerr << "GetRadiusInput() test for value consistency 5 failed." << std::endl;
    return EXIT_FAILURE;
    }


  //
  // Testing exception cases in the GenerateData() method.
  //
  filter->SetRadiusInput( NULL );

  std::cout << "GetRadiusInput() =  " <<  filter->GetRadiusInput() << std::endl;

  try
    {
    filter->Update();
    std::cerr << "Failure to throw expected exception ";
    std::cerr << " due to NULL SetRadiusInput()";
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
