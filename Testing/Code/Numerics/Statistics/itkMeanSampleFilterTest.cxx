/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanSampleFilterTest.cxx
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

#include "itkMeanSampleFilter.h"
#include "itkListSample.h"
#include "itkFixedArray.h"

int itkMeanSampleFilterTest(int, char* [] )
{
  std::cout << "MeanSampleFilter test \n \n";
  bool pass = true;
  std::string failureMeassage= "";

  const unsigned int                  MeasurementVectorSize = 2;
  const unsigned int                  numberOfMeasurementVectors = 5;
  unsigned int                        counter;

  typedef itk::FixedArray<
    float, MeasurementVectorSize >             MeasurementVectorType;
  typedef itk::Statistics::ListSample<
    MeasurementVectorType >                    SampleType;

  SampleType::Pointer sample = SampleType::New();

  sample->SetMeasurementVectorSize( MeasurementVectorSize );

  MeasurementVectorType               measure;

  //reset counter
  counter = 0;

  while ( counter < numberOfMeasurementVectors )
    {
    for( unsigned int i=0; i<MeasurementVectorSize; i++)
      {
      measure[i] = counter;
      }
    sample->PushBack( measure );
    counter++;
    }

  typedef itk::Statistics::MeanSampleFilter< SampleType >
    FilterType;

  FilterType::Pointer filter = FilterType::New();

  std::cout << filter->GetNameOfClass() << std::endl;
  filter->Print(std::cout);

  //Invoke update before adding an input. An exception should be
  //thrown.
  try
    {
    filter->Update();
    failureMeassage = "Exception should have been thrown since \
                    Update() is invoked without setting an input ";
    pass = false;
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught: " << excp << std::endl;
    }

  if ( filter->GetInput() != NULL )
    {
    pass = false;
    failureMeassage = "GetInput() should return NULL if the input \
                     has not been set";
    }

  filter->ResetPipeline();
  filter->SetInput( sample );

  try
    {
    filter->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught: " << excp << std::endl;
    }

  const FilterType::MeasurementVectorDecoratedType * decorator = filter->GetOutput();
  FilterType::MeasurementVectorType    meanOutput  = decorator->Get();

  FilterType::MeasurementVectorType mean;

  mean[0] = 2.0;
  mean[1] = 2.0;

  std::cout << meanOutput[0] << " " << mean[0] << " "
            << meanOutput[1] << " " << mean[1] << " " << std::endl;

  FilterType::MeasurementVectorType::ValueType    epsilon = 1e-6;

  if ( ( vcl_fabs( meanOutput[0] - mean[0]) > epsilon )  ||
       ( vcl_fabs( meanOutput[1] - mean[1]) > epsilon ))
    {
    pass = false;
    failureMeassage = "The result is not what is expected";
    }

  if( !pass )
    {
    std::cout << "Test failed." << failureMeassage << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
