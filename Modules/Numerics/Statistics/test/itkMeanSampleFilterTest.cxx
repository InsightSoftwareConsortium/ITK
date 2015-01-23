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

#include "itkMeanSampleFilter.h"
#include "itkListSample.h"

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

  typedef itk::Statistics::MeanSampleFilter< SampleType > FilterType;

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

  if ( filter->GetInput() != ITK_NULLPTR )
    {
    pass = false;
    failureMeassage = "GetInput() should return ITK_NULLPTR if the input \
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

  if ( ( std::fabs( meanOutput[0] - mean[0]) > epsilon )  ||
       ( std::fabs( meanOutput[1] - mean[1]) > epsilon ))
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
