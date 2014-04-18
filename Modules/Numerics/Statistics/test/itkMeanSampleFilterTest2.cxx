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

int itkMeanSampleFilterTest2(int, char* [] )
{
  std::cout << "MeanSampleFilter test \n \n";
  bool pass = true;
  std::string failureMeassage= "";

  const unsigned int                  MeasurementVectorSize = 2;
  const unsigned int                  numberOfMeasurementVectors = 2;
  unsigned int                        counter;

  typedef itk::FixedArray< int, MeasurementVectorSize >  MeasurementVectorType;

  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;

  SampleType::Pointer sample = SampleType::New();

  sample->SetMeasurementVectorSize( MeasurementVectorSize );

  MeasurementVectorType               measure;

  counter = 0;

  std::cout << "Input sample values " << std::endl;
  while ( counter < numberOfMeasurementVectors )
    {
    for( unsigned int i=0; i<MeasurementVectorSize; i++)
      {
      measure[i] = counter;
      }
    std::cout << counter << " : " << measure << std::endl;
    sample->PushBack( measure );
    counter++;
    }

  typedef itk::Statistics::MeanSampleFilter< SampleType > FilterType;

  FilterType::Pointer filter = FilterType::New();

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
  FilterType::MeasurementVectorRealType meanOutput  = decorator->Get();

  FilterType::MeasurementVectorRealType expectedMean;

  expectedMean[0] = 0.5;
  expectedMean[1] = 0.5;

  std::cout << meanOutput[0] << " " << expectedMean[0] << " "
            << meanOutput[1] << " " << expectedMean[1] << " " << std::endl;

  // FilterType::MeasurementVectorType::ValueType is an int in this case
  FilterType::MeasurementVectorType::ValueType    epsilon = 0;

  if ( ( std::fabs( meanOutput[0] - expectedMean[0]) > epsilon )  ||
       ( std::fabs( meanOutput[1] - expectedMean[1]) > epsilon ))
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
