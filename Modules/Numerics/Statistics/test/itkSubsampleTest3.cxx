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
#include "itkSubsample.h"

int itkSubsampleTest3(int, char* [] )
{
  std::cout << "MeanSampleFilter test \n \n";

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

  typedef itk::Statistics::Subsample< SampleType >  SubsampleType;

  SubsampleType::Pointer subsample = SubsampleType::New();

  subsample->SetSample( sample );

  //Initialize the subsample with all the samples
  subsample->InitializeWithAllInstances();

  typedef itk::Statistics::MeanSampleFilter< SubsampleType >
    FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput( subsample );

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

  if ( ( std::abs( meanOutput[0] - mean[0]) > epsilon )  ||
       ( std::abs( meanOutput[1] - mean[1]) > epsilon ))
    {
    std::cerr << "The result is not what is expected" << std::endl;
    return EXIT_FAILURE;
    }

  //Clear and  repopulate the subsample container
  subsample->Clear();

  // add only the first half of instances of the sample
  for (SampleType::InstanceIdentifier id = 0;
       id < static_cast< SampleType::InstanceIdentifier >
         (sample->Size() / 2);
       id++)
    {
    subsample->AddInstance(id);
    }

  std::cout << "Subsample size: " << subsample->Size() << std::endl;

  try
    {
    filter->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught: " << excp << std::endl;
    }

  decorator   = filter->GetOutput();
  meanOutput  = decorator->Get();

  mean[0] = 0.5;
  mean[1] = 0.5;

  std::cout << meanOutput[0] << " " << mean[0] << " "
            << meanOutput[1] << " " << mean[1] << " " << std::endl;

  if ( ( std::abs( meanOutput[0] - mean[0]) > epsilon )  ||
       ( std::abs( meanOutput[1] - mean[1]) > epsilon ))
    {
    std::cerr << "The result is not what is expected" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
