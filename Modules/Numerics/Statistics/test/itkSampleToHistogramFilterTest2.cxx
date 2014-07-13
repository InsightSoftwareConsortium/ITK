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

#include "itkListSample.h"
#include "itkHistogram.h"
#include "itkSampleToHistogramFilter.h"

int itkSampleToHistogramFilterTest2( int , char * [] )
{

  const unsigned int numberOfComponents = 3;
  typedef float      MeasurementType;

  typedef itk::Array< MeasurementType > MeasurementVectorType;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;

  typedef itk::Statistics::Histogram< MeasurementType,
          itk::Statistics::DenseFrequencyContainer2 > HistogramType;

  typedef itk::Statistics::SampleToHistogramFilter<
    SampleType, HistogramType > FilterType;

  typedef FilterType::HistogramSizeType                    HistogramSizeType;
  typedef FilterType::HistogramMeasurementVectorType       HistogramMeasurementVectorType;

  FilterType::Pointer filter = FilterType::New();
  SampleType::Pointer sample = SampleType::New();

  HistogramMeasurementVectorType minimum( numberOfComponents );
  HistogramMeasurementVectorType maximum( numberOfComponents );

  minimum[0] = -17.5;
  minimum[1] = -19.5;
  minimum[2] = -24.5;

  maximum[0] =  17.5;
  maximum[1] =  19.5;
  maximum[2] =  24.5;

  HistogramSizeType histogramSize( numberOfComponents );

  histogramSize[0] = 35;
  histogramSize[1] = 39;
  histogramSize[2] = 49;

  MeasurementVectorType measure( numberOfComponents );

  sample->SetMeasurementVectorSize( numberOfComponents );

  // Populate the Sample
  for( unsigned int i=0; i < histogramSize[0]; i++ )
    {
    measure[0] = minimum[0] + 0.5 + i;
    for( unsigned int j=0; j < histogramSize[1]; j++ )
      {
      measure[1] = minimum[1] + 0.5 + j;
      for( unsigned int k=0; k < histogramSize[2]; k++ )
        {
        measure[2] = minimum[2] + 0.5 + k;
        sample->PushBack( measure );
        }
      }
    }


  filter->SetInput( sample );


  // Test exception when calling Update() without having
  // defined the size of the histogram in the filter.
  try
    {
    filter->Update();
    std::cerr << "Failure to throw expected exception due to lack";
    std::cerr << " of calling SetHistogramSize() in the filter ";
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & )
    {
    std::cout << "Expected exception received" << std::endl;
    }


  const HistogramType * histogram = filter->GetOutput();

  if( histogram->Size() != 0 )
    {
    std::cerr << "Histogram Size should have been zero" << std::endl;
    return EXIT_FAILURE;
    }


  filter->SetHistogramSize( histogramSize );

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int expectedHistogramSize1 =
    histogramSize[0] * histogramSize[1] * histogramSize[2];

  if( histogram->Size() != expectedHistogramSize1 )
    {
    std::cerr << "Histogram Size error" << std::endl;
    std::cerr << "We expected " << expectedHistogramSize1 << std::endl;
    std::cerr << "We received " << histogram->Size() << std::endl;
    return EXIT_FAILURE;
    }


  HistogramType::ConstIterator histogramItr = histogram->Begin();
  HistogramType::ConstIterator histogramEnd = histogram->End();

  const unsigned int expectedFrequency1 = 1;
  while( histogramItr != histogramEnd )
    {
    if( histogramItr.GetFrequency() != expectedFrequency1 )
      {
      std::cerr << "Histogram bin error for measure " << std::endl;
      std::cerr << histogramItr.GetMeasurementVector() << std::endl;
      std::cerr << "Expected frequency = " << expectedFrequency1 << std::endl;
      std::cerr << "Computed frequency = " << histogramItr.GetFrequency() << std::endl;
      }
    ++histogramItr;
    }


  // Now resize the histogram, to see is the frequencies
  // are relocated correctly into the new size bins.
  //
  histogramSize[0] =  7;
  histogramSize[1] = 39;
  histogramSize[2] = 49;

  filter->SetHistogramSize( histogramSize );

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int expectedHistogramSize2 =
    histogramSize[0] * histogramSize[1] * histogramSize[2];

  if( histogram->Size() != expectedHistogramSize2 )
    {
    std::cerr << "Histogram Size error" << std::endl;
    std::cerr << "We expected " << expectedHistogramSize2 << std::endl;
    std::cerr << "We received " << histogram->Size() << std::endl;
    return EXIT_FAILURE;
    }


  histogramItr = histogram->Begin();
  histogramEnd = histogram->End();

  const unsigned int expectedFrequency2 = 5;
  while( histogramItr != histogramEnd )
    {
    if( histogramItr.GetFrequency() != expectedFrequency2 )
      {
      std::cerr << "Histogram bin error for measure " << std::endl;
      std::cerr << histogramItr.GetMeasurementVector() << std::endl;
      std::cerr << "Expected frequency = " << expectedFrequency2 << std::endl;
      std::cerr << "Computed frequency = " << histogramItr.GetFrequency() << std::endl;
      }
    ++histogramItr;
    }


  // Now resize the histogram, to see is the frequencies
  // are relocated correctly into the new size bins.
  //
  histogramSize[0] =  7;
  histogramSize[1] = 13;
  histogramSize[2] = 49;

  filter->SetHistogramSize( histogramSize );

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int expectedHistogramSize3 =
    histogramSize[0] * histogramSize[1] * histogramSize[2];

  if( histogram->Size() != expectedHistogramSize3 )
    {
    std::cerr << "Histogram Size error" << std::endl;
    std::cerr << "We expected " << expectedHistogramSize3 << std::endl;
    std::cerr << "We received " << histogram->Size() << std::endl;
    return EXIT_FAILURE;
    }


  histogramItr = histogram->Begin();
  histogramEnd = histogram->End();

  const unsigned int expectedFrequency3 = 15;
  while( histogramItr != histogramEnd )
    {
    if( histogramItr.GetFrequency() != expectedFrequency3 )
      {
      std::cerr << "Histogram bin error for measure " << std::endl;
      std::cerr << histogramItr.GetMeasurementVector() << std::endl;
      std::cerr << "Expected frequency = " << expectedFrequency3 << std::endl;
      std::cerr << "Computed frequency = " << histogramItr.GetFrequency() << std::endl;
      }
    ++histogramItr;
    }


  // Now resize the histogram, to see is the frequencies
  // are relocated correctly into the new size bins.
  //
  histogramSize[0] =  7;
  histogramSize[1] = 13;
  histogramSize[2] =  7;

  filter->SetHistogramSize( histogramSize );

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int expectedHistogramSize4 =
    histogramSize[0] * histogramSize[1] * histogramSize[2];

  if( histogram->Size() != expectedHistogramSize4 )
    {
    std::cerr << "Histogram Size error" << std::endl;
    std::cerr << "We expected " << expectedHistogramSize4 << std::endl;
    std::cerr << "We received " << histogram->Size() << std::endl;
    return EXIT_FAILURE;
    }


  histogramItr = histogram->Begin();
  histogramEnd = histogram->End();

  const unsigned int expectedFrequency4 = 105;
  while( histogramItr != histogramEnd )
    {
    if( histogramItr.GetFrequency() != expectedFrequency4 )
      {
      std::cerr << "Histogram bin error for measure " << std::endl;
      std::cerr << histogramItr.GetMeasurementVector() << std::endl;
      std::cerr << "Expected frequency = " << expectedFrequency4 << std::endl;
      std::cerr << "Computed frequency = " << histogramItr.GetFrequency() << std::endl;
      }
    ++histogramItr;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
