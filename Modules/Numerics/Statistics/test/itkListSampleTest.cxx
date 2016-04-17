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

int itkListSampleTest(int argc, char *argv[] )
{
  std::cerr << "ListSample Test \n \n";
  if( argc< 2 )
    {
    std::cerr << "itkListSampleTest LengthOfMeasurementVector" << std::endl;
    }

  typedef itk::Array< float >                                  MeasurementVectorType;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;

  SampleType::MeasurementVectorSizeType measurementVectorSize = atoi(argv[1]);
  std::cerr << "Measurement vector size: " << measurementVectorSize
            << std::endl;

  unsigned int sampleSize = 25;

  SampleType::Pointer sample = SampleType::New();

  sample->SetMeasurementVectorSize( measurementVectorSize );

  MeasurementVectorType mv( measurementVectorSize );

  std::cerr << "Sample length = " << sample->GetMeasurementVectorSize() << std::endl;
  std::cerr << "Vector length = "
  << itk::NumericTraits<MeasurementVectorType>::GetLength( mv ) << std::endl;

  for ( unsigned int i = 0; i < sampleSize; i++ )
    {
    for (unsigned int j = 0; j < measurementVectorSize; j++ )
      {
      mv[j] = rand() / (RAND_MAX+1.0);
      }
    sample->PushBack(mv);
    }

  // Try to push a measurement vector size different from what is set
  MeasurementVectorType  mvLargerSize( measurementVectorSize + 1 );

  for (unsigned int j=0; j <= measurementVectorSize; j++ )
    {
    mvLargerSize[j] = rand() / (RAND_MAX+1.0);
    }

  try
    {
    sample->PushBack( mvLargerSize );
    std::cerr << "Exception was expected since the vector that was\
                  added to the list has size different from what is set"
              << std::endl;
    return EXIT_FAILURE;
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << "Caught expected exception: " << excp << std::endl;
    }

  // tests begin

  //
  // general interface
  //
  std::cerr << "General interface..." << std::endl;
  std::cerr << "Trying Size()...";
  if ( sampleSize != sample->Size() )
    {
    std::cerr << "FAILED" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cerr << "PASSED" << std::endl;
    }
  std::cerr << "Trying GetMeasurementVectorSize()...";
  if (sample->GetMeasurementVectorSize() != measurementVectorSize)
    {
    std::cerr << "GetMeasurementVectorSize() failed" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cerr << "PASSED" << std::endl;
    }

  // get and set measurements
  std::cerr << "Trying GetMeasurementVector(4)..." << std::endl;
  mv = sample->GetMeasurementVector(4);
  if ( mv != sample->GetMeasurementVector(4) )
    {
    std::cerr << "FAILED" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cerr << "PASSED" << std::endl;
    }

  std::cerr << "Trying SetMeasurementVector(4,mv)...";
  float tmp = mv[0];
  mv[0] += 1.0;
  sample->SetMeasurementVector(4,mv);
  if (mv != sample->GetMeasurementVector(4))
    {
    std::cerr << "FAILED" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cerr << "PASSED" << std::endl;
    }

  std::cerr << "Trying SetMeasurement(4,0,tmp)...";
  mv[0] = tmp;
  sample->SetMeasurement(4,0,tmp);
  if (mv != sample->GetMeasurementVector(4))
    {
    std::cerr << "FAILED" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cerr << "PASSED" << std::endl;
    }

  // frequency
  std::cerr << "Trying GetTotalFrequency...";
  if (sample->GetTotalFrequency() != sampleSize)
    {
    std::cerr << "FAILED" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cerr << "PASSED" << std::endl;
    }

  //
  // iterator tests
  //
  std::cerr << "Iterators..." << std::endl;
    {
    // forward iterator
    std::cerr << "Trying Iterator::Copy Constructor...";
    typedef SampleType::Iterator IteratorType;

    IteratorType s_iter = sample->Begin();

    // copy constructor
    IteratorType bs_iter(s_iter);
    if (bs_iter != s_iter)
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    // assignment operator
    std::cerr << "Trying Iterator::assignment operator...";
    IteratorType assignment_iter( bs_iter );
    assignment_iter = s_iter;
    if (assignment_iter != s_iter)
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    SampleType::InstanceIdentifier id = 0;
    while (s_iter != sample->End())
      {
      std::cerr << "Trying Iterator::GetMeasurementVector (forward)...";
      if (sample->GetMeasurementVector(id) !=
          s_iter.GetMeasurementVector())
        {
        std::cerr << "FAILED" << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cerr << "PASSED" << std::endl;
        }
      std::cerr << "Trying Iterator::GetInstanceIdentifier (forward)...";
      if (id != s_iter.GetInstanceIdentifier())
        {
        std::cerr << "FAILED" << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cerr << "PASSED" << std::endl;
        }
      std::cerr << "Trying Iterator::GetFrequency (forward)...";
      if (s_iter.GetFrequency() != 1)
        {
        std::cerr << "FAILED" << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cerr << "PASSED" << std::endl;
        }
      std::cerr << "Trying GetFrequency (forward)...";
      if (sample->GetFrequency(id) != 1)
        {
        std::cerr << "FAILED" << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cerr << "PASSED" << std::endl;
        }
      ++id;
      ++s_iter;
      }

    std::cerr << "Trying Iterator::End (forward)...";
    if (s_iter != sample->End())
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    }

  // ConstIterator test
  std::cerr << "Const Iterators..." << std::endl;
    {
    std::cerr << "Trying Iterator::Copy Constructor (from const)...";
    // forward iterator
    typedef SampleType::ConstIterator  ConstIteratorType;

    ConstIteratorType s_iter = sample->Begin();

    // copy constructor
    ConstIteratorType bs_iter(s_iter);
    if (bs_iter != s_iter)
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    // assignment operator
    std::cerr << "Trying Const Iterator::operator= ()...";
    ConstIteratorType assignment_iter( bs_iter );
    assignment_iter = s_iter;
    if (assignment_iter != s_iter)
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    // copy from non-const iterator
    std::cerr << "Trying Iterator::Copy Constructor (from non-const)...";
    SampleType::Iterator nonconst_iter = sample->Begin();
    SampleType::ConstIterator s2_iter(nonconst_iter);
    if (s2_iter != s_iter)
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }
    // assignment from non-const iterator
    std::cerr << "Trying Iterator::assignment (from non-const)...";
    s2_iter = nonconst_iter;
    if (s2_iter != s_iter)
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    SampleType::InstanceIdentifier id = 0;
    while (s_iter != sample->End())
      {
      std::cerr << "Trying Iterator::GetMeasurementVector (forward)...";
      if (sample->GetMeasurementVector(id) !=
          s_iter.GetMeasurementVector())
        {
        std::cerr << "FAILED" << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cerr << "PASSED" << std::endl;
        }

      std::cerr << "Trying Iterator::GetInstanceIdentifier (forward)...";
      if (id != s_iter.GetInstanceIdentifier())
        {
        std::cerr << "FAILED" << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cerr << "PASSED" << std::endl;
        }
      std::cerr << "Trying Iterator::GetFrequency (forward)...";
      if (s_iter.GetFrequency() != 1)
        {
        std::cerr << "FAILED" << std::endl;
        return EXIT_FAILURE;
        }
      else
        {
        std::cerr << "PASSED" << std::endl;
        }
      ++id;
      ++s_iter;
      }

    std::cerr << "Trying Iterator::End (forward)...";
    if (s_iter != sample->End())
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    }


  //
  // resizing
    std::cerr << "Trying Clear()...";
  sample->Clear();
  if (sample->Size() != 0)
    {
    std::cerr << "FAILED" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cerr << "PASSED" << std::endl;
    }

  std::cerr << "Trying Resize()...";
  sample->Resize(sampleSize);
  if (sample->Size() != sampleSize)
    {
    std::cerr << "FAILED" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cerr << "PASSED" << std::endl;
    }


  // Test a VariableSizeVector
  typedef itk::VariableLengthVector< float >
    VariableSizeMeasurementVectorType;

  typedef itk::Statistics::ListSample< VariableSizeMeasurementVectorType >
    VariableSizeListSampleType;

  VariableSizeListSampleType::Pointer variableSizeSample =
    VariableSizeListSampleType::New();

  const unsigned int initialSize = 19;
    variableSizeSample->SetMeasurementVectorSize( initialSize );

  unsigned int returnedSize =
    variableSizeSample->GetMeasurementVectorSize();

  if( initialSize != returnedSize )
    {
    std::cerr << "Error in Get/SetMeasurementVectorSize() " << std::endl;
    return EXIT_FAILURE;
    }

  VariableSizeMeasurementVectorType variableLenghtVector;
  const unsigned int newsize = 42;
  variableLenghtVector.SetSize( newsize );

  variableSizeSample->Clear();
  variableSizeSample->SetMeasurementVectorSize( newsize );
  variableSizeSample->PushBack( variableLenghtVector );

  // Attempt to resize a non-empty ListSample should throw an exception.
  try
    {
    variableSizeSample->SetMeasurementVectorSize( initialSize );
    std::cerr << "Failed to throw expected exception in SetMeasurementVectorSize() " << std::endl;
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject &excp )
    {
    std::cerr << "Caught expected exception: " << excp << std::endl;
    }


  // Now, verify that it can be changed
  const unsigned int initialSize2 = 37;
  variableSizeSample->Clear();
  variableSizeSample->SetMeasurementVectorSize( initialSize2 );

  const unsigned int returnedSize2 =
    variableSizeSample->GetMeasurementVectorSize();

  if( initialSize2 != returnedSize2 )
    {
    std::cerr << "Error in Get/SetMeasurementVectorSize() " << std::endl;
    std::cerr << "expected " << initialSize2 << std::endl;
    std::cerr << "but got  " << returnedSize2 << std::endl;
    return EXIT_FAILURE;
    }


  // Exercise the exception throwing when requesting
  // an element id that is outside the list range.
  const unsigned int largestId = sample->Size();
  bool exceptionWorks = false;
  try
    {
    MeasurementVectorType measurement =
      sample->GetMeasurementVector( largestId + 10 );
    std::cerr << measurement << std::endl;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Caught expected exception: " << excp << std::endl;
    exceptionWorks = true;
    }

  if( !exceptionWorks )
    {
    std::cerr << "GetMeasurementVector() exception failed !";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const double outOfRangeFrequency =
    sample->GetFrequency( largestId + 10 );

  if( outOfRangeFrequency != 0.0 )
    {
    std::cerr << "GetFrequency() failed for out of range Id";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Additional iterator tests
  //
  sample->Clear();
  MeasurementVectorType mvt( measurementVectorSize );
  for ( unsigned int i = 0; i < sampleSize; i++ )
    {
    for (unsigned int j = 0; j < measurementVectorSize; j++ )
      {
      mvt[j] = j + i*i;
      }
    sample->PushBack(mv);
    }


  // Testing methods specific to Iterators
    {
    typedef SampleType::Iterator IteratorType;
    IteratorType iter = sample->Begin();
    IteratorType iter2 = sample->Begin();

    std::cerr << "Trying Iterator operator=()...";
    iter2 = iter;
    if( iter2 != iter )
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    std::cerr << "Trying Iterator constructor from sample...";
    IteratorType iter3 = sample->Begin();
    if( iter3 != sample->Begin() )
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    std::cerr << "Trying Iterator walk...";
    unsigned int counter = 0;
    while( iter3 != sample->End() )
      {
      ++iter3;
      counter++;
      }

    if( counter != sample->Size() )
      {
      std::cerr << "FAILED. counter: " << counter
                << " != sample->Size(): " << sample->Size()
                << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    std::cerr << "Trying Iterator copy constructor...";
    IteratorType iter4( iter2 );
    if( iter4 != iter2 )
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    std::cerr << "Trying Iterator operator=...";
    IteratorType iter5 = iter2;
    if( iter5 != iter2 )
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    std::cerr << "Trying Iterator Constructor with instance identifier 7...";
    IteratorType iter6( sample );
    for(unsigned int kk=0; kk<7; kk++ )
      {
      ++iter6;
      }

    if( iter6.GetInstanceIdentifier() != 7 )
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    }

  // Testing methods specific to ConstIterators
    {
    std::cerr << "Trying ConstIterator operator!=() and operator=()...";
    typedef SampleType::ConstIterator ConstIteratorType;
    ConstIteratorType iter = sample->Begin();
    ConstIteratorType iter2 = sample->End();

    iter2 = iter;

    if( iter2 != iter )
      {
      std::cerr << "ConstIterator operator!=() or operator=() FAILED" << std::endl;
      return EXIT_FAILURE;
      }

    if( !( iter2 == iter ) )
      {
      std::cerr << "ConstIterator operator==() FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    std::cerr << "Trying ConstIterator copy constructor...";
    ConstIteratorType iter3( iter2 );
    if( iter3 != iter2 )
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    std::cerr << "Trying Constructor from const container Begin() differs from non-const Begin()...";
    const SampleType * constSample = sample.GetPointer();

    ConstIteratorType iter4( constSample->Begin() );
    ConstIteratorType iter5( sample->Begin() );
    if( iter4 != iter5 )
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    std::cerr << "Trying ConstIterator Constructor from const container differs from non-const container...";
    ConstIteratorType iter6( constSample );
    ConstIteratorType iter7( sample );
    if( iter6 != iter7 )
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    std::cerr << "Trying Constructor with instance identifier 0...";
    ConstIteratorType iter8( sample );
    if( iter8.GetInstanceIdentifier() != 0 )
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }


    ConstIteratorType iter9( sample );
    std::cerr << "Trying Instance identifier = "
              << iter9.GetInstanceIdentifier()
              << "...";
    for(unsigned int kk=0; kk<7; kk++ )
      {
      ++iter9;
      }

    MeasurementVectorType vector9a = iter9.GetMeasurementVector();
    MeasurementVectorType vector9b = sample->GetMeasurementVector( 7 );
    for( unsigned int kitr =0; kitr < measurementVectorSize; kitr++ )
      {
      if( itk::Math::abs( vector9b[kitr] - vector9a[kitr] ) )
        {
        std::cerr << "Constructor with container followed by increments failed" << std::endl;
        std::cerr << "Expected " << vector9b << std::endl;
        std::cerr << "Received " << vector9a << std::endl;
        return EXIT_FAILURE;
        }
      }
    std::cerr << "PASSED" << std::endl;

    std::cerr << "Trying ConstIterator constructor from sample...";
    unsigned int counter = 0;
    ConstIteratorType iter10( constSample );
    if( iter10 != constSample->Begin() )
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }


    std::cerr << "Trying Iterator walk...";
    while( iter10 != constSample->End() )
      {
      ++iter10;
      counter++;
      }

    if( counter != constSample->Size() )
      {
      std::cerr << "FAILED" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cerr << "PASSED" << std::endl;
      }

    }

  std::cerr << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
