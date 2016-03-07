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
#include "itkSubsample.h"

int itkSubsampleTest2( int, char * [] )
{
  std::cout << "Subsample Test \n \n";

  typedef itk::Array< float > MeasurementVectorType;

  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;

  SampleType::MeasurementVectorSizeType measurementVectorSize = 3;
  std::cerr << "Measurement vector size: " << measurementVectorSize
            << std::endl;

  unsigned int sampleSize = 10;

  SampleType::Pointer sample = SampleType::New();

  sample->SetMeasurementVectorSize( measurementVectorSize );

  MeasurementVectorType mv( measurementVectorSize );
  for ( unsigned int i = 0; i < sampleSize; i++ )
    {
    for (unsigned int j = 0; j < measurementVectorSize; j++ )
      {
      mv[j] = j + i * measurementVectorSize;
      }
    std::cout << "Adding measurement vector: " << mv << std::endl;
    sample->PushBack(mv);
    }

  // tests begin
  typedef itk::Statistics::Subsample< SampleType >   SubsampleType;
  SubsampleType::Pointer   subSample = SubsampleType::New();
  subSample->SetSample( sample );

  //Add measurment vectors in sample with even id number to subSample
  for ( unsigned int i=0; i < sample->Size(); i= i+2 )
    {
    subSample->AddInstance( i );
    std::cout << "Adding instance: " << i << " to subSample" << std::endl;
    }

  if ( subSample->Size() != 5 )
    {
    std::cerr << "Size of the subsample container should be 5" << std::endl;
    return EXIT_FAILURE;
    }

  for ( unsigned int i=0; i < subSample->Size(); i++ )
    {
    std::cout << "Measurment Vector: " << i << "\t"
              << subSample->GetMeasurementVector( i ) << std::endl;

    if ( subSample->GetMeasurementVector( i ) !=
                sample->GetMeasurementVector( i*2 ) )
      {
      std::cerr << "Subsampling is not correctly done!" << std::endl;
      return EXIT_FAILURE;
      }
    }

  typedef itk::Statistics::Subsample< SubsampleType >   CascadedSubsampleType;
  CascadedSubsampleType::Pointer  subSample2  = CascadedSubsampleType::New();
  subSample2->SetSample( subSample );

  //Add measurment vectors in subsample with even id number to subSample2
  for ( unsigned int i=0; i < subSample->Size(); i= i+2 )
    {
    std::cout << "Adding instance: " << i << " to subSample2" << std::endl;
    subSample2->AddInstance( i );
    }

  if ( subSample2->Size() != 3 )
    {
    std::cerr << "Size of the subsample2 container should be 3" << std::endl;
    return EXIT_FAILURE;
    }

  subSample2->Swap( 0,2 );

  //swap back
  subSample2->Swap( 2,0 );

  for ( unsigned int i=0; i < subSample2->Size(); i++ )
    {
    std::cout << "Measurment Vector: " << i << "\t"
              << subSample2->GetMeasurementVector( i ) << std::endl;

    if ( subSample2->GetMeasurementVector( i ) !=
                sample->GetMeasurementVector( i*4 ) )
      {
      std::cerr << "Subsampling of a subsample is not correctly done!" << std::endl;
      return EXIT_FAILURE;
      }
    }

  std::cout << subSample2->GetSample() << std::endl;

  typedef CascadedSubsampleType::Iterator IteratorType;

  IteratorType iter = subSample2->Begin();
  while( iter != subSample2->End() )
    {
    std::cout << iter.GetInstanceIdentifier() << " ";
    std::cout << iter.GetMeasurementVector() << " ";
    std::cout << iter.GetFrequency() << std::endl;
    ++iter;
    }

  typedef CascadedSubsampleType::ConstIterator ConstIteratorType;

  IteratorType citer = subSample2->Begin();
  while( citer != subSample2->End() )
    {
    std::cout << citer.GetInstanceIdentifier() << " ";
    std::cout << citer.GetMeasurementVector() << " ";
    std::cout << citer.GetFrequency() << std::endl;
    ++citer;
    }

  IteratorType iter1 = subSample2->Begin();
  IteratorType iter2 = subSample2->Begin();

  citer = iter1;

  if( citer != iter1 )
    {
    std::cerr << "Error in iterator != operator " << std::endl;
    return EXIT_FAILURE;
    }


  if( !(citer == iter1) )
    {
    std::cerr << "Error in iterator == operator " << std::endl;
    return EXIT_FAILURE;
    }


  if( iter1 != iter2 )
    {
    std::cerr << "Error in iterator != operator " << std::endl;
    return EXIT_FAILURE;
    }


  if( !(iter1 == iter2) )
    {
    std::cerr << "Error in iterator == operator " << std::endl;
    return EXIT_FAILURE;
    }


  //
  // Additional iterator tests
  //

  // Testing methods specific to Iterators
    {
    IteratorType iter7 = subSample2->Begin();
    IteratorType iter8 = subSample2->End();

    iter8 = iter7;
    if( iter8 != iter7 )
      {
      std::cerr << "Iterator operator=() failed" << std::endl;
      return EXIT_FAILURE;
      }

    IteratorType iter3 = subSample2->Begin();
    if( iter3 != subSample2->Begin() )
      {
      std::cerr << "Iterator constructor from sample failed" << std::endl;
      return EXIT_FAILURE;
      }

    unsigned int counter = 0;
    while( iter3 != subSample2->End() )
      {
      ++iter3;
      counter++;
      }

    if( counter != subSample2->Size() )
      {
      std::cerr << "Iterator walk failed" << std::endl;
      return EXIT_FAILURE;
      }

    IteratorType iter4( iter8 );
    if( iter4 != iter8 )
      {
      std::cerr << "Iterator copy constructor failed" << std::endl;
      return EXIT_FAILURE;
      }

    IteratorType iter5 = iter8;
    if( iter5 != iter8 )
      {
      std::cerr << "Iterator operator= failed" << std::endl;
      return EXIT_FAILURE;
      }

    IteratorType iter6( subSample2 );
    unsigned int targetEntry = 2;
    for(unsigned int kk=0; kk<targetEntry; kk++ )
      {
      std::cout << "GetInstanceIdentifier() = " << iter6.GetInstanceIdentifier() << std::endl;
      ++iter6;
      }

    if( iter6.GetInstanceIdentifier() != targetEntry )
      {
      std::cerr << "Iterator Constructor with instance identifier failed" << std::endl;
      std::cerr << "Expected identifier = " << targetEntry << std::endl;
      std::cerr << "identifier returned = " << iter6.GetInstanceIdentifier() << std::endl;
      return EXIT_FAILURE;
      }

    }

  // Testing methods specific to ConstIterators
    {
    ConstIteratorType iter11 = subSample2->Begin();
    ConstIteratorType iter12 = subSample2->End();

    iter12 = iter11;

    if( iter12 != iter11 )
      {
      std::cerr << "ConstIterator operator!=() or operator=() failed" << std::endl;
      return EXIT_FAILURE;
      }

    if( !( iter12 == iter11 ) )
      {
      std::cerr << "ConstIterator operator==() failed" << std::endl;
      return EXIT_FAILURE;
      }

    ConstIteratorType iter3( iter12 );
    if( iter3 != iter12 )
      {
      std::cerr << "ConstIterator copy constructor failed" << std::endl;
      return EXIT_FAILURE;
      }

    const CascadedSubsampleType * constSample = subSample2.GetPointer();

    ConstIteratorType iter4( constSample->Begin() );
    ConstIteratorType iter5( subSample2->Begin() );
    if( iter4 != iter5 )
      {
      std::cerr << "Constructor from const container Begin() differs from non-const Begin() " << std::endl;
      return EXIT_FAILURE;
      }

    ConstIteratorType iter6( constSample );
    ConstIteratorType iter7( subSample2 );
    if( iter6 != iter7 )
      {
      std::cerr << "ConstIterator Constructor from const container differs from non-const container" << std::endl;
      return EXIT_FAILURE;
      }

    ConstIteratorType iter8( subSample2 );
    if( iter8.GetInstanceIdentifier() != 0 )
      {
      std::cerr << "Constructor with instance identifier 0 failed" << std::endl;
      return EXIT_FAILURE;
      }


    ConstIteratorType iter9( subSample2 );
    unsigned int targetEntry = 2;
    for( unsigned int kk = 0; kk < targetEntry; kk++ )
      {
      std::cout << "Instance identifier = " << iter9.GetInstanceIdentifier() << std::endl;
      ++iter9;
      }

    std::cout << "Instance identifier = " << iter9.GetInstanceIdentifier() << std::endl;
    MeasurementVectorType vector9a = iter9.GetMeasurementVector();
    MeasurementVectorType vector9b = subSample2->GetMeasurementVector( targetEntry );
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

    unsigned int counter = 0;
    ConstIteratorType iter10( constSample );
    if( iter10 != constSample->Begin() )
      {
      std::cerr << "ConstIterator constructor from sample failed" << std::endl;
      return EXIT_FAILURE;
      }


    while( iter10 != constSample->End() )
      {
      ++iter10;
      counter++;
      }

    if( counter != constSample->Size() )
      {
      std::cerr << "Iterator walk failed" << std::endl;
      return EXIT_FAILURE;
      }

    }

  return EXIT_SUCCESS;
}
