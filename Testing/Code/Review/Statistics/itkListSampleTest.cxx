/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkListSampleTest.cxx
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

#include "itkArray.h"
#include "itkVariableLengthVector.h"
#include "itkListSample.h"

int itkListSampleTest(int argc, char *argv[] ) 
{
  std::cout << "ListSample Test \n \n"; 
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

  std::cout << "Sample length = " << sample->GetMeasurementVectorSize() << std::endl;
  std::cout << "Vector length = " << itk::Statistics::MeasurementVectorTraits::GetLength( mv ) << std::endl;

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
  if ( sampleSize != sample->Size() )
    {
    std::cerr << "Size() failed" << std::endl;
    return EXIT_FAILURE;
    }

  if (sample->GetMeasurementVectorSize() != measurementVectorSize)
    {
    std::cerr << "GetMeasurementVectorSize() failed" << std::endl;
    return EXIT_FAILURE;
    }

  // get and set measurements
  mv = sample->GetMeasurementVector(4);
  if ( mv != sample->GetMeasurementVector(4) )
    {
    std::cerr << "GetMeasurementVector failed" << std::endl;
    return EXIT_FAILURE;
    }

  float tmp = mv[0];
  mv[0] += 1.0;
  sample->SetMeasurementVector(4,mv);
  if (mv != sample->GetMeasurementVector(4))
    {
    std::cerr << "SetMeasurementVector failed" << std::endl;
    return EXIT_FAILURE;
    }  

  mv[0] = tmp;
  sample->SetMeasurement(4,0,tmp);
  if (mv != sample->GetMeasurementVector(4))
    {
    std::cerr << "SetMeasurement failed" << std::endl;
    return EXIT_FAILURE;
    }  
  
  // frequency
  if (sample->GetTotalFrequency() != sampleSize)
    {
    std::cerr << "GetTotalFrequency failed" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // iterator tests
  //
  std::cerr << "Iterators..." << std::endl;
    {
    // forward iterator
    typedef SampleType::Iterator IteratorType;
    
    IteratorType s_iter = sample->Begin();
    
    // copy constructor
    IteratorType bs_iter(s_iter);
    if (bs_iter != s_iter)
      {
      std::cerr << "Iterator::Copy Constructor failed" << std::endl;
      return EXIT_FAILURE;
      }
    
    // assignment operator 
    IteratorType assignment_iter( bs_iter );
    assignment_iter = s_iter;
    if (assignment_iter != s_iter)
      {
      std::cerr << "Iterator::assignment operator failed" << std::endl;
      return EXIT_FAILURE;
      }

    SampleType::InstanceIdentifier id = 0;
    while (s_iter != sample->End())
      {
      if (sample->GetMeasurementVector(id) != 
          s_iter.GetMeasurementVector())
        {
        std::cerr << "Iterator::GetMeasurementVector (forward) failed" 
                  << std::endl;
        return EXIT_FAILURE;
        }
      if (id != s_iter.GetInstanceIdentifier())
        {
        std::cerr << "Iterator::GetInstanceIdentifier (forward) failed" 
                  << std::endl;
        return EXIT_FAILURE;
        }
      if (s_iter.GetFrequency() != 1)
        {
        std::cerr << "Iterator::GetFrequency (forward) failed" << std::endl;
        return EXIT_FAILURE;
        }
      if (sample->GetFrequency(id) != 1)
        {
        std::cerr << "GetFrequency (forward) failed" << std::endl;
        return EXIT_FAILURE;
        }
      ++id;
      ++s_iter;
      }
    
    if (s_iter != sample->End())
      {
      std::cerr << "Iterator::End (forward) failed" << std::endl;
      return EXIT_FAILURE;
      }
    
    }

  // ConstIterator test
  std::cerr << "Const Iterators..." << std::endl;
    {
    // forward iterator
    typedef SampleType::ConstIterator  ConstIteratorType;
    
    ConstIteratorType s_iter = sample->Begin();
    
    // copy constructor
    ConstIteratorType bs_iter(s_iter);
    if (bs_iter != s_iter)
      {
      std::cerr << "Iterator::Copy Constructor (from const) failed" 
                << std::endl;
      return EXIT_FAILURE;
      }

    // assignment operator
    ConstIteratorType assignment_iter( bs_iter );
    assignment_iter = s_iter;
    if (assignment_iter != s_iter)
      {
      std::cerr << "Const Iterator::operator= () failed" 
                << std::endl;
      return EXIT_FAILURE;
      }

    // copy from non-const iterator
    SampleType::Iterator nonconst_iter = sample->Begin();
    SampleType::ConstIterator s2_iter(nonconst_iter);
    if (s2_iter != s_iter)
      {
      std::cerr << "Iterator::Copy Constructor (from non-const) failed" 
                << std::endl;
      return EXIT_FAILURE;
      }
    // assignment from non-const iterator
    s2_iter = nonconst_iter;
    if (s2_iter != s_iter)
      {
      std::cerr << "Iterator::assignment (from non-const) failed" << std::endl;
      return EXIT_FAILURE;
      }
    
    SampleType::InstanceIdentifier id = 0;
    while (s_iter != sample->End())
      {
      if (sample->GetMeasurementVector(id) != 
          s_iter.GetMeasurementVector())
        {
        std::cerr << "Iterator::GetMeasurementVector (forward) failed" 
                  << std::endl;
        return EXIT_FAILURE;
        }
      if (id != s_iter.GetInstanceIdentifier())
        {
        std::cerr << "Iterator::GetInstanceIdentifier (forward) failed" 
                  << std::endl;
        return EXIT_FAILURE;
        }
      if (s_iter.GetFrequency() != 1)
        {
        std::cerr << "Iterator::GetFrequency (forward) failed" << std::endl;
        return EXIT_FAILURE;
        }
      ++id;
      ++s_iter;
      }
    
    if (s_iter != sample->End())
      {
      std::cerr << "Iterator::End (forward) failed" << std::endl;
      return EXIT_FAILURE;
      }
    
    }


  //
  // resizing
  //
  sample->Clear();
  if (sample->Size() != 0)
    {
    std::cerr << "Clear() failed" << std::endl;
    return EXIT_FAILURE;
    }

  sample->Resize(sampleSize);
  if (sample->Size() != sampleSize)
    {
    std::cerr << "Resize() failed" << std::endl;
    return EXIT_FAILURE;
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
  catch( itk::ExceptionObject & )
    {
    std::cout << "Caught expected exception" << std::endl;
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
    std::cout << measurement << std::endl;
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cout << excp << std::endl;
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

    iter2 = iter;
    if( iter2 != iter )
      {
      std::cerr << "Iterator operator=() failed" << std::endl;
      return EXIT_FAILURE;
      }

    IteratorType iter3 = sample->Begin();
    if( iter3 != sample->Begin() )
      {
      std::cerr << "Iterator constructor from sample failed" << std::endl;
      return EXIT_FAILURE;
      }

    unsigned int counter = 0;
    while( iter3 != sample->End() )
      {
      ++iter3;
      counter++;
      }

    if( counter != sample->Size() )
      {
      std::cerr << "Iterator walk failed" << std::endl;
      return EXIT_FAILURE;
      }

    IteratorType iter4( iter2 ); 
    if( iter4 != iter2 )
      {
      std::cerr << "Iterator copy constructor failed" << std::endl;
      return EXIT_FAILURE;
      }

    IteratorType iter5 = iter2; 
    if( iter5 != iter2 )
      {
      std::cerr << "Iterator operator= failed" << std::endl;
      return EXIT_FAILURE;
      }

    IteratorType iter6( sample );
    for(unsigned int kk=0; kk<7; kk++ )
      {
      ++iter6;
      }

    if( iter6.GetInstanceIdentifier() != 7 )
      {
      std::cerr << "Iterator Constructor with instance identifier 7 failed" << std::endl;
      return EXIT_FAILURE;
      }

    }

  // Testing methods specific to ConstIterators
    {
    typedef SampleType::ConstIterator ConstIteratorType;
    ConstIteratorType iter = sample->Begin();
    ConstIteratorType iter2 = sample->End();

    iter2 = iter;

    if( iter2 != iter )
      {
      std::cerr << "ConstIterator operator!=() or operator=() failed" << std::endl;
      return EXIT_FAILURE;
      }

    if( !( iter2 == iter ) )
      {
      std::cerr << "ConstIterator operator==() failed" << std::endl;
      return EXIT_FAILURE;
      }

    ConstIteratorType iter3( iter2 ); 
    if( iter3 != iter2 )
      {
      std::cerr << "ConstIterator copy constructor failed" << std::endl;
      return EXIT_FAILURE;
      }

    const SampleType * constSample = sample.GetPointer();

    ConstIteratorType iter4( constSample->Begin() ); 
    ConstIteratorType iter5( sample->Begin() );
    if( iter4 != iter5 )
      {
      std::cerr << "Constructor from const container Begin() differs from non-const Begin() " << std::endl;
      return EXIT_FAILURE;
      }

    ConstIteratorType iter6( constSample ); 
    ConstIteratorType iter7( sample );
    if( iter6 != iter7 )
      {
      std::cerr << "ConstIterator Constructor from const container differs from non-const container" << std::endl;
      return EXIT_FAILURE;
      }

    ConstIteratorType iter8( sample );
    if( iter8.GetInstanceIdentifier() != 0 )
      {
      std::cerr << "Constructor with instance identifier 0 failed" << std::endl;
      return EXIT_FAILURE;
      }

    
    ConstIteratorType iter9( sample );
    for(unsigned int kk=0; kk<7; kk++ )
      {
      ++iter9;
      }
    
    std::cout << "Instance identifier = " << iter9.GetInstanceIdentifier() << std::endl;
    MeasurementVectorType vector9a = iter9.GetMeasurementVector();
    MeasurementVectorType vector9b = sample->GetMeasurementVector( 7 );
    for( unsigned int kitr =0; kitr < measurementVectorSize; kitr++ )
      {
      if( vnl_math_abs( vector9b[kitr] - vector9a[kitr] ) )
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
  
  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
