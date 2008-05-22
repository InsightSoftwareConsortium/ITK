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
#include "itkListSample.h"

int itkListSampleTest(int argc, char *argv[] ) 
{
  std::cout << "ListSample Test \n \n"; 
  if( argc< 2 ) 
    {
    std::cerr << "itkListSampleTest LengthOfMeasurementVector" << std::endl;
    }
    
  typedef itk::Array< float > MeasurementVectorType ;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;

  SampleType::MeasurementVectorSizeType measurementVectorSize = atoi(argv[1]);
  std::cerr << "Measurement vector size: " << measurementVectorSize 
            << std::endl;

  unsigned int sampleSize = 25;

  SampleType::Pointer sample = SampleType::New() ;

  sample->SetMeasurementVectorSize( measurementVectorSize );

  MeasurementVectorType mv( measurementVectorSize ) ;
  for ( unsigned int i = 0 ; i < sampleSize ; i++ )
    {
    for (unsigned int j = 0 ; j < measurementVectorSize ; j++ )
      {
      mv[j] = rand() / (RAND_MAX+1.0)  ;
      }
    sample->PushBack(mv) ;
    }

  // tests begin

  //
  // general interface
  //
  std::cerr << "General interface..." << std::endl;
  std::cerr << "  if ( sampleSize != sample->Size() )" << std::endl;
  if ( sampleSize != sample->Size() )
    {
    std::cerr << "Size() failed" << std::endl;
    return EXIT_FAILURE;
    }

  std::cerr << "  if (sample->GetMeasurementVectorSize() != measurementVectorSize)" << std::endl;
  if (sample->GetMeasurementVectorSize() != measurementVectorSize)
    {
    std::cerr << "GetMeasurementVectorSize() failed" << std::endl;
    return EXIT_FAILURE;
    }

  // get and set measurements
  std::cerr << "  mv = sample->GetMeasurementVector(4) ;" << std::endl;
  mv = sample->GetMeasurementVector(4) ;
  std::cerr << "  if ( mv != sample->GetMeasurementVector(4) )" << std::endl;
  if ( mv != sample->GetMeasurementVector(4) )
    {
    std::cerr << "GetMeasurementVector failed" << std::endl;
    return EXIT_FAILURE;
    }

  std::cerr << "  float tmp = mv[0];" << std::endl;
  float tmp = mv[0];
  std::cerr << "  mv[0] += 1.0;" << std::endl;
  mv[0] += 1.0;
  std::cerr << "  sample->SetMeasurementVector(4,mv);" << std::endl;
  sample->SetMeasurementVector(4,mv);
  std::cerr << "  if (mv != sample->GetMeasurementVector(4))" << std::endl;
  if (mv != sample->GetMeasurementVector(4))
    {
    std::cerr << "SetMeasurementVector failed" << std::endl;
    return EXIT_FAILURE;
    }  

  std::cerr << "  mv[0] = tmp;" << std::endl;
  mv[0] = tmp;
  std::cerr << "  sample->SetMeasurement(4,0,tmp);" << std::endl;
  sample->SetMeasurement(4,0,tmp);
  std::cerr << "  if (mv != sample->GetMeasurementVector(4))" << std::endl;
  if (mv != sample->GetMeasurementVector(4))
    {
    std::cerr << "SetMeasurement failed" << std::endl;
    return EXIT_FAILURE;
    }  
  
  // frequency
  std::cerr << "  if (sample->GetTotalFrequency() != sampleSize)" << std::endl;
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
    SampleType::Iterator s_iter = sample->Begin() ;
    
    // copy constructor
    SampleType::Iterator bs_iter(s_iter);
    if (bs_iter != s_iter)
      {
      std::cerr << "Iterator::Copy Constructor failed" << std::endl;
      return EXIT_FAILURE;    
      }
    
    SampleType::InstanceIdentifier id = 0 ;
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
      ++id ;
      ++s_iter ;
      }
    
    if (s_iter != sample->End())
      {
      std::cerr << "Iterator::End (forward) failed" << std::endl;
      return EXIT_FAILURE;    
      }
    
    // backwards iterator
    do 
      {
      --s_iter;
      --id;
      if (sample->GetMeasurementVector(id) != 
          s_iter.GetMeasurementVector())
        {
        std::cerr << "Iterator::GetMeasurementVector (backward) failed" 
                  << std::endl;
        return EXIT_FAILURE;
        }
      if (id != s_iter.GetInstanceIdentifier())
        {
        std::cerr << "Iterator::GetInstanceIdentifier (backward) failed" 
                  << std::endl;
        return EXIT_FAILURE;
        }      
      } while (!(s_iter == sample->Begin())); // explicitly test ==
        
    if (!(s_iter == sample->Begin()))
      {
      std::cerr << "Iterator::Begin (backward) failed" << std::endl;
      return EXIT_FAILURE;    
      }
    }

  // ConstIterator test
    std::cerr << "Const Iterators..." << std::endl;
    {
    // forward iterator
    SampleType::ConstIterator s_iter = sample->Begin() ;
    
    // copy constructor
    SampleType::ConstIterator bs_iter(s_iter);
    if (bs_iter != s_iter)
      {
      std::cerr << "Iterator::Copy Constructor (from const) failed" 
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
    
    SampleType::InstanceIdentifier id = 0 ;
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
      ++id ;
      ++s_iter ;
      }
    
    if (s_iter != sample->End())
      {
      std::cerr << "Iterator::End (forward) failed" << std::endl;
      return EXIT_FAILURE;    
      }
    
    // backwards iterator
    do 
      {
      --s_iter;
      --id;
      if (sample->GetMeasurementVector(id) != 
          s_iter.GetMeasurementVector())
        {
        std::cerr << "Iterator::GetMeasurementVector (backward) failed" 
                  << std::endl;
        return EXIT_FAILURE;
        }
      if (id != s_iter.GetInstanceIdentifier())
        {
        std::cerr << "Iterator::GetInstanceIdentifier (backward) failed" 
                  << std::endl;
        return EXIT_FAILURE;
        }      
      } while (!(s_iter == sample->Begin())); // explicitly test ==
        
    if (!(s_iter == sample->Begin()))
      {
      std::cerr << "Iterator::Begin (backward) failed" << std::endl;
      return EXIT_FAILURE;    
      }
    }

    std::cerr << "Search..." << std::endl;    
    SampleType::SearchResultVectorType searchResult ;
    sample->Search(sample->GetMeasurementVector(sampleSize/2), 0.01, 
                   searchResult) ;


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

    std::cout << "Test passed." << std::endl;
    return EXIT_SUCCESS;
}



