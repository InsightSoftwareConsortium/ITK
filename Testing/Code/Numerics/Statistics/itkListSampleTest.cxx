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
    
  bool pass = true;
  std::string whereFail = "" ;

  typedef itk::Array< float > MeasurementVectorType ;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;

  SampleType::MeasurementVectorSizeType measurementVectorSize = atoi(argv[1]);
  
  unsigned int sampleSize = 50 ;

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

  if ( sampleSize != sample->Size() )
    {
      pass = false ;
      whereFail = "Size()" ;
    }

  mv = sample->GetMeasurementVector(4) ;
  if ( mv != sample->GetMeasurementVector(4) )
    {
      pass = false ;
      whereFail = "GetMeasurementVector()" ;
    }

  // iterator test
  SampleType::Iterator s_iter = sample->Begin() ;
  unsigned int id = 0 ;
  while (s_iter != sample->End())
    {
      if (sample->GetMeasurementVector(id) != 
          s_iter.GetMeasurementVector())
        {
          pass = false ;
          whereFail = "Iterator: GetMeasurementVector()" ;
        }
      ++id ;
      ++s_iter ;
    }

  if (s_iter != sample->End())
    {
      pass = false ;
      whereFail = "Iterator: End()" ;
    }



  // ConstIterator test
  {
  SampleType::ConstIterator s_iter = sample->Begin();
  SampleType::ConstIterator s_end = sample->End();
  unsigned int id = 0 ;
  while ( s_iter != s_end )
    {
      if (sample->GetMeasurementVector(id) != 
          s_iter.GetMeasurementVector())
        {
          pass = false ;
          whereFail = "Iterator: GetMeasurementVector()" ;
        }
      ++id ;
      ++s_iter ;
    }

  if (s_iter != sample->End())
    {
      pass = false ;
      whereFail = "Iterator: End()" ;
    }

  }


  SampleType::SearchResultVectorType searchResult ;
  sample->Search(sample->GetMeasurementVector(25), 0.01, searchResult) ;

  if( !pass )
    {
      std::cout << "Test failed in " << whereFail << "." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}



