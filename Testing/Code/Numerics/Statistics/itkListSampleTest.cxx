/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSampleTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkVector.h"
#include "itkListSample.h"

int itkListSampleTest(int, char**) 
{
  std::cout << "ListSample Test \n \n"; 
  bool pass = true;
  std::string whereFail = "" ;

  typedef itk::Vector< float, 3 > MeasurementVectorType ;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;

  unsigned int sampleSize = 50 ;

  SampleType::Pointer sample = SampleType::New() ;

  MeasurementVectorType mv ;
  for ( unsigned int i = 0 ; i < sampleSize ; i++ )
    {
      for (unsigned int j = 0 ; j < 3 ; j++ )
        {
          mv[j] = (float)rand() / (float)(RAND_MAX+1)  ;
        }
      sample->PushBack(mv) ;
    }

  // tests begin

  if ( sampleSize != sample->Size() )
    {
      pass = false ;
      whereFail = "Size()" ;
    }

  if ( sampleSize != sample->Size(0) )
    {
      pass = false ;
      whereFail = "Size(0)" ;
    }

  if ( sampleSize != sample->GetNumberOfInstances())
    {
      pass = false ;
      whereFail = "GetNumberOfInstances()" ;
    }

  mv = sample->GetMeasurementVector(4) ;
  if ( mv != sample->GetMeasurementVector(4) )
    {
      pass = false ;
      whereFail = "GetMeasurementVector()" ;
    }

  // iterator test
  SampleType::Iterator s_iter = sample->Begin() ;
  unsigned int count = 0 ;
  unsigned int id = 0 ;
  while (s_iter != sample->End())
    {
      if (sample->GetMeasurementVector(id) != 
          s_iter.GetMeasurementVector())
        {
          pass = false ;
          whereFail = "Iterator: GetMeasurementVector()" ;
        }
      ++count ;
      ++id ;
      ++s_iter ;
    }

  if (s_iter != sample->End())
    {
      pass = false ;
      whereFail = "Iterator: End()" ;
    }

  if( !pass )
    {
      std::cout << "Test failed in " << whereFail << "." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}



