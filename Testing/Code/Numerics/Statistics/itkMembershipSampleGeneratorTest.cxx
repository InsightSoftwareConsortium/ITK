/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMembershipSampleGeneratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkListSample.h"
#include "itkMembershipSample.h"
#include "itkMembershipSampleGenerator.h"
#include "itkVector.h"

int itkMembershipSampleGeneratorTest( int, char** )
{
  // The following code snippet will create a \code{ListSample} object
  // with three-component float measurement vectors and put three
  // measurement vectors in tht \code{ListSample} object.
  // Software Guide : EndLatex

  typedef itk::Vector< float, 3 > MeasurementVectorType ;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;
  SampleType::Pointer sample = SampleType::New() ;
  MeasurementVectorType mv ;
  mv[0] = 1.0 ;
  mv[1] = 2.0 ;
  mv[2] = 4.0 ;
  
  sample->PushBack(mv) ;

  mv[0] = 2.0 ;
  mv[1] = 4.0 ;
  mv[2] = 5.0 ;
  sample->PushBack(mv) ;
  
  mv[0] = 3.0 ;
  mv[1] = 8.0 ;
  mv[2] = 6.0 ;
  sample->PushBack(mv) ;

  
  typedef itk::Vector< unsigned int, 1 > ClassMaskVectorType ;
  typedef itk::Statistics::ListSample< ClassMaskVectorType > 
    ClassMaskSampleType ;
  ClassMaskSampleType::Pointer mask = ClassMaskSampleType::New() ;
  ClassMaskVectorType m ;
  m[0] = 0 ;
  mask->PushBack( m ) ;
  m[0] = 0 ;
  mask->PushBack( m ) ;
  m[0] = 1 ;
  mask->PushBack( m ) ;

  typedef itk::Statistics::MembershipSampleGenerator< SampleType, 
    ClassMaskSampleType > GeneratorType ;
  
  GeneratorType::Pointer generator = 
    GeneratorType::New() ;

  generator->SetInput( sample ) ;
  generator->SetClassMask ( mask ) ;
  generator->SetNumberOfClasses( 2 ) ;
  generator->GenerateData() ;
  
  GeneratorType::OutputType::Pointer membershipSample = 
    generator->GetOutput() ;

  if (  membershipSample->Size() !=  3 )
    {
    std::cout << "Test failed: Wrong total size." << std::endl ;
    return EXIT_FAILURE ;
    }

  if ( membershipSample->GetClassSampleSize(0) != 2 || 
       membershipSample->GetClassSampleSize(1) != 1 )
    {
    std::cout << "Test failed: Wrong class sample size." << std::endl ;
    return EXIT_FAILURE ;
    }

  for ( unsigned long id = 0 ; id < membershipSample->Size() ; ++id )
    {
    if ( membershipSample->GetClassLabel(id) != 
         (mask->GetMeasurementVector(id))[0] )
      {
      std::cout << "Test failed: Class labels do not match between the generated MembershipSample and the class mask sample." << std::endl ;
      return EXIT_FAILURE ;
      }
    }
 
  std::cout << "Test succeeded." << std::endl ;
  return EXIT_SUCCESS ;
}
