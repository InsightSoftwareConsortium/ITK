/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSelectiveSubsampleGeneratorTest.cxx
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
#include "itkSelectiveSubsampleGenerator.h"
#include "itkVector.h"

int itkSelectiveSubsampleGeneratorTest( int, char* [] )
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
  m[0] = 1 ;
  mask->PushBack( m ) ;
  m[0] = 2 ;
  mask->PushBack( m ) ;

  typedef itk::Statistics::SelectiveSubsampleGenerator< SampleType, 
    ClassMaskSampleType > GeneratorType ;
  
  GeneratorType::Pointer generator = 
    GeneratorType::New() ;

  GeneratorType::ClassLabelVectorType selectedClassLabels ;
  selectedClassLabels.push_back( 0 ) ;
  selectedClassLabels.push_back( 2 ) ;

  generator->SetInput( sample ) ;
  generator->SetClassMask ( mask ) ;
  generator->SetSelectedClassLabels( selectedClassLabels ) ;
  generator->GenerateData() ;
  
  GeneratorType::OutputType::Pointer subsample = 
    generator->GetOutput() ;

  if (  subsample->Size() !=  2 )
    {
    std::cout << "Test failed: Wrong total size." << std::endl ;
    return EXIT_FAILURE ;
    }

  if ( subsample->GetInstanceIdentifier( 0 ) != 0 ||
       subsample->GetInstanceIdentifier( 1 ) != 2 )
    {
    std::cout << "Test failed: Wrong instances are included or Some instances are missing." << std::endl ;
    return EXIT_FAILURE ;
    }
 
  std::cout << "Test succeeded." << std::endl ;
  return EXIT_SUCCESS ;
}
