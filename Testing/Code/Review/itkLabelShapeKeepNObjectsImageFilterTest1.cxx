/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelShapeKeepNObjectsImageFilterTest1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkLabelShapeKeepNObjectsImageFilter.h"

#include "itkTestingMacros.h"

int itkLabelShapeKeepNObjectsImageFilterTest1(int argc, char * argv[])
{

  if( argc != 7 )
    {
    std::cerr << "Usage: " << argv[0] << " input output";
    std::cerr << " background numberOfObjectsToKeep";
    std::cerr << "reverseOrdering attribute" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 2;
  
  typedef itk::Image< unsigned char, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelShapeKeepNObjectsImageFilter< IType > LabelKeepNObjectsType;
  LabelKeepNObjectsType::Pointer KeepNObjects = LabelKeepNObjectsType::New();
  
  KeepNObjects->SetInput( reader->GetOutput() );
 
  //testing get/set BackgroundValue macro
  int BackgroundValue = ( atoi(argv[3]) );
  KeepNObjects->SetBackgroundValue( BackgroundValue );
  TEST_SET_GET_VALUE( BackgroundValue, KeepNObjects->GetBackgroundValue() );

  //testing get and set macros for Lambda 
  unsigned int numberOfObjects = atoi( argv[4] );
  KeepNObjects->SetNumberOfObjects( numberOfObjects );
  TEST_SET_GET_VALUE( numberOfObjects, KeepNObjects->GetNumberOfObjects() );

  //testing boolean macro for ReverseOrdering
  KeepNObjects->ReverseOrderingOn();
  TEST_SET_GET_VALUE( true, KeepNObjects->GetReverseOrdering() ); 

  KeepNObjects->ReverseOrderingOff();
  TEST_SET_GET_VALUE( false, KeepNObjects->GetReverseOrdering() );

  //testing get and set macros or ReverseOrdering 
  bool reverseOrdering = atoi( argv[5] );
  KeepNObjects->SetReverseOrdering( reverseOrdering );
  TEST_SET_GET_VALUE( reverseOrdering , KeepNObjects->GetReverseOrdering() ); 

  //testing get and set macros for Attribute 
  LabelKeepNObjectsType::AttributeType attribute = atoi( argv[6] );
  KeepNObjects->SetAttribute( attribute );
  TEST_SET_GET_VALUE( attribute, KeepNObjects->GetAttribute() );
 
  itk::SimpleFilterWatcher watcher(KeepNObjects, "filter");

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( KeepNObjects->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();
  
  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  std::cout << "Test Complete!" << std::endl;

  return EXIT_SUCCESS;
}
