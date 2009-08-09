/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryShapeKeepNObjectsImageFilterTest1.cxx
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

#include "itkBinaryShapeKeepNObjectsImageFilter.h"

#include "itkTestingMacros.h"

int itkBinaryShapeKeepNObjectsImageFilterTest1(int argc, char * argv[])
{

  if( argc != 9 )
    {
    std::cerr << "Usage: " << argv[0] << " input output";
    std::cerr << " foreground background numberOfObjectsToKeep";
    std::cerr << "reverseOrdering connectivity attribute" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 2;
  
  typedef itk::Image< unsigned char, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::BinaryShapeKeepNObjectsImageFilter< IType > BinaryKeepNObjectsType;
  BinaryKeepNObjectsType::Pointer KeepNObjects = BinaryKeepNObjectsType::New();
  
  KeepNObjects->SetInput( reader->GetOutput() );
 
  //testing get/set ForegroundValue macro
  int ForegroundValue = ( atoi(argv[3]) );
  KeepNObjects->SetForegroundValue( ForegroundValue );
  TEST_SET_GET_VALUE( ForegroundValue, KeepNObjects->GetForegroundValue() );

  //testing get/set BackgroundValue macro
  int BackgroundValue = ( atoi(argv[4]) );
  KeepNObjects->SetBackgroundValue( BackgroundValue );
  TEST_SET_GET_VALUE( BackgroundValue, KeepNObjects->GetBackgroundValue() );

  //testing get and set macros for Lambda 
  unsigned int numberOfObjects = atoi( argv[5] );
  KeepNObjects->SetNumberOfObjects( numberOfObjects );
  TEST_SET_GET_VALUE( numberOfObjects, KeepNObjects->GetNumberOfObjects() );

  //testing boolean macro for ReverseOrdering
  KeepNObjects->ReverseOrderingOn();
  TEST_SET_GET_VALUE( true, KeepNObjects->GetReverseOrdering() ); 

  KeepNObjects->ReverseOrderingOff();
  TEST_SET_GET_VALUE( false, KeepNObjects->GetReverseOrdering() );

  //testing get and set macros or ReverseOrdering 
  bool reverseOrdering = atoi( argv[6] );
  KeepNObjects->SetReverseOrdering( reverseOrdering );
  TEST_SET_GET_VALUE( reverseOrdering , KeepNObjects->GetReverseOrdering() ); 

  //testing boolean macro for FullyConnected
  KeepNObjects->FullyConnectedOn();
  TEST_SET_GET_VALUE( true, KeepNObjects->GetFullyConnected() ); 

  KeepNObjects->FullyConnectedOff();
  TEST_SET_GET_VALUE( false, KeepNObjects->GetFullyConnected() );

  //testing get and set macros or FullyConnected 
  bool fullyConnected = atoi( argv[7] );
  KeepNObjects->SetFullyConnected( fullyConnected );
  TEST_SET_GET_VALUE( fullyConnected , KeepNObjects->GetFullyConnected() ); 

  //testing get and set macros for Attribute 
  BinaryKeepNObjectsType::AttributeType attribute = atoi( argv[8] );
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
