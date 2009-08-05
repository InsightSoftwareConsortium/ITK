/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryShapeOpeningImageFilterTest1.cxx
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

#include "itkBinaryShapeOpeningImageFilter.h"

#include "itkTestingMacros.h"

int itkBinaryShapeOpeningImageFilterTest1(int argc, char * argv[])
{

  if( argc != 9 )
    {
    std::cerr << "Usage: " << argv[0] << " input output";
    std::cerr << " foreground background lambda";
    std::cerr << "reverseOrdering connectivity attribute" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 2;
  
  typedef itk::Image< unsigned char, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::BinaryShapeOpeningImageFilter< IType > BinaryOpeningType;
  BinaryOpeningType::Pointer opening = BinaryOpeningType::New();
  
  opening->SetInput( reader->GetOutput() );
 
  //testing get/set InputForegroundValue macro
  int inputForegroundValue = ( atoi(argv[3]) );
  opening->SetInputForegroundValue( inputForegroundValue );
  TEST_SET_GET_VALUE( inputForegroundValue, opening->GetInputForegroundValue() );

  //testing get/set OutputBackgroundValue macro
  int outputBackgroundValue = ( atoi(argv[4]) );
  opening->SetOutputBackgroundValue( outputBackgroundValue );
  TEST_SET_GET_VALUE( outputBackgroundValue, opening->GetOutputBackgroundValue() );

  //testing get and set macros for Lambda 
  double lambda = atof( argv[5] );
  opening->SetLambda( lambda );
  TEST_SET_GET_VALUE( lambda , opening->GetLambda() );

  //testing boolean macro for ReverseOrdering
  opening->ReverseOrderingOn();
  TEST_SET_GET_VALUE( true, opening->GetReverseOrdering() ); 

  opening->ReverseOrderingOff();
  TEST_SET_GET_VALUE( false, opening->GetReverseOrdering() );

  //testing get and set macros or ReverseOrdering 
  bool reverseOrdering = atoi( argv[6] );
  opening->SetReverseOrdering( reverseOrdering );
  TEST_SET_GET_VALUE( reverseOrdering , opening->GetReverseOrdering() ); 

  //testing boolean macro for FullyConnected
  opening->FullyConnectedOn();
  TEST_SET_GET_VALUE( true, opening->GetFullyConnected() ); 

  opening->FullyConnectedOff();
  TEST_SET_GET_VALUE( false, opening->GetFullyConnected() );

  //testing get and set macros or FullyConnected 
  bool fullyConnected = atoi( argv[7] );
  opening->SetFullyConnected( fullyConnected );
  TEST_SET_GET_VALUE( fullyConnected , opening->GetFullyConnected() ); 

  //testing get and set macros for Attribute 
  BinaryOpeningType::AttributeType attribute = atoi( argv[8] );
  opening->SetAttribute( attribute );
  TEST_SET_GET_VALUE( attribute, opening->GetAttribute() );
 
  itk::SimpleFilterWatcher watcher(opening, "filter");

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( opening->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();
  
  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  std::cout << "Test Complete!" << std::endl;

  return EXIT_SUCCESS;
}
