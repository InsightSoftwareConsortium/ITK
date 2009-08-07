/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelShapeOpeningImageFilterTest1.cxx
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

#include "itkLabelShapeOpeningImageFilter.h"

#include "itkTestingMacros.h"

int itkLabelShapeOpeningImageFilterTest1(int argc, char * argv[])
{

  if( argc != 7 )
    {
    std::cerr << "Usage: " << argv[0] << " input output";
    std::cerr << " background lambda";
    std::cerr << "reverseOrdering attribute" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 2;
  
  typedef itk::Image< unsigned char, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelShapeOpeningImageFilter< IType > LabelOpeningType;
  LabelOpeningType::Pointer opening = LabelOpeningType::New();
  
  opening->SetInput( reader->GetOutput() );
 
  //testing get/set BackgroundValue macro
  int BackgroundValue = ( atoi(argv[3]) );
  opening->SetBackgroundValue( BackgroundValue );
  TEST_SET_GET_VALUE( BackgroundValue, opening->GetBackgroundValue() );

  //testing get and set macros for Lambda 
  double lambda = atof( argv[4] );
  opening->SetLambda( lambda );
  TEST_SET_GET_VALUE( lambda , opening->GetLambda() );

  //testing boolean macro for ReverseOrdering
  opening->ReverseOrderingOn();
  TEST_SET_GET_VALUE( true, opening->GetReverseOrdering() ); 

  opening->ReverseOrderingOff();
  TEST_SET_GET_VALUE( false, opening->GetReverseOrdering() );

  //testing get and set macros or ReverseOrdering 
  bool reverseOrdering = atoi( argv[5] );
  opening->SetReverseOrdering( reverseOrdering );
  TEST_SET_GET_VALUE( reverseOrdering , opening->GetReverseOrdering() ); 

  //testing get and set macros for Attribute 
  LabelOpeningType::AttributeType attribute = atoi( argv[6] );
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
