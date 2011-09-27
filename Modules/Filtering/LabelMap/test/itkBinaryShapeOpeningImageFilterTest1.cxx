/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
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

  //testing get/set ForegroundValue macro
  int ForegroundValue = ( atoi(argv[3]) );
  opening->SetForegroundValue( ForegroundValue );
  TEST_SET_GET_VALUE( ForegroundValue, opening->GetForegroundValue() );

  //testing get/set BackgroundValue macro
  int BackgroundValue = ( atoi(argv[4]) );
  opening->SetBackgroundValue( BackgroundValue );
  TEST_SET_GET_VALUE( BackgroundValue, opening->GetBackgroundValue() );

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
