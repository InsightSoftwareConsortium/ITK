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

#include "itkBinaryReconstructionByDilationImageFilter.h"
#include "itkTestingMacros.h"

int itkBinaryReconstructionByDilationImageFilterTest(int argc, char * argv[])
{
  if( argc != 6 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " input marker output";
    std::cerr << " fg bg";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 3;

  typedef unsigned char PixelType;

  typedef itk::Image< PixelType, dim > ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

  typedef itk::BinaryReconstructionByDilationImageFilter< ImageType > LabelReconstructionType;
  LabelReconstructionType::Pointer reconstruction = LabelReconstructionType::New();

  //testing get and set macros for Lambda
  int fg = atoi( argv[4] );
  reconstruction->SetForegroundValue( fg );
  TEST_SET_GET_VALUE( fg , reconstruction->GetForegroundValue() );

  int bg = atoi( argv[5] );
  reconstruction->SetBackgroundValue( bg );
  TEST_SET_GET_VALUE( bg , reconstruction->GetBackgroundValue() );

  reconstruction->SetMaskImage( reader->GetOutput() );
  reconstruction->SetInput( "MaskImage", reader->GetOutput() );
  reconstruction->SetMarkerImage( reader2->GetOutput() );
  reconstruction->SetInput( "MarkerImage", reader2->GetOutput() );

  itk::SimpleFilterWatcher watcher(reconstruction, "filter");

  typedef itk::ImageFileWriter< ImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( reconstruction->GetOutput() );
  writer->SetFileName( argv[3] );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
