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
#include "itkBinaryImageToLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"
#include "itkSimpleFilterWatcher.h"


int itkBinaryImageToLabelMapFilterTest( int argc, char * argv [] )
{

  if( argc != 7 )
    {
    std::cerr << "usage: " << argv[0];
    std::cerr << " inputBinaryImage outputLabelImage";
    std::cerr << " fullyConnected(0/1)  foregroundValue backgroundValue expectfailure";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 3;

  typedef unsigned char BinaryPixelType;
  typedef unsigned char LabelPixelType;

  typedef itk::Image< BinaryPixelType, Dimension > ImageType;

  typedef itk::LabelObject< LabelPixelType, Dimension >   LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >                LabelMapType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::BinaryImageToLabelMapFilter< ImageType, LabelMapType > ImageToLabelType;
  ImageToLabelType::Pointer imageToLabel = ImageToLabelType::New();
  // test the behavior without input
  TRY_EXPECT_EXCEPTION( imageToLabel->Update() );
  imageToLabel->ResetPipeline();

  imageToLabel->SetFullyConnected( atoi(argv[3]) );
  imageToLabel->SetInputForegroundValue( atoi(argv[4]) );
  imageToLabel->SetOutputBackgroundValue( atoi(argv[5]) );

  itk::SimpleFilterWatcher watcher( imageToLabel );

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> LabelToImageType;
  LabelToImageType::Pointer labelToImage = LabelToImageType::New();

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();


  imageToLabel->SetInput( reader->GetOutput() );
  labelToImage->SetInput( imageToLabel->GetOutput() );
  writer->SetInput( labelToImage->GetOutput() );

  bool expectfailure = atoi( argv[6] );

  if( expectfailure )
    {
    TRY_EXPECT_EXCEPTION( writer->Update() );
    }
  else
    {
    TRY_EXPECT_NO_EXCEPTION( writer->Update() );
    }

  imageToLabel->GetOutput()->PrintLabelObjects();

  std::cout << imageToLabel->GetNameOfClass() << std::endl;

  imageToLabel->Print( std::cout );

  return EXIT_SUCCESS;
}
