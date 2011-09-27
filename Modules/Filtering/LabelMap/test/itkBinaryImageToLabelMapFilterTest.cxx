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

  typedef itk::BinaryImageToLabelMapFilter< ImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();
  // test the behavior without input
  TRY_EXPECT_EXCEPTION( i2l->Update() );
  i2l->ResetPipeline();

  i2l->SetFullyConnected( atoi(argv[3]) );
  i2l->SetInputForegroundValue( atoi(argv[4]) );
  i2l->SetOutputBackgroundValue( atoi(argv[5]) );

  itk::SimpleFilterWatcher watcher( i2l );

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> L2IType;
  L2IType::Pointer l2i = L2IType::New();

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();


  i2l->SetInput( reader->GetOutput() );
  l2i->SetInput( i2l->GetOutput() );
  writer->SetInput( l2i->GetOutput() );

  bool expectfailure = atoi( argv[6] );

  if( expectfailure )
    {
    TRY_EXPECT_EXCEPTION( writer->Update() );
    }
  else
    {
    TRY_EXPECT_NO_EXCEPTION( writer->Update() );
    }

  i2l->GetOutput()->PrintLabelObjects();

  std::cout << i2l->GetNameOfClass() << std::endl;

  i2l->Print( std::cout );

  return EXIT_SUCCESS;
}
