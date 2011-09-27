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
#include "itkLabelImageToLabelMapFilter.h"
#include "itkLabelMapToBinaryImageFilter.h"

#include "itkTestingMacros.h"
#include "itkSimpleFilterWatcher.h"


int itkLabelMapToBinaryImageFilterTest( int argc, char * argv [] )
{

  if( argc != 5 )
    {
    std::cerr << "usage: " << argv[0];
    std::cerr << " inputLabelImage outputBinaryImage";
    std::cerr << " foregroundValue backgroundValue";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned char BinaryPixelType;
  typedef unsigned char LabelPixelType;

  typedef itk::Image< BinaryPixelType, Dimension > BinaryImageType;
  typedef itk::Image< LabelPixelType, Dimension >  LabelImageType;

  typedef itk::LabelObject< LabelPixelType, Dimension >   LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >                LabelMapType;

  typedef itk::ImageFileReader< LabelImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelImageToLabelMapFilter< LabelImageType, LabelMapType> I2LType;
  I2LType::Pointer i2l = I2LType::New();

  typedef itk::LabelMapToBinaryImageFilter< LabelMapType, BinaryImageType > L2IType;
  L2IType::Pointer l2i = L2IType::New();

  l2i->SetForegroundValue( atoi(argv[3]) );
  TEST_SET_GET_VALUE( atoi(argv[3]), l2i->GetForegroundValue() );

  l2i->SetBackgroundValue( atoi(argv[4]) );
  TEST_SET_GET_VALUE( atoi(argv[4]), l2i->GetBackgroundValue() );

  itk::SimpleFilterWatcher watcher( l2i );

  typedef itk::ImageFileWriter< BinaryImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();


  i2l->SetInput( reader->GetOutput() );
  l2i->SetInput( i2l->GetOutput() );
  writer->SetInput( l2i->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() )

  i2l->GetOutput()->PrintLabelObjects();

  std::cout << l2i->GetNameOfClass() << std::endl;

  l2i->Print( std::cout );

  return EXIT_SUCCESS;
}
