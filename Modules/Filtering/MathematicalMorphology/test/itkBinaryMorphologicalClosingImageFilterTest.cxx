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

#include "itkBinaryMorphologicalClosingImageFilter.h"
#include "itkBinaryBallStructuringElement.h"

int itkBinaryMorphologicalClosingImageFilterTest(int argc, char * argv[])
{
  if( argc < 6 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " InputImage OutputImage Radius SafeBorder Foreground" << std::endl;
    return EXIT_FAILURE;
    }

  const int dim = 2;

  // Verify that the input and output pixel types can be different
  typedef unsigned short                      InputPixelType;
  typedef itk::Image< InputPixelType, dim >   InputImageType;

  typedef unsigned char                       OutputPixelType;
  typedef itk::Image< OutputPixelType, dim >  OutputImageType;

  typedef itk::ImageFileReader< InputImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::BinaryBallStructuringElement< InputPixelType, dim > KernelType;
  KernelType ball;
  KernelType::SizeType ballSize;
  ballSize.Fill( atoi( argv[3] ) );
  ball.SetRadius( ballSize );
  ball.CreateStructuringElement();

  typedef itk::BinaryMorphologicalClosingImageFilter< InputImageType, OutputImageType, KernelType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );
  filter->SetKernel( ball );
  // test the default attribute values, and exercise the accesors
  if( !filter->GetSafeBorder() )
    {
    std::cerr << "Wrong SafeBorder default value" << std::endl;
    return EXIT_FAILURE;
    }
  filter->SafeBorderOff();
  filter->SafeBorderOn();
  filter->SetSafeBorder( atoi( argv[4] ) );

  if( filter->GetForegroundValue() != itk::NumericTraits<InputPixelType>::max() )
    {
    std::cerr << "Wrong Foreground default value" << std::endl;
    return EXIT_FAILURE;
    }
  filter->SetForegroundValue( atoi( argv[5] ) );

  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< OutputImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
