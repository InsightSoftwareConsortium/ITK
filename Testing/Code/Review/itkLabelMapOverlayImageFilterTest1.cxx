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

#include "itkLabelImageToLabelMapFilter.h"
#include "itkLabelMapOverlayImageFilter.h"


int itkLabelMapOverlayImageFilterTest1(int argc, char * argv[])
{
  if( argc != 5 )
    {
    std::cerr << "usage: " << argv[0] << " input input output opacity" << std::endl;
    // std::cerr << "  : " << std::endl;
    exit(1);
    }

  const int dim = 2;

  typedef itk::Image< unsigned char, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelImageToLabelMapFilter< IType > ConverterType;
  ConverterType::Pointer converter = ConverterType::New();
  converter->SetInput( reader->GetOutput() );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

//  typedef itk::RGBPixel< unsigned char > RGBPixelType;
//  typedef itk::Image< RGBPixelType, dim > RGBImageType;

  typedef itk::LabelMapOverlayImageFilter< ConverterType::OutputImageType, IType > ColorizerType;
  ColorizerType::Pointer colorizer = ColorizerType::New();
  colorizer->SetInput( converter->GetOutput() );
  colorizer->SetFeatureImage( reader2->GetOutput() );
  colorizer->SetOpacity( atof(argv[4]) );

  itk::SimpleFilterWatcher watcher(colorizer, "filter");

  typedef itk::ImageFileWriter< ColorizerType::OutputImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( colorizer->GetOutput() );
  writer->SetFileName( argv[3] );
  writer->Update();
  return 0;
}
