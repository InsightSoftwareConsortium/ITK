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

#include "itkLabelMapContourOverlayImageFilter.h"


int itkLabelMapContourOverlayImageFilterTest3(int argc, char * argv[])
{
  if( argc != 9 )
    {
    std::cerr << "usage: " << argv[0] << " input input output opacity type thickness dilation priority sliceDim" << std::endl;
    return EXIT_FAILURE;
    }

  const int Dimension = 2;

  typedef itk::Image< unsigned char, Dimension > ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelImageToLabelMapFilter< ImageType > ConverterType;
  ConverterType::Pointer converter = ConverterType::New();
  converter->SetInput( reader->GetOutput() );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

  typedef itk::LabelMapContourOverlayImageFilter< ConverterType::OutputImageType, ImageType > ColorizerType;
  ColorizerType::Pointer colorizer = ColorizerType::New();
  colorizer->SetInput( converter->GetOutput() );
  colorizer->SetFeatureImage( reader2->GetOutput() );
  colorizer->SetOpacity( atof(argv[4]) );
  colorizer->SetType( atoi(argv[5]) );
  ColorizerType::SizeType r;
  r.Fill( atoi(argv[6]) );
  colorizer->SetContourThickness( r );
  r.Fill( atoi(argv[7]) );
  colorizer->SetDilationRadius( r );
  colorizer->SetPriority( atoi(argv[8]) );

  // Replace colormap with a custom one.
  // Just cycle through three colors for this test.
  ColorizerType::FunctorType functor;
  functor.ResetColors();
  functor.AddColor(0, 0, 255);
  functor.AddColor(0, 255, 0);
  functor.AddColor(255, 0, 0);
  colorizer->SetFunctor(functor);

  itk::SimpleFilterWatcher watcher(colorizer, "filter");

  typedef itk::ImageFileWriter< ColorizerType::OutputImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( colorizer->GetOutput() );
  writer->SetFileName( argv[3] );
  try
    {
    writer->Update();
    }
  catch(itk::ExceptionObject & err)
    {
    std::cerr << "Unexpected exception." << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }
  return 0;
}
