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

#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkConvertLabelMapFilter.h"
#include "itkTestingMacros.h"
#include "itkSimpleFilterWatcher.h"

int itkConvertLabelMapFilterTest1(int argc, char * argv[])
{
  if( argc != 3 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputLabelImage outputLabelImage";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int dim = 2;

  typedef unsigned char PixelType;

  typedef itk::Image< PixelType, dim > ImageType;


  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelImageToShapeLabelMapFilter< ImageType > L2SType;
  L2SType::Pointer l2s = L2SType::New();
  l2s->SetInput( reader->GetOutput() );

  typedef itk::LabelObject< PixelType, dim >     LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >       LabelMapType;

  typedef itk::ConvertLabelMapFilter< L2SType::OutputImageType, LabelMapType > CastType;
  CastType::Pointer cast = CastType::New();
  cast->SetInput( l2s->GetOutput() );
  itk::SimpleFilterWatcher watcher(cast, "cast");

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> L2IType;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput( cast->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( l2i->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  // for visual validation
  std::cout << " ============ original label map ============" << std::endl;
  l2s->GetOutput()->PrintLabelObjects();
  std::cout << " ============ casted label map ============" << std::endl;
  cast->GetOutput()->PrintLabelObjects();

  return EXIT_SUCCESS;
}
