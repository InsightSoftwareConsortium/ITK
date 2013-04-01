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
#include "itkMorphologicalGradientImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkSimpleFilterWatcher.h"

int itkMorphologicalGradientImageFilterTest(int argc, char * argv[])
{
  if( argc < 3 )
  {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputImage " << std::endl;
    return EXIT_FAILURE;
  }

  const int dim = 2;

  typedef unsigned char            PType;
  typedef itk::Image< PType, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  typedef itk::BinaryBallStructuringElement< PType, dim  > StructuringElementType;
  StructuringElementType  structuringElement;
  structuringElement.SetRadius( 2 );
  structuringElement.CreateStructuringElement();

  typedef itk::MorphologicalGradientImageFilter< IType, IType, StructuringElementType > GradientType;
  GradientType::Pointer gradient = GradientType::New();
  gradient->SetInput(reader->GetOutput());
  gradient->SetKernel(  structuringElement );

  itk::SimpleFilterWatcher watcher(gradient);

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(gradient->GetOutput());
  writer->SetFileName(argv[2]);

  try
    {
  writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught ! " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
