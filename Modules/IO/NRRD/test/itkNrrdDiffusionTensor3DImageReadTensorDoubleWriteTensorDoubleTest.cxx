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

#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNrrdImageIO.h"

#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDoubleTest( int ac, char* av[] )
{
  if(ac < 2)
    {
    std::cerr << "Usage: " << av[0] << " Input Output\n";
    return EXIT_FAILURE;
    }

  typedef itk::DiffusionTensor3D<double> InPixelType;
  typedef itk::Image<InPixelType, 3>     InImage;

  typedef itk::ImageFileReader<InImage>  ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetImageIO( itk::NrrdImageIO::New() );

  reader->SetFileName(av[1]);

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  InImage::Pointer image = reader->GetOutput();
  image->Print(std::cout );

  // Generate test image
  itk::ImageFileWriter<InImage>::Pointer writer;
  writer = itk::ImageFileWriter<InImage>::New();
  writer->SetImageIO( itk::NrrdImageIO::New() );
  writer->SetInput( reader->GetOutput() );
  writer->SetFileName(av[2]);
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;

}
