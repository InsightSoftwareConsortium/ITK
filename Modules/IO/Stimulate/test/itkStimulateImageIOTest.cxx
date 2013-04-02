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
#include <iostream>
#include "itkRandomImageSource.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkStimulateImageIO.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkStimulateImageIOTest(int argc, char* argv[] )
{
  typedef itk::Image<float,2> FloatImageType;

  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  output1 output2 " << std::endl;
    return EXIT_FAILURE;
    }

  // Create a source object (in this case a random image generator).
  // The source object is templated on the output type.
  //
  FloatImageType::SizeValueType size[2];
  size[0]=128; size[1]=64;

  itk::RandomImageSource<FloatImageType>::Pointer random;
  random = itk::RandomImageSource<FloatImageType>::New();
  random->SetMin(0.0);
  random->SetMax(1.0);
  random->SetSize(size);

  // Create a mapper (in this case a writer). A mapper
  // is templated on the input type.
  //
  itk::StimulateImageIO::Pointer sprIO;
  sprIO = itk::StimulateImageIO::New();

  // Write out the image
  itk::ImageFileWriter<FloatImageType>::Pointer writer;
  writer = itk::ImageFileWriter<FloatImageType>::New();
  writer->SetInput(random->GetOutput());
  writer->SetFileName(argv[1]);
  writer->SetImageIO(sprIO);
  writer->Write();

  if ( !sprIO->CanReadFile(argv[1]) )
    {
    return EXIT_FAILURE;
    }

  try
    {
    // Create a source object (in this case a reader)
    itk::ImageFileReader<FloatImageType>::Pointer reader;
    reader = itk::ImageFileReader<FloatImageType>::New();
    reader->SetImageIO(sprIO);
    reader->SetFileName(argv[1]);
    reader->Update();

    writer->SetInput(reader->GetOutput());
    writer->SetFileName(argv[2]);
    writer->Write();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
