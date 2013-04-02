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
#include "itkRandomImageSource.h"
#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"
#include "itkRawImageIO.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkRawImageIOTest(int argc, char* argv[])
{
  typedef itk::Image<unsigned short,2>    ImageType;
  typedef ImageType::PixelType            PixelType;
  typedef itk::ImageRegionConstIterator<
                                  ImageType > ImageIteratorType;
  if(argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " Output1 Output2\n";
    return EXIT_FAILURE;
    }

  // Create a source object (in this case a random image generator).
  // The source object is templated on the output type.
  //
  ImageType::SizeValueType size[2];

  size[0]=128; size[1]=64;

  itk::RandomImageSource<ImageType>::Pointer random;
  random = itk::RandomImageSource<ImageType>::New();
  random->SetMin(0);
  random->SetMax(24680);
  random->SetSize(size);

  // Create a mapper (in this case a writer). A mapper
  // is templated on the input type.
  //
  itk::RawImageIO<unsigned short,2>::Pointer io;
  io = itk::RawImageIO<unsigned short,2>::New();

  //  io->SetFileTypeToASCII();

  // Write out the image
  itk::ImageFileWriter<ImageType>::Pointer writer;
  writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetInput(random->GetOutput());
  writer->SetFileName(argv[1]);
  writer->SetImageIO(io);

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while writing the image " << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    }

  // Create a source object (in this case a reader)
  itk::ImageFileReader<ImageType>::Pointer reader;
  reader = itk::ImageFileReader<ImageType>::New();
  reader->SetImageIO(io);
  reader->SetFileName(argv[1]);

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while reading the image " << argv[1] << std::endl;
    std::cerr << excp << std::endl;
    }


  // Compare pixel by pixel in memory


  ImageIteratorType it( reader->GetOutput(),
                        reader->GetOutput()->GetBufferedRegion() );

  ImageIteratorType ot( random->GetOutput(),
                        random->GetOutput()->GetBufferedRegion() );

  it.GoToBegin();
  ot.GoToBegin();
  while( !it.IsAtEnd() )
    {
    const PixelType iv = it.Get();
    const PixelType ov = ot.Get();
    if( iv != ov )
      {
      std::cerr << "Error in read/write of pixel " << it.GetIndex() << std::endl;
      std::cerr << "Read value  is : " << iv << std::endl;
      std::cerr << "it should be   : " << ov << std::endl;
      std::cerr << "Test FAILED ! " << std::endl;
      return EXIT_FAILURE;
      }
    ++it;
    ++ot;
    }

  writer->SetInput(reader->GetOutput());
  writer->SetFileName(argv[2]);
  writer->SetInput(reader->GetOutput());

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while writing the image " << argv[2] << std::endl;
    std::cerr << excp << std::endl;
    }


  std::cerr << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
