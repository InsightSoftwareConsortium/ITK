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
#include "itkImageRegionConstIterator.h"
#include "itkNumericTraits.h"
#include <iostream>

typedef unsigned long long       PixelType;
typedef itk::Image<PixelType, 3> ImageType;

int verifyContent(ImageType::Pointer image)
{
  itk::ImageRegionConstIterator<ImageType> it(image, image->GetBufferedRegion());
  unsigned long long imageSize = 4 * 3 * 2;
  unsigned long long value = 1;
  while (!it.IsAtEnd() && value <= imageSize)
    {
      if (value == imageSize)
        {
        //last pixel is maximum 64-bit value
        value = itk::NumericTraits< PixelType >::max();
        }

      if (it.Get() != value)
        {
        std::cerr << "Failure reading value " << value << ". Instead got: " << it.Get() << std::endl;
        return EXIT_FAILURE;
        }

      ++it;
      ++value;
    }
  return EXIT_SUCCESS;
}

int itk64bitTest(int argc, char *argv[])
{
  if (argc < 3)
    {
    std::cerr << "Invocation syntax:\n\t" << argv[0];
    std::cerr << " Test64bit.nrrd Test64bit.mha" << std::endl;
    return EXIT_FAILURE;
    }

  int returnValue = EXIT_SUCCESS;
  typedef itk::ImageFileReader<ImageType> ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]); // Input/Test64bit.nrrd

  try
    {
    std::cout << "Reading " << argv[1] << std::endl;
    reader->Update();
    ImageType::Pointer image = reader->GetOutput();
    returnValue += verifyContent(image);

    std::cout << "Writing " << argv[2] << std::endl;
    typedef itk::ImageFileWriter<ImageType> WriterType;
    WriterType::Pointer writer = WriterType::New();
    writer->SetInput(image);
    writer->SetFileName(argv[2]);
    writer->Update();

    std::cout << "Reading " << argv[2] << std::endl;
    reader->SetFileName(argv[2]);
    reader->Update();
    ImageType::Pointer image2 = reader->GetOutput();
    returnValue += verifyContent(image2);
    }
  catch (itk::ExceptionObject &exc)
    {
    std::cerr << exc;
    return EXIT_FAILURE;
    }

  if (returnValue)
    {
    std::cout << "Test FAILED" << std::endl;
    }
  else
    {
    std::cout << "Test PASSED" << std::endl;
    }
  return returnValue;
}
