/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkNrrdImageIOTest_h
#define itkNrrdImageIOTest_h

#include <fstream>
#include <string>
#include "itkImageRegionIterator.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNrrdImageIO.h"
#include "itkImage.h"
#include "itkRandomImageSource.h"
#include "itkTestingMacros.h"

template <typename TPixelType, unsigned int VImageDimension>
typename itk::Image<TPixelType, VImageDimension>::Pointer
itkNrrdImageIOTestGenerateRandomImage(unsigned int size)
{
  using ImageType = itk::Image<TPixelType, VImageDimension>;

  typename itk::RandomImageSource<ImageType>::Pointer source = itk::RandomImageSource<ImageType>::New();

  typename ImageType::SizeType    sz;
  typename ImageType::SpacingType spacing;
  typename ImageType::PointType   origin;

  for (unsigned int i = 0; i < VImageDimension; ++i)
  {
    sz[i] = size;
    spacing[i] = static_cast<float>(i + 1);
    origin[i] = static_cast<float>(i);
  }

  source->SetSize(sz);
  source->SetOrigin(origin);
  source->SetSpacing(spacing);

  source->Update();
  return (source->GetOutput());
}

template <typename TPixelType, unsigned int VImageDimension>
int
itkNrrdImageIOTestReadWriteTest(std::string fn, unsigned int size, std::string inputFile, bool compression = false)
{
  using ImageType = itk::Image<TPixelType, VImageDimension>;

  typename itk::ImageFileReader<ImageType>::Pointer reader = itk::ImageFileReader<ImageType>::New();
  typename itk::ImageFileWriter<ImageType>::Pointer writer = itk::ImageFileWriter<ImageType>::New();

  itk::NrrdImageIO::Pointer io = itk::NrrdImageIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(io, NrrdImageIO, ImageIOBase);


  ITK_TEST_EXPECT_TRUE(io->SupportsDimension(VImageDimension));

  constexpr unsigned int NRRD_DIM_MAX = 16; // taken from NrrdIO.h which is not in the include path
  unsigned long          dim = NRRD_DIM_MAX + 1;
  ITK_TEST_EXPECT_TRUE(!io->SupportsDimension(dim));

  // Binary files have no image information to read
  io->WriteImageInformation();


  reader->SetImageIO(io);
  writer->SetImageIO(io);

  typename ImageType::Pointer image;

  if (inputFile != "null")
  {
    typename itk::ImageFileReader<ImageType>::Pointer tmpReader = itk::ImageFileReader<ImageType>::New();
    tmpReader->SetImageIO(io);
    tmpReader->SetFileName(inputFile.c_str());

    ITK_TRY_EXPECT_NO_EXCEPTION(tmpReader->Update());

    std::cout << "DONE READING INPUT IMAGE" << std::endl;

    image = tmpReader->GetOutput();
  }
  else
  {
    // Generate a random image.
    image = itkNrrdImageIOTestGenerateRandomImage<TPixelType, VImageDimension>(size);

    // add custom metadata
    itk::MetaDataDictionary dictionary;

    itk::EncapsulateMetaData(dictionary, "ASimpleString", std::string("a string"));
    itk::EncapsulateMetaData(dictionary, "ASimpleFloat", 1.2f);
    itk::EncapsulateMetaData(dictionary, "ASimpleDouble", 2.3);
    itk::EncapsulateMetaData(dictionary, "ASimpleInt", 3);

    itk::EncapsulateMetaData(dictionary, "Array<int>", itk::Array<int>(3, 1));
    itk::EncapsulateMetaData(dictionary, "Array<float>", itk::Array<float>(4, 2.2f));
    itk::EncapsulateMetaData(dictionary, "Array<double>", itk::Array<double>(5, 3.3));

    image->SetMetaDataDictionary(dictionary);
  }

  // Write, then read the image.
  try
  {
    writer->SetFileName(fn.c_str());
    if (compression)
    {
      writer->UseCompressionOn();
    }
    else
    {
      writer->UseCompressionOff();
    }
    reader->SetFileName(fn.c_str());
    // writer->SetFileName("testDebug.mhd");
    // reader->SetFileName("testDebug.mhd");
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
  }

  writer->SetInput(image);

  image->Print(std::cout);
  std::cout << "----------" << std::endl;

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << "DONE WRITING TEST IMAGE" << std::endl;


  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  std::cout << "DONE READING TEST IMAGE" << std::endl;

  // Print the image information.

  reader->GetOutput()->Print(std::cout);
  std::cout << std::endl;

  // Compare input and output images.
  itk::ImageRegionIterator<ImageType> a(image, image->GetRequestedRegion());
  itk::ImageRegionIterator<ImageType> b(reader->GetOutput(), reader->GetOutput()->GetRequestedRegion());
  for (a.GoToBegin(), b.GoToBegin(); !a.IsAtEnd(); ++a, ++b)
  {
    if (itk::Math::NotExactlyEquals(b.Get(), a.Get()))
    {
      std::cerr << "At index " << b.GetIndex() << " value " << b.Get() << " should be " << a.Get() << std::endl;
      return EXIT_FAILURE;
    }
  }
  if (inputFile == "null")
  {
    // check incorporated metadata
    itk::MetaDataDictionary dictionary = reader->GetOutput()->GetMetaDataDictionary();

    auto check_metadata_entry = [](auto & dict, auto & key, auto & expected) {
      std::string val;
      if (!itk::ExposeMetaData<std::string>(dict, key, val))
      {
        std::cerr << "Missing expected metadata entry:" << key << std::endl;
        return false;
      }
      if (val != expected)
      {
        std::cerr << "Wrong metadata for " << key << ", expected:" << expected << " got:" << val.c_str() << std::endl;
        return false;
      }
      return true;
    };

    if (!check_metadata_entry(dictionary, "ASimpleString", "a string"))
      return EXIT_FAILURE;
    if (!check_metadata_entry(dictionary, "ASimpleFloat", "1.2"))
      return EXIT_FAILURE;
    if (!check_metadata_entry(dictionary, "ASimpleDouble", "2.3"))
      return EXIT_FAILURE;
    if (!check_metadata_entry(dictionary, "ASimpleInt", "3"))
      return EXIT_FAILURE;
    if (!check_metadata_entry(dictionary, "Array<int>", "[1, 1, 1]"))
      return EXIT_FAILURE;
    if (!check_metadata_entry(dictionary, "Array<float>", "[2.2, 2.2, 2.2, 2.2]"))
      return EXIT_FAILURE;
    if (!check_metadata_entry(dictionary, "Array<double>", "[3.3, 3.3, 3.3, 3.3, 3.3]"))
      return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

#endif // itkNrrdImageIOTest_h_
