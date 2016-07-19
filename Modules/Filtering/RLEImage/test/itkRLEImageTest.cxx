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

#include "itkRLEImageScanlineIterator.h"
#include "itkRLEImageRegionIterator.h"
#include "itkRLERegionOfInterestImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include <cstdlib>
#include <string>

template <typename ImageType>
void
doTest(std::string inFilename, std::string outFilename)
{
  typedef itk::ImageFileReader<ImageType> ReaderType;
  typename ReaderType::Pointer            reader = ReaderType::New();
  reader->SetFileName(inFilename);
  reader->Update();

  typedef itk::RLEImage<typename ImageType::PixelType, ImageType::ImageDimension> myRLEImage;
  typedef itk::RegionOfInterestImageFilter<ImageType, myRLEImage>                 inConverterType;
  typename inConverterType::Pointer                                               inConv = inConverterType::New();
  inConv->SetInput(reader->GetOutput());
  inConv->SetRegionOfInterest(reader->GetOutput()->GetLargestPossibleRegion());
  inConv->Update();
  typename myRLEImage::Pointer test = inConv->GetOutput();

  // region for partial coverage
  typename myRLEImage::RegionType reg = test->GetLargestPossibleRegion();
  // skip X due to RLE representation constraints
  // for (int i = 1; i < ImageType::ImageDimension; i++)
  //   {
  //   reg.GetModifiableIndex()[i] += (reg.GetSize(i) - 1) / 4;
  //   reg.SetSize(i, (reg.GetSize(i) + 1) / 2);
  //   }

  typedef itk::RegionOfInterestImageFilter<myRLEImage, ImageType> outConverterType;
  typename outConverterType::Pointer                              outConv = outConverterType::New();
  outConv->SetInput(test);
  outConv->SetRegionOfInterest(reg);
  outConv->Update();

  typedef itk::ImageFileWriter<ImageType> WriterType;
  typename WriterType::Pointer            writer = WriterType::New();
  writer->SetFileName(outFilename);
  writer->SetInput(outConv->GetOutput());
  writer->SetUseCompression(true);
  writer->Update();
}

int
itkRLEImageTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage" << std::endl;
    return EXIT_FAILURE;
  }
  const char * inputImageFileName = argv[1];
  const char * outputImageFileName = argv[2];

  typedef itk::ImageIOBase::IOComponentType ScalarPixelType;
  itk::ImageIOBase::Pointer                 imageIO =
    itk::ImageIOFactory::CreateImageIO(inputImageFileName, itk::ImageIOFactory::ReadMode);
  if (!imageIO)
  {
    std::cerr << "Could not CreateImageIO for: " << inputImageFileName << std::endl;
    return EXIT_FAILURE;
  }
  imageIO->SetFileName(inputImageFileName);
  imageIO->ReadImageInformation();
  const ScalarPixelType pixelType = imageIO->GetComponentType();
  const size_t          numDimensions = imageIO->GetNumberOfDimensions();

  try
  {
    // unused cases are not instantiated because they greatly increase compile time
    if (numDimensions == 2 && pixelType == itk::ImageIOBase::UCHAR)
    {
      doTest<itk::Image<unsigned char, 2>>(inputImageFileName, outputImageFileName);
      return EXIT_SUCCESS;
    }
    if (numDimensions == 3 && (pixelType == itk::ImageIOBase::SHORT || pixelType == itk::ImageIOBase::USHORT))
    {
      doTest<itk::Image<short, 3>>(inputImageFileName, outputImageFileName);
      return EXIT_SUCCESS;
    }
    if (numDimensions == 4 && pixelType == itk::ImageIOBase::UCHAR)
    {
      doTest<itk::Image<unsigned char, 4>>(inputImageFileName, outputImageFileName);
      return EXIT_SUCCESS;
    }

    std::cerr << "Unsupported image type:\n  Dimensions: " << numDimensions;
    std::cerr << "\n  Pixel type:" << imageIO->GetComponentTypeAsString(pixelType) << std::endl;
    return EXIT_FAILURE;
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Error: " << error << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
