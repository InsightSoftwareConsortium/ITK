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

#include "itkRLEImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"
#include <cstdlib>
#include <string>

template <typename ImageType1, typename ImageType2>
void
compare(itk::SmartPointer<ImageType1> im1, itk::SmartPointer<ImageType2> im2)
{
  itk::ImageRegionConstIterator<ImageType1> it1(im1, im1->GetLargestPossibleRegion());
  itk::ImageRegionConstIterator<ImageType2> it2(im2, im2->GetLargestPossibleRegion());
  while (!it1.IsAtEnd())
  {
    if (it1.Get() != it2.Get())
    {
      itkGenericExceptionMacro(<< "Images differ. Val1: " << it1.Get() << "  Val2: " << it2.Get()
                               << "\nInd1: " << it1.GetIndex() << "  Ind2: " << it2.GetIndex());
    }
    ++it1;
    ++it2;
  }
}

template <typename ImageType>
void
roiTest(itk::SmartPointer<itk::RLEImage<typename ImageType::PixelType, ImageType::ImageDimension>> orig)
{
  using myRLEImage = itk::RLEImage<typename ImageType::PixelType, ImageType::ImageDimension>;
  using charRLEImage = itk::RLEImage<typename ImageType::PixelType, ImageType::ImageDimension, char>;
  using charConverterType = itk::RegionOfInterestImageFilter<myRLEImage, charRLEImage>;
  typename charConverterType::Pointer cConv = charConverterType::New();
  cConv->SetInput(orig);
  cConv->SetRegionOfInterest(orig->GetLargestPossibleRegion());
  cConv->Update();
  typename charRLEImage::Pointer cIn = cConv->GetOutput();
  cIn->DisconnectPipeline();

  using ucharRLEImage = itk::RLEImage<typename ImageType::PixelType, ImageType::ImageDimension, unsigned char>;
  using ucharConverterType = itk::RegionOfInterestImageFilter<myRLEImage, ucharRLEImage>;
  typename ucharConverterType::Pointer ucConv = ucharConverterType::New();
  ucConv->SetInput(orig);
  ucConv->SetRegionOfInterest(orig->GetLargestPossibleRegion());
  ucConv->Update();
  typename ucharRLEImage::Pointer ucIn = ucConv->GetOutput();
  ucIn->DisconnectPipeline();

  std::cout << "Comparing direct conversions...";
  compare(cIn, ucIn);
  std::cout << "OK" << std::endl;

  typename myRLEImage::RegionType reg = orig->GetLargestPossibleRegion();
  typename myRLEImage::RegionType rNeg = reg;
  for (unsigned i = 0; i < ImageType::ImageDimension; i++)
  {
    rNeg.SetIndex(i, -typename myRLEImage::IndexValueType(reg.GetSize(i)) * 3 / 4);
  }
  cIn->SetRegions(rNeg);
  ucIn->SetRegions(rNeg);
  std::cout << "Comparing LPR with negative indices...";
  compare(cIn, ucIn);
  std::cout << "OK" << std::endl;

  // region for partial coverage, skip X due to RLE representation constraints
  for (unsigned i = 1; i < ImageType::ImageDimension; i++)
  {
    reg.GetModifiableIndex()[i] += (reg.GetSize(i) - 1) / 4;
    rNeg.GetModifiableIndex()[i] += typename myRLEImage::IndexValueType(rNeg.GetSize(i) - 1) / 4;
    reg.SetSize(i, (reg.GetSize(i) + 1) / 2);
    rNeg.SetSize(i, (rNeg.GetSize(i) + 1) / 2);
  }

  using myConverterType = itk::RegionOfInterestImageFilter<myRLEImage, myRLEImage>;
  typename myConverterType::Pointer myConv = myConverterType::New();
  myConv->SetInput(orig);
  myConv->SetRegionOfInterest(reg);
  myConv->Update();
  typename myRLEImage::Pointer myIn = myConv->GetOutput();

  using cRoIType = itk::RegionOfInterestImageFilter<charRLEImage, charRLEImage>;
  typename cRoIType::Pointer cRoI = cRoIType::New();
  cRoI->SetInput(cIn);
  cRoI->SetRegionOfInterest(rNeg);
  cRoI->Update();
  cIn = cRoI->GetOutput();
  cIn->DisconnectPipeline();

  using ucRoIType = itk::RegionOfInterestImageFilter<ucharRLEImage, ucharRLEImage>;
  typename ucRoIType::Pointer ucRoI = ucRoIType::New();
  ucRoI->SetInput(ucIn);
  ucRoI->SetRegionOfInterest(rNeg);
  ucRoI->Update();
  ucIn = ucRoI->GetOutput();
  ucIn->DisconnectPipeline();

  std::cout << "Comparing RoIs with negative indices...";
  compare(cIn, ucIn);
  std::cout << "OK" << std::endl;

  std::cout << "Comparing RoIs with negative and positive indices...";
  compare(cIn, myIn);
  compare(ucIn, myIn);
  std::cout << "OK" << std::endl;
}

template <typename ImageType>
int
doTest(std::string inFilename, std::string outFilename)
{
  using ReaderType = itk::ImageFileReader<ImageType>;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inFilename);
  std::cout << "Reading " << inFilename << std::endl;
  reader->Update();

  using myRLEImage = itk::RLEImage<typename ImageType::PixelType, ImageType::ImageDimension>;
  using inConverterType = itk::RegionOfInterestImageFilter<ImageType, myRLEImage>;
  typename inConverterType::Pointer inConv = inConverterType::New();
  inConv->SetInput(reader->GetOutput());
  inConv->SetRegionOfInterest(reader->GetOutput()->GetLargestPossibleRegion());
  std::cout << "Converting regular image to RLEImage" << std::endl;
  inConv->Update();
  typename myRLEImage::Pointer test = inConv->GetOutput();
  itk::SizeValueType           xSize = test->GetLargestPossibleRegion().GetSize(0);

  std::cout << "Running region of interest tests" << std::endl;
  if (xSize > 127)
  {
    std::cout << "\n  xSize>127 (CHAR_MAX)" << std::endl;
    ITK_TRY_EXPECT_EXCEPTION(roiTest<myRLEImage>(test));
  }
  else
  {
    ITK_TRY_EXPECT_NO_EXCEPTION(roiTest<myRLEImage>(test));
  }

  using outConverterType = itk::RegionOfInterestImageFilter<myRLEImage, ImageType>;
  typename outConverterType::Pointer outConv = outConverterType::New();
  outConv->SetInput(test);
  outConv->SetRegionOfInterest(test->GetLargestPossibleRegion());
  std::cout << "Converting RLEImage to regular image" << std::endl;
  outConv->Update();

  using WriterType = itk::ImageFileWriter<ImageType>;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(outFilename);
  writer->SetInput(outConv->GetOutput());
  writer->SetUseCompression(true);
  std::cout << "Writing " << outFilename << std::endl;
  writer->Update();
  std::cout << "Test finished" << std::endl;

  return EXIT_SUCCESS;
}

void
dimTest()
{
  using S2Type = itk::RLEImage<short, 2>; // 2D
  using S3Type = itk::RLEImage<short, 3>; // 3D
  S2Type::Pointer s2 = S2Type::New();
  S3Type::Pointer s3 = S3Type::New();

  // instantiation of "RoIType" is dissalowed due to different dimension
  // uncommenting the lines below should give a meaningful error message
  // using RoIType = itk::RegionOfInterestImageFilter<S3Type, S2Type>;
  // typename RoIType::Pointer roi = RoIType::New();
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

  using ScalarPixelType = itk::CommonEnums::IOComponent;
  itk::ImageIOBase::Pointer imageIO =
    itk::ImageIOFactory::CreateImageIO(inputImageFileName, itk::CommonEnums::IOFileMode::ReadMode);
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
    if (numDimensions == 2 && pixelType == itk::CommonEnums::IOComponent::UCHAR)
    {
      return doTest<itk::Image<unsigned char, 2>>(inputImageFileName, outputImageFileName);
    }
    if (numDimensions == 3 &&
        (pixelType == itk::CommonEnums::IOComponent::SHORT || pixelType == itk::CommonEnums::IOComponent::USHORT))
    {
      return doTest<itk::Image<short, 3>>(inputImageFileName, outputImageFileName);
    }
    if (numDimensions == 3) // if not (u)short, then interpret as uint
    {
      return doTest<itk::Image<unsigned int, 3>>(inputImageFileName, outputImageFileName);
    }
    if (numDimensions == 4 && pixelType == itk::CommonEnums::IOComponent::UCHAR)
    {
      return doTest<itk::Image<unsigned char, 4>>(inputImageFileName, outputImageFileName);
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
