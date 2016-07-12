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

#include "RLEImage/RLEImageScanlineIterator.h"
#include "RLEImage/RLEImageRegionIterator.h"
#include "RLEImage/RLERegionOfInterestImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMorphologicalContourInterpolator.h"
#include <cstdlib>
#include <string>

long int
string2int(char * number)
{
  long res = strtol(number, NULL, 10);
  return res;
}

template <typename ImageType>
void
doTest(std::string inFilename, std::string outFilename, bool UseDistanceTransform, bool ball, int axis, int label)
{
  typedef itk::ImageFileReader<ImageType> ReaderType;
  typename ReaderType::Pointer            reader = ReaderType::New();
  reader->SetFileName(inFilename);
  reader->Update();

  typedef RLEImage<typename ImageType::PixelType, ImageType::ImageDimension> myRLEImage;
  typedef itk::RegionOfInterestImageFilter<ImageType, myRLEImage>            inConverterType;
  typename inConverterType::Pointer                                          inConv = inConverterType::New();
  inConv->SetInput(reader->GetOutput());
  inConv->SetRegionOfInterest(reader->GetOutput()->GetLargestPossibleRegion());
  inConv->Update();
  typename myRLEImage::Pointer test = inConv->GetOutput();

  typedef itk::MorphologicalContourInterpolator<myRLEImage> mciType;
  typename mciType::Pointer                                 mci = mciType::New();
  mci->SetInput(test);
  mci->SetUseDistanceTransform(UseDistanceTransform);
  mci->SetUseBallStructuringElement(ball);
  mci->SetAxis(axis);
  mci->SetLabel(label);
  mci->Update();

  // reuse the already calculated indices
  std::vector<typename mciType::LabeledSlicesType> indices = mci->GetLabeledSliceIndices();

  typename mciType::Pointer mci2 = mciType::New();
  mci2->SetInput(test);
  mci2->SetUseDistanceTransform(UseDistanceTransform);
  mci2->SetUseBallStructuringElement(ball);
  mci2->SetAxis(axis);
  mci2->SetLabel(label);
  mci2->SetUseCustomSlicePositions(true);
  for (int i = 0; i < ImageType::ImageDimension; i++)
  {
    for (int l = 0; l < indices[i].size(); l++)
    {
      mci2->SetLabeledSliceIndices(i, l, indices[i][l]);
    }
  }
  mci2->Update();

  typedef itk::RegionOfInterestImageFilter<myRLEImage, ImageType> outConverterType;
  typename outConverterType::Pointer                              outConv = outConverterType::New();
  outConv->SetInput(mci2->GetOutput());
  outConv->SetRegionOfInterest(mci2->GetOutput()->GetLargestPossibleRegion());
  outConv->Update();

  typedef itk::ImageFileWriter<ImageType> WriterType;
  typename WriterType::Pointer            writer = WriterType::New();
  writer->SetFileName(outFilename);
  writer->SetInput(outConv->GetOutput());
  writer->SetUseCompression(true);
  writer->Update();
}

int
itkMorphologicalContourInterpolationTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage outputImage [algorithm] [axis] [label]\n";
    std::cerr << " algorithms:\n";
    std::cerr << "  B = repeated dilations with ball structuring element";
    std::cerr << "  C = repeated dilations with cross structuring element";
    std::cerr << "  T = distance transform (not geodesic!)";
    std::cerr << " defaults: algo B, axis -1 (all axes), label 0 (all labels)";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }
  const char * inputImageFileName = argv[1];
  const char * outputImageFileName = argv[2];
  bool         dt = false; // DistanceTransform
  bool         ball = true;
  int          axis = -1, label = 0;
  if (argc >= 4)
  {
    char algo = toupper(argv[3][0]);
    if (algo == 'T')
    {
      dt = true;
    }
    else if (algo == 'C')
    {
      ball = false;
    }
    // else B
  }
  if (argc >= 5)
  {
    axis = strtol(argv[4], NULL, 10);
  }
  if (argc >= 6)
  {
    label = strtol(argv[5], NULL, 10);
  }

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
    if (numDimensions == 3 && (pixelType == itk::ImageIOBase::SHORT || pixelType == itk::ImageIOBase::USHORT))
    {
      doTest<itk::Image<short, 3>>(inputImageFileName, outputImageFileName, dt, ball, axis, label);
      return EXIT_SUCCESS;
    }
    if (numDimensions == 4 && pixelType == itk::ImageIOBase::UCHAR)
    {
      doTest<itk::Image<unsigned char, 4>>(inputImageFileName, outputImageFileName, dt, ball, axis, label);
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
