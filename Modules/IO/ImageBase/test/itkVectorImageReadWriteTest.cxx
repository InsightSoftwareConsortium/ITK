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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageLinearIteratorWithIndex.h"

#include <iostream>

int
itkVectorImageReadWriteTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    itkGenericOutputMacro("Need a file to process");
    return EXIT_FAILURE;
  }

  // Test for vector pixel type.

  constexpr unsigned int Dimension = 2;

  // Create image of vector pixels
  using PixelType = itk::Vector<double, 4>;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  auto inputImage = ImageType::New();
  auto reader = ReaderType::New();
  auto writer = WriterType::New();

  // In this test, we will create a 9x9 image of vectors with pixels (4,4)
  // and (1,6) set to 'vector1'. We will filter it using
  // RecursiveGaussianImageFilter and compare a few filtered pixels.
  //
  // Create ON and OFF vectors
  constexpr PixelType vector0{};
  PixelType           vector1;
  vector1[0] = 1.0;
  vector1[1] = 2.0;
  vector1[2] = 3.0;
  vector1[3] = 4.0;

  using ConstIteratorType = itk::ImageLinearConstIteratorWithIndex<ImageType>;

  // Create the 9x9 input image
  auto                        size = ImageType::SizeType::Filled(9);
  ImageType::IndexType        index{};
  const ImageType::RegionType region{ index, size };
  inputImage->SetRegions(region);
  inputImage->Allocate();
  inputImage->FillBuffer(vector0);

  std::cout << "Create image of 9x9 image of vector pixels. IO Read and write it. " << std::endl;

  /* Set pixel (4,4) with the value 1
   * and pixel (1,6) with the value 2
   */
  index[0] = 4;
  index[1] = 4;
  inputImage->SetPixel(index, vector1);
  index[0] = 1;
  index[1] = 6;
  inputImage->SetPixel(index, vector1);

  writer->SetInput(inputImage);
  writer->SetFileName(argv[1]);
  writer->Update();

  reader->SetFileName(argv[1]);
  reader->Update();
  const ImageType::Pointer outputImage = reader->GetOutput();

  ConstIteratorType cit(outputImage, outputImage->GetLargestPossibleRegion());
  index[0] = 4;
  index[1] = 4;
  cit.SetIndex(index);
  if (cit.Get() != vector1)
  {
    std::cout << "Vector Image Write-Read failed. Tried to write " << vector1 << " But read " << cit.Get() << std::endl;
    return EXIT_FAILURE;
  }
  index[0] = 0;
  index[1] = 0;
  cit.SetIndex(index);
  if (cit.Get() != vector0)
  {
    std::cout << "Vector Image Write-Read failed. Tried to write " << vector0 << " But read " << cit.Get() << std::endl;
    return EXIT_FAILURE;
  }

  const itk::ImageIOBase::Pointer io = reader->GetModifiableImageIO();


  std::cout << "ImageIO Pixel Information: " << itk::ImageIOBase::GetPixelTypeAsString(io->GetPixelType()) << ' '
            << itk::ImageIOBase::GetComponentTypeAsString(io->GetComponentType()) << ' ' << io->GetNumberOfComponents()
            << std::endl;
  if (io->GetNumberOfComponents() != 4 || io->GetComponentType() != itk::IOComponentEnum::DOUBLE ||
      io->GetPixelType() != itk::IOPixelEnum::VECTOR)
  {
    std::cout << "Unexpected pixel information" << std::endl;
    std::cout << "Expected: vector double 4" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Image of vector pixels write-read [PASSED]" << std::endl;

  std::cout << "Test << operator:: Vector1 = " << vector1 << "[PASSED]" << std::endl;
  std::cout << "Test NumericTraits<Vector<double,4>>::ZeroValue() " << PixelType{} << std::endl;
  std::cout << "Test NumericTraits <Vector <double,4 > >::OneValue() " << itk::NumericTraits<PixelType>::OneValue()
            << std::endl;

  return EXIT_SUCCESS;
}
