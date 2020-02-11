/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkBinaryImageToLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"
#include "itkTestingMacros.h"


int
itkBinaryImageToLabelMapFilterTest2(int argc, char * argv[])
{

  if (argc != 6)
  {
    std::cerr << "usage: " << argv[0];
    std::cerr << "inputBinaryImage outputLabelImage";
    std::cerr << "foregroundValue backgroundValue NumThreads";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using BinaryPixelType = unsigned char;
  using LabelPixelType = unsigned short;

  using ImageType = itk::Image<BinaryPixelType, Dimension>;
  using LabelObjectType = itk::LabelObject<LabelPixelType, Dimension>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using ImageToLabelType = itk::BinaryImageToLabelMapFilter<ImageType, LabelMapType>;
  ImageToLabelType::Pointer imageToLabel = ImageToLabelType::New();

  imageToLabel->SetInput(reader->GetOutput());
  imageToLabel->SetFullyConnected(true);
  imageToLabel->SetInputForegroundValue(std::stoi(argv[3]));
  imageToLabel->SetOutputBackgroundValue(std::stoi(argv[4]));
  imageToLabel->SetNumberOfWorkUnits(std::stoi(argv[5]));
  imageToLabel->Update();

  std::cout << "There are " << imageToLabel->GetOutput()->GetNumberOfLabelObjects() << " objects." << std::endl;

  ITK_TEST_EXPECT_EQUAL(imageToLabel->GetOutput()->GetNumberOfLabelObjects(), imageToLabel->GetNumberOfObjects());

  using LabelToImageType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  LabelToImageType::Pointer labelToImage = LabelToImageType::New();
  labelToImage->SetInput(imageToLabel->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(labelToImage->GetOutput());
  writer->Update();

  return EXIT_SUCCESS;
}
