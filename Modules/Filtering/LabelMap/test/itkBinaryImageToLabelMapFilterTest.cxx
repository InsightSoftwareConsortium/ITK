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
#include "itkSimpleFilterWatcher.h"


int
itkBinaryImageToLabelMapFilterTest(int argc, char * argv[])
{

  if (argc != 7)
  {
    std::cerr << "usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputBinaryImage outputLabelImage";
    std::cerr << " fullyConnected(0/1)  foregroundValue backgroundValue expectfailure";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;

  using BinaryPixelType = unsigned char;
  using LabelPixelType = unsigned char;

  using ImageType = itk::Image<BinaryPixelType, Dimension>;

  using LabelObjectType = itk::LabelObject<LabelPixelType, Dimension>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using ImageToLabelType = itk::BinaryImageToLabelMapFilter<ImageType, LabelMapType>;
  ImageToLabelType::Pointer imageToLabel = ImageToLabelType::New();
  // test the behavior without input
  ITK_TRY_EXPECT_EXCEPTION(imageToLabel->Update());
  imageToLabel->ResetPipeline();

  imageToLabel->SetFullyConnected(std::stoi(argv[3]));
  imageToLabel->SetInputForegroundValue(std::stoi(argv[4]));
  imageToLabel->SetOutputBackgroundValue(std::stoi(argv[5]));

  itk::SimpleFilterWatcher watcher(imageToLabel);

  using LabelToImageType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  LabelToImageType::Pointer labelToImage = LabelToImageType::New();

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();


  imageToLabel->SetInput(reader->GetOutput());
  labelToImage->SetInput(imageToLabel->GetOutput());
  writer->SetInput(labelToImage->GetOutput());

  bool expectfailure = std::stoi(argv[6]);

  if (expectfailure)
  {
    ITK_TRY_EXPECT_EXCEPTION(writer->Update());
  }
  else
  {
    ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());
  }

  imageToLabel->GetOutput()->PrintLabelObjects();

  std::cout << imageToLabel->GetNameOfClass() << std::endl;

  imageToLabel->Print(std::cout);

  return EXIT_SUCCESS;
}
