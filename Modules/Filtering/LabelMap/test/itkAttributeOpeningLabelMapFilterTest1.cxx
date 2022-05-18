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
#include "itkSimpleFilterWatcher.h"


#include "itkLabelImageToLabelMapFilter.h"
#include "itkAttributeOpeningLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

int
itkAttributeOpeningLabelMapFilterTest1(int argc, char * argv[])
{
  if (argc != 5)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input output";
    std::cerr << " lambda reverseOrdering(0/1)";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;

  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, Dimension>;

  using LabelObjectType = itk::AttributeLabelObject<PixelType, Dimension, int>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using ImageToLabelType = itk::LabelImageToLabelMapFilter<ImageType, LabelMapType>;
  auto imageToLabel = ImageToLabelType::New();
  imageToLabel->SetInput(reader->GetOutput());


  // The next step is made outside the pipeline model, so we call Update() now.
  imageToLabel->Update();

  // Now we will valuate the attributes. The attribute will be the object position
  // in the label map
  LabelMapType::Pointer labelMap = imageToLabel->GetOutput();

  int pos = 0;
  for (LabelMapType::Iterator it(labelMap); !it.IsAtEnd(); ++it)
  {
    LabelObjectType * labelObject = it.GetLabelObject();
    labelObject->SetAttribute(pos++);
  }


  using LabelOpeningType = itk::AttributeOpeningLabelMapFilter<LabelMapType>;
  auto opening = LabelOpeningType::New();

  // testing get and set macros for Lambda
  auto lambda = static_cast<LabelOpeningType::AttributeValueType>(std::stod(argv[3]));
  opening->SetLambda(lambda);
  ITK_TEST_SET_GET_VALUE(lambda, opening->GetLambda());

  // testing get and set macros for ReverseOrdering
  // testing boolean macro for ReverseOrdering
  opening->ReverseOrderingOn();
  ITK_TEST_SET_GET_VALUE(true, opening->GetReverseOrdering());

  opening->ReverseOrderingOff();
  ITK_TEST_SET_GET_VALUE(false, opening->GetReverseOrdering());

  bool reverseOrdering = std::stoi(argv[4]);
  opening->SetReverseOrdering(reverseOrdering);
  ITK_TEST_SET_GET_VALUE(reverseOrdering, opening->GetReverseOrdering());

  opening->SetInput(labelMap);

  itk::SimpleFilterWatcher watcher(opening, "filter");

  using LabelToImageType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  auto labelToImage = LabelToImageType::New();
  labelToImage->SetInput(opening->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;

  auto writer = WriterType::New();
  writer->SetInput(labelToImage->GetOutput());
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
