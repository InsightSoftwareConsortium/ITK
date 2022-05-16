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

#include "itkLabelImageToLabelMapFilter.h"
#include "itkChangeLabelLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkChangeLabelLabelMapFilterTest(int argc, char * argv[])
{
  if (argc < 5)
  {
    std::cerr << "usage: " << itkNameOfTestExecutableMacro(argv) << " inputLabelImage outputLabelImage ";
    std::cerr << "  old_label_1 new_label_1" << std::endl;
    std::cerr << " [old_label_2 new_label_2]" << std::endl;
    std::cerr << " [old_label_3 new_label_3]" << std::endl;
    std::cerr << " ...                      " << std::endl;
    std::cerr << " [old_label_n new_label_n]" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using ImagePixelType = unsigned char;
  using LabelPixelType = unsigned char;

  using ImageType = itk::Image<ImagePixelType, Dimension>;

  using LabelObjectType = itk::LabelObject<LabelPixelType, Dimension>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using I2LType = itk::LabelImageToLabelMapFilter<ImageType, LabelMapType>;
  auto i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());

  using ChangeLabelFilterType = itk::ChangeLabelLabelMapFilter<LabelMapType>;
  auto changeFilter = ChangeLabelFilterType::New();

  changeFilter->SetInput(i2l->GetOutput());

  constexpr unsigned int numberOfArgumentsBeforeLabels = 3;
  const unsigned int     numberOfArguments = argc;

  using LabelPrintType = itk::NumericTraits<LabelPixelType>::PrintType;

  for (unsigned int i = numberOfArgumentsBeforeLabels; i < numberOfArguments; i += 2)
  {
    const LabelPixelType oldLabel = std::stoi(argv[i]);
    const LabelPixelType newLabel = std::stoi(argv[i + 1]);

    std::cout << "Label pair : ";
    std::cout << static_cast<LabelPrintType>(oldLabel) << " -> ";
    std::cout << static_cast<LabelPrintType>(newLabel) << std::endl;
    changeFilter->SetChange(oldLabel, newLabel);
  }

  itk::SimpleFilterWatcher watcher6(changeFilter, "filter");

  using L2IType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  auto l2i = L2IType::New();
  l2i->SetInput(changeFilter->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << "GetNameOfClass() = " << changeFilter->GetNameOfClass() << std::endl;

  changeFilter->Print(std::cout);

  const ChangeLabelFilterType::ChangeMapType & mapOfLabel = changeFilter->GetChangeMap();

  const unsigned int numberOfLabelsToReplace = (numberOfArguments - numberOfArgumentsBeforeLabels) / 2;

  if (mapOfLabel.size() != numberOfLabelsToReplace)
  {
    std::cerr << "Error in SetChange() or in GetChangeMap() " << std::endl;
    std::cerr << "numberOfLabelsToReplace = " << numberOfLabelsToReplace << std::endl;
    std::cerr << "mapOfLabel.size() = " << mapOfLabel.size() << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
