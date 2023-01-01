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
#include "itkBinaryReconstructionLabelMapFilter.h"
#include "itkAttributeSelectionLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"
#include <algorithm>


int
itkBinaryReconstructionLabelMapFilterTest(int argc, char * argv[])
{
  if (argc != 5)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input marker output fg" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;

  using PixelType = unsigned char;
  using AttributeValueType = bool;

  using ImageType = itk::Image<PixelType, Dimension>;

  using AttributeLabelObjectType = itk::AttributeLabelObject<PixelType, Dimension, AttributeValueType>;
  using LabelMapType = itk::LabelMap<AttributeLabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using I2LType = itk::LabelImageToLabelMapFilter<ImageType, LabelMapType>;
  auto i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());

  auto reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  using LabelReconstructionType = itk::BinaryReconstructionLabelMapFilter<LabelMapType, ImageType>;
  auto reconstruction = LabelReconstructionType::New();

  int fg = std::stoi(argv[4]);
  reconstruction->SetForegroundValue(fg);
  ITK_TEST_SET_GET_VALUE(fg, reconstruction->GetForegroundValue());

  reconstruction->SetInput(i2l->GetOutput());
  reconstruction->SetMarkerImage(reader2->GetOutput());

  itk::SimpleFilterWatcher watcher(reconstruction, "filter");
  reconstruction->Update();
  reconstruction->GetOutput()->PrintLabelObjects();

  using LabelOpeningType = itk::AttributeSelectionLabelMapFilter<LabelMapType>;
  auto opening = LabelOpeningType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(opening, AttributeSelectionLabelMapFilter, InPlaceLabelMapFilter);


  auto exclude = false;
  ITK_TEST_SET_GET_BOOLEAN(opening, Exclude, exclude);

  auto attribute = true;
  opening->SetAttribute(attribute);

  std::set<AttributeValueType> attributeSet;
  attributeSet.insert(attribute);
  opening->SetAttributeSet(attributeSet);
  auto             obtainedAttributeSet = opening->GetAttributeSet();
  std::vector<int> diff;
  std::set_difference(attributeSet.begin(),
                      attributeSet.end(),
                      obtainedAttributeSet.begin(),
                      obtainedAttributeSet.end(),
                      std::inserter(diff, diff.begin()));

  if (diff.size() != 0)
  {
    std::cerr << "Error" << std::endl;
    std::cerr << " Obtained attribute set differs from expected atribute set" << std::endl;
    for (auto it1 = attributeSet.cbegin(), it2 = obtainedAttributeSet.cbegin();
         it1 != attributeSet.cend() || it2 != obtainedAttributeSet.cend();
         ++it1, ++it2)
    {
      std::cerr << "expected: " << *it1 << "; obtained: " << *it2 << std::endl;
    }
    return EXIT_FAILURE;
  }

  opening->SetInput(reconstruction->GetOutput());

  using L2IType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  auto l2i = L2IType::New();
  l2i->SetInput(opening->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;

  auto writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[3]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
