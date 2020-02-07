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
#include "itkSimpleFilterWatcher.h"


#include "itkStatisticsRelabelLabelMapFilter.h"
#include "itkLabelImageToStatisticsLabelMapFilter.h"

#include "itkTestingMacros.h"

int
itkStatisticsRelabelLabelMapFilterTest1(int argc, char * argv[])
{

  if (argc != 6)
  {
    std::cerr << "usage: " << argv[0] << " input feature output";
    std::cerr << "background reverseOrdering attribute" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 2;

  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, dim>;

  using StatisticsLabelObjectType = itk::StatisticsLabelObject<PixelType, dim>;
  using LabelMapType = itk::LabelMap<StatisticsLabelObjectType>;

  // Reading Image File
  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  // Converting LabelImage to StatisticsLabelMap
  using I2LType = itk::LabelImageToStatisticsLabelMapFilter<ImageType, ImageType, LabelMapType>;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());
  i2l->SetFeatureImage(reader2->GetOutput());

  using RelabelType = itk::StatisticsRelabelLabelMapFilter<LabelMapType>;
  RelabelType::Pointer relabel = RelabelType::New();

  // testing get and set macros for ReverseOrdering
  bool reverseOrdering = std::stoi(argv[4]);
  relabel->SetReverseOrdering(reverseOrdering);
  ITK_TEST_SET_GET_VALUE(reverseOrdering, relabel->GetReverseOrdering());

  // testing boolean macro for ReverseOrdering
  relabel->ReverseOrderingOff();
  ITK_TEST_SET_GET_VALUE(false, relabel->GetReverseOrdering());

  relabel->ReverseOrderingOn();
  ITK_TEST_SET_GET_VALUE(true, relabel->GetReverseOrdering());

  // testing get and set macros for Attribute
  unsigned int attribute = std::stoi(argv[5]);
  relabel->SetAttribute(attribute);
  ITK_TEST_SET_GET_VALUE(attribute, relabel->GetAttribute());

  std::string attributeName = StatisticsLabelObjectType::GetNameFromAttribute(attribute);
  relabel->SetAttribute(attributeName);

  relabel->SetInput(i2l->GetOutput());

  itk::SimpleFilterWatcher watcher(relabel, "filter");

  using L2ImageType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  L2ImageType::Pointer l2i = L2ImageType::New();
  l2i->SetInput(relabel->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[3]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
