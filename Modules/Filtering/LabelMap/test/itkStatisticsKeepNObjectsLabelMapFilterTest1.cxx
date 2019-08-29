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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"


#include "itkStatisticsKeepNObjectsLabelMapFilter.h"
#include "itkLabelImageToStatisticsLabelMapFilter.h"

#include "itkTestingMacros.h"

int
itkStatisticsKeepNObjectsLabelMapFilterTest1(int argc, char * argv[])
{
  if (argc != 7)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " input feature output";
    std::cerr << " reverseOrdering attribute numberOfObjectsToKeep";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 3;

  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, dim>;

  using StatisticsLabelObjectType = itk::StatisticsLabelObject<PixelType, dim>;
  using LabelMapType = itk::LabelMap<StatisticsLabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  using I2LType = itk::LabelImageToStatisticsLabelMapFilter<ImageType, ImageType, LabelMapType>;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());
  i2l->SetFeatureImage(reader2->GetOutput());

  using LabelOpeningType = itk::StatisticsKeepNObjectsLabelMapFilter<LabelMapType>;
  LabelOpeningType::Pointer opening = LabelOpeningType::New();

  // testing boolean macro for ReverseOrdering
  opening->ReverseOrderingOn();
  ITK_TEST_SET_GET_VALUE(true, opening->GetReverseOrdering());

  opening->ReverseOrderingOff();
  ITK_TEST_SET_GET_VALUE(false, opening->GetReverseOrdering());

  // testing get and set macros or ReverseOrdering
  bool reverseOrdering = std::stoi(argv[4]);
  opening->SetReverseOrdering(reverseOrdering);
  ITK_TEST_SET_GET_VALUE(reverseOrdering, opening->GetReverseOrdering());

  // testing get and set macros for Attribute
  LabelOpeningType::AttributeType attribute = std::stoi(argv[5]);
  opening->SetAttribute(attribute);
  ITK_TEST_SET_GET_VALUE(attribute, opening->GetAttribute());

  opening->SetNumberOfObjects(std::stoi(argv[6]));
  opening->SetInput(i2l->GetOutput());

  itk::SimpleFilterWatcher watcher(opening, "filter");

  using L2IType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput(opening->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[3]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
