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


#include "itkLabelImageToStatisticsLabelMapFilter.h"
#include "itkStatisticsUniqueLabelMapFilter.h"

#include "itkTestingMacros.h"

#include "itkFlatStructuringElement.h"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkObjectByObjectLabelMapFilter.h"

int
itkStatisticsUniqueLabelMapFilterTest1(int argc, char * argv[])
{
  // ToDo: remove dilationOutput once the JIRA issue 3370 has been solved
  // Then, argc != 6
  if (argc != 7)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " input feature output dilationOutput";
    std::cerr << " reverseOrdering attribute";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }
  const char *       inputImage = argv[1];
  const char *       featureImage = argv[2];
  const char *       outputImage = argv[3];
  const char *       dilationOutput = argv[4];
  const bool         reverseOrdering = std::stoi(argv[5]);
  const unsigned int attribute = std::stoi(argv[6]);

  constexpr unsigned int Dimension = 2;

  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, Dimension>;
  using StatisticsLabelObjectType = itk::StatisticsLabelObject<PixelType, Dimension>;
  using LabelMapType = itk::LabelMap<StatisticsLabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inputImage);

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName(featureImage);


  // Dilate each label object to form overlapping label objects.
  constexpr unsigned int radiusValue = 5;

  using LabelImageToLabelMapFilterType = itk::LabelImageToLabelMapFilter<ImageType, LabelMapType>;
  LabelImageToLabelMapFilterType::Pointer labelMapConverter = LabelImageToLabelMapFilterType::New();
  labelMapConverter->SetInput(reader->GetOutput());
  labelMapConverter->SetBackgroundValue(itk::NumericTraits<PixelType>::ZeroValue());

  using StructuringElementType = itk::FlatStructuringElement<Dimension>;
  StructuringElementType::RadiusType radius;
  radius.Fill(radiusValue);

  StructuringElementType structuringElement = StructuringElementType::Box(radius);

  using MorphologicalFilterType = itk::GrayscaleDilateImageFilter<ImageType, ImageType, StructuringElementType>;
  MorphologicalFilterType::Pointer grayscaleDilateFilter = MorphologicalFilterType::New();
  grayscaleDilateFilter->SetInput(reader->GetOutput());
  grayscaleDilateFilter->SetKernel(structuringElement);

  using ObjectByObjectLabelMapFilterType = itk::ObjectByObjectLabelMapFilter<LabelMapType>;
  ObjectByObjectLabelMapFilterType::Pointer objectByObjectLabelMapFilter = ObjectByObjectLabelMapFilterType::New();
  objectByObjectLabelMapFilter->SetInput(labelMapConverter->GetOutput());
  objectByObjectLabelMapFilter->SetBinaryInternalOutput(false);
  objectByObjectLabelMapFilter->SetFilter(grayscaleDilateFilter);

  using StatisticsFilterType = itk::StatisticsLabelMapFilter<LabelMapType, ImageType>;
  StatisticsFilterType::Pointer statisticsFilter = StatisticsFilterType::New();
  statisticsFilter->SetInput1(objectByObjectLabelMapFilter->GetOutput());
  statisticsFilter->SetFeatureImage(reader2->GetOutput());

  using LabelUniqueType = itk::StatisticsUniqueLabelMapFilter<LabelMapType>;
  LabelUniqueType::Pointer unique = LabelUniqueType::New();

  // testing boolean macro for ReverseOrdering
  unique->ReverseOrderingOn();
  ITK_TEST_SET_GET_VALUE(true, unique->GetReverseOrdering());

  unique->ReverseOrderingOff();
  ITK_TEST_SET_GET_VALUE(false, unique->GetReverseOrdering());

  // testing get and set macros for ReverseOrdering
  // ToDo: decrease reverseOrdering argv index once the JIRA issue 3370 has been solved
  // Then, argv[4]
  unique->SetReverseOrdering(reverseOrdering);
  ITK_TEST_SET_GET_VALUE(reverseOrdering, unique->GetReverseOrdering());


  // testing get and set macros for Attribute
  // ToDo: decrease attribute argv index once the JIRA issue 3370 has been solved
  // Then, argv[5]
  unique->SetAttribute(attribute);
  ITK_TEST_SET_GET_VALUE(attribute, unique->GetAttribute());

  unique->SetInput(statisticsFilter->GetOutput());

  itk::SimpleFilterWatcher watcher(unique, "filter");

  using LabelMapToImageFilterType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  LabelMapToImageFilterType::Pointer labelMapToImageFilter = LabelMapToImageFilterType::New();
  labelMapToImageFilter->SetInput(unique->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(labelMapToImageFilter->GetOutput());
  writer->SetFileName(outputImage);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  // WARNING: TEMPORARY: JIRA ISSUE 3370
  // Writing an additional output of just the dilated label
  writer->SetInput(grayscaleDilateFilter->GetOutput());
  writer->SetFileName(dilationOutput);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
