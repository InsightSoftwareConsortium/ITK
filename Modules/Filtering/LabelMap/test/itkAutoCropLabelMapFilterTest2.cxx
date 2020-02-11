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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkLabelImageToLabelMapFilter.h"
#include "itkLabelSelectionLabelMapFilter.h"
#include "itkAutoCropLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

int
itkAutoCropLabelMapFilterTest2(int argc, char * argv[])
{

  if (argc != 6)
  {
    std::cerr << "usage: " << argv[0];
    std::cerr << " inputLabelImage outputLabelImage1 outputLabelImage2 label1 label2" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, Dimension>;

  using LabelObjectType = itk::LabelObject<PixelType, Dimension>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using ImageToLabelMapFilterType = itk::LabelImageToLabelMapFilter<ImageType, LabelMapType>;
  ImageToLabelMapFilterType::Pointer imageToLabelMapFilter = ImageToLabelMapFilterType::New();
  imageToLabelMapFilter->SetInput(reader->GetOutput());
  itk::SimpleFilterWatcher watcher(imageToLabelMapFilter, "LabelImageToLabelMapFilter");

  using SelectionType = itk::LabelSelectionLabelMapFilter<LabelMapType>;
  SelectionType::Pointer select = SelectionType::New();
  select->SetInput(imageToLabelMapFilter->GetOutput());
  itk::SimpleFilterWatcher watcher2(select, "LabelSelectionLabelMapFilter");

  using AutoCropLabelMapFilterType = itk::AutoCropLabelMapFilter<LabelMapType>;
  AutoCropLabelMapFilterType::Pointer autoCropFilter = AutoCropLabelMapFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(autoCropFilter, AutoCropLabelMapFilter, ChangeRegionLabelMapFilter);

  autoCropFilter->SetInput(select->GetOutput());
  itk::SimpleFilterWatcher watcher3(autoCropFilter, "AutoCropLabelMapFilter");

  using LabelMapToLabelImageFilterType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  LabelMapToLabelImageFilterType::Pointer labelMapToLabelImageFilter = LabelMapToLabelImageFilterType::New();
  labelMapToLabelImageFilter->SetInput(autoCropFilter->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(labelMapToLabelImageFilter->GetOutput());

  // First label
  select->SetLabel(std::stoi(argv[4]));
  labelMapToLabelImageFilter->UpdateLargestPossibleRegion();
  writer->SetFileName(argv[2]);
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  // Second label
  select->SetLabel(std::stoi(argv[5]));
  labelMapToLabelImageFilter->UpdateLargestPossibleRegion();
  writer->SetFileName(argv[3]);
  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
