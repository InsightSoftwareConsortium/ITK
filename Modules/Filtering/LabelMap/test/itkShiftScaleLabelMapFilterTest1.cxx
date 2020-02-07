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
#include "itkShiftScaleLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"


int
itkShiftScaleLabelMapFilterTest1(int argc, char * argv[])
{

  if (argc != 6)
  {
    std::cerr << "usage: " << argv[0] << " input output shift scale change_bg" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 2;

  using ImageType = itk::Image<unsigned char, dim>;

  using LabelObjectType = itk::LabelObject<unsigned char, dim>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using I2LType = itk::LabelImageToLabelMapFilter<ImageType, LabelMapType>;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());

  using ChangeType = itk::ShiftScaleLabelMapFilter<LabelMapType>;
  ChangeType::Pointer change = ChangeType::New();
  change->SetInput(i2l->GetOutput());

  change->SetShift(std::stod(argv[3]));
  ITK_TEST_SET_GET_VALUE(std::stod(argv[3]), change->GetShift());

  change->SetScale(std::stod(argv[4]));
  ITK_TEST_SET_GET_VALUE(std::stod(argv[4]), change->GetScale());


  const std::string        shouldChangBackgroundBoolean{ argv[5] };
  static const std::string trueString{ "true" };
  const bool               changeBackground = (shouldChangBackgroundBoolean == trueString) ? true : false;
  change->SetChangeBackgroundValue(changeBackground);
  ITK_TEST_SET_GET_VALUE(changeBackground, change->GetChangeBackgroundValue());

  change->ChangeBackgroundValueOff();
  ITK_TEST_SET_GET_VALUE(false, change->GetChangeBackgroundValue());

  change->ChangeBackgroundValueOn();
  ITK_TEST_SET_GET_VALUE(true, change->GetChangeBackgroundValue());


  itk::SimpleFilterWatcher watcher6(change, "filter");

  using L2IType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput(change->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[2]);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
