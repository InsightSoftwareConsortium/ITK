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

#include "itkObjectByObjectLabelMapFilter.h"
#include "itkFlatStructuringElement.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkTestingMacros.h"


int
itkObjectByObjectLabelMapFilterTest(int argc, char * argv[])
{

  if (argc != 6)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " input output keepLabel binaryInternalOutput constrainPaddingToImage" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr int dim = 2;

  using ImageType = itk::Image<unsigned char, dim>;

  using LabelObjectType = itk::LabelObject<unsigned char, dim>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using I2LType = itk::LabelImageToLabelMapFilter<ImageType, LabelMapType>;
  auto i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());

  using KernelType = itk::FlatStructuringElement<dim>;
  using DilateType = itk::BinaryDilateImageFilter<ImageType, ImageType, KernelType>;
  auto                 dilate = DilateType::New();
  KernelType::SizeType rad;
  rad.Fill(3);
  dilate->SetKernel(KernelType::Ball(rad));

  using ObOType = itk::ObjectByObjectLabelMapFilter<LabelMapType>;
  auto obo = ObOType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(obo, ObjectByObjectLabelMapFilter, LabelMapFilter);


  obo->SetInput(i2l->GetOutput());
  obo->SetFilter(dilate);
  ITK_TEST_SET_GET_VALUE(dilate, obo->GetFilter());

  obo->SetInputFilter(dilate);
  ITK_TEST_SET_GET_VALUE(dilate, obo->GetInputFilter());

  obo->SetPadSize(rad);
  ITK_TEST_SET_GET_VALUE(rad, obo->GetPadSize());

  auto keepLabels = static_cast<bool>(std::stoi(argv[3]));
  ITK_TEST_SET_GET_BOOLEAN(obo, KeepLabels, keepLabels);

  bool binaryInternalOutput = static_cast<bool>(std::stoi(argv[4]));
  ITK_TEST_SET_GET_BOOLEAN(obo, BinaryInternalOutput, binaryInternalOutput);

  bool constrainPaddingToImage = static_cast<bool>(std::stoi(argv[5]));
  ITK_TEST_SET_GET_BOOLEAN(obo, ConstrainPaddingToImage, constrainPaddingToImage);

  ObOType::InternalOutputPixelType internalForegroundValue =
    itk::NumericTraits<ObOType::InternalOutputPixelType>::max();
  obo->SetInternalForegroundValue(internalForegroundValue);
  ITK_TEST_SET_GET_VALUE(internalForegroundValue, obo->GetInternalForegroundValue());

  itk::SimpleFilterWatcher watcher(obo, "filter");

  using L2IType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  auto l2i = L2IType::New();
  l2i->SetInput(obo->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
