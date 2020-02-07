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


#include "itkLabelImageToLabelMapFilter.h"
#include "itkBinaryReconstructionLabelMapFilter.h"
#include "itkAttributeSelectionLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"

#include "itkTestingMacros.h"

int
itkBinaryReconstructionLabelMapFilterTest(int argc, char * argv[])
{
  if (argc != 5)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input marker output";
    std::cerr << " fg";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int dim = 3;

  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, dim>;

  using AttributeLabelObjectType = itk::AttributeLabelObject<PixelType, dim, bool>;
  using LabelMapType = itk::LabelMap<AttributeLabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using I2LType = itk::LabelImageToLabelMapFilter<ImageType, LabelMapType>;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName(argv[2]);

  using LabelReconstructionType = itk::BinaryReconstructionLabelMapFilter<LabelMapType, ImageType>;
  LabelReconstructionType::Pointer reconstruction = LabelReconstructionType::New();

  // testing get and set macros for Lambda
  int fg = std::stoi(argv[4]);
  reconstruction->SetForegroundValue(fg);
  ITK_TEST_SET_GET_VALUE(fg, reconstruction->GetForegroundValue());

  reconstruction->SetInput(i2l->GetOutput());
  reconstruction->SetMarkerImage(reader2->GetOutput());

  itk::SimpleFilterWatcher watcher(reconstruction, "filter");
  reconstruction->Update();
  reconstruction->GetOutput()->PrintLabelObjects();

  using LabelOpeningType = itk::AttributeSelectionLabelMapFilter<LabelMapType>;
  LabelOpeningType::Pointer opening = LabelOpeningType::New();
  opening->SetInput(reconstruction->GetOutput());
  opening->SetAttribute(true);

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
