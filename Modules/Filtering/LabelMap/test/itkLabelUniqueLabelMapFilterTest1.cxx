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

#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkObjectByObjectLabelMapFilter.h"
#include "itkLabelUniqueLabelMapFilter.h"

#include "itkBinaryDilateImageFilter.h"
#include "itkFlatStructuringElement.h"
#include "itkTestingMacros.h"


int
itkLabelUniqueLabelMapFilterTest1(int argc, char * argv[])
{

  if (argc != 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input output reverse" << std::endl;
    return EXIT_SUCCESS;
  }

  constexpr int dim = 2;

  using ImageType = itk::Image<unsigned char, dim>;

  using LabelObjectType = itk::ShapeLabelObject<unsigned char, dim>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using I2LType = itk::LabelImageToShapeLabelMapFilter<ImageType, LabelMapType>;
  auto i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());

  using KernelType = itk::FlatStructuringElement<dim>;
  using DilateType = itk::BinaryDilateImageFilter<ImageType, ImageType, KernelType>;
  auto dilate = DilateType::New();
  auto rad = KernelType::SizeType::Filled(15);
  dilate->SetKernel(KernelType::Ball(rad));

  using OIType = itk::ObjectByObjectLabelMapFilter<LabelMapType, LabelMapType, DilateType>;
  auto oi = OIType::New();
  oi->SetInput(i2l->GetOutput());
  oi->SetFilter(dilate);
  oi->SetPadSize(rad);

  using UniqueType = itk::LabelUniqueLabelMapFilter<LabelMapType>;
  auto unique = UniqueType::New();
  unique->SetInput(oi->GetOutput());
  unique->SetReverseOrdering(std::stoi(argv[3]));
  const itk::SimpleFilterWatcher watcher(unique, "filter");

  using L2IType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  auto l2i = L2IType::New();
  l2i->SetInput(unique->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
