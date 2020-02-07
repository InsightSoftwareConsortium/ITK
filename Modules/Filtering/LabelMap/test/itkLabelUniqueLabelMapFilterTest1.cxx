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

#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkObjectByObjectLabelMapFilter.h"
#include "itkLabelUniqueLabelMapFilter.h"

#include "itkBinaryDilateImageFilter.h"
#include "itkFlatStructuringElement.h"


int
itkLabelUniqueLabelMapFilterTest1(int argc, char * argv[])
{

  if (argc != 4)
  {
    std::cerr << "usage: " << argv[0] << " input output reverse" << std::endl;
    // std::cerr << "  : " << std::endl;
    exit(1);
  }

  constexpr int dim = 2;

  using ImageType = itk::Image<unsigned char, dim>;

  using LabelObjectType = itk::ShapeLabelObject<unsigned char, dim>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using I2LType = itk::LabelImageToShapeLabelMapFilter<ImageType, LabelMapType>;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());

  using KernelType = itk::FlatStructuringElement<dim>;
  using DilateType = itk::BinaryDilateImageFilter<ImageType, ImageType, KernelType>;
  DilateType::Pointer  dilate = DilateType::New();
  KernelType::SizeType rad;
  rad.Fill(15);
  dilate->SetKernel(KernelType::Ball(rad));

  using OIType = itk::ObjectByObjectLabelMapFilter<LabelMapType, LabelMapType, DilateType>;
  OIType::Pointer oi = OIType::New();
  oi->SetInput(i2l->GetOutput());
  oi->SetFilter(dilate);
  oi->SetPadSize(rad);

  using UniqueType = itk::LabelUniqueLabelMapFilter<LabelMapType>;
  UniqueType::Pointer unique = UniqueType::New();
  unique->SetInput(oi->GetOutput());
  unique->SetReverseOrdering(std::stoi(argv[3]));
  itk::SimpleFilterWatcher watcher(unique, "filter");

  using L2IType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput(unique->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  return 0;
}
