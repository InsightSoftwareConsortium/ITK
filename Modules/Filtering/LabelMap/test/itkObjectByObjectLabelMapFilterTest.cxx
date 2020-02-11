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

#include "itkObjectByObjectLabelMapFilter.h"
#include "itkFlatStructuringElement.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkTestingMacros.h"


int
itkObjectByObjectLabelMapFilterTest(int argc, char * argv[])
{

  if (argc != 4)
  {
    std::cerr << "usage: " << itkNameOfTestExecutableMacro(argv) << " input output keepLabel" << std::endl;
    // std::cerr << "  : " << std::endl;
    exit(1);
  }

  constexpr int dim = 2;

  using ImageType = itk::Image<unsigned char, dim>;

  using LabelObjectType = itk::LabelObject<unsigned char, dim>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using I2LType = itk::LabelImageToLabelMapFilter<ImageType, LabelMapType>;
  I2LType::Pointer i2l = I2LType::New();
  i2l->SetInput(reader->GetOutput());

  using KernelType = itk::FlatStructuringElement<dim>;
  using DilateType = itk::BinaryDilateImageFilter<ImageType, ImageType, KernelType>;
  DilateType::Pointer  dilate = DilateType::New();
  KernelType::SizeType rad;
  rad.Fill(3);
  dilate->SetKernel(KernelType::Ball(rad));

  using ObOType = itk::ObjectByObjectLabelMapFilter<LabelMapType>;
  ObOType::Pointer obo = ObOType::New();
  obo->SetInput(i2l->GetOutput());
  obo->SetFilter(dilate);
  obo->SetPadSize(rad);
  obo->SetKeepLabels(std::stoi(argv[3]));
  //  obo->SetBinaryInternalOutput( false );
  itk::SimpleFilterWatcher watcher(obo, "filter");

  using L2IType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput(obo->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  return 0;
}
