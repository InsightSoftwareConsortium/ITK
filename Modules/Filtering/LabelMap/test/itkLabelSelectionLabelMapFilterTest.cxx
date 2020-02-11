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
#include "itkLabelSelectionLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"
#include "itkTestingMacros.h"


int
itkLabelSelectionLabelMapFilterTest(int argc, char * argv[])
{

  if (argc != 5)
  {
    std::cerr << "usage: " << itkNameOfTestExecutableMacro(argv) << " input output exclude label" << std::endl;
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

  using ChangeType = itk::LabelSelectionLabelMapFilter<LabelMapType>;
  ChangeType::Pointer change = ChangeType::New();
  change->SetInput(i2l->GetOutput());
  change->SetExclude(std::stoi(argv[3]));
  change->SetLabel(std::stoi(argv[4]));
  itk::SimpleFilterWatcher watcher6(change, "filter");

  using L2IType = itk::LabelMapToLabelImageFilter<LabelMapType, ImageType>;
  L2IType::Pointer l2i = L2IType::New();
  l2i->SetInput(change->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(l2i->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();

  return 0;
}
