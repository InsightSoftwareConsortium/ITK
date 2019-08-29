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

#include "itkLabelImageToLabelMapFilter.h"
#include "itkLabelMapToRGBImageFilter.h"


int
itkLabelMapToRGBImageFilterTest2(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "usage: " << argv[0] << " input output" << std::endl;
    exit(1);
  }

  constexpr int dim = 2;

  using IType = itk::Image<unsigned char, dim>;
  using OType = itk::VectorImage<unsigned char, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using ConverterType = itk::LabelImageToLabelMapFilter<IType>;
  ConverterType::Pointer converter = ConverterType::New();
  converter->SetInput(reader->GetOutput());

  using ColorizerType = itk::LabelMapToRGBImageFilter<ConverterType::OutputImageType, OType>;
  ColorizerType::Pointer colorizer = ColorizerType::New();
  colorizer->SetInput(converter->GetOutput());

  itk::SimpleFilterWatcher watcher(colorizer, "filter");

  using WriterType = itk::ImageFileWriter<OType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(colorizer->GetOutput());
  writer->SetFileName(argv[2]);
  writer->Update();
  return 0;
}
