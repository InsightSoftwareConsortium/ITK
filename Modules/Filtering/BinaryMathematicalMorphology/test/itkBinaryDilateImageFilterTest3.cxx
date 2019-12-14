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
#include "itkBinaryDilateImageFilter.h"
#include "itkBinaryBallStructuringElement.h"

int
itkBinaryDilateImageFilterTest3(int argc, char * argv[])
{
  if (argc < 7)
  {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " InputImage OutputImage Foreground Background BoundaryToForeground Radius" << std::endl;
    return EXIT_FAILURE;
  }
  constexpr int dim = 2;

  using PType = unsigned char;
  using IType = itk::Image<PType, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using SRType = itk::BinaryBallStructuringElement<PType, dim>;
  SRType kernel;
  kernel.SetRadius(std::stoi(argv[6]));
  kernel.CreateStructuringElement();

  using FilterType = itk::BinaryDilateImageFilter<IType, IType, SRType>;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(reader->GetOutput());
  filter->SetKernel(kernel);

  // test default values
  if (filter->GetBackgroundValue() != itk::NumericTraits<PType>::NonpositiveMin())
  {
    std::cerr << "Wrong default background value." << std::endl;
    return EXIT_FAILURE;
  }
  if (filter->GetForegroundValue() != itk::NumericTraits<PType>::max())
  {
    std::cerr << "Wrong default foreground value." << std::endl;
    return EXIT_FAILURE;
  }
  if (filter->GetDilateValue() != itk::NumericTraits<PType>::max())
  {
    std::cerr << "Wrong default dilate value." << std::endl;
    return EXIT_FAILURE;
  }
  if (filter->GetBoundaryToForeground() != false)
  {
    std::cerr << "Wrong default BoundaryToForeground value." << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise Set/Get methods for Background Value
  filter->SetForegroundValue(std::stoi(argv[3]));
  if (filter->GetForegroundValue() != std::stoi(argv[3]))
  {
    std::cerr << "Set/Get Foreground value problem." << std::endl;
    return EXIT_FAILURE;
  }

  // the same with the alias
  filter->SetDilateValue(std::stoi(argv[3]));
  if (filter->GetDilateValue() != std::stoi(argv[3]))
  {
    std::cerr << "Set/Get Dilate value problem." << std::endl;
    return EXIT_FAILURE;
  }

  filter->SetBackgroundValue(std::stoi(argv[4]));
  if (filter->GetBackgroundValue() != std::stoi(argv[4]))
  {
    std::cerr << "Set/Get Background value problem." << std::endl;
    return EXIT_FAILURE;
  }

  filter->SetBoundaryToForeground(std::stoi(argv[5]));
  if (filter->GetBoundaryToForeground() != (bool)std::stoi(argv[5]))
  {
    std::cerr << "Set/Get BoundaryToForeground value problem." << std::endl;
    return EXIT_FAILURE;
  }

  itk::SimpleFilterWatcher watcher(filter, "filter");

  using WriterType = itk::ImageFileWriter<IType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
