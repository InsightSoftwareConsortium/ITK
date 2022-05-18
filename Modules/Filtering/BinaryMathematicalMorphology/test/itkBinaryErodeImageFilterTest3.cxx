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
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkTestingMacros.h"

int
itkBinaryErodeImageFilterTest3(int argc, char * argv[])
{
  if (argc < 7)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " InputImage OutputImage Foreground Background BoundaryToForeground Radius" << std::endl;
    return EXIT_FAILURE;
  }
  constexpr int dim = 2;

  using PType = unsigned char;
  using IType = itk::Image<PType, dim>;

  using ReaderType = itk::ImageFileReader<IType>;
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using SRType = itk::BinaryBallStructuringElement<PType, dim>;
  SRType kernel;
  kernel.SetRadius(std::stoi(argv[6]));
  kernel.CreateStructuringElement();

  using FilterType = itk::BinaryErodeImageFilter<IType, IType, SRType>;
  auto filter = FilterType::New();
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
  if (filter->GetErodeValue() != itk::NumericTraits<PType>::max())
  {
    std::cerr << "Wrong default dilate value." << std::endl;
    return EXIT_FAILURE;
  }
  if (filter->GetBoundaryToForeground() != true)
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
  filter->SetErodeValue(std::stoi(argv[3]));
  if (filter->GetErodeValue() != std::stoi(argv[3]))
  {
    std::cerr << "Set/Get Erode value problem." << std::endl;
    return EXIT_FAILURE;
  }

  filter->SetBackgroundValue(std::stoi(argv[4]));
  if (filter->GetBackgroundValue() != std::stoi(argv[4]))
  {
    std::cerr << "Set/Get Background value problem." << std::endl;
    return EXIT_FAILURE;
  }

  filter->SetBoundaryToForeground(std::stoi(argv[5]));
  if (filter->GetBoundaryToForeground() != static_cast<bool>(std::stoi(argv[5])))
  {
    std::cerr << "Set/Get BoundaryToForeground value problem." << std::endl;
    return EXIT_FAILURE;
  }

  itk::SimpleFilterWatcher watcher(filter, "filter");

  using WriterType = itk::ImageFileWriter<IType>;
  auto writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
