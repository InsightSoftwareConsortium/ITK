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
#include "itkBinaryImageToLevelSetImageAdaptor.h"
#include "itkTestingMacros.h"

int
itkBinaryImageToShiSparseLevelSetAdaptorTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << '\n';
    std::cerr << "Usage:" << '\n';
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputFilename outputFilename" << '\n';
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using InputPixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using InputReaderType = itk::ImageFileReader<InputImageType>;

  auto reader = InputReaderType::New();
  reader->SetFileName(argv[1]);
  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << '\n';
    return EXIT_FAILURE;
  }
  const InputImageType::Pointer input = reader->GetOutput();
  std::cout << "Input image read" << '\n';

  using LevelSetType = itk::ShiSparseLevelSetImage<Dimension>;

  using BinaryToSparseAdaptorType = itk::BinaryImageToLevelSetImageAdaptor<InputImageType, LevelSetType>;

  auto adaptor = BinaryToSparseAdaptorType::New();
  adaptor->SetInputImage(input);
  adaptor->Initialize();
  std::cout << "Finished converting to sparse format" << '\n';

  using LayerIdType = LevelSetType::LayerIdType;

  const LevelSetType::Pointer sparseLevelSet = adaptor->GetModifiableLevelSet();

  using StatusImageType = itk::Image<char, Dimension>;
  auto statusImage = StatusImageType::New();
  statusImage->SetRegions(input->GetLargestPossibleRegion());
  statusImage->CopyInformation(input);
  statusImage->Allocate();
  statusImage->FillBuffer(0);

  using StatusIteratorType = itk::ImageRegionIteratorWithIndex<StatusImageType>;
  StatusIteratorType sIt(statusImage, statusImage->GetLargestPossibleRegion());
  sIt.GoToBegin();

  StatusImageType::IndexType idx;

  while (!sIt.IsAtEnd())
  {
    idx = sIt.GetIndex();
    sIt.Set(sparseLevelSet->Evaluate(idx));
    ++sIt;
  }

  using StatusWriterType = itk::ImageFileWriter<StatusImageType>;
  auto writer = StatusWriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(statusImage);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << '\n';
    return EXIT_FAILURE;
  }

  for (LayerIdType lyr = sparseLevelSet->MinusOneLayer(); lyr <= sparseLevelSet->PlusOneLayer(); lyr += 2)
  {
    LevelSetType::LayerType layer = sparseLevelSet->GetLayer(lyr);
    auto                    lIt = layer.begin();

    std::cout << "*** " << static_cast<int>(lyr) << " ***" << '\n';

    while (lIt != layer.end())
    {
      std::cout << lIt->first << ' ' << static_cast<int>(lIt->second) << '\n';
      ++lIt;
    }
    std::cout << '\n';
  }

  using LabelObjectType = itk::LabelObject<unsigned long, 2>;
  using LabelObjectPointer = LabelObjectType::Pointer;

  const LabelObjectPointer labelObject = LabelObjectType::New();
  const LabelObjectPointer labelObjectSrc = sparseLevelSet->GetAsLabelObject<unsigned long>();
  labelObject->CopyAllFrom<LabelObjectType>(labelObjectSrc);
  labelObject->SetLabel(sparseLevelSet->PlusOneLayer());

  labelObject->Optimize();
  std::cout << labelObject->Size() << '\n';

  return EXIT_SUCCESS;
}
