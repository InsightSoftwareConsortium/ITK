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
#include "itkBinaryImageToLevelSetImageAdaptor.h"

int
itkBinaryImageToShiSparseLevelSetAdaptorTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Arguments" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using InputPixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using InputReaderType = itk::ImageFileReader<InputImageType>;

  InputReaderType::Pointer reader = InputReaderType::New();
  reader->SetFileName(argv[1]);
  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << std::endl;
    return EXIT_FAILURE;
  }
  InputImageType::Pointer input = reader->GetOutput();
  std::cout << "Input image read" << std::endl;

  using LevelSetType = itk::ShiSparseLevelSetImage<Dimension>;

  using BinaryToSparseAdaptorType = itk::BinaryImageToLevelSetImageAdaptor<InputImageType, LevelSetType>;

  BinaryToSparseAdaptorType::Pointer adaptor = BinaryToSparseAdaptorType::New();
  adaptor->SetInputImage(input);
  adaptor->Initialize();
  std::cout << "Finished converting to sparse format" << std::endl;

  using LayerIdType = LevelSetType::LayerIdType;

  LevelSetType::Pointer sparseLevelSet = adaptor->GetModifiableLevelSet();

  using StatusImageType = itk::Image<char, Dimension>;
  StatusImageType::Pointer statusImage = StatusImageType::New();
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
  StatusWriterType::Pointer writer = StatusWriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(statusImage);

  try
  {
    writer->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << std::endl;
    return EXIT_FAILURE;
  }

  for (LayerIdType lyr = sparseLevelSet->MinusOneLayer(); lyr <= sparseLevelSet->PlusOneLayer(); lyr += 2)
  {
    LevelSetType::LayerType layer = sparseLevelSet->GetLayer(lyr);
    auto                    lIt = layer.begin();

    std::cout << "*** " << static_cast<int>(lyr) << " ***" << std::endl;

    while (lIt != layer.end())
    {
      std::cout << lIt->first << ' ' << int(lIt->second) << std::endl;
      ++lIt;
    }
    std::cout << std::endl;
  }

  using LabelObjectType = itk::LabelObject<unsigned long, 2>;
  using LabelObjectPointer = LabelObjectType::Pointer;

  LabelObjectPointer labelObject = LabelObjectType::New();
  LabelObjectPointer labelObjectSrc = sparseLevelSet->GetAsLabelObject<unsigned long>();
  labelObject->CopyAllFrom<LabelObjectType>(labelObjectSrc);
  labelObject->SetLabel(sparseLevelSet->PlusOneLayer());

  labelObject->Optimize();
  std::cout << labelObject->Size() << std::endl;

  return EXIT_SUCCESS;
}
