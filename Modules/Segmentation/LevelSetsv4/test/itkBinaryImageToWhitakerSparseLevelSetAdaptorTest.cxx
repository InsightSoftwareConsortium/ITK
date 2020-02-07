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
itkBinaryImageToWhitakerSparseLevelSetAdaptorTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Missing Arguments" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using InputPixelType = unsigned char;
  using OutputPixelType = double;

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

  using LevelSetType = itk::WhitakerSparseLevelSetImage<OutputPixelType, Dimension>;

  using BinaryToSparseAdaptorType = itk::BinaryImageToLevelSetImageAdaptor<InputImageType, LevelSetType>;

  BinaryToSparseAdaptorType::Pointer adaptor = BinaryToSparseAdaptorType::New();
  adaptor->SetInputImage(input);
  adaptor->Initialize();

  std::cout << "Finished converting to sparse format" << std::endl;

  using LayerIdType = LevelSetType::LayerIdType;
  LevelSetType::Pointer sparseLevelSet = adaptor->GetModifiableLevelSet();

  using OutputImageType = itk::Image<OutputPixelType, Dimension>;
  OutputImageType::Pointer output = OutputImageType::New();
  output->SetRegions(input->GetLargestPossibleRegion());
  output->CopyInformation(input);
  output->Allocate();
  output->FillBuffer(0.0);

  using StatusImageType = itk::Image<signed char, Dimension>;
  StatusImageType::Pointer statusImage = StatusImageType::New();
  statusImage->SetRegions(input->GetLargestPossibleRegion());
  statusImage->CopyInformation(input);
  statusImage->Allocate();
  statusImage->FillBuffer(0);

  using OutputIteratorType = itk::ImageRegionIteratorWithIndex<OutputImageType>;
  OutputIteratorType oIt(output, output->GetLargestPossibleRegion());
  oIt.GoToBegin();

  using StatusIteratorType = itk::ImageRegionIteratorWithIndex<StatusImageType>;
  StatusIteratorType sIt(statusImage, statusImage->GetLargestPossibleRegion());
  sIt.GoToBegin();

  StatusImageType::IndexType idx;

  while (!oIt.IsAtEnd())
  {
    idx = oIt.GetIndex();
    oIt.Set(sparseLevelSet->Evaluate(idx));
    sIt.Set(sparseLevelSet->Status(idx));
    ++oIt;
    ++sIt;
  }

  using OutputWriterType = itk::ImageFileWriter<OutputImageType>;
  OutputWriterType::Pointer outputWriter = OutputWriterType::New();
  outputWriter->SetFileName(argv[2]);
  outputWriter->SetInput(output);

  try
  {
    outputWriter->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << std::endl;
    return EXIT_FAILURE;
  }

  using StatusWriterType = itk::ImageFileWriter<StatusImageType>;
  StatusWriterType::Pointer statusWriter = StatusWriterType::New();
  statusWriter->SetFileName(argv[3]);
  statusWriter->SetInput(statusImage);

  try
  {
    statusWriter->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << err << std::endl;
  }

  for (LayerIdType lyr = sparseLevelSet->MinusTwoLayer(); lyr <= sparseLevelSet->PlusTwoLayer(); lyr++)
  {
    LevelSetType::LayerType layer = sparseLevelSet->GetLayer(lyr);
    auto                    lIt = layer.begin();

    std::cout << "*** " << static_cast<int>(lyr) << " ***" << std::endl;

    while (lIt != layer.end())
    {
      std::cout << lIt->first << ' ' << lIt->second << std::endl;
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
