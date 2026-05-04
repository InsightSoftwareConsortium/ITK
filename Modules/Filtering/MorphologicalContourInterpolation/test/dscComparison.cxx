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
#include "itkMorphologicalContourInterpolator.h"
#include "itkTestDriverIncludeRequiredIOFactories.h"
#include "itkTimeProbe.h"
#include <fstream>
#include <iostream>
#include <string>

using TestPixelType = unsigned char;
constexpr unsigned int testDim = 3;
using TestImageType = itk::Image<TestPixelType, testDim>;

TestImageType::Pointer
createSparseCopy(const TestImageType::Pointer & inImage, TestImageType::IndexType nth)
{
  const TestImageType::RegionType & lpr = inImage->GetLargestPossibleRegion();

  TestImageType::Pointer outImage = TestImageType::New();
  outImage->CopyInformation(inImage);
  outImage->SetRegions(lpr);
  outImage->Allocate(true);

  itk::ImageRegionConstIterator<TestImageType>     iIt(inImage, lpr);
  itk::ImageRegionIteratorWithIndex<TestImageType> oIt(outImage, lpr);

  while (!oIt.IsAtEnd())
  {
    const TestPixelType & val = iIt.Get();
    if (val)
    {
      const TestImageType::IndexType & ind = oIt.GetIndex();
      bool                             write = false;
      for (unsigned axis = 0; axis < testDim; axis++)
      {
        if (ind[axis] % nth[axis] == 0)
        {
          write = true;
          break;
        }
      }

      if (write)
      {
        oIt.Set(val);
      }
    }
    ++iIt;
    ++oIt;
  }

  return outImage;
}

void
calcOverlap(const TestImageType::Pointer & autoSeg,
            const TestImageType::Pointer & groundTruth,
            unsigned long long &           tpCount,
            unsigned long long &           fpCount,
            unsigned long long &           fnCount)
{
  const TestImageType::RegionType & lpr = groundTruth->GetLargestPossibleRegion();

  itk::ImageRegionConstIterator<TestImageType> itAS(autoSeg, lpr);
  itk::ImageRegionConstIterator<TestImageType> itGT(groundTruth, lpr);
  tpCount = 0;
  fpCount = 0;
  fnCount = 0;

  while (!itAS.IsAtEnd())
  {
    if (itAS.Get() != 0 && itAS.Get() == itGT.Get())
    {
      tpCount++; // true positive
    }
    else if (itAS.Get() != 0)
    {
      fpCount++; // false positive
    }
    if (itAS.Get() == 0 && itGT.Get() != 0)
    {
      fnCount++; // false negative
    }

    ++itAS;
    ++itGT;
  }
}

int
main(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage [outFilenameBase] [saveIntermediateImages]\n";
    return 1;
  }
  bool         saveImages = argc > 3;
  std::string  outFilenameBase;
  std::fstream fout;
  if (argc > 2)
  {
    outFilenameBase = argv[2];
    fout.open((outFilenameBase + ".csv").c_str(), std::ios::out);
  }
  RegisterRequiredFactories();

  using ReaderType = itk::ImageFileReader<TestImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();
  TestImageType::Pointer inImage = reader->GetOutput();
  inImage->DisconnectPipeline();

  using mciType = itk::MorphologicalContourInterpolator<TestImageType>;
  mciType::Pointer mci = mciType::New();
  mci->SetUseBallStructuringElement(true); // test cross?

  using WriterType = itk::ImageFileWriter<TestImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetUseCompression(true);

  const TestImageType::RegionType & lpr = inImage->GetLargestPossibleRegion();
  TestImageType::IndexType          maxInd;
  for (unsigned axis = 0; axis < testDim; axis++)
  {
    maxInd[axis] = itk::IndexValueType(lpr.GetSize(axis));
  }
  fout << "Image, nth, axis, time, TP, FP, FN, TN\n";
  // the big for loop which does the work
  for (int sparsity = 2; sparsity <= 8; sparsity++)
  {
    unsigned long long tpCount, fpCount, fnCount;
    for (int axis = -1; axis < int(testDim); axis++)
    {
      mci->SetAxis(axis);
      TestImageType::IndexType axisSparsity = maxInd;
      if (axis < 0) // all axes
      {
        for (unsigned a = 0; a < testDim; a++)
        {
          axisSparsity[a] = sparsity;
        }
      }
      else // just one axis
      {
        axisSparsity[axis] = sparsity;
      }

      TestImageType::Pointer sparseImage = createSparseCopy(inImage, axisSparsity);
      mci->SetInput(sparseImage);
      itk::TimeProbe timeProbe;
      timeProbe.Start();
      mci->Update();
      timeProbe.Stop();
      TestImageType::Pointer result = mci->GetOutput();
      result->DisconnectPipeline();
      calcOverlap(result, inImage, tpCount, fpCount, fnCount);

      fout << argv[1] << ", " << sparsity << ", " << axis << ", " << timeProbe.GetMean();
      fout << ", " << tpCount << ", " << fpCount << ", " << fnCount << ", ";
      fout << (lpr.GetNumberOfPixels() - tpCount - fpCount - fnCount) << std::endl;

      if (saveImages)
      {
        std::cout << outFilenameBase + '_' + char(sparsity + '0') + char(axis + 'X') + "\nWriting sparse... ";
        writer->SetInput(sparseImage);
        writer->SetFileName(outFilenameBase + "_in" + char(sparsity + '0') + char(axis + 'X') + ".mha");
        writer->Update();
        std::cout << "interpolated... ";
        writer->SetInput(result);
        writer->SetFileName(outFilenameBase + "_out" + char(sparsity + '0') + char(axis + 'X') + ".mha");
        writer->Update();
        std::cout << "finished." << std::endl;
      }
    }
  }
  return 0;
}
