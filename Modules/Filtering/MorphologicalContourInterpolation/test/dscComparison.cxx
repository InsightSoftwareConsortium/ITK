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

#include <iostream>
#include <fstream>
#include <string>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMorphologicalContourInterpolator.h"
#include "itkTestDriverIncludeRequiredIOFactories.h"
#include "itkTimeProbe.h"

typedef unsigned char              PixelType;
const unsigned int                 dim = 3;
typedef itk::Image<PixelType, dim> ImageType;

ImageType::Pointer
createSparseCopy(const ImageType::Pointer & inImage, ImageType::IndexType nth)
{
  const ImageType::RegionType & lpr = inImage->GetLargestPossibleRegion();

  ImageType::Pointer outImage = ImageType::New();
  outImage->CopyInformation(inImage);
  outImage->SetRegions(lpr);
  outImage->Allocate(true);

  itk::ImageRegionConstIterator<ImageType>     iIt(inImage, lpr);
  itk::ImageRegionIteratorWithIndex<ImageType> oIt(outImage, lpr);

  while (!oIt.IsAtEnd())
  {
    const PixelType & val = iIt.Get();
    if (val)
    {
      const ImageType::IndexType & ind = oIt.GetIndex();
      bool                         write = false;
      for (unsigned axis = 0; axis < dim; axis++)
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
calcOverlap(const ImageType::Pointer & autoSeg,
            const ImageType::Pointer & groundTruth,
            unsigned long long &       tpCount,
            unsigned long long &       fpCount,
            unsigned long long &       fnCount)
{
  const ImageType::RegionType & lpr = groundTruth->GetLargestPossibleRegion();

  itk::ImageRegionConstIterator<ImageType> itAS(autoSeg, lpr);
  itk::ImageRegionConstIterator<ImageType> itGT(groundTruth, lpr);
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

  typedef itk::ImageFileReader<ImageType> ReaderType;
  ReaderType::Pointer                     reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->Update();
  ImageType::Pointer inImage = reader->GetOutput();
  inImage->DisconnectPipeline();

  typedef itk::MorphologicalContourInterpolator<ImageType> mciType;
  mciType::Pointer                                         mci = mciType::New();
  mci->SetUseBallStructuringElement(true); // test cross?

  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer                     writer = WriterType::New();
  writer->SetUseCompression(true);

  const ImageType::RegionType & lpr = inImage->GetLargestPossibleRegion();
  ImageType::IndexType          maxInd;
  for (unsigned axis = 0; axis < dim; axis++)
  {
    maxInd[axis] = itk::IndexValueType(lpr.GetSize(axis));
  }
  fout << "Image, nth, axis, time, TP, FP, FN, TN\n";
  // the big for loop which does the work
  for (int sparsity = 2; sparsity <= 8; sparsity++)
  {
    unsigned long long tpCount, fpCount, fnCount;
    for (int axis = -1; axis < int(dim); axis++)
    {
      mci->SetAxis(axis);
      ImageType::IndexType axisSparsity = maxInd;
      if (axis < 0) // all axes
      {
        for (unsigned a = 0; a < dim; a++)
        {
          axisSparsity[a] = sparsity;
        }
      }
      else // just one axis
      {
        axisSparsity[axis] = sparsity;
      }

      ImageType::Pointer sparseImage = createSparseCopy(inImage, axisSparsity);
      mci->SetInput(sparseImage);
      itk::TimeProbe timeProbe;
      timeProbe.Start();
      mci->Update();
      timeProbe.Stop();
      ImageType::Pointer result = mci->GetOutput();
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
