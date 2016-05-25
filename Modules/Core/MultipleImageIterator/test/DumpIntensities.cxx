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
#include <vector>
#include <string>
#include <cstdlib>
#include <itkImageFileReader.h>
#include <itkImageFileWriter.h>
#include "itkMultipleImageIterator.h"

// Dumps random samples from files into a csv file
using namespace std;

int
DumpIntensities(int argc, char * argv[])
{
  if (argc < 3)
  {
    cerr << "Usage: DumpIntensities outfile inImage [inImage ...]" << endl;
    return 1;
  }
  typedef unsigned short                  PixelType;
  typedef itk::Image<PixelType, 3>        ImageType;
  typedef itk::ImageFileReader<ImageType> ReaderType;

  typedef itk::ImageRegionIterator<ImageType> IteratorType;
  itk::MultipleImageIterator<IteratorType>    it;

  vector<ImageType::Pointer> images; // Need to keep a reference as iterators only have weak references
  for (int i = 2; i < argc; ++i)
  {
    ReaderType::Pointer r = ReaderType::New();
    r->SetFileName(argv[i]);
    r->Update();
    ImageType::Pointer im = r->GetOutput();
    im->DisconnectPipeline();
    images.push_back(im);
    it.AddIterator(itk::ImageRegionIterator<ImageType>(im, im->GetLargestPossibleRegion()));
  }

  unsigned long long                    c = 0;
  typedef itk::FixedArray<PixelType, 3> Vec3;
  vector<Vec3>                          values;
  for (it.GoToBegin(); !it.IsAtEnd(); ++it, ++c)
  {
    if (c % 42 == 0)
    {
      Vec3 v;
      for (unsigned int i = 0; i < it.Size(); ++i)
      {
        v[i] = it[i].Get();
      }
      values.push_back(v);
    }
  }

  typedef itk::Image<Vec3, 1> Image1D;
  Image1D::RegionType         region;
  region.SetIndex(0, 0);
  region.SetSize(0, values.size());
  Image1D::Pointer randImage = Image1D::New();
  randImage->SetRegions(region);
  randImage->Allocate();

  int                               index = 0;
  itk::ImageRegionIterator<Image1D> oIt(randImage, randImage->GetLargestPossibleRegion());
  while (!oIt.IsAtEnd())
  {
    oIt.Set(values[index++]);
    ++oIt;
  }

  typedef itk::ImageFileWriter<Image1D> WriterType;
  WriterType::Pointer                   writer = WriterType::New();
  writer->SetInput(randImage);
  writer->SetFileName(argv[1]);
  writer->Update();

  return 0;
}
