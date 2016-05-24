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
#include "itkMultipleImageIterator.h"

// Dumps random samples from files into a csv file
using namespace std;

int
DumpIntensities(int argc, char * argv[])
{
  if (argc < 2)
  {
    cerr << "Usage: DumpIntensities file [file ...]" << endl;
    return 1;
  }
  typedef itk::Image<float, 3>            ImageType;
  typedef itk::ImageFileReader<ImageType> ReaderType;

  typedef itk::ImageRegionIterator<ImageType> IteratorType;
  itk::MultipleImageIterator<IteratorType>    it;

  vector<ImageType::Pointer> images; // Need to keep a reference as iterators only have weak references
  for (int i = 1; i < argc; ++i)
  {
    ReaderType::Pointer r = ReaderType::New();
    r->SetFileName(argv[i]);
    r->Update();
    ImageType::Pointer im = r->GetOutput();
    im->DisconnectPipeline();
    images.push_back(im);
    it.AddIterator(itk::ImageRegionIterator<ImageType>(im, im->GetLargestPossibleRegion()));
  }

  srand(42);
  ofstream f("output.txt");
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    if (((float)rand()) / RAND_MAX < 0.1)
    {
      for (unsigned int i = 0; i < it.Size(); ++i)
      {
        if (i != 0)
          f << ";";
        f << it[i].Get();
      }
      f << endl;
    }
  }
  return 0;
}
