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

#include "itkHardConnectedComponentImageFilter.h"
#include "itkFilterWatcher.h"

const int HEIGHT = 20;
const int WIDTH = 20;

int itkHardConnectedComponentImageFilterTest(int, char* [] )
{
  typedef itk::Image<bool,2>           InputImageType;
  typedef itk::Image<unsigned short,2> OutputImageType;
  typedef InputImageType::IndexType    IndexType;

  itk::HardConnectedComponentImageFilter<InputImageType, OutputImageType>::Pointer
    filter = itk::HardConnectedComponentImageFilter<InputImageType, OutputImageType>::New();
  InputImageType::Pointer inputimg = InputImageType::New();
  IndexType index;
  index.Fill(0);
  InputImageType::RegionType region;

  itk::Size<2> sz;
  sz[0] = WIDTH;
  sz[1] = HEIGHT;

  region.SetSize(sz);
  region.SetIndex(index);

  inputimg->SetLargestPossibleRegion( region );
  inputimg->SetBufferedRegion( region );
  inputimg->SetRequestedRegion( region );
  inputimg->Allocate();

  int row, col;
  IndexType myIndex;
  for(row = 0;row<20;row++)
    {
    for(col = 0;col < 20;col++)
      {
      myIndex[1] = row;
      myIndex[0] = col;
      inputimg->SetPixel(myIndex,false);
      }
    }
  for(row = 0;row<15;row++)
    {
    for(col = 0;col<20;col++)
      {
      myIndex[1] = row;
      myIndex[0] = col;
      inputimg->SetPixel(myIndex,true);
      }
    }
  for(row = 0;row<10;row++)
    {
    for(col = 5;col<15;col++)
      {
      myIndex[1] = row;
      myIndex[0] = col;
      inputimg->SetPixel(myIndex,false);
      }
    }
  for(row = 0;row<7;row++)
    {
    for(col = 7;col<12;col++)
      {
      myIndex[1] = row;
      myIndex[0] = col;
      inputimg->SetPixel(myIndex,true);
      }
    }
  //InputImageType::IndexType Seed = {10,2};

  FilterWatcher watcher(filter);

  filter->SetInput(inputimg);
  //filter->SetObjectSeed(Seed);
  filter->Update();

  typedef itk::ImageRegionIterator<InputImageType> inputIterator;
  inputIterator it = inputIterator(inputimg, region);

  std::cout << "Input Image" << std::endl;
  it.GoToBegin();
  for(int i = 0;i < HEIGHT*WIDTH; i++)
    {
    if((i%WIDTH) == 0)
      {
      std::cout<<std::endl;
      }
    std::cout << ( it.Get() ? 1 : 0 );
    ++it;
    }
  std::cout<<std::endl;

  typedef itk::ImageRegionIterator<OutputImageType> outputIterator;
  outputIterator ot = outputIterator(filter->GetOutput(), region);

  std::cout << std::endl << "Output Image" << std::endl;
  ot.GoToBegin();
  for(int i = 0;i < HEIGHT*WIDTH; i++)
    {
    if((i%WIDTH) == 0)
      {
      std::cout<<std::endl;
      }
    std::cout<<ot.Get();
    ++ot;
    }

  std::cout<<std::endl;

  return EXIT_SUCCESS;
}
