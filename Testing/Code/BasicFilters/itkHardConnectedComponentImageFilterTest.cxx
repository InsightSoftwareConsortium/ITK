/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHardConnectedComponentImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkHardConnectedComponentImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkFilterWatcher.h"

const int HEIGHT = 20;
const int WIDTH = 20;

int itkHardConnectedComponentImageFilterTest(int, char* [] )
{
  typedef itk::Image<bool,2> InputImageType;
  typedef itk::Image<unsigned short,2> OutputImageType;
  typedef InputImageType::IndexType IndexType;

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
    std::cout << it.Get() ? 1 : 0;
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

  return 0;
}
