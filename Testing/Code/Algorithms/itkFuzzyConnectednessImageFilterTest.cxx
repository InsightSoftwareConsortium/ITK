/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFuzzyConnectednessImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkFuzzyConnectednessImageFilter.h"
#include "itkSimpleImageRegionIterator.h"
#include <cstdlib>

const int WIDTH = 20;
const int HEIGHT = 20;

int main(){

  typedef itk::Image<bool,2> BinaryImage2D;
  typedef itk::Image<unsigned short,2> UShortImage2D;
  typedef itk::FuzzyConnectednessImageFilter<UShortImage2D,BinaryImage2D> FuzzyUShort;

  FuzzyUShort::Pointer testFuzzy=FuzzyUShort::New();
  UShortImage2D::Pointer inputimg=UShortImage2D::New();
  UShortImage2D::SizeType size={HEIGHT,WIDTH};
  UShortImage2D::IndexType index=UShortImage2D::IndexType::ZeroIndex;
  UShortImage2D::RegionType region;

  region.SetSize(size);
  region.SetIndex(index);

  inputimg->SetLargestPossibleRegion( region );
  inputimg->SetBufferedRegion( region );
  inputimg->SetRequestedRegion( region );

  inputimg->Allocate();

  // background: uniform distributed random number ~(300-350) 
  itk::SimpleImageRegionIterator <UShortImage2D> it(inputimg, region);
  it.Begin();
  while( !it.IsAtEnd())
    {    
    it.Set( (unsigned short)(50*rand()/RAND_MAX+300) );
    ++it;
    }

  // object, value ~(270-320)
  for(int i = 0;i < HEIGHT/2; i++)
    {
    for(int j = 0; j < WIDTH/2; j++)
      {
      index[0] = i;
      index[1] = j;
      inputimg->SetPixel(index,(unsigned short)(50 * rand() / RAND_MAX + 270));
      }
    }

  // print the input image
  std::cout << "\n\n The Input Image\n";
  it.Begin();
  for(int i = 0;i < HEIGHT; i++)
    {
    for (int j = 0; j < WIDTH; j++)
      {
      std::cout << "%3i ";
      ++it;
      }
    std::cout << "\n";
    }

  // execute the segmentation subroutine
  testFuzzy->SetInput(inputimg);

  index[0] = 5;
  index[1] = 5;
  testFuzzy->SetSeed(index);
  testFuzzy->SetParameters(270.0,50.0,1.0,1.0,1.0);
  testFuzzy->SetThreshold(0.8);
  testFuzzy->ExcuteSegment();

  // printout the segmentation result
  std::cout << "\n\n Segmentation Result:\n";
  itk::SimpleImageRegionIterator <BinaryImage2D> ot(testFuzzy->GetOutput(), region);

  ot.Begin();
  for(int i = 0;i < HEIGHT; i++)
    {
    for (int j = 0; j < WIDTH; j++)
      {
      std::cout << ot.Get();
      ++ot;
      }
    std::cout << "\n";
    }
  return 1;
}
