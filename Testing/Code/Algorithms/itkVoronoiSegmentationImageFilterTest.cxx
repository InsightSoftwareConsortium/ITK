/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiSegmentationImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkVoronoiSegmentationImageFilter.h"
#include "itkSimpleImageRegionIterator.h"
#include "itkImage.h"
#include "vnl/vnl_sample.h"
#include <stdio.h>

const int WIDTH = 256;
const int HEIGHT = 256;

typedef itk::Image<unsigned short,2> UShortImage;
typedef itk::VoronoiSegmentationImageFilter<UShortImage, UShortImage> VorSeg;

int main(void){
  VorSeg::Pointer testVorseg(VorSeg::New());
  UShortImage::Pointer inputIMG = UShortImage::New();
  UShortImage::SizeType size={{WIDTH,HEIGHT}};
  UShortImage::IndexType index=UShortImage::IndexType::ZeroIndex;
  UShortImage::RegionType region;

  region.SetSize(size);
  region.SetIndex(index);

  inputIMG->SetLargestPossibleRegion( region );
  inputIMG->SetBufferedRegion( region );
  inputIMG->SetRequestedRegion( region );
  inputIMG->Allocate();


  itk::SimpleImageRegionIterator <UShortImage> it(inputIMG, region);

  // background: random field with mean: 500, std: 50
  it.Begin();
  while( !it.IsAtEnd()) {    
    it.Set((unsigned short)(vnl_sample_uniform(450,550)) );
    ++it;
  }

  //object (2): random field with mean: 520, std: 20;
  unsigned int i;
  unsigned int j;
  for(i = 30; i < 94; i++){
    index[0] = i;
    for (j = 30; j< 94; j++){
      index[1] = j;
      inputIMG->SetPixel(index, (unsigned short)(vnl_sample_uniform(500,540)) );
    }
  }

  for(i = 150; i < 214; i++){
    index[0] = i;
    for (j = 150; j< 214; j++){
      index[1] = j;
      inputIMG->SetPixel(index, (unsigned short)(vnl_sample_uniform(500,540)) );
    }
  }


  testVorseg->SetInput(inputIMG);
  testVorseg->SetMean(520);
  testVorseg->SetVar(20);
  testVorseg->SetMeanTolerance(10);
  testVorseg->SetVarTolerance(20);
  testVorseg->SetNumberOfSeeds(200);
  testVorseg->SetSteps(5);

  testVorseg->InitializeSegment();
  testVorseg->ExcuteSegment();

  itk::SimpleImageRegionIterator <UShortImage> ot(testVorseg->GetOutput(), region);

  unsigned short TestImg[65536];
  ot.Begin();
  int k=0;
  while( !ot.IsAtEnd()){
    TestImg[k]=ot.Get();
    k++;
    ++ot;
  }

  /* Test Ok on local machine.
  FILE *imgfile = fopen("output.raw","wb");
  fwrite(TestImg,2,65536,imgfile);
  fclose(imgfile);
 
  imgfile = fopen("input.raw","wb");
  it.Begin();
  k=0;
  while( !it.IsAtEnd()){
    TestImg[k]=it.Get();
    k++;
    ++it;
  }
  fwrite(TestImg,2,65536,imgfile);
  fclose(imgfile);
  */
  return 0;
}

