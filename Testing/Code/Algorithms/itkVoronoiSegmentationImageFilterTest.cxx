/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiSegmentationImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkVoronoiSegmentationImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImage.h"
#include "vnl/vnl_sample.h"
#include <stdio.h>
#ifdef __BORLANDC__
#include <float.h>
#endif
int itkVoronoiSegmentationImageFilterTest(int, char**){
  const int WIDTH = 256;
  const int HEIGHT = 256;

#ifdef __BORLANDC__
  _control87(MCW_EM,MCW_EM);
#endif
  typedef itk::Image<unsigned short,2> UShortImage;
  typedef itk::VoronoiSegmentationImageFilter<UShortImage, UShortImage> VorSeg;

  VorSeg::Pointer testVorseg(VorSeg::New());
  UShortImage::Pointer inputIMG = UShortImage::New();
  UShortImage::SizeType size={{WIDTH,HEIGHT}};
  UShortImage::IndexType index;
  index.Fill(0);

  UShortImage::RegionType region;

  region.SetSize(size);
  region.SetIndex(index);

  std::cout << "Allocating image" << std::endl;
  inputIMG->SetLargestPossibleRegion( region );
  inputIMG->SetBufferedRegion( region );
  inputIMG->SetRequestedRegion( region );
  inputIMG->Allocate();


  itk::ImageRegionIteratorWithIndex <UShortImage> it(inputIMG, region);

  // background: random field with mean: 500, std: 50
  std::cout << "Setting background random pattern image" << std::endl;
  while( !it.IsAtEnd()) {    
    it.Set((unsigned short)(vnl_sample_uniform(450,550)) );
    ++it;
  }

  //object (2): random field with mean: 520, std: 20;
  std::cout << "Defining object #2" << std::endl;
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

  int k;
  unsigned short TestImg[65536];

  testVorseg->SetInput(inputIMG);
  testVorseg->SetMean(520);
  testVorseg->SetSTD(20);
  testVorseg->SetMeanTolerance(10);
  testVorseg->SetSTDTolerance(20);
  testVorseg->SetNumberOfSeeds(400);
  testVorseg->SetSteps(5);

  std::cout << "Running algorithm" << std::endl;
  testVorseg->Update();

  std::cout << "Walking output" << std::endl;
  itk::ImageRegionIteratorWithIndex <UShortImage> ot(testVorseg->GetOutput(), region);

  k=0;
  while( !ot.IsAtEnd()){
    TestImg[k]=ot.Get();
    TestImg[k] = TestImg[k];  // dummy line to eliminate warning
    k++;
    ++ot;
  }

  /* Test Ok on local machine. 
  FILE *imgfile = fopen("output.raw","wb");
  fwrite(TestImg,2,65536,imgfile);
  fclose(imgfile);
 
  imgfile = fopen("input.raw","wb");
  it.GoToBegin();
  k=0;
  while( !it.IsAtEnd()){
    TestImg[k]=it.Get();
    k++;
    ++it;
  }
  fwrite(TestImg,2,65536,imgfile);
  fclose(imgfile);
*/
  std::cout<<"Test Succeed!"<<std::endl;
  return 0;
}

