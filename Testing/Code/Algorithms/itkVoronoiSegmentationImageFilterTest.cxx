/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiSegmentationImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "itkVoronoiSegmentationImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
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


  itk::ImageRegionIteratorWithIndex <UShortImage> it(inputIMG, region);

  // background: random field with mean: 500, std: 50
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

  itk::ImageRegionIteratorWithIndex <UShortImage> ot(testVorseg->GetOutput(), region);

  unsigned short TestImg[65536];
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
  return 0;
}

