/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiSegmentationRGBImageFilter.txx
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
#ifndef _itkVoronoiSegmentationRGBImageFilter_txx
#define _itkVoronoiSegmentationRGBImageFilter_txx

#include "itkImageRegionIteratorWithIndex.h"
#include <math.h>

namespace itk
{

/* constructor: seting the default value of the parameters */
template <class TInputImage, class TOutputImage>
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
VoronoiSegmentationRGBImageFilter(){
  unsigned int i;
  for(i=0;i<6;i++){
    m_Mean[i] = 0;
	m_Var[i] = 0;
    m_MeanTolerance[i] = 10;
    m_VarTolerance[i] = 10;
    m_MeanPercentError[i] = 0.10;
    m_VarPercentError[i] = 1.5;
  }
//testing RGB for both mean and variance. (default).
  m_TestMean[0] = 0;
  m_TestMean[1] = 1;
  m_TestMean[2] = 2;
  m_TestVar[0] = 0;
  m_TestVar[1] = 1;
  m_TestVar[2] = 2;
  m_MaxValueOfRGB = 256;
}

/* destructor */
template <class TInputImage, class TOutputImage>
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
~VoronoiSegmentationRGBImageFilter(){
}

template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
SetMeanPercentError(double x[6]){
  for(unsigned int i=0;i<6;i++){
    m_MeanPercentError[i] = x[i];
    m_MeanTolerance[i] = x[i]*m_Mean[i];
  }
}

template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
SetVarPercentError(double x[6]){
  for(unsigned int i=0;i<6;i++){
    m_VarPercentError[i] = x[i];
    m_VarTolerance[i] = x[i]*m_Var[i];
  }
}


/* initialization for the segmentation */
template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
InitializeSegment(void){
  m_InputImage = this->GetInput();
  m_OutputImage = this->GetOutput(); 

  m_Size = m_InputImage->GetLargestPossibleRegion().GetSize();
  IndexType index = IndexType::ZeroIndex;
  RegionType region;
  region.SetSize(m_Size);
  region.SetIndex(index);
  m_OutputImage->SetLargestPossibleRegion( region );
  m_OutputImage->SetBufferedRegion( region );
  m_OutputImage->SetRequestedRegion( region );
  m_OutputImage->Allocate();  

  m_WorkingImage=RGBHCVImage::New();
  m_WorkingImage->SetLargestPossibleRegion( region );
  m_WorkingImage->SetBufferedRegion( region );
  m_WorkingImage->SetRequestedRegion( region );
  m_WorkingImage->Allocate();  

  itk::ImageRegionIteratorWithIndex <RGBHCVImage> wit(m_WorkingImage, region);
  itk::ImageRegionIteratorWithIndex <InputImageType> iit(m_InputImage, region);
  PixelType ipixel;
  RGBHCVPixel wpixel;

  double X;
  double Y;
  double Z;
  double L;
  double a;
  double b;
  double X0 = m_MaxValueOfRGB*0.982;
  double Y0 = m_MaxValueOfRGB;
  double Z0 = m_MaxValueOfRGB*1.183;

  while( !iit.IsAtEnd()) {    
    ipixel = iit.Get();
    wpixel[0] = ipixel[0];
    wpixel[1] = ipixel[1];
    wpixel[2] = ipixel[2];

    X =  0.607*ipixel[0] + 0.174*ipixel[1] + 0.201*ipixel[2];
    Y =  0.299*ipixel[0] + 0.587*ipixel[1] + 0.114*ipixel[2];
    Z =  0.066*ipixel[1] + 1.117*ipixel[2];
    X = pow((X/X0),0.3333);
    Y = pow((Y/Y0),0.3333);
    Z = pow((Z/Z0),0.3333);
    L = 116*Y - 16;
    a = 500*(X - Y);
    b = 200*(Y - Z);
	
    wpixel[3] = atan(b/a);     //H
    wpixel[4] = sqrt(a*a+b*b); //C
    wpixel[5] = L;             //V 
    wit.Set(wpixel);
    ++wit;
    ++iit;
  }

  m_WorkingVD=VoronoiDiagram::New();
  m_VDGenerator=VoronoiDiagramGenerator::New();

  VoronoiDiagram::PointType VDsize;
  VDsize[0] = (VoronoiDiagram::CoordRepType)(m_Size[0]-0.1);
  VDsize[1] = (VoronoiDiagram::CoordRepType)(m_Size[1]-0.1);
  m_VDGenerator->SetBoundary(VDsize);
  m_VDGenerator->SetRandomSeeds(m_NumberOfSeeds);
  m_StepsRuned = 0;
}

template <class TInputImage, class TOutputImage>
bool
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
TestHomogeneity(IndexList Plist)
{
  int num=Plist.size();
  int i,j;
  RGBHCVPixel getp;
  double addp[6]={0,0,0,0,0,0};
  double addpp[6]={0,0,0,0,0,0};
  for(i=0;i<num;i++){
    getp = m_WorkingImage->GetPixel(Plist[i]);
    for(j=0;j<6;j++){
      addp[j]=addp[j]+getp[j];
      addpp[j]=addpp[j]+getp[j]*getp[j];
    }
  }

  double savemean[6],savevar[6];
  if(num > 1){
    for(i=0;i<6;i++){
      savemean[i] = addp[i]/num;
      savevar[i] = sqrt((addpp[i] - (addp[i]*addp[i])/(num) )/(num-1));
	}
  }
  else{
    for(i=0;i<6;i++){
      savemean[i] = 0;
      savevar[i] = -1;
	}
  }

  bool ok = 1;
  j = 0;
  double savem,savev;
  while (ok && (j < 3)){
	savem = savemean[m_TestMean[j]] - m_Mean[m_TestMean[j]];
	savev = savevar[m_TestVar[j]] - m_Var[m_TestVar[j]];
	if( (savem < -m_MeanTolerance[m_TestMean[j]]) ||
	      (savem > m_MeanTolerance[m_TestMean[j]]) ){
	  ok = 0;
	}
	if( (savev < -m_VarTolerance[m_TestVar[j]]) ||
	    (savev > m_VarTolerance[m_TestVar[j]]) ){
      ok = 0;
    }
    j++;
  }
  if(ok)
	return 1;
  else
    return 0;
}

template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
Reset(void)
{
  RegionType region = m_InputImage->GetRequestedRegion();
  m_VDGenerator->SetRandomSeeds(m_NumberOfSeeds);
  m_StepsRuned = 0;
  m_LastStepSeeds=m_NumberOfSeeds;
  m_NumberOfSeedsToAdded=0;
  itk::ImageRegionIteratorWithIndex <RGBHCVImage> wit(m_WorkingImage, region);
  itk::ImageRegionIteratorWithIndex <InputImageType> iit(m_InputImage, region);
  PixelType ipixel;
  RGBHCVPixel wpixel;

  double X;
  double Y;
  double Z;
  double L;
  double a;
  double b;
  double X0 = m_MaxValueOfRGB*0.982;
  double Y0 = m_MaxValueOfRGB;
  double Z0 = m_MaxValueOfRGB*1.183;

  while( !iit.IsAtEnd()) {    
    ipixel = iit.Get();
    wpixel[0] = ipixel[0];
    wpixel[1] = ipixel[1];
    wpixel[2] = ipixel[2];

    X =  0.607*ipixel[0] + 0.174*ipixel[1] + 0.201*ipixel[2];
    Y =  0.299*ipixel[0] + 0.587*ipixel[1] + 0.114*ipixel[2];
    Z =  0.066*ipixel[1] + 1.117*ipixel[2];
    X = pow((X/X0),0.3333);
    Y = pow((Y/Y0),0.3333);
    Z = pow((Z/Z0),0.3333);
    L = 116*Y - 16;
    a = 500*(X - Y);
    b = 200*(Y - Z);
	
    wpixel[3] = atan(b/a);     //H
    wpixel[4] = sqrt(a*a+b*b); //C
    wpixel[5] = L;             //V 
    wit.Set(wpixel);
    ++wit;
    ++iit;
  }
}
template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
TakeAPrior(BinaryObjectImage* aprior)
{
  RegionType region = m_InputImage->GetRequestedRegion();
  itk::ImageRegionIteratorWithIndex <BinaryObjectImage> ait(aprior, region);
  itk::ImageRegionIteratorWithIndex <RGBHCVImage> iit(m_WorkingImage, region);

  int i,j;
  int minx=0,miny=0,maxx=0,maxy=0;
  bool status=0;
  for(i=0;i<m_Size[1];i++){
    for(j=0;j<m_Size[0];j++){
       if( (status==0)&&(ait.Get()) ){
         miny=i;
         minx=j;
         maxy=i;
         maxx=j;
         status=1;
       } 
       else if( (status==1)&&(ait.Get()) ){
         maxy=i;
         if(minx>j) minx=j;
         if(maxx<j) maxx=j;
       }  
    ++ait;
    }
  }       


  int objnum = 0;
  int bkgnum = 0;
  
  float objaddp[6] = {0.0,0.0,0.0,0.0,0.0,0.0};
  float objaddpp[6] = {0.0,0.0,0.0,0.0,0.0,0.0};
  float bkgaddp[6] = {0.0,0.0,0.0,0.0,0.0,0.0};
  float bkgaddpp[6] = {0.0,0.0,0.0,0.0,0.0,0.0};
  RGBHCVPixel currp;

  ait.GoToBegin();
  iit.GoToBegin();
  int k;
  for(i=0;i<miny;i++){
    for(j=0;j<m_Size[0];j++){
      ++ait;
      ++iit;
    }
  }
  for(i=miny;i<=maxy;i++){
    for(j=0;j<minx;j++){
      ++ait;
      ++iit;
    }
    for(j=minx;j<=maxx;j++){
      currp = iit.Get();
      if(ait.Get()){
	    objnum++;
	    for(k=0;k<6;k++){
	      objaddp[k] += currp[k];
	      objaddpp[k] += currp[k]*currp[k];
	    }
      }
      else{
	    bkgnum++;
	    for(k=0;k<6;k++){
	      bkgaddp[k] += currp[k];
	      bkgaddpp[k] += currp[k]*currp[k];
	    }
	  }
      ++ait;++iit;
    }
    for(j=maxx+1;j<m_Size[0];j++){
      ++ait;
      ++iit;
    }
  }

  double b_Mean[6];
  double b_Var[6];
  float diffMean[6];
  float diffVar[6];
  for(i=0;i<6;i++){
    m_Mean[i] = objaddp[i]/objnum;
    m_Var[i] = sqrt((objaddpp[i] - (objaddp[i]*objaddp[i])/objnum)/(objnum-1));
    m_VarTolerance[i] = m_Var[i]*m_VarPercentError[i];
    b_Mean[i] = bkgaddp[i]/bkgnum;
    b_Var[i] = sqrt((bkgaddpp[i] - (bkgaddp[i]*bkgaddp[i])/bkgnum)/(bkgnum-1));
	diffMean[i] = (b_Mean[i]-m_Mean[i])/m_Mean[i];
	if(diffMean[i] < 0) diffMean[i] = -diffMean[i];
	diffVar[i] = (b_Var[i]-m_Var[i])/m_Var[i];
	if(diffVar[i] < 0) diffVar[i] = -diffVar[i];
    if(m_UseBackgroundInAPrior)
      m_MeanTolerance[i] = diffMean[i]*m_Mean[i]*m_MeanDeviation;
    else
      m_MeanTolerance[i] = m_Mean[i]*m_MeanPercentError[i];
  }

/* stupid sorting...*/
  unsigned char tmp[6]={0,1,2,3,4,5};
  for(j=0;j<3;j++){
    k=0;
    for(i=1;i<6-j;i++){
      if(diffMean[tmp[i]]>diffMean[tmp[k]]){
	    k=i;
	  }   
    }
	m_TestMean[j]=tmp[k];
    tmp[k]=tmp[5-j];
  }
  unsigned char tmp1[6]={0,1,2,3,4,5};
  for(j=0;j<3;j++){
    k=0;
    for(i=1;i<6-j;i++){
      if(diffVar[tmp1[i]]>diffVar[tmp1[k]]){
	    k=i;
	  }   
    }
	m_TestVar[j]=tmp1[k];
    tmp1[k]=tmp1[5-j];
  }
}

} //end namespace

#endif


