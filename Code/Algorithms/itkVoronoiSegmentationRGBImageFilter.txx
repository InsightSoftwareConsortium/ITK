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

#include "itkSimpleImageRegionIterator.h"
#include <math.h>

namespace itk
{

/* constructor: seting the default value of the parameters */
template <class TInputImage, class TOutputImage>
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
VoronoiSegmentationRGBImageFilter(){
  m_MinRegion = 20;
  m_Steps = 0;
  m_LastStepSeeds = 0;
  m_NumberOfSeeds = 200;
  m_MaxValueOfRGB = 256.0;
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
  m_MeanDeviation = 0.8;
  m_UseBackgroundInAPrior = 0;
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

  itk::SimpleImageRegionIterator <RGBHCVImage> wit(m_WorkingImage, region);
  itk::SimpleImageRegionIterator <InputImageType> iit(m_InputImage, region);
  wit.Begin();
  iit.Begin();
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
  VoronoiDiagram::PointType VDsize;
  VDsize[0] = (VoronoiDiagram::CoordRepType)(m_Size[0]-0.1);
  VDsize[1] = (VoronoiDiagram::CoordRepType)(m_Size[1]-0.1);
  m_WorkingVD->SetBoundary(VDsize);
  m_WorkingVD->SetRandomSeeds(m_NumberOfSeeds);
  m_StepsRuned = 0;

}

template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
GetStats(PointTypeDeque vertlist, double *savemean, double *savevar, int *nump)
{

  int num=0;
  float addp[6]={0.0,0.0,0.0,0.0,0.0,0.0};
  float addpp[6]={0.0,0.0,0.0,0.0,0.0,0.0};

  IndexType idx;
  RGBHCVPixel getp;

  PointType currP;
  PointType leftP;
  PointType rightP;
  currP = vertlist.front();
  vertlist.pop_front();
  leftP = vertlist.front();
  while(currP[1] > leftP[1]){
    vertlist.push_back(currP);
    currP=vertlist.front();
    vertlist.pop_front();
    leftP=vertlist.front();
  }
  rightP=vertlist.back();
  while(currP[1] > rightP[1]){
    vertlist.push_front(currP);
    currP=vertlist.back();
    vertlist.pop_back();
    rightP=vertlist.back();
  }
  leftP=vertlist.front();


  int vsize = vertlist.size();
  double beginx;
  double beginy;
  double endx;
  double endy;
  double leftendy;
  double rightendy;
  double offset;
  double leftDx;
  double rightDx;
  bool RorL;
  bool saveRorL;
  int intbeginx;
  int intendx;
  int intbeginy;
  int intendy;
  int i;
  int j;
  unsigned int k;

  beginx = currP[0];
  endx = currP[0];
  beginy = currP[1];
  leftendy = leftP[1];
  rightendy = rightP[1];
  if(leftendy > rightendy){
    RorL = 1;
    endy = rightendy;
  }
  else{
    RorL = 0;
    endy = leftendy;
  }
  intbeginy = (int)(beginy);
  intendy = (int)endy;

  if(intbeginy == intendy){
    if(RorL){
      leftDx = (leftP[0]-beginx)/(leftP[1]-beginy);
    }
    else{
      rightDx = (rightP[0]-beginx)/(rightP[1]-beginy);
    }    
  }
  else{
    leftDx = (leftP[0]-beginx)/(leftP[1]-beginy);
    rightDx = (rightP[0]-beginx)/(rightP[1]-beginy);
    intbeginy++;
    offset = (double)intbeginy - beginy;  
    if(RorL){
      endx += offset*rightDx;
    }
    else{
      beginx += offset*leftDx;
    }
    if(beginx < endx){
      for(i = intbeginy;i<=intendy;i++){
        intbeginx = (int)(beginx+1);
        intendx = (int)endx;
        idx[1]=i;
        for(j = intbeginx; j < intendx; j++){
          num++;
		  idx[0]=j;
		  getp=m_WorkingImage->GetPixel(idx);
          for(k=0;k<6;k++){ 
		    addp[k] += getp[k];
		    addpp[k] += getp[k]*getp[k];
		  }
        }
  	    beginx += leftDx;
	    endx += rightDx;
	  }
    }		  
    else{
	  for(i = intbeginy;i<=intendy;i++){
	    intendx = (int)(beginx+1);
	    intbeginx = (int)endx;
	    idx[1]=i;
	    for(j = intbeginx; j < intendx; j++){
          num++;
  		  idx[0]=j;
		  getp = m_WorkingImage->GetPixel(idx);
          for(k=0;k<6;k++){ 
		    addp[k] += getp[k];
		    addpp[k] += getp[k]*getp[k];
		  }
        }
	    beginx += leftDx;
	    endx += rightDx;
	  }
    }		  
  }
  while(vsize > 1){
    vsize--;
    if(RorL){
	  endx = rightP[0];
	  beginy = rightP[1];
	  vertlist.pop_back();
	  rightP = vertlist.back();
      rightDx = (rightP[0]-endx)/(rightP[1]-beginy);
	}
	else{
	  beginx = leftP[0];
	  beginy = leftP[1];
	  vertlist.pop_front();
	  leftP = vertlist.front();
      leftDx = (leftP[0]-beginx)/(leftP[1]-beginy);
	}
	leftendy = leftP[1];
	rightendy = rightP[1];
	if(leftendy > rightendy){
	  saveRorL = 1;
	  endy = rightendy;
	}
	else{
	  saveRorL = 0;
	  endy = leftendy;
	}
	
	intbeginy = (int)(beginy);
	intendy = (int)(endy);
	if(intbeginy == intendy){
      i = i; //donothing;
	}
	else{
	  intbeginy++;
	  offset = (double)intbeginy - beginy;  
      if(RorL){
	    endx += offset*rightDx;
	  }
	  else{
	    beginx += offset*leftDx;
	  }
    }
    if(beginx < endx){
	  for(i = intbeginy;i<=intendy;i++){
	    intbeginx = (int)(beginx+1);
		intendx = (int)endx;
	    idx[1]=i;
		for(j = intbeginx; j < intendx; j++){
          num++;
		  idx[0]=j;
		  getp = m_WorkingImage->GetPixel(idx);
          for(k=0;k<6;k++){ 
		    addp[k] = addp[k] + getp[k];
		    addpp[k] += getp[k]*getp[k];
		  }
        }
		beginx += leftDx;
		endx += rightDx;
	  }
	}		  
    else{
	  for(i = intbeginy;i<=intendy;i++){
	    intendx = (int)(beginx+1);
		intbeginx = (int)endx;
	    idx[1]=i;
		for(j = intbeginx; j < intendx; j++){
          num++;
		  idx[0]=j;
		  getp=m_WorkingImage->GetPixel(idx);
          for(k=0;k<6;k++){ 
		    addp[k] += getp[k];
		    addpp[k] += getp[k]*getp[k];
		  }
        }
		beginx += leftDx;
		endx += rightDx;
	  }
	}		  
	RorL = saveRorL;
  }


  (*nump) = num;
  if(num > 1){
    for(k=0;k<6;k++){
      savemean[k] = addp[k]/num;
      savevar[k] = sqrt(addpp[k] - (addp[k]*addp[k])/(num) )/(num-1);
	}
  }
  else{
    for(k=0;k<6;k++){
      savemean[k] = 0;
      savevar[k] = -1;
	}
  }  

}


template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
ClassifyDiagram(void)
{
  CellPointer currCell; 
  double currMean[6];
  double currVar[6];
  int totalnum;
  PointIdIterator currPit;
  PointIdIterator currPitEnd;
  PointType currP;
  PointTypeDeque VertList;
  double savem;
  double savev;
  unsigned int k;
  bool ok;
  for(int i=0;i<m_NumberOfSeeds;i++){
    currCell = m_WorkingVD->GetCellId(i);
	currPitEnd = currCell->PointIdsEnd();
	VertList.clear();
	for(currPit=currCell->PointIdsBegin();currPit!=currPitEnd;++currPit){
	  m_WorkingVD->GetPointId((*currPit),&(currP));
	  VertList.push_back(currP);
	}
	GetStats(VertList,currMean,currVar,&totalnum);
    ok = 1;
    k = 0;
    m_NumberOfPixels[i] = totalnum;
    while (ok && (k < 3)){
	  savem = currMean[m_TestMean[k]] - m_Mean[m_TestMean[k]];
	  savev = currVar[m_TestVar[k]] - m_Var[m_TestVar[k]];
	  if( (savem < -m_MeanTolerance[m_TestMean[k]]) ||
	      (savem > m_MeanTolerance[m_TestMean[k]]) ){
		 ok = 0;
	  }
	  if( (savev < -m_VarTolerance[m_TestVar[k]]) ||
	      (savev > m_VarTolerance[m_TestVar[k]]) ){
		 ok = 0;
	  }
      k++;
	}
	if(ok)
	  m_Label[i] = 1;
	else
	  m_Label[i] = 0;
  }

  m_NumberOfBoundary = 0;
  for(int i=0;i<m_NumberOfSeeds;i++){
    if(m_Label[i] == 0){
	  NeighborIdIterator itend = m_WorkingVD->NeighborIdsEnd(i);
	  NeighborIdIterator it=m_WorkingVD->NeighborIdsBegin(i);
	  bool bnd = 0;
	  while((it != itend) && (!bnd)){
	    bnd = (m_Label[*it] == 1);
		++it;
	  }
	  if(bnd){
	    m_Label[i] = 2;
		m_NumberOfBoundary++;
	  }
	}
  }

}

template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
GenerateAddingSeeds(void)
{
  EdgeIterator eit;
  EdgeIterator eitend =	m_WorkingVD->EdgeEnd();  
  PointType adds;
  Point<int,2> seeds;

  for(eit = m_WorkingVD->EdgeBegin();eit != eitend; ++eit){
	seeds = m_WorkingVD->GetSeedsIDAroundEdge(eit);
	if( ((m_Label[seeds[0]]==2)||(m_Label[seeds[1]]==2))
	  && (m_NumberOfPixels[seeds[0]]>m_MinRegion)
	  && (m_NumberOfPixels[seeds[1]]>m_MinRegion) ){
	  adds[0] = (eit->m_left[0] + eit->m_right[0])*0.5;
	  adds[1] = (eit->m_left[1] + eit->m_right[1])*0.5;
	  m_SeedsToAdded.push_back(adds);
	}
  }
}


template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
ExcuteSegmentOneStep(void){
  m_NumberOfPixels.resize(m_NumberOfSeeds);
  m_Label.resize(m_NumberOfSeeds);
  m_SeedsToAdded.clear();
  m_WorkingVD->GenerateDiagram();
  ClassifyDiagram();
  GenerateAddingSeeds();
  m_NumberOfSeedsToAdded = m_SeedsToAdded.size();
  m_StepsRuned++;
}

template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
ExcuteSegment(void){
  bool ok = 1;
  if(m_Steps == 0){
    ExcuteSegmentOneStep();
	if(m_NumberOfBoundary == 0){
	  ok=0;
    }
    while( (m_NumberOfSeedsToAdded != 0) && ok){
	  m_WorkingVD->AddSeeds(m_NumberOfSeedsToAdded,m_SeedsToAdded.begin());
	  m_LastStepSeeds = m_NumberOfSeeds;
	  m_NumberOfSeeds += m_NumberOfSeedsToAdded;
      ExcuteSegmentOneStep();
	}
  }
  else if(m_Steps == 1){
    ExcuteSegmentOneStep();
  }
  else{
    ExcuteSegmentOneStep();
	if(m_NumberOfBoundary == 0){
	  ok=0;
    }
	int i = 1;
	while((i<m_Steps) && ok){
	  m_WorkingVD->AddSeeds(m_NumberOfSeedsToAdded, m_SeedsToAdded.begin());
	  m_LastStepSeeds = m_NumberOfSeeds;
	  m_NumberOfSeeds += m_NumberOfSeedsToAdded;
      ExcuteSegmentOneStep();
	  i++;
	}
  }	  
  MakeSegmentBoundary();
}

template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
BeforeNextStep(void){ 
  m_WorkingVD->AddSeeds(m_NumberOfSeedsToAdded, m_SeedsToAdded.begin()); 
  m_LastStepSeeds = m_NumberOfSeeds; 
  m_NumberOfSeeds += m_NumberOfSeedsToAdded; 
} 


template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
MakeSegmentBoundary(void)
{

  RegionType region = m_InputImage->GetRequestedRegion(); 
  itk::SimpleImageRegionIterator <OutputImageType> oit(m_OutputImage, region); 
  oit.Begin(); 
  while( !oit.IsAtEnd()) {     
  oit.Set(0); 
  ++oit; 
  }

  NeighborIdIterator nit;
  NeighborIdIterator nitend;
  for(int i=0;i<m_NumberOfSeeds;i++){
    if(m_Label[i] == 2){
      nitend = m_WorkingVD->NeighborIdsEnd(i);
	    for(nit=m_WorkingVD->NeighborIdsBegin(i);nit!=nitend;++nit){
	      if(((*nit)>i)&&(m_Label[*nit]==2)){
		      drawLine(m_WorkingVD->getSeed(i),m_WorkingVD->getSeed(*nit));
		      i=i;
	      }
	    }
    }
  }
}

template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
Reset(void)
{
  m_WorkingVD->SetRandomSeeds(m_NumberOfSeeds);
  m_StepsRuned = 0;
  m_LastStepSeeds=m_NumberOfSeeds;
  m_NumberOfSeedsToAdded=0;
}

template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
MakeSegmentObject(void)
{
  RegionType region = m_InputImage->GetRequestedRegion(); 
  itk::SimpleImageRegionIterator <OutputImageType> oit(m_OutputImage, region); 
  oit.Begin(); 
  while( !oit.IsAtEnd()) {     
  oit.Set(0); 
  ++oit; 
  }
  CellPointer currCell; 
  PointIdIterator currPit;
  PointIdIterator currPitEnd;
  PointType currP;
  PointTypeDeque VertList;
  for(int i=0;i<m_NumberOfSeeds;i++){
    if(m_Label[i] == 1){
      currCell = m_WorkingVD->GetCellId(i);
      currPitEnd = currCell->PointIdsEnd();
	  VertList.clear();
	  for(currPit=currCell->PointIdsBegin();currPit!=currPitEnd;++currPit){
	    m_WorkingVD->GetPointId((*currPit),&(currP));
	    VertList.push_back(currP);
	  }
	  FillPolygon(VertList);
    }
  }
}

template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
FillPolygon(PointTypeDeque vertlist)
{
  IndexType idx;

  PointType currP;
  PointType leftP;
  PointType rightP;
  currP = vertlist.front();
  vertlist.pop_front();
  leftP = vertlist.front();
  while(currP[1] > leftP[1]){
    vertlist.push_back(currP);
    currP=vertlist.front();
    vertlist.pop_front();
    leftP=vertlist.front();
  }
  rightP=vertlist.back();
  while(currP[1] > rightP[1]){
    vertlist.push_front(currP);
    currP=vertlist.back();
    vertlist.pop_back();
    rightP=vertlist.back();
  }
  leftP=vertlist.front();
  PointTypeDeque tmpQ; 
  tmpQ.clear();
  if(leftP[0]>rightP[0]){
    while(!(vertlist.empty())){
      tmpQ.push_back(vertlist.front());
      vertlist.pop_front(); 
    }
    while(!(tmpQ.empty())){
      vertlist.push_front(tmpQ.front());
      tmpQ.pop_front();
    }
  } 
  tmpQ.clear();
  leftP=vertlist.front();
  rightP=vertlist.back();

  double beginy=currP[1];
  int intbeginy=(int)ceil(beginy); 
  idx[1]=intbeginy;
  double leftendy=leftP[1];
  double rightendy=rightP[1];
  double beginx=currP[0];
  double endx=currP[0];
  double leftDx,rightDx;
  double offset;
  double leftheadx=beginx;
  double rightheadx=endx;
  double leftheady=beginy;
  double rightheady=beginy;

  double endy;
  bool RorL;
  int i,j;
  if(leftendy>rightendy){
    RorL=1;
    endy=rightendy;
  }
  else{
    RorL=0;
    endy=leftendy;
  }
  leftDx=(leftP[0]-beginx)/(leftP[1]-beginy);
  rightDx=(rightP[0]-endx)/(rightP[1]-beginy);
  int intendy=(int)floor(endy);
  if(intbeginy>intendy){ //no scanline
    if(RorL){
      endx=rightP[0];
      beginx+=leftDx*(rightP[1]-beginy);
      beginy=rightP[1];
    }
    else{
      beginx=leftP[0];
      endx+=rightDx*(leftP[1]-beginy); 
      beginy=leftP[1];
    }
  }
  else if((intbeginy==intendy) && (intbeginy==0)){ //only one scanline at 0;
    if(RorL) endx=rightP[0];
    else beginx=leftP[0];
    for(i=ceil(beginx);i<=floor(endx);i++){
      idx[0]=i;
      m_OutputImage->SetPixel(idx,1);  
    }
    idx[1]=idx[1]+1;
  }
  else{ //normal case some scanlines
    offset=(double)intbeginy-beginy;
    endx+=offset*rightDx;
    beginx+=offset*leftDx;
    while(idx[1]<=intendy){
      for(i=ceil(beginx);i<=floor(endx);i++){
        idx[0]=i;
        m_OutputImage->SetPixel(idx,1);  
      }
      endx+=rightDx;
      beginx+=leftDx;        
      idx[1]=idx[1]+1;
    }
    beginy=endy;
  }

  int vsize=vertlist.size();
  while(vsize>2){
    vsize--;
    if(RorL){
      vertlist.pop_back();
      currP=rightP;
      rightheadx=currP[0];
      rightheady=currP[1];
      endx=currP[0];
      beginx=leftheadx+leftDx*(beginy-leftheady); 
      rightP=vertlist.back();
      rightDx=(rightP[0]-currP[0])/(rightP[1]-currP[1]);
    }
    else{
      vertlist.pop_front();
      currP=leftP;
      leftheadx=currP[0];
      leftheady=currP[1];
      beginx=currP[0];
      endx=rightheadx+rightDx*(beginy-rightheady);
      leftP=vertlist.front();
      leftDx=(leftP[0]-currP[0])/(leftP[1]-currP[1]);
    }
        
    leftendy=leftP[1];
    rightendy=rightP[1];
    if(leftendy>rightendy){
      RorL=1;
      endy=rightendy;
    }
    else{
      RorL=0;
      endy=leftendy;
    }

    intendy=(int)floor(endy);
    intbeginy=(int)ceil(beginy); 

    if(intbeginy>intendy){ //no scanline
      if(RorL){
        endx=rightP[0];
        beginx+=leftDx*(rightP[1]-beginy);
        beginy=rightP[1];
      }
      else{
        beginx=leftP[0];
        endx+=rightDx*(leftP[1]-beginy); 
        beginy=leftP[1];
      }
    }
    else{ //normal case some scanlines
      offset=(double)intbeginy-beginy;
      endx+=offset*rightDx;
      beginx+=offset*leftDx;
      while(idx[1]<=intendy){
        for(i=ceil(beginx);i<=floor(endx);i++){
          idx[0]=i;
          m_OutputImage->SetPixel(idx,1);  
        }
        endx+=rightDx;
        beginx+=leftDx;        
        idx[1]=idx[1]+1;
      }
      beginy=idx[1];
    }
  }


  if(RorL){
    beginy=rightP[1];
    endy=leftP[1];
  }
  else{
    beginy=leftP[1];
    endy=rightP[1];
  }
  intbeginy=(int)ceil(beginy);
  intendy=(int)floor(endy);
  if(intbeginy<=intendy){
    if(RorL){
      rightDx=(rightP[0]-leftP[0])/(rightP[1]-leftP[1]);
      endx=rightP[0];
      beginx=leftP[0]+leftDx*(rightP[1]-leftP[1]);
    }
    else{
      leftDx=(rightP[0]-leftP[0])/(rightP[1]-leftP[1]);
      beginx=leftP[0];
      endx=rightP[0]+rightDx*(leftP[1]-rightP[1]);
    }
    offset=(double)intbeginy-beginy;
    beginx+=offset*leftDx;
    endx+=offset*rightDx;
    while(idx[1]<=intendy){
      for(i=ceil(beginx);i<=floor(endx);i++){
        idx[0]=i;
        m_OutputImage->SetPixel(idx,1);  
      }
      endx+=rightDx;
      beginx+=leftDx;        
      idx[1]=idx[1]+1;
    }
  }

}

template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
drawLine(PointType p1,PointType p2){
  int x1=(int)(p1[0]+0.5);
  int x2=(int)(p2[0]+0.5);
  int y1=(int)(p1[1]+0.5);
  int y2=(int)(p2[1]+0.5);
  if(x1==m_Size[0]) x1--;  
  if(x2==m_Size[0]) x2--;  
  if(y1==m_Size[1]) y1--;  
  if(y2==m_Size[1]) y2--;  


  int dx=x1-x2;
  int adx=(dx>0)?dx:-dx;
  int dy=y1-y2;
  int ady=(dy>0)?dy:-dy;
  int save;
  float curr;
  IndexType idx;
  if (adx > ady){
    if(x1>x2){
	  save=x1; x1=x2; x2=save;
	  save=y1; y1=y2; y2=save;
	}
    curr=(float)y1;
    float offset=(float)dy/dx;
	for(int i=x1;i<=x2;i++){
	  idx[0]=i;
	  idx[1]=y1;
	  m_OutputImage->SetPixel(idx,1);
	  curr += offset;
  	  y1=(int)(curr+0.5);
    }
  }
  else { 
    if(y1>y2){
	  save=x1; x1=x2; x2=save;
	  save=y1; y1=y2; y2=save;
	}
    curr=(float)x1;
    float offset=(float)dx/dy;
	for(int i=y1;i<=y2;i++){
	  idx[0]=x1;
	  idx[1]=i;
	  m_OutputImage->SetPixel(idx,1);
	  curr += offset;
	  x1=(int)(curr+0.5);
    }
  }

}

template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
TakeAPrior(BinaryObjectImage* aprior)
{
  RegionType region = m_InputImage->GetRequestedRegion();
  itk::SimpleImageRegionIterator <BinaryObjectImage> ait(aprior, region);
  itk::SimpleImageRegionIterator <RGBHCVImage> iit(m_WorkingImage, region);
  ait.Begin();
  iit.Begin();


  int i,j;
  int minx,miny,maxx,maxy;
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

  ait.Begin();
  iit.Begin();
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
    tmp1[k]=tmp[5-j];
  }
}


template <class TInputImage, class TOutputImage> 
void 
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>:: 
DrawDiagram(VDImagePointer result,unsigned char incolor, 
    unsigned char outcolor,unsigned char boundcolor) 
{ 
  
  RegionType region = m_InputImage->GetRequestedRegion(); 
  itk::SimpleImageRegionIterator <VDImage> vdit(result, region); 
  vdit.Begin(); 
  while( !vdit.IsAtEnd()) {     
    vdit.Set(0); 
    ++vdit; 
  } 
      
  EdgeIterator eit; 
  EdgeIterator eitend = m_WorkingVD->EdgeEnd();   
  PointType adds; 
  Point<int,2> seeds; 
  for(eit = m_WorkingVD->EdgeBegin();eit != eitend; ++eit){ 
    seeds = m_WorkingVD->GetSeedsIDAroundEdge(eit); 
    if((m_Label[seeds[0]]==2)||(m_Label[seeds[1]]==2)){ 
      drawVDline(result,eit->m_left,eit->m_right,boundcolor); 
    } 
    else if(m_Label[seeds[0]]){ 
      drawVDline(result,eit->m_left,eit->m_right,incolor); 
    } 
    else { 
      drawVDline(result,eit->m_left,eit->m_right,outcolor); 
    } 
  } 
    
} 
    
template <class TInputImage, class TOutputImage> 
void 
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>:: 
drawVDline(VDImagePointer result,PointType p1,PointType p2, unsigned char color) 
{ 
  int x1=(int)(p1[0]+0.5); 
  int x2=(int)(p2[0]+0.5); 
  int y1=(int)(p1[1]+0.5); 
  int y2=(int)(p2[1]+0.5); 
  if(x1==m_Size[0]) x1--;  
  if(x2==m_Size[0]) x2--;  
  if(y1==m_Size[1]) y1--;  
  if(y2==m_Size[1]) y2--; 
  int dx=x1-x2; 
  int adx=(dx>0)?dx:-dx; 
  int dy=y1-y2; 
  int ady=(dy>0)?dy:-dy; 
  int save; 
  float curr; 
  IndexType idx; 
  if (adx > ady){ 
    if(x1>x2){ 
      save=x1; x1=x2; x2=save; 
      save=y1; y1=y2; y2=save; 
    } 
    curr=(float)y1; 
    float offset=(float)dy/dx; 
    for(int i=x1;i<=x2;i++){ 
      idx[0]=i; 
      idx[1]=y1; 
      result->SetPixel(idx,color); 
      curr += offset; 
      y1=(int)(curr+0.5); 
    } 
  } 
  else {  
    if(y1>y2){ 
      save=x1; x1=x2; x2=save; 
      save=y1; y1=y2; y2=save; 
    } 
    curr=(float)x1; 
    float offset=(float)dx/dy; 
    for(int i=y1;i<=y2;i++){ 
      idx[0]=x1; 
      idx[1]=i; 
      result->SetPixel(idx,color); 
      curr += offset; 
      x1=(int)(curr+0.5); 
    } 
  } 
} 

} //end namespace

#endif


