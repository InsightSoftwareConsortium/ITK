/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiSegmentationImageFilter.txx
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
#ifndef _itkVoronoiSegmentationImageFilter_txx
#define _itkVoronoiSegmentationImageFilter_txx

#include "itkImageRegionIteratorWithIndex.h"


namespace itk
{

/* constructor: seting the default value of the parameters */
template <class TInputImage, class TOutputImage>
VoronoiSegmentationImageFilter <TInputImage,TOutputImage>::
VoronoiSegmentationImageFilter(){
  m_MeanPercentError = 0.10;
  m_VarPercentError = 1.5;
}

/* destructor */
template <class TInputImage, class TOutputImage>
VoronoiSegmentationImageFilter <TInputImage,TOutputImage>::
~VoronoiSegmentationImageFilter(){
}

template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationImageFilter <TInputImage,TOutputImage>::
SetMeanPercentError(double x){
  m_MeanPercentError = x;
  m_MeanTolerance = x*m_Mean;
}


template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationImageFilter <TInputImage,TOutputImage>::
SetVarPercentError(double x){
  m_VarPercentError = x;
  m_VarTolerance = x*m_Var;
}

template <class TInputImage, class TOutputImage>
bool
VoronoiSegmentationImageFilter <TInputImage,TOutputImage>::
TestHomogeneity(IndexList Plist)
{
  int num=Plist.size();
  int i;
  double getp;
  double addp=0;
  double addpp=0;
  for(i=0;i<num;i++){
    getp = (double)(m_InputImage->GetPixel(Plist[i]));
    addp=addp+getp;
    addpp=addpp+getp*getp;
  }

  double savemean,savevar;
  if(num > 1){
    savemean = addp/num;
    savevar = sqrt((addpp - (addp*addp)/(num) )/(num-1));
  }
  else{
    savemean = 0;
    savevar = -1;
  }

  savemean -= m_Mean;
  savevar -= m_Var;
  if( (savemean>-m_MeanTolerance) && (savemean<m_MeanTolerance) 
     && (savevar<m_VarTolerance) )
	return 1;
  else
	return 0;
}

/* initialization for the segmentation */
template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationImageFilter <TInputImage,TOutputImage>::
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
void
VoronoiSegmentationImageFilter <TInputImage,TOutputImage>::
TakeAPrior(BinaryObjectImage* aprior)
{
  RegionType region = m_InputImage->GetRequestedRegion();
  itk::ImageRegionIteratorWithIndex <BinaryObjectImage> ait(aprior, region);
  itk::ImageRegionIteratorWithIndex <InputImageType> iit(m_InputImage, region);

  int num=0;
  float addp=0;
  float addpp=0;
  float currp;

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
  
  float addb=0;
  float addbb=0;
  int numb=0;

  ait.GoToBegin();
  iit.GoToBegin();
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
      if(ait.Get()){
	    num++;
	    currp = (float)(iit.Get());
	    addp += currp;
	    addpp += currp*currp;
	  }
      else{
	    numb++;
	    currp = (float)(iit.Get());
	    addb += currp;
	    addbb += currp*currp;
	  }
      ++ait;++iit;
    }
    for(j=maxx+1;j<m_Size[0];j++){
      ++ait;
      ++iit;
    }
  }

  m_Mean = addp/num;
  m_Var = sqrt((addpp - (addp*addp)/num)/(num-1));
  float b_Mean = addb/numb;
  float b_Var = sqrt((addbb - (addb*addb)/numb)/(numb-1));
  if(m_UseBackgroundInAPrior)
    m_MeanTolerance = fabs(m_Mean-b_Mean)*m_MeanDeviation;
  else 
    m_MeanTolerance = m_Mean*m_MeanPercentError;
  m_VarTolerance = m_Var*m_VarPercentError;
}


template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationImageFilter <TInputImage,TOutputImage>::
Reset(void)
{
  m_VDGenerator->SetRandomSeeds(m_NumberOfSeeds);
  m_StepsRuned = 0;
  m_LastStepSeeds=m_NumberOfSeeds;
  m_NumberOfSeedsToAdded=0;
}

} //end namespace

#endif

