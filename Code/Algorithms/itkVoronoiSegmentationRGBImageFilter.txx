/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiSegmentationRGBImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVoronoiSegmentationRGBImageFilter_txx
#define _itkVoronoiSegmentationRGBImageFilter_txx
#include "itkVoronoiSegmentationRGBImageFilter.h"

#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include <math.h>

namespace itk
{

/* Constructor: setting of the default values for the parameters. */
template <class TInputImage, class TOutputImage>
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
VoronoiSegmentationRGBImageFilter(){
  unsigned int i;
  for(i=0;i<6;i++){
    m_Mean[i] = 0;
    m_STD[i] = 0;
    m_MeanTolerance[i] = 10;
    m_STDTolerance[i] = 10;
    m_MeanPercentError[i] = 0.10;
    m_STDPercentError[i] = 1.5;
  }
//testing RGB for both mean and STD. (default).
  m_TestMean[0] = 0;
  m_TestMean[1] = 1;
  m_TestMean[2] = 2;
  m_TestSTD[0] = 0;
  m_TestSTD[1] = 1;
  m_TestSTD[2] = 2;
  m_MaxValueOfRGB = 256;
}

/* Destructor. */
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
    m_MeanTolerance[i] = fabs(x[i]*m_Mean[i]);
  }
}

template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
SetSTDPercentError(double x[6]){
  for(unsigned int i=0;i<6;i++){
    m_STDPercentError[i] = x[i];
    m_STDTolerance[i] = x[i]*m_STD[i];
  }
}


/* Initialization for the segmentation. */
template <class TInputImage, class TOutputImage>
void
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>
::SetInput(const InputImageType *input)
{
//  m_InputImage = this->GetInput();
//  m_OutputImage = this->GetOutput(); 

  Superclass::SetInput(input);

  m_Size = this->GetInput()->GetLargestPossibleRegion().GetSize();
  IndexType index;
  index.Fill(0);
  RegionType region;
  region.SetSize(m_Size);
  region.SetIndex(index);

  m_WorkingImage=RGBHCVImage::New();
  m_WorkingImage->SetLargestPossibleRegion( region );
  m_WorkingImage->SetBufferedRegion( region );
  m_WorkingImage->SetRequestedRegion( region );
  m_WorkingImage->Allocate();  

  itk::ImageRegionIteratorWithIndex <RGBHCVImage> wit(m_WorkingImage, region);
  itk::ImageRegionConstIteratorWithIndex <InputImageType> iit(this->GetInput(), region);
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
bool
VoronoiSegmentationRGBImageFilter <TInputImage,TOutputImage>::
TestHomogeneity(IndexList &Plist)
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

  double savemean[6],saveSTD[6];
  if(num > 1){
    for(i=0;i<6;i++){
      savemean[i] = addp[i]/num;
      saveSTD[i] = sqrt((addpp[i] - (addp[i]*addp[i])/(num) )/(num-1));
  }
  }
  else{
    for(i=0;i<6;i++){
      savemean[i] = 0;
      saveSTD[i] = -1;
  }
  }


  bool ok = 1;
  j = 0;
  double savem,savev;
  while (ok && (j < 3)){
  savem = savemean[m_TestMean[j]] - m_Mean[m_TestMean[j]];
  savev = saveSTD[m_TestSTD[j]] - m_STD[m_TestSTD[j]];
  if( (savem < -m_MeanTolerance[m_TestMean[j]]) ||
        (savem > m_MeanTolerance[m_TestMean[j]]) ){
    ok = 0;
  }
  if( (savev < -m_STDTolerance[m_TestSTD[j]]) ||
      (savev > m_STDTolerance[m_TestSTD[j]]) ){
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
TakeAPrior(BinaryObjectImage* aprior)
{

  RegionType region = this->GetInput()->GetRequestedRegion();
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
  double b_STD[6];
  float diffMean[6];
  float diffSTD[6];
  for(i=0;i<6;i++){
    m_Mean[i] = objaddp[i]/objnum;
    m_STD[i] = sqrt((objaddpp[i] - (objaddp[i]*objaddp[i])/objnum)/(objnum-1));
    m_STDTolerance[i] = m_STD[i]*m_STDPercentError[i];
    b_Mean[i] = bkgaddp[i]/bkgnum;
    b_STD[i] = sqrt((bkgaddpp[i] - (bkgaddp[i]*bkgaddp[i])/bkgnum)/(bkgnum-1));
  diffMean[i] = (b_Mean[i]-m_Mean[i])/m_Mean[i];
  if(diffMean[i] < 0) diffMean[i] = -diffMean[i];
  diffSTD[i] = (b_STD[i]-m_STD[i])/m_STD[i];
  if(diffSTD[i] < 0) diffSTD[i] = -diffSTD[i];
    if(m_UseBackgroundInAPrior)
      m_MeanTolerance[i] = diffMean[i]*m_Mean[i]*m_MeanDeviation;
    else
      m_MeanTolerance[i] = fabs(m_Mean[i]*m_MeanPercentError[i]);
  }

  if(objnum<10){ 
/* a-prior doen's make too much sense */
    for(i=0;i<6;i++){
      m_MeanTolerance[i] = 0;
      m_STDTolerance[i] = 0;
    } 
  }

/*  Sorting. */
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
      if(diffSTD[tmp1[i]]>diffSTD[tmp1[k]]){
      k=i;
    }   
    }
  m_TestSTD[j]=tmp1[k];
    tmp1[k]=tmp1[5-j];
  }
}

} //end namespace

#endif


