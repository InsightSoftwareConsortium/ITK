/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFuzzyConnectednessRGBImageFilter.txx
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
#ifndef __itkFuzzyConnectednessRGBImageFilter_txx
#define __itkFuzzyConnectednessRGBImageFilter_txx

#include "vnl/vnl_math.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkNumericTraits.h"

namespace itk{


/**
 *
 */
template <class TInputImage, class TOutputImage>
FuzzyConnectednessRGBImageFilter<TInputImage,TOutputImage>
::FuzzyConnectednessRGBImageFilter()
{
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
FuzzyConnectednessRGBImageFilter<TInputImage,TOutputImage>
::~FuzzyConnectednessRGBImageFilter()
{
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
FuzzyConnectednessRGBImageFilter<TInputImage,TOutputImage>
::SetSeed(const IndexType &seed)
{
  m_Seed = seed;
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
FuzzyConnectednessRGBImageFilter<TInputImage,TOutputImage>
::PushNeighbors(const IndexType &center)
{
  IndexType current=center;
    
  for(int i = 0; i < ImageDimension; i++)
  {
	if(current[i] < m_size[i]-1)
	{
      current[i]++;
	  m_Queue.push(current);
	  current[i]--;
    }
	
	if(current[i]>0){
	  current[i]--;
	  m_Queue.push(current);
      current[i]++;
    }
  }
}

/**
 *
 */

template <class TInputImage, class TOutputImage>
double 
FuzzyConnectednessRGBImageFilter<TInputImage,TOutputImage>
::FuzzyAffinity(const PixelRGB f1, const PixelRGB f2)
{
  double save[3];
  save[0] = 0.5 * (f1[0]+f2[0]) - m_Mean[0];
  save[1] = 0.5 * (f1[1]+f2[1]) - m_Mean[1];
  save[2] = 0.5 * (f1[2]+f2[2]) - m_Mean[2];

/* save some result to reduce repeating computation */ 
  double s00 = save[0]*save[0];
  double s01 = save[0]*save[1];
  double s02 = save[0]*save[2];
  double s11 = save[1]*save[1];
  double s12 = save[1]*save[2];
  double s22 = save[2]*save[2];

  double tmp1 = s00*(m_Var_inverse[0][0])
              + s11*(m_Var_inverse[1][1])
			  + s22*(m_Var_inverse[2][2])
			  + s01*(m_Var_inverse[0][1]+m_Var_inverse[1][0])
			  + s02*(m_Var_inverse[0][2]+m_Var_inverse[2][0])
			  + s12*(m_Var_inverse[1][2]+m_Var_inverse[2][1]);


  if(m_Weight == 1)
  {
    return( (NumericTraits<unsigned short>::max())*(exp(-0.5*tmp1)) );
  }
  else{
    save[0] = f1[0]-f2[0];
    save[1] = f1[1]-f2[1];
    save[2] = f1[2]-f2[2];
	if(save[0] < 0)
      save[0]=-save[0];
	if(save[1] < 0)
      save[1]=-save[1];
	if(save[2] < 0)
      save[2]=-save[2];
	save[0] = save[0] - m_Diff_Mean[0];
	save[1] = save[1] - m_Diff_Mean[1];
	save[2] = save[2] - m_Diff_Mean[2];

    s00 = save[0]*save[0];
    s01 = save[0]*save[1];
    s02 = save[0]*save[2];
    s11 = save[1]*save[1];
    s12 = save[1]*save[2];
    s22 = save[2]*save[2];

    double tmp3 = s00*(m_Diff_Var_inverse[0][0])
                + s11*(m_Diff_Var_inverse[1][1])
				+ s22*(m_Diff_Var_inverse[2][2])
			    + s01*(m_Diff_Var_inverse[0][1]+m_Diff_Var_inverse[1][0])
			    + s02*(m_Diff_Var_inverse[0][2]+m_Diff_Var_inverse[2][0])
			    + s12*(m_Diff_Var_inverse[1][2]+m_Diff_Var_inverse[2][1]);

	return( (NumericTraits<unsigned short>::max())*(m_Weight*exp(-0.5*tmp1)  
	                  +(1-m_Weight)*exp(-0.5*tmp3)) );
	}
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
double 
FuzzyConnectednessRGBImageFilter<TInputImage,TOutputImage>
::FindStrongPath(const IndexType &center)
{
  IndexType current = center;
  double tmp=0;
  double tmp1;
  double tmp2;

  PixelRGB centerpixel = m_InputImage->GetPixel(current);
  PixelRGB savepixel;

  if(current[0] > 0)
  {
    current[0]--;
    tmp = (double)(m_FuzzyScene->GetPixel(current));
    savepixel = m_InputImage->GetPixel(current);
    tmp1 = FuzzyAffinity(savepixel,centerpixel);
    if(tmp > tmp1)
	{
      tmp=tmp1;
    }
    current[0]++;
  }
  if(current[0] < m_size[0]-1)
  {
    current[0]++;  
    tmp2 = (double)(m_FuzzyScene->GetPixel(current));
    tmp1 = FuzzyAffinity(m_InputImage->GetPixel(current), centerpixel);
    if(tmp2 > tmp1)
	{
      tmp2 = tmp1;
    }
    if(tmp < tmp2){
      tmp = tmp2;
    }
    current[0]--;
  }

  
  for(int i = 1;i < ImageDimension; i++)
  {
    if(current[i] > 0)
	{
      current[i]--;
      tmp2 = (double)(m_FuzzyScene->GetPixel(current));
      tmp1 = FuzzyAffinity(m_InputImage->GetPixel(current), centerpixel);
      if(tmp2 > tmp1)
	  {
        tmp2 = tmp1;
      }
      if(tmp < tmp2)
	  {
        tmp = tmp2;
      }
      current[i]++;
	}
	if(current[i] < m_size[i]-1)
	{
	  current[i]++;
      tmp2 = (double)(m_FuzzyScene->GetPixel(current));
      tmp1 = FuzzyAffinity(m_InputImage->GetPixel(current), centerpixel);
      if(tmp2 > tmp1)
	  {
        tmp2 = tmp1;
      }
      if(tmp < tmp2)
	  {
        tmp = tmp2;
      }
      current[i]--;
	}
  }

  return(tmp);
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
FuzzyConnectednessRGBImageFilter<TInputImage,TOutputImage>
::MakeSegmentObject()
{
  RegionType regionOUT = this->m_SegmentObject->GetRequestedRegion();
  UShortImage::RegionType regionIN = this->m_FuzzyScene->GetRequestedRegion();
  
  double activeThreshold = (NumericTraits<unsigned short>::max()) * m_Threshold;


  ImageRegionIteratorWithIndex <UShortImage> it(this->m_FuzzyScene, regionIN);
  ImageRegionIteratorWithIndex <OutputImageType> ot(this->m_SegmentObject, regionOUT);

  while( !it.IsAtEnd())
    {    
    ot.Set(it.Get() > activeThreshold);
    ++it;
    ++ot;
    }
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
FuzzyConnectednessRGBImageFilter<TInputImage,TOutputImage>
::GenerateData()
{
  IndexType current;
  unsigned short fmax;

/* compute the Determinate and inverse of the Variance Matrices */
  m_Var_Det = m_Var[0][0]*m_Var[1][1]*m_Var[2][2]
             +m_Var[1][0]*m_Var[2][1]*m_Var[0][2]
			 +m_Var[0][1]*m_Var[1][2]*m_Var[2][0]
			 -m_Var[2][0]*m_Var[1][1]*m_Var[0][2]
			 -m_Var[0][1]*m_Var[1][0]*m_Var[2][2]
			 -m_Var[0][0]*m_Var[1][2]*m_Var[2][1];
  m_Var_inverse[0][0]=(m_Var[1][1]*m_Var[2][2]-m_Var[2][1]*m_Var[1][2])
                      /m_Var_Det;  
  m_Var_inverse[0][1]=-(m_Var[1][0]*m_Var[2][2]-m_Var[2][0]*m_Var[1][2])
                      /m_Var_Det;  
  m_Var_inverse[0][2]=(m_Var[1][0]*m_Var[2][1]-m_Var[2][0]*m_Var[1][1])
                      /m_Var_Det;  
  m_Var_inverse[1][0]=-(m_Var[0][1]*m_Var[2][2]-m_Var[2][1]*m_Var[0][2])
                      /m_Var_Det;  
  m_Var_inverse[1][1]=(m_Var[0][0]*m_Var[2][2]-m_Var[2][0]*m_Var[0][2])
                      /m_Var_Det;  
  m_Var_inverse[1][2]=-(m_Var[0][0]*m_Var[2][1]-m_Var[2][0]*m_Var[0][1])
                      /m_Var_Det;  
  m_Var_inverse[2][0]=(m_Var[0][1]*m_Var[1][2]-m_Var[1][1]*m_Var[0][2])
                      /m_Var_Det;  
  m_Var_inverse[2][1]=-(m_Var[0][0]*m_Var[1][2]-m_Var[1][0]*m_Var[0][2])
                      /m_Var_Det;  
  m_Var_inverse[2][2]=(m_Var[0][0]*m_Var[1][1]-m_Var[1][0]*m_Var[0][1])
                      /m_Var_Det;  
  if((int)(m_Weight*100+0.5) > 1){ //need to use the difference information.

  m_Diff_Var_Det = m_Diff_Var[0][0]*m_Diff_Var[1][1]*m_Diff_Var[2][2]
                  +m_Diff_Var[1][0]*m_Diff_Var[2][1]*m_Diff_Var[0][2]
			      +m_Diff_Var[0][1]*m_Diff_Var[1][2]*m_Diff_Var[2][0]
			      -m_Diff_Var[2][0]*m_Diff_Var[1][1]*m_Diff_Var[0][2]
			      -m_Diff_Var[0][1]*m_Diff_Var[1][0]*m_Diff_Var[2][2]
			      -m_Diff_Var[0][0]*m_Diff_Var[1][2]*m_Diff_Var[2][1];
  m_Diff_Var_inverse[0][0]=(m_Diff_Var[1][1]*m_Diff_Var[2][2]-m_Diff_Var[2][1]*m_Diff_Var[1][2])
                      /m_Diff_Var_Det;  
  m_Diff_Var_inverse[0][1]=-(m_Diff_Var[1][0]*m_Diff_Var[2][2]-m_Diff_Var[2][0]*m_Diff_Var[1][2])
                      /m_Diff_Var_Det;  
  m_Diff_Var_inverse[0][2]=(m_Diff_Var[1][0]*m_Diff_Var[2][1]-m_Diff_Var[2][0]*m_Diff_Var[1][1])
                      /m_Diff_Var_Det;  
  m_Diff_Var_inverse[1][0]=-(m_Diff_Var[0][1]*m_Diff_Var[2][2]-m_Diff_Var[2][1]*m_Diff_Var[0][2])
                      /m_Diff_Var_Det;  
  m_Diff_Var_inverse[1][1]=(m_Diff_Var[0][0]*m_Diff_Var[2][2]-m_Diff_Var[2][0]*m_Diff_Var[0][2])
                      /m_Diff_Var_Det;  
  m_Diff_Var_inverse[1][2]=-(m_Diff_Var[0][0]*m_Diff_Var[2][1]-m_Diff_Var[2][0]*m_Diff_Var[0][1])
                      /m_Diff_Var_Det;  
  m_Diff_Var_inverse[2][0]=(m_Diff_Var[0][1]*m_Diff_Var[1][2]-m_Diff_Var[1][1]*m_Diff_Var[0][2])
                      /m_Diff_Var_Det;  
  m_Diff_Var_inverse[2][1]=-(m_Diff_Var[0][0]*m_Diff_Var[1][2]-m_Diff_Var[1][0]*m_Diff_Var[0][2])
                      /m_Diff_Var_Det;  
  m_Diff_Var_inverse[2][2]=(m_Diff_Var[0][0]*m_Diff_Var[1][1]-m_Diff_Var[1][0]*m_Diff_Var[0][1])
                      /m_Diff_Var_Det;  
  }
            
  m_InputImage = this->GetInput();
  m_SegmentObject = this->GetOutput(); 

  m_size = m_InputImage->GetLargestPossibleRegion().GetSize();
  IndexType index = IndexType::ZeroIndex;
  UShortImage::RegionType region;
  region.SetSize(m_size);
  region.SetIndex(index);
  m_FuzzyScene = UShortImage::New();  
  m_FuzzyScene->SetLargestPossibleRegion( region );
  m_FuzzyScene->SetBufferedRegion( region );
  m_FuzzyScene->SetRequestedRegion( region );
  m_FuzzyScene->Allocate();  

  ImageRegionIteratorWithIndex <UShortImage> it(this->m_FuzzyScene, region);

  while( !it.IsAtEnd())
    {    
    it.Set(0);
    ++it;
    }

  RegionType region1;
  region1.SetSize(m_size);
  region1.SetIndex(index);
  m_SegmentObject->SetLargestPossibleRegion( region1 );
  m_SegmentObject->SetBufferedRegion( region1 );
  m_SegmentObject->SetRequestedRegion( region1 );
  m_SegmentObject->Allocate();  

  PushNeighbors(m_Seed);
  m_FuzzyScene->SetPixel(m_Seed,NumericTraits<unsigned short>::max());

  while(! m_Queue.empty())
  {
    current = m_Queue.front();
	m_Queue.pop();
	fmax = (unsigned short)(FindStrongPath(current));
	if(fmax > m_FuzzyScene->GetPixel(current))
	{
	  m_FuzzyScene->SetPixel(current,fmax);
	  PushNeighbors(current);
	}
  }

  MakeSegmentObject();
}

template <class TInputImage, class TOutputImage>
void 
FuzzyConnectednessRGBImageFilter<TInputImage,TOutputImage>
::UpdateThreshold(double x){
  SetThreshold(x);
  MakeSegmentObject();
}

} // end namespace itk

#endif





	




