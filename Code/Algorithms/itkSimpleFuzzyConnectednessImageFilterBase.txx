/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleFuzzyConnectednessImageFilterBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSimpleFuzzyConnectednessImageFilterBase_txx
#define __itkSimpleFuzzyConnectednessImageFilterBase_txx
#include "itkSimpleFuzzyConnectednessImageFilterBase.h"

#include "vnl/vnl_math.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkNumericTraits.h"

namespace itk{


template <class TInputImage, class TOutputImage>
SimpleFuzzyConnectednessImageFilterBase<TInputImage,TOutputImage>
::SimpleFuzzyConnectednessImageFilterBase()
{
  m_Threshold = 1.0;
  m_ObjectsSeed.Fill(0);
  m_Weight = 1.0;
}

template <class TInputImage, class TOutputImage>
SimpleFuzzyConnectednessImageFilterBase<TInputImage,TOutputImage>
::~SimpleFuzzyConnectednessImageFilterBase()
{
}


template <class TInputImage, class TOutputImage>
void 
SimpleFuzzyConnectednessImageFilterBase<TInputImage,TOutputImage>
::SetObjectsSeed(const IndexType &seed)
{
  m_ObjectsSeed = seed;
}

template <class TInputImage, class TOutputImage>
void 
SimpleFuzzyConnectednessImageFilterBase<TInputImage,TOutputImage>
::PushNeighbors(const IndexType &center)
{
  IndexType current=center;
    
  for(unsigned int i = 0; i < ImageDimension; i++)
    {
    if(current[i] < static_cast<typename IndexType::IndexValueType>(m_Size[i])-1)
      {
      current[i]++;
      m_Queue.push(current);
      current[i]--;
      }
  
    if(current[i]>0)
      {
      current[i]--;
      m_Queue.push(current);
      current[i]++;
      }
    }
}

template <class TInputImage, class TOutputImage>
double 
SimpleFuzzyConnectednessImageFilterBase<TInputImage,TOutputImage>
::FindStrongPath(const IndexType &center)
{
  IndexType current = center;
  double tmp=0;
  double tmp1;
  double tmp2;

  PixelType centerpixel = m_InputImage->GetPixel(current);

  if(current[0] > 0)
    {
    current[0]--;
    tmp = (double)(m_FuzzyScene->GetPixel(current));
    tmp1 = FuzzyAffinity(m_InputImage->GetPixel(current),centerpixel);
    if(tmp > tmp1)
      {
      tmp=tmp1;
      }
    current[0]++;
    }
  if(current[0] < static_cast<typename IndexType::IndexValueType>(m_Size[0])-1)
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

  
  for(unsigned int i = 1;i < ImageDimension; i++)
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
    if(current[i] < static_cast<typename IndexType::IndexValueType>(m_Size[i])-1)
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

template <class TInputImage, class TOutputImage>
void 
SimpleFuzzyConnectednessImageFilterBase<TInputImage,TOutputImage>
::MakeSegmentObject()
{
  RegionType regionOUT = this->m_SegmentObject->GetRequestedRegion();
  typename UShortImage::RegionType regionIN = this->m_FuzzyScene->GetRequestedRegion();
  
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


template <class TInputImage, class TOutputImage>
void 
SimpleFuzzyConnectednessImageFilterBase<TInputImage,TOutputImage>
::GenerateData()
{
  IndexType current;
  unsigned short fmax;
      
  m_InputImage = this->GetInput();
  m_SegmentObject = this->GetOutput(); 

  m_Size = m_InputImage->GetLargestPossibleRegion().GetSize();
  IndexType index;
  index.Fill(0);
  typename UShortImage::RegionType region;
  region.SetSize(m_Size);
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
  region1.SetSize(m_Size);
  region1.SetIndex(index);
  m_SegmentObject->SetLargestPossibleRegion( region1 );
  m_SegmentObject->SetBufferedRegion( region1 );
  m_SegmentObject->SetRequestedRegion( region1 );
  m_SegmentObject->Allocate();  

  PushNeighbors(m_ObjectsSeed);
  m_FuzzyScene->SetPixel(m_ObjectsSeed,NumericTraits<unsigned short>::max());

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
SimpleFuzzyConnectednessImageFilterBase<TInputImage,TOutputImage>
::UpdateThreshold(const double x){
  SetThreshold(x);
  MakeSegmentObject();
}

template <class TInputImage, class TOutputImage>
void 
SimpleFuzzyConnectednessImageFilterBase<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Weight: " << m_Weight << std::endl;
  os << indent << "Threshold: " << m_Threshold << std::endl;
}

} /* end namespace itk. */

#endif





  




