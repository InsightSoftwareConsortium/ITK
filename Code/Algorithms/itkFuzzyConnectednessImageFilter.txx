/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFuzzyConnectednessImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkImageRegionSimpleIterator.h"

namespace itk{

const int MAXUSHORT = 65535;

/**
 *
 */
template <class TInputImage, class TOutputImage>
FuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::FuzzyConnectednessImageFilter()
{
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
FuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::~FuzzyConnectednessImageFilter()
{
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
FuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::SetParameters
(const double inmean,const double invar,const double indifmean,
 const double indifvar, const double inweight)
{
  m_Mean = inmean;
  m_Var = invar;
  m_Diff_Mean = indifmean;
  m_Diff_Var = indifvar;

  if(inweight < 0)
  {
    m_Weight = 0;
  }
  else if(inweight > 1)
  {
	m_Weight = 1;
  }
  else 
  {
	m_Weight = inweight;
  }
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
FuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::SetSeed(const IndexType &seed)
{
  m_Seed = seed;
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
FuzzyConnectednessImageFilter<TInputImage,TOutputImage>
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
FuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::FuzzyAffinity(const double f1,const double f2)
{
  double tmp1 = (0.5 * (f1 + f2) - m_Mean) / m_Var;
  if(m_Weight == 1)
  {
    return(MAXUSHORT * (exp(-0.5 * tmp1 * tmp1)));
  }
  else{
    double tmp2 = (fabs(f1 - f2) - m_Diff_Mean) / m_Diff_Var;
	return(MAXUSHORT *
	  (m_Weight * exp(-0.5 * tmp1 * tmp1) + (1 - m_Weight) * exp(-0.5 * tmp2 * tmp2)));
	}
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
double 
FuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::FindStrongPath(const IndexType &center)
{
  IndexType current = center;
  double tmp;
  double tmp1;
  double tmp2;

  double centerpixel = (double)(m_InputImage->GetPixel(current));

  if(current[0] > 0)
  {
    current[0]--;
    tmp = (double)(m_FuzzyScene->GetPixel(current));
    tmp2 = (double)(m_InputImage->GetPixel(current));
    tmp1 = FuzzyAffinity(tmp2,centerpixel);
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
    tmp1 = FuzzyAffinity(
      (double)(m_InputImage->GetPixel(current)), centerpixel);
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
      tmp1 = FuzzyAffinity(
       (double)(m_InputImage->GetPixel(current)), centerpixel);
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
      tmp1 = FuzzyAffinity(
       (double)(m_InputImage->GetPixel(current)), centerpixel);
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
FuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::MakeSegmentObject()
{
  RegionType regionOUT = this->m_SegmentObject->GetRequestedRegion();
  UShortImage::RegionType regionIN = this->m_FuzzyScene->GetRequestedRegion();
  
  double activeThreshold = MAXUSHORT * m_Threshold;


  ImageRegionSimpleIterator <UShortImage> it(this->m_FuzzyScene, regionIN);
  ImageRegionSimpleIterator <OutputImageType> ot(this->m_SegmentObject, regionOUT);


  it.Begin();
  ot.Begin();

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
FuzzyConnectednessImageFilter<TInputImage,TOutputImage>
::ExcuteSegment()
{
  IndexType current;
  unsigned short fmax;

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

  ImageRegionSimpleIterator <UShortImage> it(this->m_FuzzyScene, region);

  it.Begin();

  while( !it.IsAtEnd())
  {    
    it.Set(0);
	++it;
  }

  OutputImageType::RegionType region1;
  region1.SetSize(m_size);
  region1.SetIndex(index);
  m_SegmentObject->SetLargestPossibleRegion( region1 );
  m_SegmentObject->SetBufferedRegion( region1 );
  m_SegmentObject->SetRequestedRegion( region1 );
  m_SegmentObject->Allocate();  

  PushNeighbors(m_Seed);
  m_FuzzyScene->SetPixel(m_Seed,MAXUSHORT);

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

} // end namespace itk





	




