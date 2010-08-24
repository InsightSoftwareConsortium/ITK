/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleFuzzyConnectednessImageFilterBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
#include "itkProgressReporter.h"

namespace itk
{
template< class TInputImage, class TOutputImage >
SimpleFuzzyConnectednessImageFilterBase< TInputImage, TOutputImage >
::SimpleFuzzyConnectednessImageFilterBase()
{
  m_Threshold = 1.0;
  m_ObjectSeed.Fill(0);
  m_Weight = 1.0;
  m_InsideValue  = NumericTraits< OutputPixelType >::max();
  m_OutsideValue = NumericTraits< OutputPixelType >::min();
}

template< class TInputImage, class TOutputImage >
SimpleFuzzyConnectednessImageFilterBase< TInputImage, TOutputImage >
::~SimpleFuzzyConnectednessImageFilterBase()
{}

template< class TInputImage, class TOutputImage >
void
SimpleFuzzyConnectednessImageFilterBase< TInputImage, TOutputImage >
::PushNeighbors(const IndexType & center)
{
  IndexType currentIndex = center;

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( currentIndex[i] < static_cast< typename IndexType::IndexValueType >( m_Size[i] ) - 1 )
      {
      currentIndex[i]++;
      m_Queue.push(currentIndex);
      currentIndex[i]--;
      }

    if ( currentIndex[i] > 0 )
      {
      currentIndex[i]--;
      m_Queue.push(currentIndex);
      currentIndex[i]++;
      }
    }
}

template< class TInputImage, class TOutputImage >
double
SimpleFuzzyConnectednessImageFilterBase< TInputImage, TOutputImage >
::FindStrongPath(const IndexType & center)
{
  IndexType currentIndex = center;
  double    tmp = 0;
  double    tmp1;
  double    tmp2;

  PixelType centerpixel = m_InputImage->GetPixel(currentIndex);

  if ( currentIndex[0] > 0 )
    {
    currentIndex[0]--;
    tmp = (double)( m_FuzzyScene->GetPixel(currentIndex) );
    tmp1 = this->FuzzyAffinity(m_InputImage->GetPixel(currentIndex), centerpixel);
    if ( tmp > tmp1 )
      {
      tmp = tmp1;
      }
    currentIndex[0]++;
    }
  if ( currentIndex[0] < static_cast< typename IndexType::IndexValueType >( m_Size[0] ) - 1 )
    {
    currentIndex[0]++;
    tmp2 = (double)( m_FuzzyScene->GetPixel(currentIndex) );
    tmp1 = this->FuzzyAffinity(m_InputImage->GetPixel(currentIndex), centerpixel);
    if ( tmp2 > tmp1 )
      {
      tmp2 = tmp1;
      }
    if ( tmp < tmp2 )
      {
      tmp = tmp2;
      }
    currentIndex[0]--;
    }

  for ( unsigned int i = 1; i < ImageDimension; i++ )
    {
    if ( currentIndex[i] > 0 )
      {
      currentIndex[i]--;
      tmp2 = (double)( m_FuzzyScene->GetPixel(currentIndex) );
      tmp1 = this->FuzzyAffinity(m_InputImage->GetPixel(currentIndex), centerpixel);
      if ( tmp2 > tmp1 )
        {
        tmp2 = tmp1;
        }
      if ( tmp < tmp2 )
        {
        tmp = tmp2;
        }
      currentIndex[i]++;
      }
    if ( currentIndex[i] < static_cast< typename IndexType::IndexValueType >( m_Size[i] ) - 1 )
      {
      currentIndex[i]++;
      tmp2 = (double)( m_FuzzyScene->GetPixel(currentIndex) );
      tmp1 = this->FuzzyAffinity(m_InputImage->GetPixel(currentIndex), centerpixel);
      if ( tmp2 > tmp1 )
        {
        tmp2 = tmp1;
        }
      if ( tmp < tmp2 )
        {
        tmp = tmp2;
        }
      currentIndex[i]--;
      }
    }

  return ( tmp );
}

template< class TInputImage, class TOutputImage >
void
SimpleFuzzyConnectednessImageFilterBase< TInputImage, TOutputImage >
::MakeSegmentObject()
{
  RegionType regionOUT = m_SegmentObject->GetRequestedRegion();

  typename FuzzySceneType::RegionType regionIN = m_FuzzyScene->GetRequestedRegion();

  const double activeThreshold = ( NumericTraits< unsigned short >::max() ) * m_Threshold;

  ImageRegionIteratorWithIndex< FuzzySceneType  > it(m_FuzzyScene,    regionIN);
  ImageRegionIteratorWithIndex< OutputImageType > ot(m_SegmentObject, regionOUT);

  while ( !it.IsAtEnd() )
    {
    if ( it.Get() > activeThreshold )
      {
      ot.Set(m_InsideValue);
      }
    else
      {
      ot.Set(m_OutsideValue);
      }
    ++it;
    ++ot;
    }
}

template< class TInputImage, class TOutputImage >
void
SimpleFuzzyConnectednessImageFilterBase< TInputImage, TOutputImage >
::GenerateData()
{
  IndexType      currentIndex;
  unsigned short fmax;

  m_InputImage = this->GetInput();
  m_SegmentObject = this->GetOutput();

  m_Size = m_InputImage->GetLargestPossibleRegion().GetSize();
  IndexType index;
  index.Fill(0);
  typename FuzzySceneType::RegionType region;
  region.SetSize(m_Size);
  region.SetIndex(index);
  m_FuzzyScene = FuzzySceneType::New();
  m_FuzzyScene->SetRegions(region);
  m_FuzzyScene->Allocate();
  m_FuzzyScene->FillBuffer(0);

  m_FuzzyScene->CopyInformation(m_InputImage);

  RegionType region1;
  region1.SetSize(m_Size);
  region1.SetIndex(index);
  m_SegmentObject->SetRegions(region1);
  m_SegmentObject->Allocate();

  this->PushNeighbors(m_ObjectSeed);
  m_FuzzyScene->SetPixel( m_ObjectSeed, NumericTraits< unsigned short >::max() );

  ProgressReporter progress( this, 0, region.GetNumberOfPixels() * 2 * m_InputImage->GetImageDimension() );

  while ( !m_Queue.empty() )
    {
    currentIndex = m_Queue.front();
    m_Queue.pop();
    fmax = static_cast< unsigned short >( this->FindStrongPath(currentIndex) );
    if ( fmax > m_FuzzyScene->GetPixel(currentIndex) )
      {
      m_FuzzyScene->SetPixel(currentIndex, fmax);
      this->PushNeighbors(currentIndex);
      }
    progress.CompletedPixel();  // potential exception thrown here
    }

  this->MakeSegmentObject();
}

template< class TInputImage, class TOutputImage >
void
SimpleFuzzyConnectednessImageFilterBase< TInputImage, TOutputImage >
::UpdateThreshold(const double x)
{
  this->SetThreshold(x);
  this->MakeSegmentObject();
}

template< class TInputImage, class TOutputImage >
void
SimpleFuzzyConnectednessImageFilterBase< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Weight: " << m_Weight << std::endl;
  os << indent << "Threshold: " << m_Threshold << std::endl;
  os << indent << "Inside  value: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_InsideValue )
     << std::endl;
  os << indent << "Outside value: "
     << static_cast< typename NumericTraits< OutputPixelType >::PrintType >( m_OutsideValue )
     << std::endl;
  os << indent << "Object seed : " << m_ObjectSeed << std::endl;
}
} /* end namespace itk. */

#endif
