/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMRASlabIdentifier.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMRASlabIdentifier_txx
#define __itkMRASlabIdentifier_txx

#include <algorithm>
#include <vector>
#include "itkMRASlabIdentifier.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"

namespace itk
{

template<class TInputImage>
MRASlabIdentifier<TInputImage>
::MRASlabIdentifier()
{
  m_Image = 0;
  m_NumberOfMinimumsPerSlice = 10 ;
  // default slicing axis is z
  m_SlicingDirection = 2 ;
}



template<class TInputImage>
void 
MRASlabIdentifier<TInputImage>
::GenerateSlabRegions(void)
{
  // this method only works with 3D MRI image
  if (ImageType::ImageDimension != 3)
    {
    throw ExceptionObject(__FILE__, __LINE__) ;
    }

  ImageSizeType size ;
  ImageRegionType region ;
  ImageIndexType index ;

  region = m_Image->GetLargestPossibleRegion() ;  
  size = region.GetSize() ;
  index = region.GetIndex() ;
  long firstSlice = index[m_SlicingDirection] ;
  long lastSlice = firstSlice + size[m_SlicingDirection] ;
  unsigned long totalSlices = size[m_SlicingDirection] ;

  double sum ;
  std::vector<double> avgMin(totalSlices) ;
  // calculate minimum intensities for each slice
  std::vector<ImagePixelType> mins(m_NumberOfMinimumsPerSlice, 1000) ;

  ImagePixelType pixel ;
  typename std::vector<ImagePixelType>::iterator target ;

  long currentSlice = firstSlice ;

  for (int i = 0 ; i < 3 ; i++)
    {
    if (i != m_SlicingDirection)
      {
      index[i] = 0 ;
      }
    }

  size[m_SlicingDirection] = 1 ;
  region.SetSize(size) ;

  unsigned long count = 0 ;
  while (currentSlice < lastSlice)
    {
    index[m_SlicingDirection] = currentSlice ;
    region.SetIndex(index) ;

    ImageRegionConstIterator<TInputImage> iter(m_Image, region) ;
    iter.GoToBegin() ;

    std::fill(mins.begin(), mins.end(), 1000) ;
    while (!iter.IsAtEnd())
      {
      pixel = iter.Get() ;
      if (pixel > 0)
        {
        target = 
          std::find_if(mins.begin(), mins.end(), 
                       std::bind2nd(std::greater<ImagePixelType>(), pixel)) ;
        *target = pixel ;
        }
      ++iter ;
      }

    sum = 0 ;
    typename std::vector<ImagePixelType>::iterator m_iter = mins.begin() ;
    while (m_iter != mins.end())
      {
      sum += *m_iter ;
      ++m_iter ;
      }

    avgMin[count] = sum / (double) m_NumberOfMinimumsPerSlice ;

    ++count ;
    ++currentSlice ;
    }

  // calculate overall average
  sum = 0 ;
  std::vector<double>::iterator am_iter = avgMin.begin() ;
  while (am_iter != avgMin.end())
    {
    sum += *am_iter ;
    ++am_iter ;
    }

  double average = sum / (double) totalSlices ; 

  // determine slabs
  am_iter = avgMin.begin() ;

  double prevSign = *am_iter - average ;
  double avgMinValue ;

  ImageIndexType slabIndex ;
  ImageRegionType slabRegion ;
  ImageSizeType slabSize ;

  long slabLength = 0 ;
  long slabBegin = firstSlice ;
  slabSize = size ;
  slabIndex = index ;
  while (am_iter != avgMin.end())
    {
    avgMinValue = *am_iter ;
    double sign = avgMinValue - average ;
    if (sign * prevSign < 0)
      { 
      slabIndex[m_SlicingDirection] = slabBegin ;
      slabSize[m_SlicingDirection] = slabLength ;
      slabRegion.SetSize(slabSize) ;
      slabRegion.SetIndex(slabIndex) ;
      m_Slabs.push_back(slabRegion) ;

      prevSign = sign ;
      slabBegin += slabLength ;
      slabLength = 0 ;
      }
    am_iter++ ;
    slabLength++ ;
    }
  slabIndex[m_SlicingDirection] = slabBegin ;
  slabSize[m_SlicingDirection] = slabLength ;
  slabRegion.SetIndex(slabIndex) ;
  slabRegion.SetSize(slabSize) ;
  m_Slabs.push_back(slabRegion) ;
}


template<class TInputImage>
typename MRASlabIdentifier<TInputImage>::SlabRegionVectorType 
MRASlabIdentifier<TInputImage>
::GetSlabRegionVector(void)
{
  return m_Slabs ;
}

template<class TInputImage>
void
MRASlabIdentifier<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  if (m_Image)
    {
    os << indent << "Image: " << m_Image << std::endl;
    }
  else
    {
    os << indent << "Image: " << "(None)" << std::endl;
    }
  os << indent << "NumberOfMinimumsPerSlice: " << m_NumberOfMinimumsPerSlice << std::endl;
  os << indent << "SlicingDirection: " << m_SlicingDirection << std::endl;
}

} // end namespace itk

#endif /* __itkMRASlabIdentifier_txx */
