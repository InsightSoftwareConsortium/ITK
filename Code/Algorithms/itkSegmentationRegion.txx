/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSegmentationRegion.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
namespace itk
{

template<class TInputImage, class TOutputImage>
SegmentationRegion<TInputImage,TOutputImage>
::SegmentationRegion(void):
  m_RegionLabel(0),
  m_RegionArea(0),
  m_UniqueLabel(0)
{
  m_MeanVec = 0;
}

template<class TInputImage, class TOutputImage>
SegmentationRegion<TInputImage,TOutputImage>
::~SegmentationRegion()
{

}

/**
 * PrintSelf
 */
template <class TInputImage, class TOutputImage>
void
SegmentationRegion<TInputImage,TOutputImage>
::PrintSelf( std::ostream& os, Indent indent )
{

  Superclass::PrintSelf(os,indent);
  os << indent << "Region border object" << std::endl;

}// end PrintSelf


template<class TInputImage, class TOutputImage>
void
SegmentationRegion<TInputImage,TOutputImage>
::SetMeanRegionIntensity( VecDblType averageRegionIntensity )
{

  m_MeanVec = averageRegionIntensity;

}// end SetMeanRegionIntensity()



template<class TInputImage, class TOutputImage>
SegmentationRegion<TInputImage,TOutputImage>::VecDblType
SegmentationRegion<TInputImage,TOutputImage>
::GetMeanRegionIntensity()
{

  return m_MeanVec;

}// end GetMeanRegionIntensity()

} // namespace itk

