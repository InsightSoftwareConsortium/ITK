/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGRegion.txx
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
RGRegion<TInputImage,TOutputImage>
::RGRegion(void)
{
  m_RegionLabel = NULL;
  m_UniqueLabel = NULL;
  m_RegionArea  = NULL;
  m_MeanVec     = NULL; 
}

template<class TInputImage, class TOutputImage>
RGRegion<TInputImage,TOutputImage>
::~RGRegion()
{

}

/**
 * PrintSelf
 */
template <class TInputImage, class TOutputImage>
void
RGRegion<TInputImage,TOutputImage>
::PrintSelf( std::ostream& os, Indent indent )
{

  Superclass::PrintSelf(os,indent);
  os << indent << "Region border object" << std::endl;

}// end PrintSelf


template<class TInputImage, class TOutputImage>
void
RGRegion<TInputImage,TOutputImage>
::SetMeanRegionIntensity( VecDblType averageRegionIntensity )
{

  m_MeanVec = averageRegionIntensity;

}// end SetMeanRegionIntensity()



template<class TInputImage, class TOutputImage>
RGRegion<TInputImage,TOutputImage>::VecDblType
RGRegion<TInputImage,TOutputImage>
::GetMeanRegionIntensity()
{

  return m_MeanVec;

}// end GetMeanRegionIntensity()

} // namespace itk

