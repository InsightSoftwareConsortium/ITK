/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionGrowImageFilter.txx
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
RegionGrowImageFilter<TInputImage,TOutputImage>
::RegionGrowImageFilter(void):
  m_MaxNumRegions(0),
  m_RowGridSize(2),
  m_ColGridSize(2)
{
}

template<class TInputImage, class TOutputImage>
RegionGrowImageFilter<TInputImage,TOutputImage>
::~RegionGrowImageFilter()
{

}

/**
 * PrintSelf
 */
template <class TInputImage, class TOutputImage>
void
RegionGrowImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{

  Superclass::PrintSelf(os,indent);

  os << indent << "Region grow segmentation" << std::endl;

}// end PrintSelf

} // namespace itk



