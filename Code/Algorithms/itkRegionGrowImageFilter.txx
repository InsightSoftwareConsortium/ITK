/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionGrowImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRegionGrowImageFilter_txx
#define _itkRegionGrowImageFilter_txx

namespace itk
{

template<class TInputImage, class TOutputImage>
RegionGrowImageFilter<TInputImage,TOutputImage>
::RegionGrowImageFilter(void):
  m_MaximumNumberOfRegions( 0 ),
  m_RowGridSize( 2 ),
  m_ColGridSize( 2 ),
  m_SliceGridSize( 2 )
{
}

template<class TInputImage, class TOutputImage>
RegionGrowImageFilter<TInputImage,TOutputImage>
::~RegionGrowImageFilter()
{

}

/*
 * PrintSelf
 */
template <class TInputImage, class TOutputImage>
void
RegionGrowImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{

  Superclass::PrintSelf(os,indent);
  os << indent << "Region grow image filter object" << std::endl;
  os << indent << "Maximum number of regions: " << m_MaximumNumberOfRegions << std::endl;
  os << indent << "Maximum column grid size : " << m_ColGridSize << std::endl;
  os << indent << "Maximum row grid size    : " << m_RowGridSize << std::endl;
  os << indent << "Maximum slice grid size  : " << m_SliceGridSize << std::endl;

}// end PrintSelf

} // namespace itk




#endif
