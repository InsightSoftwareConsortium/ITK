/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionGrow.txx
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
RegionGrow<TInputImage,TOutputImage>
::RegionGrow(void)
{
  m_MaxNumRegions   = NULL;
  m_RowGridSize     = 2;
  m_ColGridSize     = 2;
}

template<class TInputImage, class TOutputImage>
RegionGrow<TInputImage,TOutputImage>
::~RegionGrow()
{

}

/**
 * PrintSelf
 */
template <class TInputImage, class TOutputImage>
void
RegionGrow<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{

  Superclass::PrintSelf(os,indent);

  os << indent << "Region grow segmentation" << std::endl;

}// end PrintSelf

} // namespace itk



