/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionGrowImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRegionGrowImageFilter_txx
#define __itkRegionGrowImageFilter_txx

#include "itkRegionGrowImageFilter.h"

namespace itk
{
template< class TInputImage, class TOutputImage >
RegionGrowImageFilter< TInputImage, TOutputImage >
::RegionGrowImageFilter()
{
  m_GridSize.Fill(2);
  m_MaximumNumberOfRegions = 0;
}

template< class TInputImage, class TOutputImage >
RegionGrowImageFilter< TInputImage, TOutputImage >
::~RegionGrowImageFilter()
{}

/**
 * PrintSelf
 */
template< class TInputImage, class TOutputImage >
void
RegionGrowImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Region grow image filter object" << std::endl;
  os << indent << "Maximum number of regions: " << m_MaximumNumberOfRegions << std::endl;
  os << indent << "Maximum grid size : " << m_GridSize << std::endl;
} // end PrintSelf
} // namespace itk

#endif
