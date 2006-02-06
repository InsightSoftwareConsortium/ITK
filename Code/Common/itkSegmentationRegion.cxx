/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSegmentationRegion.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkSegmentationRegion.h"

namespace itk
{


SegmentationRegion
::SegmentationRegion(void):
  m_RegionLabel(0),
  m_RegionArea(0)
{

}


SegmentationRegion
::~SegmentationRegion()
{

}

/**
 * PrintSelf
 */
void
SegmentationRegion
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Segmentation region object" << std::endl;
  os << indent << "Region label            : " << m_RegionLabel << std::endl;
  os << indent << "Area of the region      : " << m_RegionArea << std::endl;
}

} // end namespace itk

