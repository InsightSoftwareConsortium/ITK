/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeUniqueLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapeUniqueLabelMapFilter_txx
#define __itkShapeUniqueLabelMapFilter_txx

#include "itkShapeUniqueLabelMapFilter.h"
#include "itkLabelMapUtilities.h"

namespace itk
{
template< class TImage >
ShapeUniqueLabelMapFilter< TImage >
::ShapeUniqueLabelMapFilter()
{
  m_ReverseOrdering = false;
  m_Attribute = LabelObjectType::NUMBER_OF_PIXELS;
}

template< class TImage >
void
ShapeUniqueLabelMapFilter< TImage >
::GenerateData()
{
  switch ( m_Attribute )
    {
    itkShapeLabelMapFilterDispatchMacro()
    default:
      itkExceptionMacro(<< "Unknown attribute type");
      break;
    }
}

template< class TImage >
void
ShapeUniqueLabelMapFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ReverseOrdering: "  << m_ReverseOrdering << std::endl;
  os << indent << "Attribute: "  << LabelObjectType::GetNameFromAttribute(m_Attribute) << " (" << m_Attribute << ")"
     << std::endl;
}
} // end namespace itk
#endif
