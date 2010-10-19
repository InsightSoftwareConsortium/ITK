/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeOpeningLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapeOpeningLabelMapFilter_txx
#define __itkShapeOpeningLabelMapFilter_txx

#include "itkShapeOpeningLabelMapFilter.h"
#include "itkLabelMapUtilities.h"

namespace itk
{
template< class TImage >
ShapeOpeningLabelMapFilter< TImage >
::ShapeOpeningLabelMapFilter()
{
  m_Lambda = NumericTraits< double >::Zero;
  m_ReverseOrdering = false;
  m_Attribute = LabelObjectType::NUMBER_OF_PIXELS;

  // create the output image for the removed objects
  this->SetNumberOfRequiredOutputs(2);
  this->SetNthOutput( 1, static_cast< TImage * >( this->MakeOutput(1).GetPointer() ) );
}

template< class TImage >
void
ShapeOpeningLabelMapFilter< TImage >
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
ShapeOpeningLabelMapFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ReverseOrdering: "  << m_ReverseOrdering << std::endl;
  os << indent << "Lambda: "  << m_Lambda << std::endl;
  os << indent << "Attribute: "  << LabelObjectType::GetNameFromAttribute(m_Attribute) << " (" << m_Attribute << ")"
     << std::endl;
}
} // end namespace itk
#endif
