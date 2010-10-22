/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkShapePositionLabelMapFilter.txx,v $
  Language:  C++
  Date:      $Date: 2005/08/23 15:09:03 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapePositionLabelMapFilter_txx
#define __itkShapePositionLabelMapFilter_txx

#include "itkShapePositionLabelMapFilter.h"
#include "itkLabelMapUtilities.h"
#include "itkShapeLabelObjectAccessors.h"

namespace itk {

template <class TImage>
ShapePositionLabelMapFilter<TImage>
::ShapePositionLabelMapFilter()
{
  m_Attribute = LabelObjectType::CENTROID;
}


template <class TImage>
void
ShapePositionLabelMapFilter<TImage>
::ThreadedProcessLabelObject( LabelObjectType * labelObject )
{
  switch( m_Attribute )
    {
    case LabelObjectType::CENTROID:
      {
      typedef typename Functor::CentroidLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedThreadedProcessLabelObject(accessor, true, labelObject);
      break;
      }
    default:
      itkExceptionMacro(<< "Unknown attribute type");
      break;
    }
}

template <class TImage>
void
ShapePositionLabelMapFilter<TImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Attribute: "  << LabelObjectType::GetNameFromAttribute(m_Attribute) << " (" << m_Attribute << ")" << std::endl;
}

}// end namespace itk
#endif
