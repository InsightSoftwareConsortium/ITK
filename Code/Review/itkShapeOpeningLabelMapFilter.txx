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

namespace itk
{
template< class TImage >
ShapeOpeningLabelMapFilter< TImage >
::ShapeOpeningLabelMapFilter()
{
  m_Lambda = NumericTraits< double >::Zero;
  m_ReverseOrdering = false;
  m_Attribute = LabelObjectType::SIZE;

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
    case LabelObjectType::LABEL:
      {
      typedef typename Functor::LabelLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::SIZE:
      {
      typedef typename Functor::SizeLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::PHYSICAL_SIZE:
      {
      typedef typename Functor::PhysicalSizeLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::SIZE_REGION_RATIO:
      {
      typedef typename Functor::SizeRegionRatioLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::REGION_ELONGATION:
      {
      typedef typename Functor::RegionElongationLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::SIZE_ON_BORDER:
      {
      typedef typename Functor::SizeOnBorderLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::PHYSICAL_SIZE_ON_BORDER:
      {
      typedef typename Functor::PhysicalSizeOnBorderLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::FERET_DIAMETER:
      {
      typedef typename Functor::FeretDiameterLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::BINARY_ELONGATION:
      {
      typedef typename Functor::BinaryElongationLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::PERIMETER:
      {
      typedef typename Functor::PerimeterLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::ROUNDNESS:
      {
      typedef typename Functor::RoundnessLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::EQUIVALENT_RADIUS:
      {
      typedef typename Functor::EquivalentRadiusLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::EQUIVALENT_PERIMETER:
      {
      typedef typename Functor::EquivalentPerimeterLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::BINARY_FLATNESS:
      {
      typedef typename Functor::BinaryFlatnessLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
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
