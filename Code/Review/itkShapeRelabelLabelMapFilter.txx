/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapeRelabelLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapeRelabelLabelMapFilter_txx
#define __itkShapeRelabelLabelMapFilter_txx

#include "itkShapeRelabelLabelMapFilter.h"

namespace itk {

template <class TImage>
ShapeRelabelLabelMapFilter<TImage>
::ShapeRelabelLabelMapFilter()
{
  m_ReverseOrdering = false;
  m_Attribute = LabelObjectType::SIZE;
}


template <class TImage>
void
ShapeRelabelLabelMapFilter<TImage>
::GenerateData()
{
  switch( m_Attribute )
    {
    case LabelObjectType::LABEL:
      TemplatedGenerateData< typename Functor::LabelLabelObjectAccessor< LabelObjectType > >();
      break;
    case LabelObjectType::SIZE:
      TemplatedGenerateData< typename Functor::SizeLabelObjectAccessor< LabelObjectType > >();
      break;
    case LabelObjectType::PHYSICAL_SIZE:
      TemplatedGenerateData< typename Functor::PhysicalSizeLabelObjectAccessor< LabelObjectType > >();
      break;
    case LabelObjectType::SIZE_REGION_RATIO:
      TemplatedGenerateData< typename Functor::SizeRegionRatioLabelObjectAccessor< LabelObjectType > >();
      break;
    case LabelObjectType::REGION_ELONGATION:
      TemplatedGenerateData< typename Functor::RegionElongationLabelObjectAccessor< LabelObjectType > >();
      break;
    case LabelObjectType::SIZE_ON_BORDER:
      TemplatedGenerateData< typename Functor::SizeOnBorderLabelObjectAccessor< LabelObjectType > >();
      break;
    case LabelObjectType::PHYSICAL_SIZE_ON_BORDER:
      TemplatedGenerateData< typename Functor::PhysicalSizeOnBorderLabelObjectAccessor< LabelObjectType > >();
      break;
    case LabelObjectType::FERET_DIAMETER:
      TemplatedGenerateData< typename Functor::FeretDiameterLabelObjectAccessor< LabelObjectType > >();
      break;
    case LabelObjectType::BINARY_ELONGATION:
      TemplatedGenerateData< typename Functor::BinaryElongationLabelObjectAccessor< LabelObjectType > >();
      break;
    case LabelObjectType::PERIMETER:
      TemplatedGenerateData< typename Functor::PerimeterLabelObjectAccessor< LabelObjectType > >();
      break;
    case LabelObjectType::ROUNDNESS:
      TemplatedGenerateData< typename Functor::RoundnessLabelObjectAccessor< LabelObjectType > >();
      break;
    case LabelObjectType::EQUIVALENT_RADIUS:
      TemplatedGenerateData< typename Functor::EquivalentRadiusLabelObjectAccessor< LabelObjectType > >();
      break;
    case LabelObjectType::EQUIVALENT_PERIMETER:
      TemplatedGenerateData< typename Functor::EquivalentPerimeterLabelObjectAccessor< LabelObjectType > >();
      break;
    case LabelObjectType::BINARY_FLATNESS:
      TemplatedGenerateData< typename Functor::BinaryFlatnessLabelObjectAccessor< LabelObjectType > >();
      break;
    default:
      itkExceptionMacro(<< "Unknown attribute type");
      break;
    }
}


template <class TImage>
template <class TAttributeAccessor>
void
ShapeRelabelLabelMapFilter<TImage>
::TemplatedGenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  ImageType * output = this->GetOutput();

  typedef typename ImageType::LabelObjectContainerType LabelObjectContainerType;
  const LabelObjectContainerType & labelObjectContainer = output->GetLabelObjectContainer();
  typedef typename std::vector< typename LabelObjectType::Pointer > VectorType;

  ProgressReporter progress( this, 0, 2 * labelObjectContainer.size() );

  // Get the label objects in a vector, so they can be sorted
  VectorType labelObjects;
  labelObjects.reserve( labelObjectContainer.size() );
  for( typename LabelObjectContainerType::const_iterator it = labelObjectContainer.begin();
    it != labelObjectContainer.end();
    it++ )
    {
    labelObjects.push_back( it->second );
    progress.CompletedPixel();
    }

  // Instantiate the comparator and sort the vector
  if( m_ReverseOrdering )
    {
    Functor::LabelObjectReverseComparator< LabelObjectType, TAttributeAccessor > comparator;
    std::sort( labelObjects.begin(), labelObjects.end(), comparator );
    }
  else
    {
    Functor::LabelObjectComparator< LabelObjectType, TAttributeAccessor > comparator;
    std::sort( labelObjects.begin(), labelObjects.end(), comparator );
    }
  //   progress.CompletedPixel();
  
  // and put back the objects in the map
  typedef typename ImageType::LabelObjectType LabelObjectType;
  output->ClearLabels();
  unsigned int label = 0;
  typename VectorType::const_iterator it = labelObjects.begin();
  while( it != labelObjects.end() )
    {
    // Avoid the background label if it is used
    if( label == output->GetBackgroundValue() )
      {
      label++;
      }
    ( *it )->SetLabel( label );
    output->AddLabelObject( *it );
    
    // Go to the next label
    label++;
    progress.CompletedPixel();

    it++;
    }
}


template <class TImage>
void
ShapeRelabelLabelMapFilter<TImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "ReverseOrdering: "  << m_ReverseOrdering << std::endl;
  os << indent << "Attribute: "  << LabelObjectType::GetNameFromAttribute(m_Attribute) << " (" << m_Attribute << ")" << std::endl;
}

}// end namespace itk
#endif
