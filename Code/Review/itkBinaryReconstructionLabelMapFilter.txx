/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkBinaryReconstructionLabelMapFilter.txx,v $
  Language:  C++
  Date:      $Date: 2005/08/23 15:09:03 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryReconstructionLabelMapFilter_txx
#define __itkBinaryReconstructionLabelMapFilter_txx

#include "itkBinaryReconstructionLabelMapFilter.h"
#include "itkProgressReporter.h"


namespace itk {

template <class TImage, class TMarkerImage, class TAttributeAccessor>
BinaryReconstructionLabelMapFilter<TImage, TMarkerImage, TAttributeAccessor>
::BinaryReconstructionLabelMapFilter()
{
  this->SetNumberOfRequiredInputs(2);
  m_ForegroundValue = NumericTraits< MarkerImagePixelType >::max();
}


template <class TImage, class TMarkerImage, class TAttributeAccessor>
void
BinaryReconstructionLabelMapFilter<TImage, TMarkerImage, TAttributeAccessor>
::ThreadedProcessLabelObject( LabelObjectType * labelObject )
{
  AttributeAccessorType accessor;

  const MarkerImageType * maskImage = this->GetMarkerImage();

  typename LabelObjectType::LineContainerType::const_iterator lit;
  typename LabelObjectType::LineContainerType & lineContainer = labelObject->GetLineContainer();

  // iterate over all the lines to find a pixel inside the object
  for( lit = lineContainer.begin(); lit != lineContainer.end(); lit++ )
    {
    const IndexType & firstIdx = lit->GetIndex();
    unsigned long length = lit->GetLength();

    long endIdx0 = firstIdx[0] + length;
    for( IndexType idx = firstIdx; idx[0]<endIdx0; idx[0]++ )
      {
      const MarkerImagePixelType & v = maskImage->GetPixel( idx );
      if( v == m_ForegroundValue )
        {
        // keep the object
        accessor( labelObject, true );
        return;
        }
      }
    }

  // remove the object
  accessor( labelObject, false );

}


template <class TImage, class TMarkerImage, class TAttributeAccessor>
void
BinaryReconstructionLabelMapFilter<TImage, TMarkerImage, TAttributeAccessor>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ForegroundValue: "  << static_cast<typename NumericTraits<MarkerImagePixelType>::PrintType>(m_ForegroundValue) << std::endl;
}

}// end namespace itk
#endif
