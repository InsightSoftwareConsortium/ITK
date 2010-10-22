/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAttributePositionLabelMapFilter.txx,v $
  Language:  C++
  Date:      $Date: 2005/08/23 15:09:03 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAttributePositionLabelMapFilter_txx
#define __itkAttributePositionLabelMapFilter_txx

#include "itkAttributePositionLabelMapFilter.h"
#include "itkProgressReporter.h"
#include "itkLabelMapUtilities.h"


namespace itk {

template <class TImage, class TAttributeAccessor, bool VPhysicalPosition>
AttributePositionLabelMapFilter<TImage, TAttributeAccessor, VPhysicalPosition>
::AttributePositionLabelMapFilter()
{
}

template <class TImage, class TAttributeAccessor, bool VPhysicalPosition>
void
AttributePositionLabelMapFilter<TImage, TAttributeAccessor, VPhysicalPosition>
::ThreadedProcessLabelObject( LabelObjectType * labelObject )
{
  TAttributeAccessor accessor;
  AttributeValueType position = accessor( labelObject );
  // change it to an index position if it is physical
  IndexType idx;
  if( VPhysicalPosition )
    {
    Point< double, ImageDimension > point;
    // copy the position to a point, required by TransformPhysicalPointToIndex
    for( int i=0; i<ImageDimension; i++ )
      {
      point[i] = (long)position[i];
      }
    this->GetOutput()->TransformPhysicalPointToIndex( point, idx );
    }
  else
    {
    // copy the position to the index, to avoid warnings
    for( int i=0; i<ImageDimension; i++ )
      {
      idx[i] = (long)position[i];
      }
    }
  // clear the label object
  labelObject->GetLineContainer().clear();
  // and mark only the pixel we are interested in
  labelObject->AddIndex( idx );
}

template <class TImage, class TAttributeAccessor, bool VPhysicalPosition>
void
AttributePositionLabelMapFilter<TImage, TAttributeAccessor, VPhysicalPosition>
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

}// end namespace itk
#endif
