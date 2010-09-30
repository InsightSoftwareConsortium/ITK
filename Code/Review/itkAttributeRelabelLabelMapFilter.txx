/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAttributeRelabelLabelMapFilter.txx,v $
  Language:  C++
  Date:      $Date: 2005/08/23 15:09:03 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAttributeRelabelLabelMapFilter_txx
#define __itkAttributeRelabelLabelMapFilter_txx

#include "itkAttributeRelabelLabelMapFilter.h"
#include "itkProgressReporter.h"


namespace itk {

template <class TImage, class TAttributeAccessor>
AttributeRelabelLabelMapFilter<TImage, TAttributeAccessor>
::AttributeRelabelLabelMapFilter()
{
  m_ReverseOrdering = false;
}


template <class TImage, class TAttributeAccessor>
void
AttributeRelabelLabelMapFilter<TImage, TAttributeAccessor>
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  ImageType * output = this->GetOutput();

  typedef typename ImageType::LabelObjectContainerType LabelObjectContainerType;
  const LabelObjectContainerType & labelObjectContainer = output->GetLabelObjectContainer();
  typedef typename std::vector< typename LabelObjectType::Pointer > VectorType;

  ProgressReporter progress( this, 0, 2 * labelObjectContainer.size() );

  // get the label objects in a vector, so they can be sorted
  VectorType labelObjects;
  labelObjects.reserve( labelObjectContainer.size() );
  for( typename LabelObjectContainerType::const_iterator it = labelObjectContainer.begin();
    it != labelObjectContainer.end();
    it++ )
    {
    labelObjects.push_back( it->second );
    progress.CompletedPixel();
    }

  // instantiate the comparator and sort the vector
  if( m_ReverseOrdering )
    {
    ReverseComparator comparator;
    std::sort( labelObjects.begin(), labelObjects.end(), comparator );
    }
  else
    {
    Comparator comparator;
    std::sort( labelObjects.begin(), labelObjects.end(), comparator );
    }
//   progress.CompletedPixel();

  // and put back the objects in the map
  typedef typename ImageType::LabelObjectType LabelObjectType;
  output->ClearLabels();
  unsigned int label = 0;
  for( typename VectorType::const_iterator it = labelObjects.begin();
    it != labelObjects.end();
    it++ )
    {
    // avoid the background label if it is used
    if( label == output->GetBackgroundValue() )
      {
      label++;
      }
    (*it)->SetLabel( label );
    output->AddLabelObject( *it );

    // go to the nex label
    label++;
    progress.CompletedPixel();
    }
}


template <class TImage, class TAttributeAccessor>
void
AttributeRelabelLabelMapFilter<TImage, TAttributeAccessor>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "ReverseOrdering: "  << m_ReverseOrdering << std::endl;
}

}// end namespace itk
#endif
