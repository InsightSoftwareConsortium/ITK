/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAttributeKeepNObjectsLabelMapFilter.txx,v $
  Language:  C++
  Date:      $Date: 2005/08/23 15:09:03 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAttributeKeepNObjectsLabelMapFilter_txx
#define __itkAttributeKeepNObjectsLabelMapFilter_txx

#include "itkAttributeKeepNObjectsLabelMapFilter.h"
#include "itkProgressReporter.h"


namespace itk {

template <class TImage, class TAttributeAccessor>
AttributeKeepNObjectsLabelMapFilter<TImage, TAttributeAccessor>
::AttributeKeepNObjectsLabelMapFilter()
{
  m_ReverseOrdering = false;
  m_NumberOfObjects = 1;
  // create the output image for the removed objects
  this->SetNumberOfRequiredOutputs(2);
  this->SetNthOutput(1, static_cast<TImage*>(this->MakeOutput(1).GetPointer()));
}


template <class TImage, class TAttributeAccessor>
void
AttributeKeepNObjectsLabelMapFilter<TImage, TAttributeAccessor>
::GenerateData()
{
  // Allocate the output
  this->AllocateOutputs();

  ImageType * output = this->GetOutput();
  ImageType * output2 = this->GetOutput( 1 );

  // set the background value for the second output - this is not done in the superclasses
  output2->SetBackgroundValue( output->GetBackgroundValue() );

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
  if( m_NumberOfObjects < labelObjectContainer.size() )
    {
    typename VectorType::iterator end = labelObjects.begin() + m_NumberOfObjects;
    if( m_ReverseOrdering )
      {
      ReverseComparator comparator;
      std::nth_element( labelObjects.begin(), end, labelObjects.end(), comparator );
      }
    else
      {
      Comparator comparator;
      std::nth_element( labelObjects.begin(), end, labelObjects.end(), comparator );
      }
//   progress.CompletedPixel();

    // and move the last objects to the second output
    for( typename VectorType::const_iterator it = end;
      it != labelObjects.end();
      it++ )
      {
      output->RemoveLabelObject( *it );
      output2->AddLabelObject( *it );
      progress.CompletedPixel();
      }
    }
}


template <class TImage, class TAttributeAccessor>
void
AttributeKeepNObjectsLabelMapFilter<TImage, TAttributeAccessor>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "ReverseOrdering: "  << m_ReverseOrdering << std::endl;
  os << indent << "NumberOfObjects: "  << m_NumberOfObjects << std::endl;
}

}// end namespace itk
#endif
