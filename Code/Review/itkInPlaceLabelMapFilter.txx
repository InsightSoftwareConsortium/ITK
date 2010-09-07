/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInPlaceLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkInPlaceLabelMapFilter_txx
#define __itkInPlaceLabelMapFilter_txx

#include "itkInPlaceLabelMapFilter.h"

namespace itk
{
/**
 *
 */
template< class TInputImage >
InPlaceLabelMapFilter< TInputImage >
::InPlaceLabelMapFilter():m_InPlace(true)
{}

/**
 *
 */
template< class TInputImage >
InPlaceLabelMapFilter< TInputImage >
::~InPlaceLabelMapFilter()
{}

template< class TInputImage >
void
InPlaceLabelMapFilter< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "InPlace: " << ( this->m_InPlace ? "On" : "Off" ) << std::endl;
  if ( this->CanRunInPlace() )
    {
    os << indent << "The input and output to this filter are the same type. The filter can be run in place."
       << std::endl;
    }
  else
    {
    os << indent << "The input and output to this filter are different types. The filter cannot be run in place."
       << std::endl;
    }
}

template< class TInputImage >
void
InPlaceLabelMapFilter< TInputImage >
::AllocateOutputs()
{
  // if told to run in place and the types support it,
  if ( this->m_InPlace && this->CanRunInPlace() )
    {
    // Graft this first input to the output.  Later, we'll need to
    // remove the input's hold on the bulk data.
    //
    OutputImagePointer inputAsOutput = dynamic_cast< TOutputImage * >( const_cast< TInputImage * >( this->GetInput() ) );

    if ( inputAsOutput )
      {
      // save the largest possible region to restore it after the graft output.
      // the largest possible region is not that important with LabelMap and
      // can be managed by the filter, even when running inplace
      RegionType region = this->GetOutput()->GetLargestPossibleRegion();
      this->GraftOutput(inputAsOutput);
      this->GetOutput()->SetRegions(region);
      }

    // If there are more than one outputs, allocate the remaining outputs
    for ( unsigned int i = 1; i < this->GetNumberOfOutputs(); i++ )
      {
      OutputImagePointer outputPtr;

      outputPtr = this->GetOutput(i);
      outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
      outputPtr->Allocate();
      }
    }
  else
    {
    Superclass::AllocateOutputs();
    // copy the content of the input image to the output image
    const TInputImage *input = this->GetInput();
    TOutputImage *     output = this->GetOutput();
    assert(input != NULL);
    assert(output != NULL);

    output->SetBackgroundValue( input->GetBackgroundValue() );

    const LabelObjectContainerType & labelObjectContainer = input->GetLabelObjectContainer();

    LabelObjectContainerConstIterator it = labelObjectContainer.begin();

    while ( it != labelObjectContainer.end() )
      {
      const LabelObjectType *labeObject = it->second;

      assert(labeObject != NULL);
      assert(labeObject->GetLabel() == it->first);

      typename LabelObjectType::Pointer newLabelObject = LabelObjectType::New();
      newLabelObject->CopyAllFrom(labeObject);

      output->AddLabelObject(newLabelObject);
      it++;
      }
    }
}

template< class TInputImage >
void
InPlaceLabelMapFilter< TInputImage >
::ReleaseInputs()
{
  // if told to run in place and the types support it,
  if ( this->m_InPlace && ( typeid( TInputImage ) == typeid( TOutputImage ) ) )
    {
    // Release any input where the ReleaseData flag has been set
    ProcessObject::ReleaseInputs();

    // Release input 0 by default since we overwrote it
    TInputImage *ptr = const_cast< TInputImage * >( this->GetInput() );
    if ( ptr )
      {
      ptr->ReleaseData();
      }
    }
  else
    {
    Superclass::ReleaseInputs();
    }
}
} // end namespace itk

#endif
