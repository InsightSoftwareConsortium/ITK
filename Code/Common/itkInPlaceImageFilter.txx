/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInPlaceImageFilter.txx
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
#ifndef __itkInPlaceImageFilter_txx
#define __itkInPlaceImageFilter_txx

#include "itkInPlaceImageFilter.h"

namespace itk
{
/**
 *
 */
template< class TInputImage, class TOutputImage >
InPlaceImageFilter< TInputImage, TOutputImage >
::InPlaceImageFilter():
  m_InPlace(true)
{}

/**
 *
 */
template< class TInputImage, class TOutputImage >
InPlaceImageFilter< TInputImage, TOutputImage >
::~InPlaceImageFilter()
{}

template< class TInputImage, class TOutputImage >
void
InPlaceImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "InPlace: " << ( m_InPlace ? "On" : "Off" ) << std::endl;
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

template< class TInputImage, class TOutputImage >
void
InPlaceImageFilter< TInputImage, TOutputImage >
::AllocateOutputs()
{
  // if told to run in place and the types support it,
  if ( this->GetInPlace() && this->CanRunInPlace() )
    {
    // Graft this first input to the output.  Later, we'll need to
    // remove the input's hold on the bulk data.
    //
    OutputImagePointer inputAsOutput =
      dynamic_cast< TOutputImage * >( const_cast< TInputImage * >( this->GetInput() ) );
    if ( inputAsOutput )
      {
      this->GraftOutput(inputAsOutput);
      }
    else
      {
      // if we cannot cast the input to an output type, then allocate
      // an output usual.
      OutputImagePointer outputPtr;

      outputPtr = this->GetOutput(0);
      outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
      outputPtr->Allocate();
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
    }
}

template< class TInputImage, class TOutputImage >
void
InPlaceImageFilter< TInputImage, TOutputImage >
::ReleaseInputs()
{
  // if told to run in place and the types support it,
  if ( this->GetInPlace() && this->CanRunInPlace() )
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
