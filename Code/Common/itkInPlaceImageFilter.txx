/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
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
bool
InPlaceImageFilter< TInputImage, TOutputImage >
::CanRunInPlace() const
{
  return Self::CanRunInPlace(is_same<TInputImage,TOutputImage>());
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
