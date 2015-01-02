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
#ifndef itkInPlaceImageFilter_hxx
#define itkInPlaceImageFilter_hxx

#include "itkInPlaceImageFilter.h"

namespace itk
{
/**
 *
 */
template< typename TInputImage, typename TOutputImage >
InPlaceImageFilter< TInputImage, TOutputImage >
::InPlaceImageFilter():
  m_InPlace(true),
  m_RunningInPlace(false)
{}

/**
 *
 */
template< typename TInputImage, typename TOutputImage >
InPlaceImageFilter< TInputImage, TOutputImage >
::~InPlaceImageFilter()
{}

template< typename TInputImage, typename TOutputImage >
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

template< typename TInputImage, typename TOutputImage >
void
InPlaceImageFilter< TInputImage, TOutputImage >
::InternalAllocateOutputs( const TrueType& )
{
  // Use ProcessObject's GetInput method to get a DataObject pointer,
  // then perform a dynamic_cast to the expected InputImageType. This
  // may fail and that is an expected likely hood, if inputPtr is ITK_NULLPTR
  // then this filter will not run in-place.
  const InputImageType *inputPtr = dynamic_cast<const InputImageType *>( this->ProcessObject::GetInput(0) );
  OutputImageType      *outputPtr = this->GetOutput();

  // if told to run in place and the types support it,
  // additionally the buffered and requested regions of the input and
  // output must match.
  bool rMatch = true;
  if( inputPtr != ITK_NULLPTR && (unsigned int)InputImageDimension == (unsigned int)OutputImageDimension )
    {
    for( unsigned int i=0; i<(unsigned int)InputImageDimension; i++ )
      {
      if( inputPtr->GetBufferedRegion().GetIndex(i) != outputPtr->GetRequestedRegion().GetIndex(i) )
        {
        rMatch = false;
        }
      if( inputPtr->GetBufferedRegion().GetSize(i) != outputPtr->GetRequestedRegion().GetSize(i) )
        {
        rMatch = false;
        }
      }
    }
  else
    {
    rMatch = false;
    }
  if ( inputPtr != ITK_NULLPTR &&
       this->GetInPlace() &&
       this->CanRunInPlace() &&
       rMatch )
    {
    // Graft this first input to the output.  Later, we'll need to
    // remove the input's hold on the bulk data.
    //
    OutputImagePointer inputAsOutput = ITK_NULLPTR;
    if ( IsSame<TInputImage, TOutputImage>() )
      {
      inputAsOutput = reinterpret_cast<TOutputImage *>( const_cast< TInputImage * >( inputPtr ) );
      }
    itkAssertOrThrowMacro( inputAsOutput.IsNotNull(), "Unable to convert input image to output image as expected!" );

    this->GraftOutput(inputAsOutput);
    this->m_RunningInPlace = true;

    typedef ImageBase< OutputImageDimension > ImageBaseType;

    // If there are more than one outputs, allocate the remaining outputs
    for ( unsigned int i = 1; i < this->GetNumberOfIndexedOutputs(); i++ )
      {
      // Check whether the output is an image of the appropriate
      // dimension (use ProcessObject's version of the GetInput()
      // method since it returns the input as a pointer to a
      // DataObject as opposed to the subclass version which
      // static_casts the input to an TInputImage).
      typename ImageBaseType::Pointer nthOutputPtr = dynamic_cast< ImageBaseType * >( this->ProcessObject::GetOutput(i) );

      if ( nthOutputPtr )
        {
        nthOutputPtr->SetBufferedRegion( nthOutputPtr->GetRequestedRegion() );
        nthOutputPtr->Allocate();
        }
      // if the output is not of similar type then it is assumed the
      // the derived class allocated the output if needed.
      }

    }
  else
    {
    this->m_RunningInPlace = false;
    Superclass::AllocateOutputs();
    }
}

template< typename TInputImage, typename TOutputImage >
bool
InPlaceImageFilter< TInputImage, TOutputImage >
::CanRunInPlace() const
{
  return IsSame<TInputImage,TOutputImage>();
}

template< typename TInputImage, typename TOutputImage >
void
InPlaceImageFilter< TInputImage, TOutputImage >
::ReleaseInputs()
{
  // if told to run in place and the types support it,
  if ( this->m_RunningInPlace )
    {
    // Release any input where the ReleaseData flag has been set
    ProcessObject::ReleaseInputs();

    // Release input 0 by default since we overwrote it
    TInputImage *ptr = const_cast< TInputImage * >( this->GetInput() );
    if ( ptr )
      {
      ptr->ReleaseData();
      }

     this->m_RunningInPlace = false;
    }
  else
    {
    Superclass::ReleaseInputs();
    }
}

} // end namespace itk

#endif
