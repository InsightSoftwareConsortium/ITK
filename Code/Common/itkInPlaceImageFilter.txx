/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInPlaceImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkInPlaceImageFilter_txx
#define _itkInPlaceImageFilter_txx
#include "itkInPlaceImageFilter.h"


namespace itk
{

/**
 *
 */
template <class TInputImage>
InPlaceImageFilter<TInputImage>
::InPlaceImageFilter()
{
}

/**
 *
 */
template <class TInputImage>
InPlaceImageFilter<TInputImage>
::~InPlaceImageFilter()
{
}
  


template<class TInputImage>
void 
InPlaceImageFilter<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template<class TInputImage>
void 
InPlaceImageFilter<TInputImage>
::AllocateOutputs()
{
  // Graft this first input to the output.  Later, we'll need to
  // remove the input's hold on the bulk data.
  this->GraftOutput( const_cast<TInputImage *>(this->GetInput()) );

  // If there are more than output, allocate the remaining outputs
  for (unsigned int i=1; i < this->GetNumberOfOutputs(); i++)
    {
    typename InPlaceImageFilter<TInputImage>::OutputImagePointer outputPtr;

    outputPtr = this->GetOutput(i);
    outputPtr->SetBufferedRegion(outputPtr->GetRequestedRegion());
    outputPtr->Allocate();
    }
}

template<class TInputImage>
void 
InPlaceImageFilter<TInputImage>
::ReleaseInputs()
{
  // Release any input where the ReleaseData flag has been set
  ProcessObject::ReleaseInputs();
  
  // Release input 0 by default since we overwrote it
  const_cast<TInputImage*>(this->GetInput())->ReleaseData();
}


} // end namespace itk

#endif
