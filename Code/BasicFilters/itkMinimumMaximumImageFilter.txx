/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinimumMaximumImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkMinimumMaximumImageFilter_txx
#define _itkMinimumMaximumImageFilter_txx

#include "itkMinimumMaximumImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"

namespace itk
{


template <class TInputImage>
void
MinimumMaximumImageFilter<TInputImage>
::GenerateData(void)
{
  
  itkDebugMacro(<< "MinimumMaximumImageFilter::GenerateData() called");

  // Get the input and output pointers
  typename Superclass::InputImageConstPointer  inputPtr  = this->GetInput(0);
  typename Superclass::OutputImagePointer      outputPtr = this->GetOutput(0);

  // Make sure we're getting everything
  {
    typename TInputImage::Pointer image = 
                  const_cast< TInputImage * >( inputPtr.GetPointer() ); 
    image->SetRequestedRegionToLargestPossibleRegion();
  }

  // How big is the input image?
  typename TInputImage::SizeType inputSize = inputPtr->GetLargestPossibleRegion().GetSize();

  // Create a region object native to the output image type
  typename Superclass::OutputImageRegionType outputRegion;

  // Resize the output region
  outputRegion.SetSize( inputSize );

  // Set the largest legal region size (i.e. the size of the whole image)
  // to what we just defined
  outputPtr->SetLargestPossibleRegion( outputRegion );
  outputPtr->SetBufferedRegion( outputRegion );
  outputPtr->SetRequestedRegion( outputRegion );
  outputPtr->Allocate();

  // Create an iterator that will walk the output region
  typedef ImageRegionIterator<TInputImage> OutputIterator;

  OutputIterator outIt = OutputIterator(outputPtr,
                                        outputPtr->GetRequestedRegion());

 
  ImageRegionConstIteratorWithIndex< TInputImage >  it( 
                                          this->GetInput(),  
                                          this->GetInput()->GetRequestedRegion() );

  m_Maximum = NumericTraits<InputPixelType>::NonpositiveMin() ;
  m_Minimum = NumericTraits<InputPixelType>::max() ;

  while( !it.IsAtEnd() )
    {
    const InputPixelType value = it.Get();
    if (value > m_Maximum) 
      {
      m_Maximum = value;
      }

    if (value < m_Minimum) 
      {
      m_Minimum = value;
      }

    outIt.Set(value);
    ++outIt;
    ++it;
    }

  itkDebugMacro(<< "MinimumMaximumImageFilter::GenerateData() finished");
}

template <class TInputImage>
void
MinimumMaximumImageFilter<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << "Minimum: " << m_Minimum << std::endl;
  os << "Maximum: " << m_Maximum << std::endl;
}
} // end namespace itk

#endif
