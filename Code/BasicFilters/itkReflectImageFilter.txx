/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReflectImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkReflectImageFilter_txx
#define _itkReflectImageFilter_txx

#include "itkReflectImageFilter.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkImageLinearConstIteratorWithIndex.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage >
ReflectImageFilter<TInputImage,TOutputImage >
::ReflectImageFilter()
{
  this->SetNumberOfRequiredInputs( 1 );
  m_Direction = 0;
}


/**
 * GenerateData Performs the reflection
 */
template <class TInputImage, class TOutputImage >
void
ReflectImageFilter<TInputImage,TOutputImage>
::GenerateData( void )
{
  typename Superclass::InputImageConstPointer  inputPtr = this->GetInput();
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput(0);

  outputPtr->SetRequestedRegion( inputPtr->GetRequestedRegion() );
  outputPtr->SetBufferedRegion( inputPtr->GetBufferedRegion() );
  outputPtr->SetLargestPossibleRegion( inputPtr->GetLargestPossibleRegion() );
  outputPtr->Allocate();

  typedef ImageLinearConstIteratorWithIndex<TInputImage>  InputIterator;
  typedef ImageLinearIteratorWithIndex<TOutputImage> OutputIterator;

  InputIterator  inputIt(  inputPtr,  inputPtr->GetRequestedRegion() );
  OutputIterator outputIt( outputPtr, outputPtr->GetRequestedRegion() );

  // support progress methods/callbacks
  ProgressReporter progress(this, 0,  inputPtr->GetRequestedRegion().GetNumberOfPixels() );
       
  inputIt.SetDirection( m_Direction );
  outputIt.SetDirection( m_Direction );

  inputIt.GoToBegin();
  outputIt.GoToBegin();

  while( !inputIt.IsAtEnd() ) 
    {

    outputIt.GoToEndOfLine();
    --outputIt;
    while( !inputIt.IsAtEndOfLine() ) 
      {
      outputIt.Set( inputIt.Get() );
      ++inputIt;
      --outputIt;
      progress.CompletedPixel();
      }

    inputIt.NextLine();
    outputIt.GoToEndOfLine(); // NextLine() assumes that the 
    outputIt.NextLine();      // iterator is at the end of line.
    }
}

template <class TInputImage, class TOutputImage >
void
ReflectImageFilter<TInputImage,TOutputImage>::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Direction: " << m_Direction << std::endl;
}

} // end namespace itk

#endif
