/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtractOrthogonalSwath2DImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkExtractOrthogonalSwath2DImageFilter_txx
#define _itkExtractOrthogonalSwath2DImageFilter_txx

#include "itkExtractOrthogonalSwath2DImageFilter.h"
#include "itkPathConstIterator.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 * GenerateData Performs the reflection
 */
template <class TImage>
void
ExtractOrthogonalSwath2DImageFilter<TImage>
::GenerateData( void )
{
  ImageConstPointer inputImagePtr = this->GetImageInput();
  PathConstPointer  inputPathPtr  = this->GetPathInput();
  ImagePointer      outputPtr     = this->GetOutput(0);

  // Calculate new region size
// **** This is VERY WRONG ****
  ImageRegionType outputRegion = inputImagePtr->GetLargestPossibleRegion();
  
  outputPtr->SetRequestedRegion( outputRegion );
  outputPtr->SetBufferedRegion( outputRegion );
  outputPtr->SetLargestPossibleRegion( outputRegion );
  outputPtr->Allocate();
/*
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
  */
}

template <class TImage>
void
ExtractOrthogonalSwath2DImageFilter<TImage>::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk

#endif
