/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarToArrayCastImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkScalarToArrayCastImageFilter_txx
#define _itkScalarToArrayCastImageFilter_txx

#include "itkScalarToArrayCastImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkProgressReporter.h"
#include "itkPixelTraits.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage  >
ScalarToArrayCastImageFilter<TInputImage,TOutputImage>
::ScalarToArrayCastImageFilter()
{
  this->SetNumberOfRequiredInputs
    ( PixelTraits< OutputImagePixelType >::Dimension );
}


/**
 * ThreadedGenerateData Performs the pixel-wise addition
 */
template <class TInputImage, class TOutputImage  >
void
ScalarToArrayCastImageFilter<TInputImage,TOutputImage>
::ThreadedGenerateData( const OutputImageRegionType &outputRegionForThread,
                        int threadId)
{
  unsigned int length = 
    PixelTraits< OutputImagePixelType >::Dimension ;
  std::vector< const TInputImage* > inputs ;
  std::vector< ImageRegionConstIterator< TInputImage > > i_iters ;

  for ( unsigned int i = 0 ; i < length ; i++ )
    {
    inputs.push_back(this->GetInput(i)) ;
    i_iters.push_back
      ( ImageRegionConstIterator< TInputImage >
        (inputs[i], outputRegionForThread) );
    (i_iters[i]).GoToBegin();
    }

  typename TOutputImage::Pointer outputPtr = this->GetOutput(0);
  
  ImageRegionIterator<TOutputImage> outputIt(outputPtr, outputRegionForThread);

  ProgressReporter progress(this, 
                            threadId, 
                            outputRegionForThread.GetNumberOfPixels());
  outputIt.GoToBegin();
  typename TOutputImage::PixelType arrayPixel ;

  while( !outputIt.IsAtEnd() ) 
    {
    for ( unsigned int j = 0 ; j < length ; j++ )
      {
      arrayPixel[j] = (i_iters[j]).Get() ;
      ++(i_iters[j]);
      }
    outputIt.Set( arrayPixel );
    ++outputIt;
    progress.CompletedPixel();
    }
}

} // end namespace itk

#endif
