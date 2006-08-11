/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNaryFunctorImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNaryFunctorImageFilter_txx
#define _itkNaryFunctorImageFilter_txx

#include "itkNaryFunctorImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage, class TFunction >
NaryFunctorImageFilter<TInputImage,TOutputImage,TFunction>
::NaryFunctorImageFilter()
{
  // This number will be incremented each time an image
  // is added over the two minimum required
  this->SetNumberOfRequiredInputs( 2 );
  this->InPlaceOff();
}


/**
 * ThreadedGenerateData Performs the pixel-wise addition
 */
template <class TInputImage, class TOutputImage, class TFunction>
void
NaryFunctorImageFilter<TInputImage, TOutputImage, TFunction>
::ThreadedGenerateData( const OutputImageRegionType &outputRegionForThread,
                        int threadId)
{

  const unsigned int numberOfInputImages = 
    static_cast< unsigned int >( this->GetNumberOfInputs() );
  
  OutputImagePointer outputPtr = this->GetOutput(0);
  ImageRegionIterator<TOutputImage> outputIt(outputPtr, outputRegionForThread);


  // Clear the content of the output
  outputIt.GoToBegin();
  while( !outputIt.IsAtEnd() )
    {
    outputIt.Set( itk::NumericTraits< OutputImagePixelType >::Zero );
    ++outputIt;
    }
  
  typedef ImageRegionConstIterator<TInputImage> ImageRegionConstIteratorType;
  std::vector< ImageRegionConstIteratorType * > inputItrVector;
  inputItrVector.resize(numberOfInputImages);
  //Array< ImageRegionConstIteratorType > inputItrVector(numberOfInputImages);
  
  // support progress methods/callbacks.
  // count the number of inputs that are non-null
  unsigned int numberOfValidInputImages = 0;
  unsigned int lastValidImage = 0;
  for (unsigned int i=0; i < numberOfInputImages; ++i)
    {
    InputImagePointer inputPtr =
      dynamic_cast<TInputImage*>( ProcessObject::GetInput( i ) );

    if (inputPtr)
      {
      ImageRegionConstIteratorType *inputIt = new ImageRegionConstIteratorType(inputPtr,outputRegionForThread);
      lastValidImage = i;
      inputItrVector[i] = reinterpret_cast< ImageRegionConstIteratorType * >( inputIt );
      inputItrVector[i]->GoToBegin();
      }
    else
      {
      inputItrVector[i] = reinterpret_cast< ImageRegionConstIteratorType * >(NULL);
      }
    }
  ProgressReporter progress(this, threadId,
                            numberOfValidInputImages
                            *outputRegionForThread.GetNumberOfPixels());
     
  if( !inputItrVector[lastValidImage] )
    { 
    //No valid regions in the thread
    return;
    }
  
    
  NaryArrayType naryInputArray; 
  naryInputArray.SetSize( numberOfInputImages );
    
  outputIt.GoToBegin();
  
  while( !inputItrVector[lastValidImage]->IsAtEnd())  
    {
    for (unsigned int inputNumber=0; inputNumber < numberOfInputImages; inputNumber++)
      {
      naryInputArray[ inputNumber ] = inputItrVector[inputNumber]->Get();
      ++(*inputItrVector[inputNumber]);
      }
    outputIt.Set( m_Functor( naryInputArray ) );
    ++outputIt;
    progress.CompletedPixel();
    }
  
  for (unsigned int i=0; i < numberOfInputImages; ++i)
    {
    if (inputItrVector[i])
      {
      delete inputItrVector[i];
      }
    }
}

} // end namespace itk

#endif


