/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageSub.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkFilterImageSub.h"

#include <itkImageRegionSimpleIterator.h>

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage1, class TInputImage2, class TOutputImage>
FilterImageSub<TInputImage1,TInputImage2,TOutputImage>
::FilterImageSub()
{
  outputImage = GetOutput();
}

/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, class TOutputImage>
void
FilterImageSub<TInputImage1,TInputImage2,TOutputImage>
::SetInput1( InputImage1Pointer inputImage1 ) 
{
  this->inputImage1 = inputImage1;
  SetNthInput(0, inputImage1.GetPointer() );
}

/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, class TOutputImage>
void
FilterImageSub<TInputImage1,TInputImage2,TOutputImage>
::SetInput2( InputImage2Pointer inputImage2 ) 
{
  this->inputImage2 = inputImage2;
  SetNthInput(0, inputImage2.GetPointer());
}

/**
 * GenerateDatas filter. Performs the pixel-wise subtraction
 */
template <class TInputImage1, class TInputImage2, class TOutputImage>
void
FilterImageSub<TInputImage1,TInputImage2,TOutputImage>
::GenerateData( void )
{
  outputImage->SetImageSize(  inputImage1->GetImageSize() );
  outputImage->SetBufferSize( inputImage1->GetBufferSize() );
  outputImage->Allocate();
  outputImage->SetImageStartIndex(  inputImage1->GetImageStartIndex() );
  outputImage->SetBufferStartIndex( inputImage1->GetBufferStartIndex() );

  InputImage1Iterator itIn1(inputImage1, inputImage1->GetBufferStartIndex(),inputImage1->GetBufferSize());
  InputImage2Iterator itIn2(inputImage2, inputImage2->GetBufferStartIndex(),inputImage2->GetBufferSize());
  OutputImageIterator itOut(outputImage, outputImage->GetBufferStartIndex(),outputImage->GetBufferSize());

  itIn1.Begin();
  itIn2.Begin();
  itOut.Begin();
  while( !itIn1.IsAtEnd() ) 
  {
	*itOut = (*itIn1 - *itIn2);
	++itIn1;
	++itIn2;
	++itOut;
  }
}

} // end namespace itk
