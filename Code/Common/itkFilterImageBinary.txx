/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageBinary.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkFilterImageBinary.h"


#include <itkImageRegionSimpleIterator.h>

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage1, class TInputImage2, 
          class TOutputImage, class TFunction  >
FilterImageBinary<TInputImage1,TInputImage2,TOutputImage,TFunction>
::FilterImageBinary()
{
  image3 = GetOutput();
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TOutputImage, class TFunction  >
void
FilterImageBinary<TInputImage1,TInputImage2,TOutputImage,TFunction>
::SetInput1( Image1Pointer image1 ) 
{
  this->image1 = image1;
  SetNthInput(0, image1.GetPointer() );
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TOutputImage, class TFunction  >
void
FilterImageBinary<TInputImage1,TInputImage2,TOutputImage,TFunction>
::SetInput2( Image2Pointer image2 ) 
{
  this->image2 = image2;
  SetNthInput(0, image2.GetPointer());
}





/**
 * Executes filter. Performs the pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TOutputImage, class TFunction  >
void
FilterImageBinary<TInputImage1,TInputImage2,TOutputImage,TFunction>
::Execute( void )
{

  image3->SetImageSize(  image1->GetImageSize() );
  image3->SetBufferSize( image1->GetBufferSize() );
  image3->Allocate();
  image3->SetImageStartIndex(  image1->GetImageStartIndex() );
  image3->SetBufferStartIndex( image1->GetBufferStartIndex() );

  Image1Iterator it1(image1, image1->GetBufferStartIndex(),image1->GetBufferSize());
  Image2Iterator it2(image2, image2->GetBufferStartIndex(),image2->GetBufferSize());
  Image3Iterator it3(image3, image3->GetBufferStartIndex(),image3->GetBufferSize());

  it1.Begin();
  it2.Begin();
  it3.Begin();

  TFunction function;

  while( !it1.IsAtEnd() ) 
  {
	*it3 = function(*it1,*it2);
	++it1;
	++it2;
	++it3;
  }


}





} // end namespace itk
