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
  this->m_OutputImage = GetOutput();
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
  this->m_Image1 = image1;
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
  this->m_Image2 = image2;
  SetNthInput(0, image2.GetPointer());
}





/**
 * GenerateData Performs the pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TOutputImage, class TFunction  >
void
FilterImageBinary<TInputImage1,TInputImage2,TOutputImage,TFunction>
::GenerateData( void )
{

  RegionType region = this->m_OutputImage->GetRequestedRegion();

  Image1Iterator it1( this->m_Image1, region );
  Image2Iterator it2( this->m_Image2, region );

  ImageOutputIterator ot( this->m_OutputImage, region );

  it1.Begin();
  it2.Begin();

  ot.Begin();

  TFunction function;

  while( !it1.IsAtEnd() ) 
  {
    ot.Set( function( it1.Get(), it2.Get()  ) );
    ++it1;
    ++it2;
    ++ot;
  }


}





} // end namespace itk
