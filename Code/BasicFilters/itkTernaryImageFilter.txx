/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTernaryImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkTernaryImageFilter.h"


#include <itkSimpleImageRegionIterator.h>

namespace itk
{

/**
 * Constructor
 */
template < class TInputImage1, class TInputImage2, 
           class TInputImage3, class TOutputImage, class TFunction  >
TernaryImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::TernaryImageFilter()
{
  m_OutputImage = GetOutput();
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::SetInput1( Image1Pointer image1 ) 
{
  this->m_Image1 = image1;
  SetNthInput(0, image1.GetPointer() );
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::SetInput2( Image2Pointer image2 ) 
{
  this->m_Image2 = image2;
  SetNthInput(1, image2.GetPointer());
}



/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::SetInput3( Image3Pointer image3 ) 
{
  this->m_Image3 = image3;
  SetNthInput(2, image3.GetPointer());
}





/**
 * GenerateDatas filter. Performs the pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::GenerateData( void )
{

  m_OutputImage->SetLargestPossibleRegion( 
      m_Image1->GetLargestPossibleRegion() );

  m_OutputImage->SetBufferedRegion( 
      m_Image1->GetBufferedRegion() );

  m_OutputImage->SetRequestedRegion( 
      m_Image1->GetRequestedRegion() );

  m_OutputImage->Allocate();


  RegionType region = this->m_OutputImage->GetRequestedRegion();

  Image1Iterator it1( this->m_Image1, region );
  Image2Iterator it2( this->m_Image2, region );
  Image3Iterator it3( this->m_Image3, region );

  ImageOutputIterator ot( this->m_OutputImage, region );

  it1.Begin();
  it2.Begin();
  it3.Begin();

  ot.Begin();

  TFunction function;

  while( !it1.IsAtEnd() ) 
  {
    ot.Set( function( it1.Get(), it2.Get(), it3.Get() ) );
    ++it1;
    ++it2;
    ++it3;
    ++ot;
  }


}





} // end namespace itk
