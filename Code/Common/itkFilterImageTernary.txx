/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageTernary.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkFilterImageTernary.h"


#include <itkImageRegionSimpleIterator.h>

namespace itk
{

/**
 * Constructor
 */
template < class TInputImage1, class TInputImage2, 
           class TInputImage3, class TOutputImage, class TFunction  >
FilterImageTernary<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::FilterImageTernary()
{
  m_OutputImage = GetOutput();
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
FilterImageTernary<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
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
FilterImageTernary<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::SetInput2( Image2Pointer image2 ) 
{
  this->m_Image2 = image2;
  SetNthInput(0, image2.GetPointer());
}



/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
FilterImageTernary<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::SetInput3( Image3Pointer image3 ) 
{
  this->m_Image3 = image3;
  SetNthInput(0, image3.GetPointer());
}





/**
 * GenerateDatas filter. Performs the pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
FilterImageTernary<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::GenerateData( void )
{

  m_OutputImage->SetLargestPossibleRegion( 
      this->m_Image1->GetLargestPossibleRegion() );

  m_OutputImage->SetBufferedRegion( 
      this->m_Image1->GetBufferedRegion() );

  m_OutputImage->Allocate();

  Image1Iterator it1(this->m_Image1, this->m_Image1->GetBufferedRegion() );
  Image2Iterator it2(this->m_Image2, this->m_Image2->GetBufferedRegion() );
  Image3Iterator it3(this->m_Image3, this->m_Image3->GetBufferedRegion() );

  ImageOutputIterator ot(m_OutputImage, m_OutputImage->GetBufferedRegion());

  it1.Begin();
  it2.Begin();
  it3.Begin();

  ot.Begin();

  TFunction function;

  while( !it1.IsAtEnd() ) 
  {
    *ot = function(*it1,*it2,*it3);
    ++it1;
    ++it2;
    ++it3;
    ++ot;
  }


}





} // end namespace itk
