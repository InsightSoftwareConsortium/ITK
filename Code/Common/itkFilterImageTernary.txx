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
 * Executes filter. Performs the pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
FilterImageTernary<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::Execute( void )
{

  m_OutputImage->SetImageSize(  this->m_Image1->GetImageSize() );
  m_OutputImage->SetBufferSize( this->m_Image1->GetBufferSize() );
  m_OutputImage->Allocate();
  m_OutputImage->SetImageStartIndex(  this->m_Image1->GetImageStartIndex() );
  m_OutputImage->SetBufferStartIndex( this->m_Image1->GetBufferStartIndex() );

  Image1Iterator it1(this->m_Image1, this->m_Image1->GetBufferStartIndex(),
                     this->m_Image1->GetBufferSize());

  Image2Iterator it2(this->m_Image2, this->m_Image2->GetBufferStartIndex(),
                     this->m_Image2->GetBufferSize());

  Image3Iterator it3(this->m_Image3, this->m_Image3->GetBufferStartIndex(),
                     this->m_Image3->GetBufferSize());

  ImageOutputIterator ot(m_OutputImage, m_OutputImage->GetBufferStartIndex(),
                         m_OutputImage->GetBufferSize());

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
