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
 * Executes filter. Performs the pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TOutputImage, class TFunction  >
void
FilterImageBinary<TInputImage1,TInputImage2,TOutputImage,TFunction>
::Execute( void )
{

  m_OutputImage->SetImageSize(  m_Image1->GetImageSize() );
  m_OutputImage->SetBufferSize( m_Image1->GetBufferSize() );
  m_OutputImage->Allocate();
  m_OutputImage->SetImageStartIndex(  m_Image1->GetImageStartIndex() );
  m_OutputImage->SetBufferStartIndex( m_Image1->GetBufferStartIndex() );

  Image1Iterator it1(m_Image1, m_Image1->GetBufferStartIndex(),
                     m_Image1->GetBufferSize());
  
  Image2Iterator it2(m_Image2, m_Image2->GetBufferStartIndex(),
                     m_Image2->GetBufferSize());
  
  Image3Iterator ot(m_OutputImage, m_OutputImage->GetBufferStartIndex(),
                     m_OutputImage->GetBufferSize());

  it1.Begin();
  it2.Begin();

  ot.Begin();

  TFunction function;

  while( !it1.IsAtEnd() ) 
  {
    *ot = function(*it1,*it2);
    ++it1;
    ++it2;
    ++ot;
  }


}





} // end namespace itk
