/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTernaryImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkTernaryImageFilter_txx
#define _itkTernaryImageFilter_txx

#include "itkTernaryImageFilter.h"
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
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::SetInput1( TInputImage1 *image1 ) 
{
  SetNthInput(0, image1 );
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::SetInput2( TInputImage2 *image2 ) 
{
  SetNthInput(1, image2 );
}



/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::SetInput3( TInputImage3 *image3 ) 
{
  SetNthInput(2, image3 );
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

  typename TOutputImage::Pointer outputImage = dynamic_cast<TOutputImage *>(
                      (ProcessObject::GetOutput( 0 )).GetPointer());

  typename TInputImage1::Pointer inputImage1  = 
				dynamic_cast<TInputImage1  *>(
                      (ProcessObject::GetInput(  0 )).GetPointer());

  typename TInputImage2::Pointer inputImage2  = 
				dynamic_cast<TInputImage2  *>(
                      (ProcessObject::GetInput(  1 )).GetPointer());

  typename TInputImage3::Pointer inputImage3  = 
				dynamic_cast<TInputImage3  *>(
                      (ProcessObject::GetInput(  2 )).GetPointer());

  outputImage->SetLargestPossibleRegion( 
      inputImage1->GetLargestPossibleRegion() );

  outputImage->SetBufferedRegion( 
      inputImage1->GetBufferedRegion() );

  outputImage->SetRequestedRegion( 
      inputImage1->GetRequestedRegion() );

  outputImage->Allocate();


  typename TOutputImage::RegionType region = outputImage->GetRequestedRegion();

  SimpleImageRegionIterator< TInputImage1 > it1( inputImage1, region );
  SimpleImageRegionIterator< TInputImage2 > it2( inputImage2, region );
  SimpleImageRegionIterator< TInputImage3 > it3( inputImage3, region );

  SimpleImageRegionIterator< TOutputImage > ot( outputImage, region );

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

#endif
