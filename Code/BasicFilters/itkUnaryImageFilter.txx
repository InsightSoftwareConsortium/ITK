/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUnaryImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkUnaryImageFilter.h"


#include <itkSimpleImageRegionIterator.h>

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage, class TFunction  >
UnaryImageFilter<TInputImage,TOutputImage,TFunction>
::UnaryImageFilter()
{
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage, class TOutputImage, class TFunction  >
void
UnaryImageFilter<TInputImage,TOutputImage,TFunction>
::SetInput( TInputImage * image ) 
{
  SetNthInput(0, image1 );
}



/**
 * GenerateData Performs the pixel-wise addition
 */
template <class TInputImage, class TOutputImage, class TFunction  >
void
UnaryImageFilter<TInputImage,TOutputImage,TFunction>
::GenerateData( void )
{
  
  typename TOutputImage::Pointer outputImage = dynamic_cast<TOutputImage *>(
                      (ProcessObject::GetOutput( 0 )).GetPointer());

  typename TInputImage::Pointer  inputImage  = dynamic_cast<TInputImage  *>(
                      (ProcessObject::GetInput(  0 )).GetPointer());

  outputImage->SetLargestPossibleRegion( 
                  inputImage->GetLargestPossibleRegion() );

  outputImage->SetBufferedRegion( 
                  inputImage->GetBufferedRegion() );

  outputImage->SetRequestedRegion( 
                  inputImage->GetRequestedRegion() );

  outputImage->Allocate();

  typename TOutputImage::RegionType region  = outputImage->GetRequestedRegion();

  SimpleImageRegionIterator< TInputImage >  it( inputImage,  region );
  SimpleImageRegionIterator< TOutputImage > ot( outputImage, region );

  it.Begin();
  ot.Begin();

  TFunction function;

  while( !it.IsAtEnd() ) 
  {
    ot.Set( function( it.Get() ) );
    ++it;
    ++ot;
  }


}





} // end namespace itk
