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
 * copy information from first input to the outputs
 * The default implementation of itkProcessObject
 * should not be used because it calls CopyInformation()
 * on the outputObject, and for the case of itkImage this
 * method assumes that the input image is exactly of the
 * same type as the output image. (and... that they are 
 * really images)
 */
template <class TInputImage, class TOutputImage, class TFunction  >
void
UnaryImageFilter<TInputImage,TOutputImage,TFunction>
::GenerateOutputInformation()
{
  typename TOutputImage::Pointer outputImage = dynamic_cast<TOutputImage *>(
                      (ProcessObject::GetOutput( 0 )).GetPointer());

  typename TInputImage::Pointer inputImage  = 
				dynamic_cast<TInputImage  *>(
                      (ProcessObject::GetInput(  0 )).GetPointer());

  if ( outputImage && inputImage1 )
  {
    // this is the equivalent of the CopyInformation() method
    // defined in itkImage.
    outputImage->SetLargestPossibleRegion( 
      inputImage->GetLargestPossibleRegion() );
  }
}


/**
 * Define the size of the input region required to 
 * generate a requested output region
 */
template <class TInputImage, class TOutputImage, class TFunction  >
void
UnaryImageFilter<TInputImage,TOutputImage,TFunction>
::GenerateInputRequestedRegion()
{

  Superclass::GenerateInputRequestedRegion();

  typename TOutputImage::Pointer outputImage =
        dynamic_cast<TOutputImage *>(
                      (ProcessObject::GetOutput( 0 )).GetPointer());

  typename TInputImage::Pointer inputImage  = 
				dynamic_cast<TInputImage  *>(
                      (ProcessObject::GetInput(  0 )).GetPointer());

  if( inputImage )
  {
    inputImage->SetRequestedRegion( outputImage->GetRequestedRegion() );
  }


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
