/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkBinaryImageFilter.h"


#include <itkSimpleImageRegionIterator.h>

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage1, class TInputImage2, 
          class TOutputImage, class TFunction  >
BinaryImageFilter<TInputImage1,TInputImage2,TOutputImage,TFunction>
::BinaryImageFilter()
{
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TOutputImage, class TFunction  >
void
BinaryImageFilter<TInputImage1,TInputImage2,TOutputImage,TFunction>
::SetInput1( TInputImage1 * image1 ) 
{
  SetNthInput(0, image1 );
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TOutputImage, class TFunction  >
void
BinaryImageFilter<TInputImage1,TInputImage2,TOutputImage,TFunction>
::SetInput2( TInputImage2 * image2 ) 
{
  SetNthInput(1, image2 );
}



/**
 * GenerateData Performs the pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TOutputImage, class TFunction  >
void
BinaryImageFilter<TInputImage1,TInputImage2,TOutputImage,TFunction>
::GenerateData( void )
{
  
  typename TOutputImage::Pointer outputImage = GetOutput();


  typename TInputImage1::Pointer inputImage1  = 
				dynamic_cast<TInputImage1  *>(
                      (ProcessObject::GetInput(  0 )).GetPointer());

  typename TInputImage2::Pointer inputImage2  = 
				dynamic_cast<TInputImage2  *>(
                      (ProcessObject::GetInput(  1 )).GetPointer());


  outputImage->SetLargestPossibleRegion( 
      inputImage1->GetLargestPossibleRegion() );

  outputImage->SetBufferedRegion( 
      inputImage1->GetBufferedRegion() );

  outputImage->SetRequestedRegion( 
      inputImage1->GetRequestedRegion() );

  outputImage->Allocate();

  typedef typename TOutputImage::RegionType RegionType;

  RegionType region  = outputImage->GetRequestedRegion();
  RegionType region1 = inputImage1->GetRequestedRegion();
  RegionType region2 = inputImage2->GetRequestedRegion();

  SimpleImageRegionIterator< TInputImage1 > it1( inputImage1, region1 );
  SimpleImageRegionIterator< TInputImage2 > it2( inputImage2, region2 );
  SimpleImageRegionIterator< TOutputImage > ot(  outputImage, region  );

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
