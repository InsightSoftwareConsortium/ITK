/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryThinningImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBinaryThinningImageFilter_txx
#define _itkBinaryThinningImageFilter_txx

#include <iostream>

#include "itkBinaryThinningImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodIterator.h"


namespace itk
{

/**
 *    Constructor
 */
template <class TInputImage,class TOutputImage>
BinaryThinningImageFilter<TInputImage,TOutputImage>
::BinaryThinningImageFilter()
{

  this->SetNumberOfRequiredOutputs( 1 );

  OutputImagePointer thinImage = OutputImageType::New();
  this->SetNthOutput( 0, thinImage.GetPointer() );

}

/**
 *  Return the thinning Image pointer
 */
template <class TInputImage,class TOutputImage>
typename BinaryThinningImageFilter<
  TInputImage,TOutputImage>::OutputImageType * 
BinaryThinningImageFilter<TInputImage,TOutputImage>
::GetThinning(void)
{
  return  dynamic_cast< OutputImageType * >(
    this->ProcessObject::GetOutput(0) );
}


/**
 *  Prepare data for computation
 */
template <class TInputImage,class TOutputImage>
void 
BinaryThinningImageFilter<TInputImage,TOutputImage>
::PrepareData(void) 
{
  
  itkDebugMacro(<< "PrepareData Start");
  OutputImagePointer thinImage = GetThinning();

  InputImagePointer  inputImage  = 
    dynamic_cast<const TInputImage  *>( ProcessObject::GetInput(0) );

  thinImage->SetLargestPossibleRegion( 
    inputImage->GetLargestPossibleRegion() );

  thinImage->SetBufferedRegion( 
    inputImage->GetBufferedRegion() );

  thinImage->SetRequestedRegion( 
    inputImage->GetRequestedRegion() );

  thinImage->Allocate();

  typename OutputImageType::RegionType region  = thinImage->GetRequestedRegion();


  ImageRegionConstIterator< TInputImage >  it( inputImage,  region );
  ImageRegionIterator< TOutputImage > ot( thinImage,  region );

  it.GoToBegin();
  ot.GoToBegin();

  itkDebugMacro(<< "PrepareData: Copy input to output");
 
  while( !ot.IsAtEnd() )
      {
      ot.Set( static_cast< typename OutputImageType::PixelType >( it.Get() ) );
      ++it;
      ++ot;
      }
  itkDebugMacro(<< "PrepareData End");    
}

/**
 *  Post processing for computing thinning
 */
template <class TInputImage,class TOutputImage>
void 
BinaryThinningImageFilter<TInputImage,TOutputImage>
::ComputeThinImage() 
{

  itkDebugMacro( << "ComputeThinImage Start");
  OutputImagePointer    thinImage          =  GetThinning();

  typename OutputImageType::RegionType region  = thinImage->GetRequestedRegion();

  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill(1);
  NeighborhoodIteratorType ot( radius, thinImage, region );


  typename NeighborhoodIteratorType::OffsetType offset1 = {{-1,-1}};
  typename NeighborhoodIteratorType::OffsetType offset2 = {{-1,0}};
  typename NeighborhoodIteratorType::OffsetType offset3 = {{-1,1 }};
  typename NeighborhoodIteratorType::OffsetType offset4 = {{0,1}};
  typename NeighborhoodIteratorType::OffsetType offset5 = {{1,1}};
  typename NeighborhoodIteratorType::OffsetType offset6 = {{1,0}};
  typename NeighborhoodIteratorType::OffsetType offset7 = {{1,-1}};
  typename NeighborhoodIteratorType::OffsetType offset8 = {{0,-1}};


  
  int count = 1;
  while(count)
    {   
    count = 0;
    ot.GoToBegin();
    while( ! ot.IsAtEnd() )
      {
      if (ot.GetCenterPixel())
       {
         PixelType genus;
         genus  = ot.GetPixel(offset1) + ot.GetPixel(offset2);
         genus += ot.GetPixel(offset3) + ot.GetPixel(offset4);
         genus += ot.GetPixel(offset5) + ot.GetPixel(offset6);
         genus += ot.GetPixel(offset7) + ot.GetPixel(offset8);
         if (genus != 1)
          {
            genus += ot.GetPixel(offset8) * ot.GetPixel(offset1) * ot.GetPixel(offset2);
            genus += ot.GetPixel(offset2) * ot.GetPixel(offset3) * ot.GetPixel(offset4);
            genus += ot.GetPixel(offset4) * ot.GetPixel(offset5) * ot.GetPixel(offset6);
            genus += ot.GetPixel(offset6) * ot.GetPixel(offset7) * ot.GetPixel(offset8);
            genus -= ot.GetPixel(offset1) * ot.GetPixel(offset2);
            genus -= ot.GetPixel(offset2) * ot.GetPixel(offset3);
            genus -= ot.GetPixel(offset3) * ot.GetPixel(offset4);
            genus -= ot.GetPixel(offset4) * ot.GetPixel(offset5);
            genus -= ot.GetPixel(offset5) * ot.GetPixel(offset6);
            genus -= ot.GetPixel(offset6) * ot.GetPixel(offset7);
            genus -= ot.GetPixel(offset7) * ot.GetPixel(offset8);
            genus -= ot.GetPixel(offset8) * ot.GetPixel(offset1);
            genus -= ot.GetPixel(offset8) * ot.GetPixel(offset2);
            genus -= ot.GetPixel(offset2) * ot.GetPixel(offset4);
            genus -= ot.GetPixel(offset4) * ot.GetPixel(offset6);
            genus -= ot.GetPixel(offset6) * ot.GetPixel(offset8);
            --genus;

            if ( genus == 0 )
             {
                ot.SetCenterPixel( genus );
                ++count;
              }
          }
       }

      ++ot;
      }

    }  
    itkDebugMacro( << "ComputeThinImage End");
}

/**
 *  Generate ThinImage
 */
template <class TInputImage,class TOutputImage>
void 
BinaryThinningImageFilter<TInputImage,TOutputImage>
::GenerateData() 
{

  this->PrepareData();

  itkDebugMacro(<< "GenerateData: Computing Thinning Image");
  this->ComputeThinImage();

 

} // end GenerateData()

/**
 *  Print Self
 */
template <class TInputImage,class TOutputImage>
void 
BinaryThinningImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Thinning image: " << std::endl;

}



} // end namespace itk

#endif
