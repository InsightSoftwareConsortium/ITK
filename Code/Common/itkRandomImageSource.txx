/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomImageSource.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkRandomImageSource.h"
#include "itkPixelTraits.h"
#include "itkObjectFactory.h"

//------------------------------------------------------------------------
template <class TOutputImage>
itkRandomImageSource<TOutputImage>::Pointer 
itkRandomImageSource<TOutputImage>
::New()
{
  itkRandomImageSource<TOutputImage>* ret = 
    itkObjectFactory< itkRandomImageSource<TOutputImage> >::Create();
  if ( ret )
    {
    return ret;
    }
  return new itkRandomImageSource<TOutputImage>;
}

//----------------------------------------------------------------------------
template <class TOutputImage>
itkRandomImageSource<TOutputImage>
::itkRandomImageSource()
{
}

//----------------------------------------------------------------------------
template <class TOutputImage>
void 
itkRandomImageSource<TOutputImage>
::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkImageSource<TOutputImage>::PrintSelf(os,indent);

}

// Microsoft compiler defines these and screws up the traits

//----------------------------------------------------------------------------
template <class TOutputImage>
void 
itkRandomImageSource<TOutputImage>
::Execute()
{
  TOutputImage *image=this->GetOutput(0);
  unsigned int imageDimension = image->GetImageDimension();
  TOutputImage::Index ind;
  long index[2];
  TOutputImage::PixelType pixel;

  TOutputImage::ScalarValueType min = 
    itkNumericTraits<TOutputImage::ScalarValueType>::min();
  TOutputImage::ScalarValueType max = 
    itkNumericTraits<TOutputImage::ScalarValueType>::max();
  TOutputImage::ScalarValueType value;
  unsigned int pixelDimension = pixel.GetPixelDimension();

  itkDebugMacro(<<"Generating random image");
  
  index[0] = 0;
  index[1] = 0;
  ind.SetIndex(index);
  
  pixel.SetScalar(min);

  value = (min + max) / 2.0;

  
 // for (int j=0; j<imageSize; j++)
  
}
