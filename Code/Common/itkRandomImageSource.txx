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

ITK_NAMESPACE_BEGIN

/**
 *
 */
template <class TOutputImage>
RandomImageSource<TOutputImage>
::RandomImageSource()
{
  m_Size = new unsigned long [TOutputImage::GetImageDimension()];
  m_Spacing = new float [TOutputImage::GetImageDimension()];
  m_Origin = new float [TOutputImage::GetImageDimension()];  
}


template <class TOutputImage>
RandomImageSource<TOutputImage>
::~RandomImageSource()
{
  delete [] m_Size;
  delete [] m_Spacing;
  delete [] m_Origin;
}


/**
 *
 */
template <class TOutputImage>
void 
RandomImageSource<TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  ImageSource<TOutputImage>::PrintSelf(os,indent);
}


/**
 * Microsoft compiler defines these and screws up the traits
 */

//----------------------------------------------------------------------------
/**
 *
 */
template <class TOutputImage>
void 
RandomImageSource<TOutputImage>
::Execute()
{
  typedef typename TOutputImage::ScalarValueType scalarType;

  TOutputImage *image=this->GetOutput(0);
  unsigned int N = image->GetImageDimension();

  scalarType min = NumericTraits<scalarType>::min();
  scalarType max = NumericTraits<scalarType>::max();
  typename TOutputImage::ScalarIterator scalarIterator = image->ScalarBegin();
  typename TOutputImage::ScalarIterator scalarEnd = image->ScalarEnd();

  itkDebugMacro(<<"Generating a random image of scalars");
  
  for ( scalarIterator=image->ScalarBegin(); 
        scalarIterator != scalarEnd; ++scalarIterator )
    {
    *scalarIterator = (min + max) / 2.0;
    }
}

ITK_NAMESPACE_END
