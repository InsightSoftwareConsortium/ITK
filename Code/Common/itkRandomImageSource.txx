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
#include "itkImageScalarRegionIterator.h"

namespace itk
{

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

  for (int i=0; i<TOutputImage::GetImageDimension(); i++)
    {
    m_Size[i] = 64;
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0.0;
    }
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
template <typename TOutputImage>
void 
RandomImageSource<TOutputImage>
::ExecuteInformation()
{
  TOutputImage *output;
  typename TOutputImage::Index index = {0};
  
  output = this->GetOutput(0);
  
  output->SetImageSize( m_Size );
  output->SetImageStartIndex( index );
  output->SetSpacing(m_Spacing);
  output->SetOrigin(m_Origin);
}

//----------------------------------------------------------------------------
template <typename TOutputImage>
void 
RandomImageSource<TOutputImage>
::Execute()
{
  typedef typename TOutputImage::ScalarValueType scalarType;

  typename TOutputImage::Pointer image=this->GetOutput(0);

  image->SetBufferSize(image->GetRegionSize());
  image->SetBufferStartIndex( image->GetRegionStartIndex() );
  image->Allocate();

  scalarType min = NumericTraits<scalarType>::min();
  scalarType max = NumericTraits<scalarType>::max();

  ImageScalarRegionIterator<OutputImagePixelType, TOutputImage::ImageDimension>
    scalarIterator(image, image->GetRegionStartIndex(),image->GetRegionSize());

  itkDebugMacro(<<"Generating a random image of scalars");

  for ( ; !scalarIterator.IsAtEnd(); ++scalarIterator)
    {
    *scalarIterator = (min + max) / 2.0;
    }
}

} // end namespace itk
