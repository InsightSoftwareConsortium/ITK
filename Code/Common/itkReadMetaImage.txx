/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReadMetaImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkReadMetaImage.h"
#include "itkObjectFactory.h"
#include "itkImageRegionSimpleIterator.h"

namespace itk
{

/**
 *
 */
template <class TOutputImage>
ReadMetaImage<TOutputImage>
::ReadMetaImage()
{
  m_FileName = "";
}


/**
 *
 */
template <class TOutputImage>
void 
ReadMetaImage<TOutputImage>
::Execute()
{
  
  m_MetaImage.OpenMetaFile(m_FileName.c_str(),true);
  if( m_MetaImage.Error() ) 
  {
    throw ExceptionObject();
  }

  if( m_MetaImage.NDims() != TOutputImage::ImageDimension )
  {
    throw ExceptionObject();
  }
  
  typename TOutputImage::Pointer m_OutputImage( GetOutput() );

  m_OutputImage->SetSpacing( m_MetaImage.ElemSize() );

  unsigned long dimSize[ TOutputImage::ImageDimension ];

  for(unsigned int i=0; i<TOutputImage::ImageDimension; i++) 
  {
    dimSize[i] = m_MetaImage.DimSize(i);
  }

  m_OutputImage->SetImageSize( dimSize );
  m_OutputImage->SetBufferSize( dimSize );
  m_OutputImage->Allocate();

  const long startPosition[] = { 0, 0, 0 };
  typename TOutputImage::Index start;
  start.SetIndex( startPosition );
     
  m_OutputImage->SetImageStartIndex( start );
  m_OutputImage->SetBufferStartIndex( start );

  typedef typename TOutputImage::PixelType  PixelType;

  typedef itk::ImageRegionSimpleIterator<PixelType,
                  TOutputImage::ImageDimension> IteratorType;
  
  IteratorType it(m_OutputImage,
                  m_OutputImage->GetBufferStartIndex(),
                  m_OutputImage->GetBufferSize() );

  PixelType * source = (PixelType *)m_MetaImage.Get();

  
  it.Begin();
  while( !it.IsAtEnd() ) 
  {
    *it = *source++;
    ++it;
  }


}


/**
 *
 */
template <class TOutputImage>
void 
ReadMetaImage<TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  ImageSource<TOutputImage>::PrintSelf(os,indent);

}

} // end namespace itk
