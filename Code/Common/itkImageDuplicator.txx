/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkImageDuplicator.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageDuplicator_txx
#define __itkImageDuplicator_txx

#include "itkImageDuplicator.h"

namespace itk
{
/** Constructor */
template< class TInputImage >
ImageDuplicator< TInputImage >
::ImageDuplicator()
{
  m_InputImage = NULL;
  m_Output = NULL;
  m_InternalImageTime = 0;
}

/** */
template< class TInputImage >
void
ImageDuplicator< TInputImage >
::Update(void)
{
  if ( !m_InputImage )
    {
    itkExceptionMacro(<< "Input image has not been connected");
    return;
    }

  // Update only if the input image has been modified
  const unsigned long t1 = m_InputImage->GetPipelineMTime();
  const unsigned long t2 = m_InputImage->GetMTime();
  const unsigned long t = ( t1 > t2 ? t1 : t2 );

  if ( t == m_InternalImageTime )
    {
    return; // No need to update
    }

  // Cache the timestamp
  m_InternalImageTime = t;

  // Allocate the image
  m_Output = ImageType::New();
  m_Output->CopyInformation( m_InputImage );
  m_Output->SetRequestedRegion( m_InputImage->GetRequestedRegion() );
  m_Output->SetBufferedRegion( m_InputImage->GetBufferedRegion() );
  m_Output->Allocate();

  // Do the copy
  typedef typename TInputImage::PixelContainer   PixelContainer;
  const PixelContainer * pixelContainer = m_InputImage->GetPixelContainer();

  // This is the number of pixels times the number of components per pixel
  const typename TInputImage::SizeValueType sizeInNumberOfComponents = pixelContainer->Size();

  // This must be the internal pixel type, which is the one that we actually
  // use for allocating internal buffers.
  typedef typename TInputImage::InternalPixelType   InternalPixelType;

  const typename TInputImage::SizeValueType sizeOfComponentInBytes = sizeof( InternalPixelType );
  const typename TInputImage::SizeValueType sizeInNumberOfBytes = sizeInNumberOfComponents * sizeOfComponentInBytes;

  memcpy( m_Output->GetBufferPointer(), m_InputImage->GetBufferPointer(), sizeInNumberOfBytes );
}

template< class TInputImage >
void
ImageDuplicator< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Input Image: " << m_InputImage << std::endl;
  os << indent << "Output Image: " << m_Output << std::endl;
  os << indent << "Internal Image Time: " << m_InternalImageTime << std::endl;
}
} // end namespace itk

#endif
