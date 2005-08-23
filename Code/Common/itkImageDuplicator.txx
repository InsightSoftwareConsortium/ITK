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
#ifndef _itkImageDuplicator_txx
#define _itkImageDuplicator_txx

#include "itkImageDuplicator.h"


namespace itk
{ 
    
/** Constructor */
template<class TInputImage>
ImageDuplicator<TInputImage>
::ImageDuplicator()
{
  m_InputImage = NULL;
  m_Output = NULL;
  m_InternalImageTime = 0;
}

/** */
template<class TInputImage>
void
ImageDuplicator<TInputImage>
::Update(void)
{
  if(!m_InputImage )
    {
    itkExceptionMacro(<<"Input image has not been connected");
    return;
    }

  // Update only if the input image has been modified
  unsigned long t, t1, t2;
  t1 = m_InputImage->GetPipelineMTime();
  t2 = m_InputImage->GetMTime();
  t = (t1 > t2 ? t1 : t2);

  if(t == m_InternalImageTime) 
    {
    return; // No need to update
    }

  // Cache the timestamp
  m_InternalImageTime = t;

  // Allocate the image
  m_Output = ImageType::New();
  m_Output->SetOrigin(m_InputImage->GetOrigin());
  m_Output->SetSpacing(m_InputImage->GetSpacing());
  m_Output->SetDirection(m_InputImage->GetDirection());
  m_Output->SetLargestPossibleRegion(m_InputImage->GetLargestPossibleRegion());
  m_Output->SetRequestedRegion(m_InputImage->GetRequestedRegion());
  m_Output->SetBufferedRegion(m_InputImage->GetBufferedRegion());
  m_Output->Allocate();

  // Do the copy
  unsigned long size = 1;
  for(unsigned int i=0;i<itkGetStaticConstMacro(ImageDimension);i++)
    {
    size *= m_InputImage->GetBufferedRegion().GetSize()[i];
    }

  memcpy(m_Output->GetBufferPointer(),m_InputImage->GetBufferPointer(),size*sizeof(PixelType));

}

template<class TInputImage>
void
ImageDuplicator<TInputImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Input Image: " << m_InputImage << std::endl;
  os << indent << "Output Image: " << m_Output << std::endl;
  os << indent << "Internal Image Time: " << m_InternalImageTime << std::endl;
}

} // end namespace itk

#endif
