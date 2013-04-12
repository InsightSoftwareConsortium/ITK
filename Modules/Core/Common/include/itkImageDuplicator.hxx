/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkImageDuplicator_hxx
#define __itkImageDuplicator_hxx

#include "itkImageDuplicator.h"
#include "itkStdAlgorithm.h"

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
  const ModifiedTimeType t1 = m_InputImage->GetPipelineMTime();
  const ModifiedTimeType t2 = m_InputImage->GetMTime();
  const ModifiedTimeType t = ( t1 > t2 ? t1 : t2 );

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
  const SizeValueType sizeInNumberOfComponents = pixelContainer->Size();

  // This must be the internal pixel type, which is the one that we actually
  // use for allocating internal buffers.
  typedef typename TInputImage::InternalPixelType   InternalPixelType;

  itk::algorithm::copy_n(m_InputImage->GetBufferPointer(),sizeInNumberOfComponents,
              m_Output->GetBufferPointer());
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
