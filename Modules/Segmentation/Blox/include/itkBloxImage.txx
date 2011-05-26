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
#ifndef __itkBloxImage_txx
#define __itkBloxImage_txx

#include "itkBloxImage.h"

namespace itk
{
template< typename TBloxPixelType, unsigned int VImageDimension >
BloxImage< TBloxPixelType, VImageDimension >
::BloxImage()
{}

template< typename TBloxPixelType, unsigned int VImageDimension >
BloxImage< TBloxPixelType, VImageDimension >
::~BloxImage()
{}

template< typename TBloxPixelType, unsigned int VImageDimension >
void
BloxImage< TBloxPixelType, VImageDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template< typename TBloxPixelType, unsigned int VImageDimension >
void
BloxImage< TBloxPixelType, VImageDimension >
::EmptyImage()
{
  const unsigned long numberOfPixels =
    this->GetBufferedRegion().GetNumberOfPixels();

  for ( unsigned int i = 0; i < numberOfPixels; i++ )
    {
    this->GetBufferPointer()[i].DeleteListEntries();
    }
}
} // end namespace itk

#endif
