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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkSpecialCoordinatesImage_hxx
#define itkSpecialCoordinatesImage_hxx
#include "itkSpecialCoordinatesImage.h"
#include "itkProcessObject.h"

namespace itk
{

template< typename TPixel, unsigned int VImageDimension >
SpecialCoordinatesImage< TPixel, VImageDimension >
::SpecialCoordinatesImage()
{
  m_Buffer = PixelContainer::New();
}

template< typename TPixel, unsigned int VImageDimension >
void
SpecialCoordinatesImage< TPixel, VImageDimension >
::Allocate(bool initialize)
{
  SizeValueType num;

  this->ComputeOffsetTable();
  num = static_cast<SizeValueType>(this->GetOffsetTable()[VImageDimension]);

  m_Buffer->Reserve(num,initialize);
}

template< typename TPixel, unsigned int VImageDimension >
void
SpecialCoordinatesImage< TPixel, VImageDimension >
::Initialize()
{
  //
  // We don't modify ourselves because the "ReleaseData" methods depend upon
  // no modification when initialized.
  //

  // Call the superclass which should initialize the BufferedRegion ivar.
  Superclass::Initialize();

  // Replace the handle to the buffer. This is the safest thing to do,
  // since the same container can be shared by multiple images (e.g.
  // Grafted outputs and in place filters).
  m_Buffer = PixelContainer::New();
}

template< typename TPixel, unsigned int VImageDimension >
void
SpecialCoordinatesImage< TPixel, VImageDimension >
::FillBuffer(const TPixel & value)
{
  const SizeValueType numberOfPixels =
    this->GetBufferedRegion().GetNumberOfPixels();

  for ( unsigned int i = 0; i < numberOfPixels; i++ )
    {
    ( *m_Buffer )[i] = value;
    }
}

template< typename TPixel, unsigned int VImageDimension >
void
SpecialCoordinatesImage< TPixel, VImageDimension >
::SetPixelContainer(PixelContainer *container)
{
  if ( m_Buffer != container )
    {
    m_Buffer = container;
    this->Modified();
    }
}

template< typename TPixel, unsigned int VImageDimension >
void
SpecialCoordinatesImage< TPixel, VImageDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "PixelContainer: " << std::endl;
  m_Buffer->Print( os, indent.GetNextIndent() );

  // m_Origin and m_Spacing are printed in the Superclass
}
} // end namespace itk

#endif
