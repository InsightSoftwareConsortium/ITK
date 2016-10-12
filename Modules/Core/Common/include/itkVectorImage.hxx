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
#ifndef itkVectorImage_hxx
#define itkVectorImage_hxx
#include "itkVectorImage.h"
#include "itkProcessObject.h"

namespace itk
{
/**
 *
 */
template< typename TPixel, unsigned int VImageDimension >
VectorImage< TPixel, VImageDimension >
::VectorImage():
  m_VectorLength(0)
{
  m_Buffer = PixelContainer::New();
}

//----------------------------------------------------------------------------
template< typename TPixel, unsigned int VImageDimension >
void
VectorImage< TPixel, VImageDimension >
::Allocate(const bool UseDefaultConstructor)
{
  if ( m_VectorLength == 0 )
    {
    itkExceptionMacro(<< "Cannot allocate VectorImage with VectorLength = 0");
    }

  SizeValueType num;
  this->ComputeOffsetTable();
  num = this->GetOffsetTable()[VImageDimension];

  m_Buffer->Reserve(num * m_VectorLength,UseDefaultConstructor);
}

template< typename TPixel, unsigned int VImageDimension >
void
VectorImage< TPixel, VImageDimension >
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
VectorImage< TPixel, VImageDimension >
::FillBuffer(const PixelType & value)
{
  const SizeValueType numberOfPixels =
    this->GetBufferedRegion().GetNumberOfPixels();

  SizeValueType ctr = 0;

  for ( SizeValueType i = 0; i < numberOfPixels; i++ )
    {
    for ( VectorLengthType j = 0; j < m_VectorLength; j++ )
      {
      ( *m_Buffer )[ctr++] = value[j];
      }
    }
}

template< typename TPixel, unsigned int VImageDimension >
void
VectorImage< TPixel, VImageDimension >
::SetPixelContainer(PixelContainer *container)
{
  if ( m_Buffer != container )
    {
    m_Buffer = container;
    this->Modified();
    }
}

//----------------------------------------------------------------------------
template< typename TPixel, unsigned int VImageDimension >
void
VectorImage< TPixel, VImageDimension >
::Graft(const Self *image)
{
  if(image == ITK_NULLPTR)
    {
    return;
    }
  // call the superclass' implementation
  Superclass::Graft(image);

  // Copy from VectorImage< TPixel, dim >
  // Now copy anything remaining that is needed
  this->SetPixelContainer( const_cast< PixelContainer * >
                           ( image->GetPixelContainer() ) );
}

//----------------------------------------------------------------------------
template< typename TPixel, unsigned int VImageDimension >
void
VectorImage< TPixel, VImageDimension >
::Graft(const DataObject *data)
{
  if(data == ITK_NULLPTR)
    {
    return;
    }
  // Attempt to cast data to an Image
  const Self *imgData = dynamic_cast< const Self * >( data );

  if( imgData == ITK_NULLPTR )
    {
    // pointer could not be cast back down
    itkExceptionMacro( << "itk::VectorImage::Graft() cannot cast "
                       << typeid( data ).name() << " to "
                       << typeid( const Self * ).name() );
    }
  // Copy from VectorImage< TPixel, dim >
  // Now copy anything remaining that is needed
  this->Graft(imgData);
}

//----------------------------------------------------------------------------
template< typename TPixel, unsigned int VImageDimension >
unsigned int
VectorImage< TPixel, VImageDimension >
::GetNumberOfComponentsPerPixel() const
{
  return this->m_VectorLength;
}

//----------------------------------------------------------------------------
template< typename TPixel, unsigned int VImageDimension >
void
VectorImage< TPixel, VImageDimension >
::SetNumberOfComponentsPerPixel(unsigned int n)
{
  this->SetVectorLength( static_cast< VectorLengthType >( n ) );
}

/**
 *
 */
template< typename TPixel, unsigned int VImageDimension >
void
VectorImage< TPixel, VImageDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "VectorLength: " << m_VectorLength << std::endl;
  os << indent << "PixelContainer: " << std::endl;
  m_Buffer->Print( os, indent.GetNextIndent() );

  // m_Origin and m_Spacing are printed in the Superclass
}
} // end namespace itk

#endif
