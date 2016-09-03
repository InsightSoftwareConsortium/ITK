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
#ifndef itkImage_hxx
#define itkImage_hxx

#include "itkImage.h"
#include "itkProcessObject.h"
#include <algorithm>

namespace itk
{

template< typename TPixel, unsigned int VImageDimension >
Image< TPixel, VImageDimension >
::Image()
{
  m_Buffer = PixelContainer::New();
}


template< typename TPixel, unsigned int VImageDimension >
void
Image< TPixel, VImageDimension >
::Allocate(bool initializePixels)
{
  SizeValueType num;

  this->ComputeOffsetTable();
  num = static_cast<SizeValueType>(this->GetOffsetTable()[VImageDimension]);

  m_Buffer->Reserve(num, initializePixels);
}


template< typename TPixel, unsigned int VImageDimension >
void
Image< TPixel, VImageDimension >
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
Image< TPixel, VImageDimension >
::FillBuffer(const TPixel & value)
{
  const SizeValueType numberOfPixels =
    this->GetBufferedRegion().GetNumberOfPixels();

  std::fill_n( &( *m_Buffer )[0], numberOfPixels, value );
}


template< typename TPixel, unsigned int VImageDimension >
void
Image< TPixel, VImageDimension >
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
Image< TPixel, VImageDimension >
::Graft(const Self *image)
{
  // call the superclass' implementation
  Superclass::Graft(image);

  if ( image )
    {
      // Now copy anything remaining that is needed
      this->SetPixelContainer( const_cast< PixelContainer * >
                               ( image->GetPixelContainer() ) );
    }
}


template< typename TPixel, unsigned int VImageDimension >
void
Image< TPixel, VImageDimension >
::Graft(const DataObject *data)
{
  if ( data )
    {
    // Attempt to cast data to an Image
    const Self * const imgData = dynamic_cast< const Self * >( data );

    if ( imgData != ITK_NULLPTR )
      {
        this->Graft(imgData);
      }
    else
      {
      // pointer could not be cast back down
      itkExceptionMacro( << "itk::Image::Graft() cannot cast "
                         << typeid( data ).name() << " to "
                         << typeid( const Self * ).name() );
      }
    }
}


template< typename TPixel, unsigned int VImageDimension >
void
Image< TPixel, VImageDimension >
::ComputeIndexToPhysicalPointMatrices()
{
  this->Superclass::ComputeIndexToPhysicalPointMatrices();
}


template< typename TPixel, unsigned int VImageDimension >
unsigned int
Image< TPixel, VImageDimension >
::GetNumberOfComponentsPerPixel() const
{
  // use the GetLength() method which works with variable length arrays,
  // to make it work with as much pixel types as possible
  PixelType p;
  return NumericTraits< PixelType >::GetLength(p);
}


template< typename TPixel, unsigned int VImageDimension >
void
Image< TPixel, VImageDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "PixelContainer: " << std::endl;
  m_Buffer->Print( os, indent.GetNextIndent() );

  // m_Origin and m_Spacing are printed in the Superclass
}

} // end namespace itk

#endif
