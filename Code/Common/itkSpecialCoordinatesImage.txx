/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpecialCoordinatesImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpecialCoordinatesImage_txx
#define __itkSpecialCoordinatesImage_txx
#include "itkSpecialCoordinatesImage.h"
#include "itkProcessObject.h"
#include "itkAffineTransform.h"

namespace itk
{
/**
 *
 */
template< class TPixel, unsigned int VImageDimension >
SpecialCoordinatesImage< TPixel, VImageDimension >
::SpecialCoordinatesImage()
{
  m_Buffer = PixelContainer::New();
}

//----------------------------------------------------------------------------
template< class TPixel, unsigned int VImageDimension >
void
SpecialCoordinatesImage< TPixel, VImageDimension >
::Allocate()
{
  unsigned long num;

  this->ComputeOffsetTable();
  num = this->GetOffsetTable()[VImageDimension];

  m_Buffer->Reserve(num);
}

template< class TPixel, unsigned int VImageDimension >
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

template< class TPixel, unsigned int VImageDimension >
void
SpecialCoordinatesImage< TPixel, VImageDimension >
::FillBuffer(const TPixel & value)
{
  const unsigned long numberOfPixels =
    this->GetBufferedRegion().GetNumberOfPixels();

  for ( unsigned int i = 0; i < numberOfPixels; i++ )
    {
    ( *m_Buffer )[i] = value;
    }
}

template< class TPixel, unsigned int VImageDimension >
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

/**
 *
 */
template< class TPixel, unsigned int VImageDimension >
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
