/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImage.txx
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
#ifndef _itkImage_txx
#define _itkImage_txx
#include "itkImage.h"
#include "itkProcessObject.h"
#include "itkAffineTransform.h"

namespace itk
{

/**
 *
 */
template<class TPixel, unsigned int VImageDimension>
Image<TPixel, VImageDimension>
::Image()
{
  m_Buffer = PixelContainer::New();
}


//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::Allocate()
{
  unsigned long num;

  this->ComputeOffsetTable();
  num = this->GetOffsetTable()[VImageDimension];
  
  m_Buffer->Reserve(num);
}

template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
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


template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::FillBuffer (const TPixel& value)
{
  const unsigned long numberOfPixels =
    this->GetBufferedRegion().GetNumberOfPixels();

  for(unsigned int i=0; i<numberOfPixels; i++) 
    {
    (*m_Buffer)[i] = value;
    }
}

template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::SetPixelContainer(PixelContainer *container)
{
  if (m_Buffer != container)
    {
    m_Buffer = container;
    this->Modified();
    }
}
    

//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::SetSpacing(const double spacing[VImageDimension] )
{
  SpacingType s(spacing);
  this->SetSpacing(s);
}


//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::SetSpacing(const float spacing[VImageDimension] )
{
  Vector<float, VImageDimension> sf(spacing);
  SpacingType s;
  s.CastFrom( sf );
  this->SetSpacing(s);
}

//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::SetOrigin(const double origin[VImageDimension] )
{
  PointType p(origin);
  this->SetOrigin( p );
}


//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::SetOrigin(const float origin[VImageDimension] )
{
  Point<float, VImageDimension> of(origin);
  PointType p;
  p.CastFrom( of );
  this->SetOrigin( p );
}


//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension>
void
Image<TPixel, VImageDimension>
::CopyInformation(const DataObject *data)
{
  // Standard call to the superclass' method
  Superclass::CopyInformation(data);

  // Attempt to cast data to an ImageBase.  All subclasses of ImageBase
  // respond to GetSpacing(), GetOrigin()
  const ImageBase<VImageDimension> *phyData;
  
  phyData = dynamic_cast<const ImageBase<VImageDimension>*>(data);

  if (phyData)
    {
    // Copy the origin and spacing
    this->SetSpacing( phyData->GetSpacing() );
    this->SetOrigin( phyData->GetOrigin() );
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro(<< "itk::Image::CopyInformation() cannot cast "
                      << typeid(data).name() << " to "
                      << typeid(ImageBase<VImageDimension>*).name() );
    }
}


/**
 *
 */
template<class TPixel, unsigned int VImageDimension>
void 
Image<TPixel, VImageDimension>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "PixelContainer: " << m_Buffer << std::endl;
}


} // end namespace itk

#endif
