/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkImage_txx
#define _itkImage_txx
#include "itkImage.h"
#include "itkProcessObject.h"

namespace itk
{

/**
 *
 */
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
Image<TPixel, VImageDimension, TImageTraits>
::Image()
: m_PixelAccessor ()
{
  m_Buffer = PixelContainer::New();

  unsigned int i;
  for (i=0; i < VImageDimension; i++)
    {
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0.0;
    }
}


/**
 *
 */
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
Image<TPixel, VImageDimension, TImageTraits>
::~Image()
{
}


//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
void 
Image<TPixel, VImageDimension, TImageTraits>
::Allocate()
{
  unsigned long num;

  this->ComputeOffsetTable();
  num = this->GetOffsetTable()[VImageDimension];
  
  m_Buffer->Reserve(num);
}



//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
void 
Image<TPixel, VImageDimension, TImageTraits>
::SetSpacing(const double spacing[VImageDimension] )
{
  unsigned int i; 
  for (i=0; i<VImageDimension; i++)
    {
    if ( spacing[i] != m_Spacing[i] )
      {
      break;
      }
    } 
  if ( i < VImageDimension ) 
    { 
    this->Modified(); 
    for (i=0; i<VImageDimension; i++)
      {
      m_Spacing[i] = spacing[i];
      }
    } 
}

//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
void 
Image<TPixel, VImageDimension, TImageTraits>
::SetSpacing(const float spacing[VImageDimension] )
{
  unsigned int i; 
  for (i=0; i<VImageDimension; i++)
    {
    if ( (double)spacing[i] != m_Spacing[i] )
      {
      break;
      }
    } 
  if ( i < VImageDimension ) 
    { 
    this->Modified(); 
    for (i=0; i<VImageDimension; i++)
      {
      m_Spacing[i] = spacing[i];
      }
    } 
}



//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
const double *
Image<TPixel, VImageDimension, TImageTraits>
::GetSpacing() const
{
  return m_Spacing;
}




//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
void 
Image<TPixel, VImageDimension, TImageTraits>
::SetOrigin(const double origin[VImageDimension] )
{
  unsigned int i; 
  for (i=0; i<VImageDimension; i++)
    {
    if ( origin[i] != m_Origin[i] )
      {
      break;
      }
    } 
  if ( i < VImageDimension ) 
    { 
    this->Modified(); 
    for (i=0; i<VImageDimension; i++)
      {
      m_Origin[i] = origin[i];
      }
    } 
}


//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
void 
Image<TPixel, VImageDimension, TImageTraits>
::SetOrigin(const float origin[VImageDimension] )
{
  unsigned int i; 
  for (i=0; i<VImageDimension; i++)
    {
    if ( (double)origin[i] != m_Origin[i] )
      {
      break;
      }
    } 
  if ( i < VImageDimension ) 
    { 
    this->Modified(); 
    for (i=0; i<VImageDimension; i++)
      {
      m_Origin[i] = origin[i];
      }
    } 
}


//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
void 
Image<TPixel, VImageDimension, TImageTraits>
::SetOrigin(const OriginOffsetType & origin )
{
  unsigned int i; 
  for (i=0; i<VImageDimension; i++)
    {
    if ( (double)origin[i] != m_Origin[i] )
      {
      break;
      }
    } 
  if ( i < VImageDimension ) 
    { 
    this->Modified(); 
    for (i=0; i<VImageDimension; i++)
      {
      m_Origin[i] = origin[i];
      }
    } 
}


//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
const double *
Image<TPixel, VImageDimension, TImageTraits>
::GetOrigin() const
{
  return m_Origin;
}

//---------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
Image<TPixel, VImageDimension, TImageTraits>::AffineTransformType::Pointer
Image<TPixel, VImageDimension, TImageTraits>
::GetIndexToPhysicalTransform(void) const
{
  typename AffineTransformType::MatrixType matrix;
  typename AffineTransformType::OffsetType offset;
  for (unsigned int i = 0; i < VImageDimension; i++)
    {
    for (unsigned int j = 0; j < VImageDimension; j++)
      {
      matrix[i][j] = 0.0;
      }
    matrix[i][i] = m_Spacing[i];
    offset[i]    = m_Origin [i];
    }

  AffineTransformType::Pointer result = AffineTransformType::New();
  
  result->SetMatrix(matrix);
  result->SetOffset(offset);

  return result;

}


//---------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
Image<TPixel, VImageDimension, TImageTraits>::AffineTransformType::Pointer
Image<TPixel, VImageDimension, TImageTraits>
::GetPhysicalToIndexTransform(void) const
{
  typename AffineTransformType::MatrixType matrix;
  typename AffineTransformType::OffsetType offset;

  for (unsigned int i = 0; i < VImageDimension; i++)
    {
    for (unsigned int j = 0; j < VImageDimension; j++)
      {
      matrix[i][j] = 0.0;
      }
    matrix[i][i] = 1.0 / m_Spacing[i];
    offset[i]    = -m_Origin[i] / m_Spacing[i];
    }

  AffineTransformType::Pointer result = AffineTransformType::New();

  result->SetMatrix(matrix);
  result->SetOffset(offset);

  return result;
}

//----------------------------------------------------------------------------
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
void
Image<TPixel, VImageDimension, TImageTraits>
::CopyInformation(DataObject *data)
{
  // Standard call to the superclass' method
  Superclass::CopyInformation(data);

  // Attempt to cast data to an ImageBase.  All subclasses of ImageBase
  // respond to GetSpacing(), GetOrigin()
  ImageBase<VImageDimension> *phyData;
  
  phyData = dynamic_cast<ImageBase<VImageDimension>*>(data);

  if (phyData)
    {
    // Copy the origin and spacing
    this->SetSpacing( phyData->GetSpacing() );
    this->SetOrigin( phyData->GetOrigin() );
    }
  else
    {
    // pointer could not be cast back down
    std::cerr << "itk::Image::CopyInformation() cannot cast "
              << typeid(data).name() << " to "
              << typeid(ImageBase<VImageDimension>*).name() << std::endl;
    }
}


/**
 *
 */
template<class TPixel, unsigned int VImageDimension, class TImageTraits>
void 
Image<TPixel, VImageDimension, TImageTraits>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Data: " << m_Buffer << std::endl;
}


} // end namespace itk

#endif
