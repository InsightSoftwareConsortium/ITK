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
#ifndef itkImageRegion_hxx
#define itkImageRegion_hxx

#include "itkImageRegion.h"

namespace itk
{
/**
 * Instantiate object.
 */
template< unsigned int VImageDimension >
ImageRegion< VImageDimension >
::ImageRegion()
{
  m_Index.Fill(0);
  m_Size.Fill(0);
}

/**
 * Destructor for the ImageRegion class.
 */
template< unsigned int VImageDimension >
ImageRegion< VImageDimension >
::~ImageRegion()
{}

template< unsigned int VImageDimension >
typename ImageRegion< VImageDimension >::IndexType
ImageRegion< VImageDimension >
::GetUpperIndex() const
{
  IndexType idx;
  for ( unsigned int i = 0; i < VImageDimension; i++ )
    {
    idx[i] = m_Index[i] + m_Size[i] - 1;
    }

  return idx;
}

template< unsigned int VImageDimension >
void
ImageRegion< VImageDimension >
::SetUpperIndex( const IndexType & idx )
{
  for ( unsigned int i = 0; i < VImageDimension; i++ )
    {
    m_Size[i] = idx[i] - m_Index[i] + 1;
    }
}

template< unsigned int VImageDimension >
void
ImageRegion< VImageDimension >
::ComputeOffsetTable(OffsetTableType offsetTable) const
{
  OffsetValueType num=1;

  offsetTable[0] = num;
  for (unsigned int i=0; i < ImageDimension; i++)
    {
    num *= m_Size[i];
    offsetTable[i+1] = num;
    }
}

template< unsigned int VImageDimension >
typename ImageRegion< VImageDimension >::SizeValueType
ImageRegion< VImageDimension >
::GetNumberOfPixels() const
{
  SizeValueType numPixels = 1;

  for ( unsigned int i = 0; i < VImageDimension; i++ )
    {
    numPixels *= m_Size[i];
    }

  return numPixels;
}

template< unsigned int VImageDimension >
void
ImageRegion< VImageDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Dimension: " << this->GetImageDimension() << std::endl;
  os << indent << "Index: " << this->GetIndex() << std::endl;
  os << indent << "Size: " << this->GetSize() << std::endl;
}

template< unsigned int VImageDimension >
void
ImageRegion< VImageDimension >
::PadByRadius(OffsetValueType radius)
{
  SizeType radiusVector;

  for ( unsigned int i = 0; i < VImageDimension; ++i )
    {
    radiusVector[i] = radius;
    }

  this->PadByRadius(radiusVector);
}

template< unsigned int VImageDimension >
void
ImageRegion< VImageDimension >
::PadByRadius(const SizeType & radius)
{
  for ( unsigned int i = 0; i < VImageDimension; i++ )
    {
    m_Size[i] += 2 * radius[i];
    m_Index[i] -= static_cast< OffsetValueType >( radius[i] );
    }
}

template< unsigned int VImageDimension >
void
ImageRegion< VImageDimension >
::PadByRadius(const IndexValueArrayType radius)
{
  for ( unsigned int i = 0; i < VImageDimension; i++ )
    {
    m_Size[i] += 2 * radius[i];
    m_Index[i] -= static_cast< OffsetValueType >( radius[i] );
    }
}

template< unsigned int VImageDimension >
bool
ImageRegion< VImageDimension >
::ShrinkByRadius(OffsetValueType radius)
{
  SizeType radiusVector;

  for ( unsigned int i = 0; i < VImageDimension; ++i )
    {
    radiusVector[i] = radius;
    }

  return this->ShrinkByRadius(radiusVector);
}

template< unsigned int VImageDimension >
bool
ImageRegion< VImageDimension >
::ShrinkByRadius(const SizeType & radius)
{
  bool shrunkSuccessfully = true;
  for ( unsigned int i = 0; i < VImageDimension; i++ )
    {
    if( m_Size[i] <= 2 * radius[i] )
      {
      shrunkSuccessfully = false;
      }
    else
      {
      m_Size[i] -= 2 * radius[i];
      m_Index[i] += static_cast< OffsetValueType >( radius[i] );
      }
    }
  return shrunkSuccessfully;
}

template< unsigned int VImageDimension >
bool
ImageRegion< VImageDimension >
::ShrinkByRadius(const IndexValueArrayType radius)
{
  bool shrunkSuccessfully = true;
  for ( unsigned int i = 0; i < VImageDimension; i++ )
    {
    if( static_cast< IndexValueType >( m_Size[i] ) <= 2 * radius[i] )
      {
      shrunkSuccessfully = false;
      }
    else
      {
      m_Size[i] -= 2 * radius[i];
      m_Index[i] += static_cast< OffsetValueType >( radius[i] );
      }
    }
  return shrunkSuccessfully;
}

template< unsigned int VImageDimension >
bool
ImageRegion< VImageDimension >
::Crop(const Self & region)
{
  OffsetValueType crop;
  unsigned int    i;
  bool            cropPossible = true;

  // Can we crop?
  for ( i = 0; i < VImageDimension && cropPossible; i++ )
    {
    // Is left edge of current region to the right of the right edge
    // of the region to crop with? (if so, we cannot crop)
    if ( m_Index[i] >= region.GetIndex()[i]
         + static_cast< OffsetValueType >( region.GetSize()[i] ) )
      {
      cropPossible = false;
      }
    // If right edge of the current region to the left of the left
    // edge of the region to crop with? (if so, we cannot crop)
    if ( m_Index[i] + static_cast< OffsetValueType >( m_Size[i] ) <= region.GetIndex()[i] )
      {
      cropPossible = false;
      }
    }

  // if we cannot crop, return without changing anythin
  if ( !cropPossible )
    {
    return cropPossible;
    }

  // we can crop, so crop
  for ( i = 0; i < VImageDimension; i++ )
    {
    // first check the start index
    if ( m_Index[i] < region.GetIndex()[i] )
      {
      // how much do we need to adjust
      crop = region.GetIndex()[i] - m_Index[i];

      // adjust the start index and the size of the current region
      m_Index[i] += crop;
      m_Size[i] -= static_cast< SizeValueType >( crop );
      }
    // now check the final size
    if ( m_Index[i] + static_cast< OffsetValueType >( m_Size[i] )
         > region.GetIndex()[i] + static_cast< OffsetValueType >( region.GetSize()[i] ) )
      {
      // how much do we need to adjust
      crop = m_Index[i] + static_cast< OffsetValueType >( m_Size[i] )
             - region.GetIndex()[i] - static_cast< OffsetValueType >( region.GetSize()[i] );

      // adjust the size
      m_Size[i] -= static_cast< SizeValueType >( crop );
      }
    }

  return cropPossible;
}

template< unsigned int VImageDimension >
typename ImageRegion< VImageDimension >::SliceRegion
ImageRegion< VImageDimension >
::Slice(const unsigned int dim) const
{
  if ( dim >= VImageDimension )
    {
    itkGenericExceptionMacro(
      << "The dimension to remove: " << dim
      << " is greater than the dimension of the image: " << VImageDimension);
    }

  Index< SliceDimension > sliceIndex;
  Size< SliceDimension >  sliceSize;

  sliceIndex.Fill(0);
  sliceSize.Fill(0);
  unsigned int ii = 0;
  for ( unsigned int i = 0; i < VImageDimension; i++ )
    {
    if ( i != dim )
      {
      sliceIndex[ii] = m_Index[i];
      sliceSize[ii] = m_Size[i];
      ++ii;
      }
    }

  return ImageRegion< SliceDimension >(sliceIndex, sliceSize);
}

template< unsigned int VImageDimension >
std::ostream & operator<<(std::ostream & os, const ImageRegion< VImageDimension > & region)
{
  region.Print(os);
  return os;
}
} // end namespace itk

#endif
