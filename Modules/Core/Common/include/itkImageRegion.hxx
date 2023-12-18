/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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


namespace itk
{
template <unsigned int VImageDimension>
auto
ImageRegion<VImageDimension>::GetUpperIndex() const -> IndexType
{
  IndexType idx;
  for (unsigned int i = 0; i < VImageDimension; ++i)
  {
    idx[i] = m_Index[i] + m_Size[i] - 1;
  }

  return idx;
}

template <unsigned int VImageDimension>
void
ImageRegion<VImageDimension>::SetUpperIndex(const IndexType & idx)
{
  for (unsigned int i = 0; i < VImageDimension; ++i)
  {
    m_Size[i] = idx[i] - m_Index[i] + 1;
  }
}

template <unsigned int VImageDimension>
void
ImageRegion<VImageDimension>::ComputeOffsetTable(OffsetTableType offsetTable) const
{
  OffsetValueType num = 1;

  offsetTable[0] = num;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    num *= m_Size[i];
    offsetTable[i + 1] = num;
  }
}

template <unsigned int VImageDimension>
auto
ImageRegion<VImageDimension>::GetNumberOfPixels() const -> SizeValueType
{
  return m_Size.CalculateProductOfElements();
}

template <unsigned int VImageDimension>
void
ImageRegion<VImageDimension>::Print(std::ostream & os, Indent indent) const
{
  os << indent << this->GetNameOfClass() << " (" << this << ")\n";
  this->PrintSelf(os, indent.GetNextIndent());
}

template <unsigned int VImageDimension>
void
ImageRegion<VImageDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "Dimension: " << this->GetImageDimension() << std::endl;
  os << indent << "Index: " << m_Index << std::endl;
  os << indent << "Size: " << m_Size << std::endl;
}

template <unsigned int VImageDimension>
void
ImageRegion<VImageDimension>::PadByRadius(OffsetValueType radius)
{
  SizeType radiusVector;

  for (unsigned int i = 0; i < VImageDimension; ++i)
  {
    radiusVector[i] = radius;
  }

  this->PadByRadius(radiusVector);
}

template <unsigned int VImageDimension>
void
ImageRegion<VImageDimension>::PadByRadius(const SizeType & radius)
{
  for (unsigned int i = 0; i < VImageDimension; ++i)
  {
    m_Size[i] += 2 * radius[i];
    m_Index[i] -= static_cast<OffsetValueType>(radius[i]);
  }
}

template <unsigned int VImageDimension>
void
ImageRegion<VImageDimension>::PadByRadius(const IndexValueArrayType radius)
{
  for (unsigned int i = 0; i < VImageDimension; ++i)
  {
    m_Size[i] += 2 * radius[i];
    m_Index[i] -= static_cast<OffsetValueType>(radius[i]);
  }
}

template <unsigned int VImageDimension>
bool
ImageRegion<VImageDimension>::ShrinkByRadius(OffsetValueType radius)
{
  SizeType radiusVector;

  for (unsigned int i = 0; i < VImageDimension; ++i)
  {
    radiusVector[i] = radius;
  }

  return this->ShrinkByRadius(radiusVector);
}

template <unsigned int VImageDimension>
bool
ImageRegion<VImageDimension>::ShrinkByRadius(const SizeType & radius)
{
  bool shrunkSuccessfully = true;
  for (unsigned int i = 0; i < VImageDimension; ++i)
  {
    if (m_Size[i] <= 2 * radius[i])
    {
      shrunkSuccessfully = false;
    }
    else
    {
      m_Size[i] -= 2 * radius[i];
      m_Index[i] += static_cast<OffsetValueType>(radius[i]);
    }
  }
  return shrunkSuccessfully;
}

template <unsigned int VImageDimension>
bool
ImageRegion<VImageDimension>::ShrinkByRadius(const IndexValueArrayType radius)
{
  bool shrunkSuccessfully = true;
  for (unsigned int i = 0; i < VImageDimension; ++i)
  {
    if (static_cast<IndexValueType>(m_Size[i]) <= 2 * radius[i])
    {
      shrunkSuccessfully = false;
    }
    else
    {
      m_Size[i] -= 2 * radius[i];
      m_Index[i] += static_cast<OffsetValueType>(radius[i]);
    }
  }
  return shrunkSuccessfully;
}

template <unsigned int VImageDimension>
bool
ImageRegion<VImageDimension>::Crop(const Self & region)
{
  // Can we crop?
  for (unsigned int i = 0; i < VImageDimension; ++i)
  {
    // Is the left edge of current region to the right of the right edge of the region to crop with? Or is the right
    // edge of the current region to the left of the left edge of the region to crop with? (if so, we cannot crop)
    if (m_Index[i] >= region.m_Index[i] + static_cast<OffsetValueType>(region.m_Size[i]) ||
        m_Index[i] + static_cast<OffsetValueType>(m_Size[i]) <= region.m_Index[i])
    {
      // if we cannot crop, return without changing anything
      return false;
    }
  }

  // we can crop, so crop
  for (unsigned int i = 0; i < VImageDimension; ++i)
  {
    // first check the start index
    if (m_Index[i] < region.m_Index[i])
    {
      // adjust the size and the start index of the current region
      m_Size[i] -= static_cast<SizeValueType>(region.m_Index[i] - m_Index[i]);
      m_Index[i] = region.m_Index[i];
    }
    // now check the final size
    if (m_Index[i] + static_cast<OffsetValueType>(m_Size[i]) >
        region.m_Index[i] + static_cast<OffsetValueType>(region.m_Size[i]))
    {
      // adjust the size
      m_Size[i] = region.m_Size[i] - static_cast<SizeValueType>(m_Index[i] - region.m_Index[i]);
    }
  }

  return true;
}

template <unsigned int VImageDimension>
auto
ImageRegion<VImageDimension>::Slice(const unsigned int dim) const -> SliceRegion
{
  if (dim >= VImageDimension)
  {
    itkGenericExceptionMacro(
      "The dimension to remove: " << dim << " is greater than the dimension of the image: " << VImageDimension);
  }

  Index<SliceDimension> sliceIndex;
  Size<SliceDimension>  sliceSize;

  sliceIndex.Fill(0);
  sliceSize.Fill(0);
  unsigned int ii = 0;
  for (unsigned int i = 0; i < VImageDimension; ++i)
  {
    if (i != dim)
    {
      sliceIndex[ii] = m_Index[i];
      sliceSize[ii] = m_Size[i];
      ++ii;
    }
  }

  return ImageRegion<SliceDimension>(sliceIndex, sliceSize);
}

template <unsigned int VImageDimension>
std::ostream &
operator<<(std::ostream & os, const ImageRegion<VImageDimension> & region)
{
  region.Print(os);
  return os;
}
} // end namespace itk

#endif
