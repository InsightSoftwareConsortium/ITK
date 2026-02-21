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
#ifndef itkImageConstIteratorWithIndex_hxx
#define itkImageConstIteratorWithIndex_hxx


namespace itk
{
//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template <typename TImage>
ImageConstIteratorWithIndex<TImage>::ImageConstIteratorWithIndex(const Self & it)
  : m_Image(it.m_Image) // copy the smart pointer
  , m_PositionIndex(it.m_PositionIndex)
  , m_BeginIndex(it.m_BeginIndex)
  , m_EndIndex(it.m_EndIndex)
  , m_Region(it.m_Region)
  , m_Position(it.m_Position)
  , m_Begin(it.m_Begin)
  , m_End(it.m_End)
  , m_Remaining(it.m_Remaining)
  , m_PixelAccessor(it.m_PixelAccessor)
  , m_PixelAccessorFunctor(it.m_PixelAccessorFunctor)
{
  std::copy_n(it.m_OffsetTable, ImageDimension + 1, m_OffsetTable);

  m_PixelAccessorFunctor.SetBegin(m_Image->GetBufferPointer());
}

//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template <typename TImage>
ImageConstIteratorWithIndex<TImage>::ImageConstIteratorWithIndex(const TImage * ptr, const RegionType & region)
  : m_Image(ptr)
  , m_BeginIndex(region.GetIndex())
  , m_Region(region)
{
  const InternalPixelType * buffer = m_Image->GetBufferPointer();
  m_PositionIndex = m_BeginIndex;
  if (region.GetNumberOfPixels() > 0) // If region is non-empty
  {
    const RegionType & bufferedRegion = m_Image->GetBufferedRegion();
    itkAssertOrThrowMacro((bufferedRegion.IsInside(m_Region)),
                          "Region " << m_Region << " is outside of buffered region " << bufferedRegion);
  }

  std::copy_n(m_Image->GetOffsetTable(), ImageDimension + 1, m_OffsetTable);

  // Compute the start position
  const OffsetValueType offs = m_Image->ComputeOffset(m_BeginIndex);
  m_Begin = buffer + offs;
  m_Position = m_Begin;

  // Compute the end offset
  m_Remaining = false;
  IndexType pastEnd;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    const SizeValueType size = region.GetSize()[i];
    if (size > 0)
    {
      m_Remaining = true;
    }
    m_EndIndex[i] = m_BeginIndex[i] + static_cast<OffsetValueType>(size);
    pastEnd[i] = m_BeginIndex[i] + static_cast<OffsetValueType>(size) - 1;
  }
  m_End = buffer + m_Image->ComputeOffset(pastEnd);

  m_PixelAccessor = m_Image->GetPixelAccessor();
  m_PixelAccessorFunctor.SetPixelAccessor(m_PixelAccessor);
  m_PixelAccessorFunctor.SetBegin(buffer);

  GoToBegin();
}

//----------------------------------------------------------------------
//    Assignment Operator
//----------------------------------------------------------------------
template <typename TImage>
ImageConstIteratorWithIndex<TImage> &
ImageConstIteratorWithIndex<TImage>::operator=(const Self & it)
{
  if (this != &it)
  {
    m_Image = it.m_Image; // copy the smart pointer

    m_BeginIndex = it.m_BeginIndex;
    m_EndIndex = it.m_EndIndex;
    m_PositionIndex = it.m_PositionIndex;
    m_Region = it.m_Region;

    std::copy_n(it.m_OffsetTable, ImageDimension + 1, m_OffsetTable);

    m_Position = it.m_Position;
    m_Begin = it.m_Begin;
    m_End = it.m_End;
    m_Remaining = it.m_Remaining;

    m_PixelAccessor = it.m_PixelAccessor;
    m_PixelAccessorFunctor = it.m_PixelAccessorFunctor;
    m_PixelAccessorFunctor.SetBegin(m_Image->GetBufferPointer());
  }
  return *this;
}

//----------------------------------------------------------------------------
// GoToBegin() is the first pixel in the region.
//----------------------------------------------------------------------------
template <typename TImage>
void
ImageConstIteratorWithIndex<TImage>::GoToBegin()
{
  // Set the position at begin

  m_Position = m_Begin;
  m_PositionIndex = m_BeginIndex;

  m_Remaining = m_Region.GetNumberOfPixels() > 0;
}

//----------------------------------------------------------------------------
// GoToReverseBegin() is the last pixel in the region.
//----------------------------------------------------------------------------
template <typename TImage>
void
ImageConstIteratorWithIndex<TImage>::GoToReverseBegin()
{
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    m_PositionIndex[i] = m_EndIndex[i] - 1;
  }

  m_Remaining = m_Region.GetNumberOfPixels() > 0;

  // Set the position at the end
  const InternalPixelType * buffer = m_Image->GetBufferPointer();
  const OffsetValueType     offset = m_Image->ComputeOffset(m_PositionIndex);
  m_Position = buffer + offset;
}

} // end namespace itk

#endif
