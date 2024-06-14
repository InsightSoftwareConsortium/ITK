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
#ifndef itkImageConstIteratorWithOnlyIndex_hxx
#define itkImageConstIteratorWithOnlyIndex_hxx


namespace itk
{
//----------------------------------------------------------------------
//  Constructor
//----------------------------------------------------------------------
template <typename TImage>
ImageConstIteratorWithOnlyIndex<TImage>::ImageConstIteratorWithOnlyIndex(const TImage * ptr, const RegionType & region)
{
  m_Image = ptr;

  m_BeginIndex = region.GetIndex();
  m_PositionIndex = m_BeginIndex;
  m_Region = region;

  std::copy_n(m_Image->GetOffsetTable(), ImageDimension + 1, m_OffsetTable);

  // Compute the end offset
  m_Remaining = false;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    SizeValueType size = region.GetSize()[i];
    if (size > 0)
    {
      m_Remaining = true;
    }
    m_EndIndex[i] = m_BeginIndex[i] + static_cast<OffsetValueType>(size);
  }

  GoToBegin();
}

//----------------------------------------------------------------------------
// GoToBegin() is the first pixel in the region.
//----------------------------------------------------------------------------
template <typename TImage>
void
ImageConstIteratorWithOnlyIndex<TImage>::GoToBegin()
{
  // Set the position at begin

  m_PositionIndex = m_BeginIndex;

  if (m_Region.GetNumberOfPixels() > 0)
  {
    m_Remaining = true;
  }
  else
  {
    m_Remaining = false;
  }
}

//----------------------------------------------------------------------------
// GoToReverseBegin() is the last pixel in the region.
//----------------------------------------------------------------------------
template <typename TImage>
void
ImageConstIteratorWithOnlyIndex<TImage>::GoToReverseBegin()
{
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    m_PositionIndex[i] = m_EndIndex[i] - 1;
  }

  if (m_Region.GetNumberOfPixels() > 0)
  {
    m_Remaining = true;
  }
  else
  {
    m_Remaining = false;
  }
}

} // end namespace itk

#endif
