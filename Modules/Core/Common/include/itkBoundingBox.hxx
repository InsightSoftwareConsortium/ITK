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
#ifndef itkBoundingBox_hxx
#define itkBoundingBox_hxx

#include <algorithm> // For max.
#include <bitset>

namespace itk
{
/**
 * Print out the bounding box.
 */
template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
void
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::PrintSelf(std::ostream & os,
                                                                                         Indent         indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Bounding Box: ( ";
  for (unsigned int i = 0; i < PointDimension; ++i)
  {
    os << m_Bounds[2 * i] << ',' << m_Bounds[2 * i + 1] << ' ';
  }
  os << " )" << std::endl;
}

/**
 * Access routine to set the points container.
 */
template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
void
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::SetPoints(const PointsContainer * points)
{
  itkDebugMacro("setting Points container to " << points);
  if (m_PointsContainer != points)
  {
    m_PointsContainer = points;
    this->Modified();
  }
}

/** Access routine to get the points container. */
template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
auto
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::GetPoints() const
  -> const PointsContainer *
{
  itkDebugMacro("returning Points container of " << m_PointsContainer);

  return m_PointsContainer.GetPointer();
}

/** Compute and get the corners of the bounding box */
template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
auto
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::ComputeCorners() const
  -> std::array<PointType, NumberOfCorners>
{
  std::array<PointType, NumberOfCorners> result;

  PointType center = this->GetCenter();
  PointType radius;

  for (unsigned int i = 0; i < PointDimension; ++i)
  {
    radius[i] = m_Bounds[2 * i + 1] - center[i];
  }

  for (SizeValueType j = 0; j < NumberOfCorners; ++j)
  {
    const std::bitset<PointDimension> cornerBitset{ j };
    PointType                         pnt;
    for (unsigned int i = 0; i < PointDimension; ++i)
    {
      pnt[i] = center[i] + (cornerBitset[i] ? -1 : 1) * radius[i];
    }

    result[j] = pnt;
  }

  return result;
}

#if !defined(ITK_LEGACY_REMOVE)
/** Compute and get the corners of the bounding box */
template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
auto
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::GetCorners() -> const PointsContainer *
{
  m_CornersContainer->clear();
  m_CornersContainer->Reserve(NumberOfCorners);

  SizeValueType index = 0;
  for (const PointType & pnt : this->ComputeCorners())
  {
    m_CornersContainer->SetElement(index++, pnt);
  }

  return m_CornersContainer.GetPointer();
}
#endif


template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
bool
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::ComputeBoundingBox() const
{
  if (!m_PointsContainer)
  {
    if (this->GetMTime() > m_BoundsMTime)
    {
      m_Bounds.Fill(CoordinateType{});
      m_BoundsMTime.Modified();
    }
    return false;
  }

  if (this->GetMTime() > m_BoundsMTime)
  {
    // iterate over points determining min/max
    // start by initializing the values
    if (m_PointsContainer->Size() < 1)
    {
      m_Bounds.Fill(CoordinateType{});
      m_BoundsMTime.Modified();
      return false;
    }

    PointsContainerConstIterator        ci = m_PointsContainer->Begin();
    Point<TCoordinate, VPointDimension> point = ci->Value(); // point value
    for (unsigned int i = 0; i < PointDimension; ++i)
    {
      m_Bounds[2 * i] = point[i];
      m_Bounds[2 * i + 1] = point[i];
    }
    ++ci;

    // use a const iterator to grab the points and compute
    // the bounding box.
    while (ci != m_PointsContainer->End())
    {
      point = ci->Value(); // point value
      for (unsigned int i = 0; i < PointDimension; ++i)
      {
        if (point[i] < m_Bounds[2 * i])
        {
          m_Bounds[2 * i] = point[i];
        }
        if (point[i] > m_Bounds[2 * i + 1])
        {
          m_Bounds[2 * i + 1] = point[i];
        }
      }
      ++ci;
    } // for all points in container

    m_BoundsMTime.Modified();
  }

  return true;
}

template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
auto
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::GetCenter() const -> PointType
{
  this->ComputeBoundingBox();

  PointType center;
  for (unsigned int i = 0; i < PointDimension; ++i)
  {
    center[i] = (m_Bounds[2 * i] + m_Bounds[2 * i + 1]) / 2.0;
  }

  return center;
}

template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
auto
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::GetMinimum() const -> PointType
{
  this->ComputeBoundingBox();

  PointType minimum;
  for (unsigned int i = 0; i < PointDimension; ++i)
  {
    minimum[i] = m_Bounds[2 * i];
  }

  return minimum;
}

template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
void
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::SetMinimum(const PointType & point)
{
  for (unsigned int i = 0; i < PointDimension; ++i)
  {
    m_Bounds[2 * i] = point[i];
  }

  m_BoundsMTime.Modified();
}

template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
auto
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::GetMaximum() const -> PointType
{
  this->ComputeBoundingBox();

  PointType maximum;
  for (unsigned int i = 0; i < PointDimension; ++i)
  {
    maximum[i] = m_Bounds[2 * i + 1];
  }

  return maximum;
}

template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
void
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::SetMaximum(const PointType & point)
{
  for (unsigned int i = 0; i < PointDimension; ++i)
  {
    m_Bounds[2 * i + 1] = point[i];
  }

  m_BoundsMTime.Modified();
}

template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
void
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::ConsiderPoint(const PointType & point)
{
  bool changed = false;

  for (unsigned int i = 0; i < PointDimension; ++i)
  {
    if (point[i] < m_Bounds[2 * i])
    {
      m_Bounds[2 * i] = point[i];
      changed = true;
    }
    if (point[i] > m_Bounds[2 * i + 1])
    {
      m_Bounds[2 * i + 1] = point[i];
      changed = true;
    }
  }

  if (changed)
  {
    m_BoundsMTime.Modified();
  }
}

template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
auto
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::GetDiagonalLength2() const
  -> AccumulateType
{
  typename NumericTraits<CoordinateType>::AccumulateType dist2{};

  if (this->ComputeBoundingBox())
  {
    for (unsigned int i = 0; i < PointDimension; ++i)
    {
      dist2 += (m_Bounds[2 * i] - m_Bounds[2 * i + 1]) * (m_Bounds[2 * i] - m_Bounds[2 * i + 1]);
    }
  }

  return dist2;
}

template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
bool
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::IsInside(const PointType & point) const
{
  unsigned int j = 0;
  unsigned int i = 0;

  while (i < PointDimension)
  {
    if (point[i] < m_Bounds[j++])
    {
      return false;
    }
    if (point[i] > m_Bounds[j++])
    {
      return false;
    }
    ++i;
  }
  return true;
}

template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
ModifiedTimeType
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::GetMTime() const
{
  ModifiedTimeType latestTime = Object::GetMTime();

  if (m_PointsContainer)
  {
    latestTime = std::max(latestTime, m_PointsContainer->GetMTime());
  }
  return latestTime;
}

template <typename TPointIdentifier, unsigned int VPointDimension, typename TCoordinate, typename TPointsContainer>
auto
BoundingBox<TPointIdentifier, VPointDimension, TCoordinate, TPointsContainer>::DeepCopy() const -> Pointer
{
  Pointer clone = Self::New();

  // Connect the same points container into the clone
  clone->SetPoints(this->m_PointsContainer);

#if !defined(ITK_LEGACY_REMOVE)
  // Copy the corners into the clone.
  clone->m_CornersContainer->clear();

  PointsContainerConstIterator       itr = this->m_CornersContainer->Begin();
  const PointsContainerConstIterator end = this->m_CornersContainer->End();

  clone->m_CornersContainer->Reserve(this->m_CornersContainer->Size());
  const PointsContainerIterator dest = clone->m_CornersContainer->Begin();

  while (itr != end)
  {
    dest.Value() = itr.Value();
    ++itr;
  }
#endif

  // Copy the bounds into the clone
  for (unsigned int i = 0; i < 2 * PointDimension; ++i)
  {
    clone->m_Bounds[i] = this->m_Bounds[i];
  }

  return clone;
}
} // end namespace itk

#endif
