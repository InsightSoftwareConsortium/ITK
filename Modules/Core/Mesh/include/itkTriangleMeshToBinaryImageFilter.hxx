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
#ifndef itkTriangleMeshToBinaryImageFilter_hxx
#define itkTriangleMeshToBinaryImageFilter_hxx

#include "itkImageRegionIteratorWithIndex.h"
#include "itkNumericTraits.h"
#include <cstdlib>
#include <algorithm> // For max.

namespace itk
{

template <typename TInputMesh, typename TOutputImage>
TriangleMeshToBinaryImageFilter<TInputMesh, TOutputImage>::TriangleMeshToBinaryImageFilter()
{
  this->SetNumberOfRequiredInputs(1);

  m_Size.Fill(0);
  m_Index.Fill(0);

  for (unsigned int i = 0; i < 3; ++i)
  {
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0;
  }

  m_InsideValue = NumericTraits<ValueType>::OneValue();
  m_OutsideValue = ValueType{};
  m_Direction.GetVnlMatrix().set_identity();

  m_Tolerance = 1e-5;
  m_InfoImage = nullptr;
}

template <typename TInputMesh, typename TOutputImage>
void
TriangleMeshToBinaryImageFilter<TInputMesh, TOutputImage>::SetInput(TInputMesh * input)
{
  this->ProcessObject::SetNthInput(0, input);
}

template <typename TInputMesh, typename TOutputImage>
auto
TriangleMeshToBinaryImageFilter<TInputMesh, TOutputImage>::GetInput() -> InputMeshType *
{
  return static_cast<TInputMesh *>(this->ProcessObject::GetInput(0));
}

template <typename TInputMesh, typename TOutputImage>
auto
TriangleMeshToBinaryImageFilter<TInputMesh, TOutputImage>::GetInput(unsigned int idx) -> InputMeshType *
{
  return itkDynamicCastInDebugMode<TInputMesh *>(this->ProcessObject::GetInput(idx));
}

template <typename TInputMesh, typename TOutputImage>
void
TriangleMeshToBinaryImageFilter<TInputMesh, TOutputImage>::SetSpacing(const double spacing[3])
{
  SpacingType s;
  for (unsigned int i = 0; i < TOutputImage::ImageDimension; ++i)
  {
    s[i] = static_cast<SpacePrecisionType>(spacing[i]);
  }
  this->SetSpacing(s);
}

template <typename TInputMesh, typename TOutputImage>
void
TriangleMeshToBinaryImageFilter<TInputMesh, TOutputImage>::SetSpacing(const float spacing[3])
{
  const Vector<float, 3> sf(spacing);
  SpacingType            s;
  s.CastFrom(sf);
  this->SetSpacing(s);
}

template <typename TInputMesh, typename TOutputImage>
void
TriangleMeshToBinaryImageFilter<TInputMesh, TOutputImage>::SetOrigin(const double origin[3])
{
  const PointType p(origin);

  this->SetOrigin(p);
}

template <typename TInputMesh, typename TOutputImage>
void
TriangleMeshToBinaryImageFilter<TInputMesh, TOutputImage>::SetOrigin(const float origin[3])
{
  const Point<float, 3> of(origin);
  PointType             p;
  p.CastFrom(of);
  this->SetOrigin(p);
}

template <typename TInputMesh, typename TOutputImage>
bool
TriangleMeshToBinaryImageFilter<TInputMesh, TOutputImage>::ComparePoints2D(Point2DType a, Point2DType b)
{
  // sort xy points by ascending y value, then x
  if (Math::ExactlyEquals(a[1], b[1]))
  {
    return (a[0] < b[0]);
  }

  return (a[1] < b[1]);
}

template <typename TInputMesh, typename TOutputImage>
bool
TriangleMeshToBinaryImageFilter<TInputMesh, TOutputImage>::ComparePoints1D(Point1D a, Point1D b)
{
  return (a.m_X < b.m_X);
}

template <typename TInputMesh, typename TOutputImage>
void
TriangleMeshToBinaryImageFilter<TInputMesh, TOutputImage>::GenerateData()
{
  itkDebugMacro("TriangleMeshToBinaryImageFilter::Update() called");

  // Get the input and output pointers
  const OutputImagePointer OutputImage = this->GetOutput();
  if (m_InfoImage == nullptr)
  {
    if (m_Size[0] == 0 || m_Size[1] == 0 || m_Size[2] == 0)
    {
      itkExceptionMacro("Must Set Image Size");
    }

    const typename OutputImageType::RegionType region(m_Index, m_Size);

    OutputImage->SetRegions(region);        // set the region
    OutputImage->SetSpacing(m_Spacing);     // set spacing
    OutputImage->SetOrigin(m_Origin);       //   and origin
    OutputImage->SetDirection(m_Direction); // direction cosines
  }
  else
  {
    itkDebugMacro("Using info image");
    m_InfoImage->Update();
    OutputImage->CopyInformation(m_InfoImage);
    OutputImage->SetRegions(m_InfoImage->GetLargestPossibleRegion());
    m_Size = m_InfoImage->GetLargestPossibleRegion().GetSize();
    m_Index = m_InfoImage->GetLargestPossibleRegion().GetIndex();
    m_Spacing = m_InfoImage->GetSpacing();
    m_Origin = m_InfoImage->GetOrigin();
    m_Direction = m_InfoImage->GetDirection();
  }

  OutputImage->Allocate();

  RasterizeTriangles();

  itkDebugMacro("TriangleMeshToBinaryImageFilter::Update() finished");
}

template <typename TInputMesh, typename TOutputImage>
int
TriangleMeshToBinaryImageFilter<TInputMesh, TOutputImage>::PolygonToImageRaster(PointVector    coords,
                                                                                Point1DArray & zymatrix,
                                                                                int            extent[6])
{
  // convert the polygon into a rasterizable form by finding its
  // intersection with each z plane, and store the (x,y) coords
  // of each intersection in a vector called "matrix"
  const int    zSize = extent[5] - extent[4] + 1;
  const int    zInc = extent[3] - extent[2] + 1;
  Point2DArray matrix(zSize);

  // each iteration of the following loop examines one edge of the
  // polygon, where the endpoints of the edge are p1 and p2
  auto      n = static_cast<int>(coords.size());
  PointType p0 = coords[0];
  PointType p1 = coords[n - 1];
  double    area = 0.0;

  for (int i = 0; i < n; ++i)
  {
    PointType p2 = coords[i];
    // calculate the area (actually double the area) of the polygon's
    // projection into the zy plane via cross product, one triangle
    // at a time
    const double v1y = p1[1] - p0[1];
    const double v1z = p1[2] - p0[2];
    const double v2y = p2[1] - p0[1];
    const double v2z = p2[2] - p0[2];
    area += (v1y * v2z - v2y * v1z);

    // skip any line segments that are perfectly horizontal
    if (Math::ExactlyEquals(p1[2], p2[2]))
    {
      p1 = coords[i];
      continue;
    }

    // sort the endpoints, this improves robustness
    if (p1[2] > p2[2])
    {
      std::swap(p1, p2);
    }

    auto zmin = static_cast<int>(std::ceil(p1[2]));
    auto zmax = static_cast<int>(std::ceil(p2[2]));

    if (zmin > extent[5] || zmax < extent[4])
    {
      continue;
    }

    // cap to the volume extents
    zmin = std::max(zmin, extent[4]);
    if (zmax >= extent[5])
    {
      zmax = extent[5] + 1;
    }
    const double temp = 1.0 / (p2[2] - p1[2]);
    for (int z = zmin; z < zmax; ++z)
    {
      const double r = (p2[2] - static_cast<double>(z)) * temp;
      const double f = 1.0 - r;
      Point2DType  XY;
      XY[0] = r * p1[0] + f * p2[0];
      XY[1] = r * p1[1] + f * p2[1];
      matrix[z - extent[4]].push_back(XY);
    }

    p1 = coords[i];
  } // end of for loop

  // area is not really needed, we just need the sign
  int sign = 0;
  if (area < 0.0)
  {
    sign = -1;
  }
  else if (area > 0.0)
  {
    sign = 1;
  }
  else
  {
    return 0;
  }

  // rasterize the polygon and store the x coord for each (y,z)
  // point that we rasterize, kind of like using a depth buffer
  // except that 'x' is our depth value and we can store multiple
  // 'x' values per (y,z) value.

  for (int z = extent[4]; z <= extent[5]; ++z)
  {
    Point2DVector & xylist = matrix[z - extent[4]];

    if (xylist.empty())
    {
      continue;
    }

    // sort by ascending y, then x
    std::sort(xylist.begin(), xylist.end(), ComparePoints2D);

    n = static_cast<int>(xylist.size()) / 2;
    for (int k = 0; k < n; ++k)
    {
      Point2DType & p2D1 = xylist[2 * k];
      const double  X1 = p2D1[0];
      const double  Y1 = p2D1[1];
      Point2DType & p2D2 = xylist[2 * k + 1];
      const double  X2 = p2D2[0];
      const double  Y2 = p2D2[1];

      if (Math::ExactlyEquals(Y2, Y1))
      {
        continue;
      }
      const double temp = 1.0 / (Y2 - Y1);
      auto         ymin = static_cast<int>(std::ceil(Y1));
      auto         ymax = static_cast<int>(std::ceil(Y2));
      for (int y = ymin; y < ymax; ++y)
      {
        const double r = (Y2 - y) * temp;
        const double f = 1.0 - r;
        const double X = r * X1 + f * X2;
        if (extent[2] <= y && y <= extent[3])
        {
          const int zyidx = (z - extent[4]) * zInc + (y - extent[2]);
          zymatrix[zyidx].emplace_back(X, sign);
        }
      }
    }
  }

  return sign;
}

template <typename TInputMesh, typename TOutputImage>
void
TriangleMeshToBinaryImageFilter<TInputMesh, TOutputImage>::RasterizeTriangles()
{
  const InputMeshPointer input = this->GetInput(0);

  const InputPointsContainerPointer myPoints = input->GetPoints();
  InputPointsContainerIterator      points = myPoints->Begin();

  int extent[6];

  // create a similar extent like vtk
  extent[0] = m_Index[0];
  extent[1] = m_Size[0] - 1;
  extent[2] = m_Index[1];
  extent[3] = m_Size[1] - 1;
  extent[4] = m_Index[2];
  extent[5] = m_Size[2] - 1;

  const OutputImagePointer OutputImage = this->GetOutput();

  // need to transform points from physical to index coordinates
  auto                    NewPoints = PointsContainer::New();
  auto                    NewPointSet = PointSetType::New();
  PointSetType::PointType newpoint;

  unsigned int pointId = 0;

  while (points != myPoints->End())
  {
    const PointType p = points.Value();
    // the index value type must match the point value type
    const ContinuousIndex<PointType::ValueType, 3> ind =
      OutputImage->template TransformPhysicalPointToContinuousIndex<PointType::ValueType>(p);
    NewPoints->InsertElement(pointId++, ind);

    ++points;
  }
  NewPointSet->SetPoints(NewPoints);

  // the stencil is kept in 'zymatrix' that provides
  // the x extents for each (y,z) coordinate for which a ray
  // parallel to the x axis intersects the polydata
  const int    zInc = extent[3] - extent[2] + 1;
  const int    zSize = extent[5] - extent[4] + 1;
  Point1DArray zymatrix(zInc * zSize);
  PointVector  coords;

  const CellsContainerPointer cells = input->GetCells();
  CellsContainerIterator      cellIt = cells->Begin();

  while (cellIt != cells->End())
  {
    CellType *                         nextCell = cellIt->Value();
    typename CellType::PointIdIterator pointIt = nextCell->PointIdsBegin();
    PointType                          p;

    switch (nextCell->GetType())
    {
      case CellGeometryEnum::VERTEX_CELL:
      case CellGeometryEnum::LINE_CELL:
        break;
      case CellGeometryEnum::TRIANGLE_CELL:
      case CellGeometryEnum::POLYGON_CELL:
      {
        coords.clear();
        while (pointIt != nextCell->PointIdsEnd())
        {
          if (!NewPointSet->GetPoint(*pointIt++, &newpoint))
          {
            itkExceptionMacro("Point with id " << *pointIt - 1 << " does not exist in the new pointset");
          }
          p[0] = newpoint[0];
          p[1] = newpoint[1];
          p[2] = newpoint[2];
          coords.push_back(p);
        }
        this->PolygonToImageRaster(coords, zymatrix, extent);
      }
      break;
      default:
        itkExceptionMacro("Need Triangle or Polygon cells ONLY");
    }
    ++cellIt;
  }

  const OutputImagePointer outputImage = this->GetOutput();
  outputImage->FillBuffer(m_OutsideValue);

  for (int z = extent[4]; z <= extent[5]; ++z)
  {
    for (int y = extent[2]; y <= extent[3]; ++y)
    {
      const int     zyidx = (z - extent[4]) * zInc + (y - extent[2]);
      Point1DVector xlist = zymatrix[zyidx];

      if (xlist.size() <= 1)
      {
        continue; // this is a peripheral point in the zy projection plane
      }

      std::sort(xlist.begin(), xlist.end(), ComparePoints1D);

      // get the first entry
      double lastx = xlist[0].m_X;
      int    lastSign = xlist[0].m_Sign;
      int    signproduct = 1;

      // if adjacent x values are within tolerance of each
      // other, check whether the number of 'exits' and
      // 'entrances' are equal (via signproduct) and if so,
      // ignore all x values, but if not, then count
      // them as a single intersection of the ray with the
      // surface

      std::vector<double> nlist;
      const size_t        m = xlist.size();
      for (size_t j = 1; j < m; ++j)
      {
        const Point1D p1D = xlist[j];
        const double  x = p1D.m_X;
        const int     sign = p1D.m_Sign;

        // check absolute distance from lastx to x
        if (itk::Math::abs(x - lastx) > m_Tolerance)
        {
          signproduct = sign * lastSign;
          if (signproduct < 0)
          {
            nlist.push_back(lastx);
          }
        }
        lastx = x;
        lastSign = sign;
      }

      nlist.push_back(lastx);

      // create the stencil extents
      int       minx1 = extent[0]; // minimum allowable x1 value
      const int n = static_cast<int>(nlist.size()) / 2;

      for (int i = 0; i < n; ++i)
      {
        auto x1 = static_cast<int>(std::ceil(nlist[2 * i]));
        auto x2 = static_cast<int>(std::floor(nlist[2 * i + 1]));

        if (x2 < extent[0] || x1 > (extent[1]))
        {
          continue;
        }
        x1 = (x1 > minx1) ? (x1) : (minx1);         // max(x1,minx1)
        x2 = (x2 < extent[1]) ? (x2) : (extent[1]); // min(x2,extent[1])

        if (x2 >= x1)
        {
          IndexType ind;
          ind[1] = y;
          ind[2] = z;
          for (int idX = x1; idX <= x2; ++idX)
          {
            // TODO: check whether replacing this for loop by ImageScanlineIterator is faster
            ind[0] = idX;
            outputImage->SetPixel(ind, m_InsideValue);
          }
        }
        // next x1 value must be at least x2+1
        minx1 = x2 + 1;
      }
    }
  }
}

template <typename TInputMesh, typename TOutputImage>
void
TriangleMeshToBinaryImageFilter<TInputMesh, TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Size : " << m_Size << std::endl;
  os << indent << "Inside Value : " << static_cast<typename NumericTraits<ValueType>::PrintType>(m_InsideValue)
     << std::endl;
  os << indent << "Outside Value : " << static_cast<typename NumericTraits<ValueType>::PrintType>(m_OutsideValue)
     << std::endl;
  os << indent << "Tolerance: " << m_Tolerance << std::endl;
  os << indent << "Origin: " << m_Origin << std::endl;
  os << indent << "Spacing: " << m_Spacing << std::endl;
  os << indent << "Direction: " << std::endl << m_Direction << std::endl;
  os << indent << "Index: " << m_Index << std::endl;
}
} // end namespace itk

#endif
