/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkHexahedronCell_hxx
#define itkHexahedronCell_hxx
#include "itkHexahedronCell.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/algo/vnl_determinant.h"

namespace itk
{

// C++11 work-around for compile time minimize compatible with constexpr
// https://stackoverflow.com/a/40285868/485602
template <class T>
constexpr T &
hexahedron_constexpr_min(T & a, T & b)
{
  return a > b ? b : a;
}

/**
 * Standard CellInterface:
 */
template <typename TCellInterface>
void
HexahedronCell<TCellInterface>::MakeCopy(CellAutoPointer & cellPointer) const
{
  cellPointer.TakeOwnership(new Self);
  cellPointer->SetPointIds(this->GetPointIds());
}

/**
 * Standard CellInterface:
 * Get the topological dimension of this cell.
 */
template <typename TCellInterface>
unsigned int
HexahedronCell<TCellInterface>::GetDimension() const
{
  return Self::CellDimension3D;
}

/**
 * Standard CellInterface:
 * Get the number of points required to define the cell.
 */
template <typename TCellInterface>
unsigned int
HexahedronCell<TCellInterface>::GetNumberOfPoints() const
{
  return Self::NumberOfPoints;
}

/**
 * Standard CellInterface:
 * Get the number of boundary features of the given dimension.
 */
template <typename TCellInterface>
typename HexahedronCell<TCellInterface>::CellFeatureCount
HexahedronCell<TCellInterface>::GetNumberOfBoundaryFeatures(int dimension) const
{
  switch (dimension)
  {
    case 0:
      return GetNumberOfVertices();
    case 1:
      return GetNumberOfEdges();
    case 2:
      return GetNumberOfFaces();
    default:
      return 0;
  }
}

/**
 * Standard CellInterface:
 * Get the boundary feature of the given dimension specified by the given
 * cell feature Id.
 * The Id can range from 0 to GetNumberOfBoundaryFeatures(dimension)-1.
 */
template <typename TCellInterface>
bool
HexahedronCell<TCellInterface>::GetBoundaryFeature(int                   dimension,
                                                   CellFeatureIdentifier featureId,
                                                   CellAutoPointer &     cellPointer)
{
  switch (dimension)
  {
    case 0:
    {
      VertexAutoPointer vertexPointer;
      if (this->GetVertex(featureId, vertexPointer))
      {
        TransferAutoPointer(cellPointer, vertexPointer);
        return true;
      }
      break;
    }
    case 1:
    {
      EdgeAutoPointer edgePointer;
      if (this->GetEdge(featureId, edgePointer))
      {
        TransferAutoPointer(cellPointer, edgePointer);
        return true;
      }
      break;
    }
    case 2:
    {
      FaceAutoPointer facePointer;
      if (this->GetFace(featureId, facePointer))
      {
        TransferAutoPointer(cellPointer, facePointer);
        return true;
      }
      break;
    }
    default:
      break; // just fall through
  }
  cellPointer.Reset();
  return false;
}

/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the given
 * iterator can be incremented and safely de-referenced enough times to
 * get all the point ids needed by the cell.
 */
template <typename TCellInterface>
void
HexahedronCell<TCellInterface>::SetPointIds(PointIdConstIterator first)
{
  PointIdConstIterator ii(first);

  for (unsigned int i = 0; i < Self::NumberOfPoints; ++i)
  {
    m_PointIds[i] = *ii++;
  }
}

/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the range
 * of iterators [first, last) contains the correct number of points needed to
 * define the cell.  The position *last is NOT referenced, so it can safely
 * be one beyond the end of an array or other container.
 */
template <typename TCellInterface>
void
HexahedronCell<TCellInterface>::SetPointIds(PointIdConstIterator first, PointIdConstIterator last)
{
  int                  localId = 0;
  PointIdConstIterator ii(first);

  while (ii != last)
  {
    m_PointIds[localId++] = *ii++;
  }
}

/**
 * Standard CellInterface:
 * Set an individual point identifier in the cell.
 */
template <typename TCellInterface>
void
HexahedronCell<TCellInterface>::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}

/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TCellInterface>
typename HexahedronCell<TCellInterface>::PointIdIterator
HexahedronCell<TCellInterface>::PointIdsBegin()
{
  return &m_PointIds[0];
}

/**
 * Standard CellInterface:
 * Get a const begin iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TCellInterface>
typename HexahedronCell<TCellInterface>::PointIdConstIterator
HexahedronCell<TCellInterface>::PointIdsBegin() const
{
  return &m_PointIds[0];
}

/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */
template <typename TCellInterface>
typename HexahedronCell<TCellInterface>::PointIdIterator
HexahedronCell<TCellInterface>::PointIdsEnd()
{
  return &m_PointIds[Self::NumberOfPoints - 1] + 1;
}

/**
 * Standard CellInterface:
 * Get a const end iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TCellInterface>
typename HexahedronCell<TCellInterface>::PointIdConstIterator
HexahedronCell<TCellInterface>::PointIdsEnd() const
{
  return &m_PointIds[Self::NumberOfPoints - 1] + 1;
}

/**
 * Hexahedron-specific:
 * Get the number of vertices defining the hexahedron.
 */
template <typename TCellInterface>
typename HexahedronCell<TCellInterface>::CellFeatureCount
HexahedronCell<TCellInterface>::GetNumberOfVertices() const
{
  return Self::NumberOfVertices;
}

/**
 * Hexahedron-specific:
 * Get the number of edges defined for the hexahedron.
 */
template <typename TCellInterface>
typename HexahedronCell<TCellInterface>::CellFeatureCount
HexahedronCell<TCellInterface>::GetNumberOfEdges() const
{
  return Self::NumberOfEdges;
}

/**
 * Hexahedron-specific:
 * Get the number of faces defined for the hexahedron.
 */
template <typename TCellInterface>
typename HexahedronCell<TCellInterface>::CellFeatureCount
HexahedronCell<TCellInterface>::GetNumberOfFaces() const
{
  return Self::NumberOfFaces;
}

/**
 * Hexahedron-specific:
 * Get the vertex specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfVertices()-1.
 */
template <typename TCellInterface>
bool
HexahedronCell<TCellInterface>::GetVertex(CellFeatureIdentifier vertexId, VertexAutoPointer & vertexPointer)
{
  auto * vert = new VertexType;

  vert->SetPointId(0, m_PointIds[vertexId]);
  vertexPointer.TakeOwnership(vert);
  return true;
}

/**
 * Hexahedron-specific:
 * Get the edge specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfEdges()-1.
 */
template <typename TCellInterface>
bool
HexahedronCell<TCellInterface>::GetEdge(CellFeatureIdentifier edgeId, EdgeAutoPointer & edgePointer)
{
  auto * edge = new EdgeType;

  for (unsigned int i = 0; i < EdgeType::NumberOfPoints; ++i)
  {
    edge->SetPointId(i, m_PointIds[m_Edges[edgeId][i]]);
  }
  edgePointer.TakeOwnership(edge);
  return true;
}

/**
 * Hexahedron-specific:
 * Get the face specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfFaces()-1.
 */
template <typename TCellInterface>
bool
HexahedronCell<TCellInterface>::GetFace(CellFeatureIdentifier faceId, FaceAutoPointer & facePointer)
{
  auto * face = new FaceType;

  for (unsigned int i = 0; i < FaceType::NumberOfPoints; ++i)
  {
    face->SetPointId(i, m_PointIds[m_Faces[faceId][i]]);
  }
  facePointer.TakeOwnership(face);
  return true;
}

/** Evaluate the position inside the cell */
template <typename TCellInterface>
bool
HexahedronCell<TCellInterface>::EvaluatePosition(CoordRepType *            x,
                                                 PointsContainer *         points,
                                                 CoordRepType *            closestPoint,
                                                 CoordRepType              pcoord[],
                                                 double *                  dist2,
                                                 InterpolationWeightType * weight)
{
  // Throw an exception if trying to EvaluatePosition for anything other than
  // a 3D point or cell dimension. This implementation is hard-coded to 3D.
  if ((Self::CellDimension3D != 3) || (Self::PointDimension3D != 3))
  {
    itkGenericExceptionMacro("ERROR: only 3D supported for HexahedronCell");
    // return false;
  }

  static constexpr int    ITK_HEX_MAX_ITERATION = 10;
  static constexpr double ITK_HEX_CONVERGED = 1.e-03;
  static constexpr double ITK_DIVERGED = 1.e6;

  double                  params[Self::CellDimension3D]{ 0.5, 0.5, 0.5 };
  double                  fcol[Self::PointDimension3D];
  double                  rcol[Self::PointDimension3D];
  double                  scol[Self::PointDimension3D];
  double                  tcol[Self::PointDimension3D];
  double                  d;
  PointType               pt;
  CoordRepType            derivs[CellDimension3D * Self::NumberOfPoints];
  InterpolationWeightType weights[Self::NumberOfPoints];

  //  set initial position for Newton's method
  int          subId{ 0 };
  CoordRepType pcoords[CellDimension3D]{ 0.5, 0.5, 0.5 };

  // NOTE: Avoid compiler warning.  The code below only runs if PointType::Dimension == Self::PointDimension3D
  constexpr unsigned int PREVENT_OVERRUN_OF_INVALID_INSTANTIATIONS =
    hexahedron_constexpr_min(PointType::Dimension, Self::PointDimension3D);

  //  enter iteration loop
  int converged{ 0 };
  for (int iteration = 0; !converged && (iteration < ITK_HEX_MAX_ITERATION); ++iteration)
  {
    //  calculate element interpolation functions and derivatives
    this->InterpolationFunctions(pcoords, weights);
    this->InterpolationDerivs(pcoords, derivs);

    //  calculate newton functions
    for (unsigned int i = 0; i < Self::PointDimension3D; ++i)
    {
      fcol[i] = rcol[i] = scol[i] = tcol[i] = 0.0;
    }
    for (unsigned int i = 0; i < Self::NumberOfPoints; ++i)
    {

      pt = points->GetElement(m_PointIds[i]);
      for (unsigned int j = 0; j < PREVENT_OVERRUN_OF_INVALID_INSTANTIATIONS; ++j)
      {
        fcol[j] += pt[j] * weights[i];
        rcol[j] += pt[j] * derivs[i];
        scol[j] += pt[j] * derivs[i + Self::NumberOfPoints];
        tcol[j] += pt[j] * derivs[i + 2 * Self::NumberOfPoints];
      }
    }

    for (unsigned int i = 0; i < Self::PointDimension3D; i++)
    {
      fcol[i] -= x[i];
    }

    constexpr unsigned int HARD_CODED_POINT_DIM = 3; // This variable is used to
    static_assert(Self::PointDimension3D == HARD_CODED_POINT_DIM,
                  "ERROR: Self::PointDimension3D does not equal HARD_CODED_POINT_DIM (i.e. 3).");
    //  compute determinants and generate improvements
    vnl_matrix_fixed<CoordRepType, HARD_CODED_POINT_DIM, CellDimension3D> mat;
    for (unsigned int i = 0; i < Self::PointDimension3D; ++i)
    {
      mat.put(0, i, rcol[i]);
      mat.put(1, i, scol[i]);
      mat.put(2, i, tcol[i]);
    }

    // ONLY 3x3 determinants are supported.
    d = vnl_determinant(mat);
    // d=vtkMath::Determinant3x3(rcol,scol,tcol);
    if (std::abs(d) < 1.e-20)
    {
      return false;
    }

    vnl_matrix_fixed<CoordRepType, HARD_CODED_POINT_DIM, CellDimension3D> mat1;
    for (unsigned int i = 0; i < Self::PointDimension3D; ++i)
    {
      mat1.put(0, i, fcol[i]);
      mat1.put(1, i, scol[i]);
      mat1.put(2, i, tcol[i]);
    }

    vnl_matrix_fixed<CoordRepType, HARD_CODED_POINT_DIM, CellDimension3D> mat2;
    for (unsigned int i = 0; i < Self::PointDimension3D; ++i)
    {
      mat2.put(0, i, rcol[i]);
      mat2.put(1, i, fcol[i]);
      mat2.put(2, i, tcol[i]);
    }

    vnl_matrix_fixed<CoordRepType, HARD_CODED_POINT_DIM, CellDimension3D> mat3;
    for (unsigned int i = 0; i < Self::PointDimension3D; ++i)
    {
      mat3.put(0, i, rcol[i]);
      mat3.put(1, i, scol[i]);
      mat3.put(2, i, fcol[i]);
    }

    pcoords[0] = params[0] - vnl_determinant(mat1) / d;
    pcoords[1] = params[1] - vnl_determinant(mat2) / d;
    pcoords[2] = params[2] - vnl_determinant(mat3) / d;

    if (pcoord)
    {
      pcoord[0] = pcoords[0];
      pcoord[1] = pcoords[1];
      pcoord[2] = pcoords[2];
    }

    //  check for convergence
    if (((std::abs(pcoords[0] - params[0])) < ITK_HEX_CONVERGED) &&
        ((std::abs(pcoords[1] - params[1])) < ITK_HEX_CONVERGED) &&
        ((std::abs(pcoords[2] - params[2])) < ITK_HEX_CONVERGED))
    {
      converged = 1;
    }

    // Test for bad divergence (S.Hirschberg 11.12.2001)
    else if ((std::abs(pcoords[0]) > ITK_DIVERGED) || (std::abs(pcoords[1]) > ITK_DIVERGED) ||
             (std::abs(pcoords[2]) > ITK_DIVERGED))
    {
      return -1;
    }

    //  if not converged, repeat
    else
    {
      params[0] = pcoords[0];
      params[1] = pcoords[1];
      params[2] = pcoords[2];
    }
  }

  //  if not converged, set the parametric coordinates to arbitrary values
  //  outside of element
  if (!converged)
  {
    return false;
  }

  this->InterpolationFunctions(pcoords, weights);

  constexpr unsigned int HARD_CODED_WEIGHTS_DIM = 8;
  static_assert(Self::NumberOfPoints == HARD_CODED_WEIGHTS_DIM,
                "ERROR: Self::NumberOfPoints does not equal HARD_CODED_WEIGHTS_DIM (i.e. 8)");
  if (weight)
  {
    for (unsigned int i = 0; i < HARD_CODED_WEIGHTS_DIM; ++i)
    {
      weight[i] = weights[i];
    }
  }

  if (pcoords[0] >= -0.001 && pcoords[0] <= 1.001 && pcoords[1] >= -0.001 && pcoords[1] <= 1.001 &&
      pcoords[2] >= -0.001 && pcoords[2] <= 1.001)
  {
    if (closestPoint)
    {
      closestPoint[0] = x[0];
      closestPoint[1] = x[1];
      closestPoint[2] = x[2];
      *dist2 = 0.0; // inside hexahedron
    }
    return true;
  }
  else
  {
    CoordRepType pc[CellDimension3D], w[Self::NumberOfPoints];
    if (closestPoint)
    {
      for (unsigned int i = 0; i < CellDimension3D; ++i) // only approximate, not really true
                                                         // for warped hexa
      {
        if (pcoords[i] < 0.0)
        {
          pc[i] = 0.0;
        }
        else if (pcoords[i] > 1.0)
        {
          pc[i] = 1.0;
        }
        else
        {
          pc[i] = pcoords[i];
        }
      }
      this->EvaluateLocation(subId, points, pc, closestPoint, (InterpolationWeightType *)w);

      *dist2 = 0;
      for (unsigned int i = 0; i < Self::PointDimension3D; ++i)
      {
        *dist2 += (closestPoint[i] - x[i]) * (closestPoint[i] - x[i]);
      }
    }
    return false;
  }
}

/** Compute iso-parametric interpolation functions */
template <typename TCellInterface>
void
HexahedronCell<TCellInterface>::InterpolationFunctions(CoordRepType            pcoords[Self::CellDimension],
                                                       InterpolationWeightType sf[Self::NumberOfPoints])
{
  // Throw an exception if trying to EvaluatePosition for anything other than
  // a 3D point or cell dimension. This implementation is hard-coded to 3D.
  if ((Self::CellDimension3D != 3) || (Self::PointDimension3D != 3))
  {
    itkGenericExceptionMacro("ERROR: only 3D supported for HexahedronCell");
    // return false;
  }
  else
  {
    CoordRepType pcoords3D[Self::CellDimension3D]{ 0 };
    // NOTE: Avoid compiler warning.  The code below only runs if PointType::Dimension == Self::PointDimension3D
    constexpr unsigned int PREVENT_OVERRUN_OF_INVALID_INSTANTIATIONS =
      hexahedron_constexpr_min(PointType::Dimension, Self::PointDimension3D);
    for (unsigned int k = 0; k < PREVENT_OVERRUN_OF_INVALID_INSTANTIATIONS; ++k)
    {
      pcoords3D[k] = pcoords[k];
    }

    const double rm = 1. - pcoords3D[0];
    const double sm = 1. - pcoords3D[1];
    const double tm = 1. - pcoords3D[2];

    sf[0] = rm * sm * tm;
    sf[1] = pcoords3D[0] * sm * tm;
    sf[2] = pcoords3D[0] * pcoords3D[1] * tm;
    sf[3] = rm * pcoords3D[1] * tm;
    sf[4] = rm * sm * pcoords3D[2];
    sf[5] = pcoords3D[0] * sm * pcoords3D[2];
    sf[6] = pcoords3D[0] * pcoords3D[1] * pcoords3D[2];
    sf[7] = rm * pcoords3D[1] * pcoords3D[2];

    for (unsigned int k = 0; k < PREVENT_OVERRUN_OF_INVALID_INSTANTIATIONS; ++k)
    {
      pcoords[k] = pcoords3D[k];
    }
  }
}

/** Compute iso-parametric interpolation functions */
template <typename TCellInterface>
void
HexahedronCell<TCellInterface>::InterpolationDerivs(CoordRepType pcoords[Self::CellDimension],
                                                    CoordRepType derivs[Self::CellDimension * Self::NumberOfPoints])
{
  // Throw an exception if trying to EvaluatePosition for anything other than
  // a 3D point or cell dimension. This implementation is hard-coded to 3D.
  if ((Self::CellDimension3D != 3) || (Self::PointDimension3D != 3))
  {
    itkGenericExceptionMacro("ERROR: only 3D supported for HexahedronCell");
    // return false;
  }
  else
  {

    CoordRepType pcoords3D[Self::CellDimension3D]{ 0 };
    CoordRepType derivs3D[Self::CellDimension3D * Self::NumberOfPoints]{ 0 };
    // NOTE: Avoid compiler warning.  The code below only runs if PointType::Dimension == Self::PointDimension3D
    constexpr unsigned int PREVENT_OVERRUN_OF_INVALID_INSTANTIATIONS =
      hexahedron_constexpr_min(PointType::Dimension, Self::PointDimension3D);
    for (unsigned int k = 0; k < PREVENT_OVERRUN_OF_INVALID_INSTANTIATIONS; ++k)
    {
      pcoords3D[k] = pcoords[k];
    }
    for (unsigned int k = 0; k < PREVENT_OVERRUN_OF_INVALID_INSTANTIATIONS * Self::NumberOfPoints; ++k)
    {
      derivs3D[k] = derivs[k];
    }

    const double rm = 1. - pcoords3D[0];
    const double sm = 1. - pcoords3D[1];
    const double tm = 1. - pcoords3D[2];

    // r-derivatives
    derivs3D[0] = -sm * tm;
    derivs3D[1] = sm * tm;
    derivs3D[2] = pcoords3D[1] * tm;
    derivs3D[3] = -pcoords3D[1] * tm;
    derivs3D[4] = -sm * pcoords3D[2];
    derivs3D[5] = sm * pcoords3D[2];
    derivs3D[6] = pcoords3D[1] * pcoords3D[2];
    derivs3D[7] = -pcoords3D[1] * pcoords3D[2];

    // s-derivatives
    derivs3D[8] = -rm * tm;
    derivs3D[9] = -pcoords3D[0] * tm;
    derivs3D[10] = pcoords3D[0] * tm;
    derivs3D[11] = rm * tm;
    derivs3D[12] = -rm * pcoords3D[2];
    derivs3D[13] = -pcoords3D[0] * pcoords3D[2];
    derivs3D[14] = pcoords3D[0] * pcoords3D[2];
    derivs3D[15] = rm * pcoords3D[2];

    // t-derivatives
    derivs3D[16] = -rm * sm;
    derivs3D[17] = -pcoords3D[0] * sm;
    derivs3D[18] = -pcoords3D[0] * pcoords3D[1];
    derivs3D[19] = -rm * pcoords3D[1];
    derivs3D[20] = rm * sm;
    derivs3D[21] = pcoords3D[0] * sm;
    derivs3D[22] = pcoords3D[0] * pcoords3D[1];
    derivs3D[23] = rm * pcoords3D[1];

    for (unsigned int k = 0; k < PREVENT_OVERRUN_OF_INVALID_INSTANTIATIONS; ++k)
    {
      pcoords[k] = pcoords3D[k];
    }
    for (unsigned int k = 0; k < PREVENT_OVERRUN_OF_INVALID_INSTANTIATIONS * Self::NumberOfPoints; ++k)
    {
      derivs[k] = derivs3D[k];
    }
  }
}


/** Evaluate the location inside the cell */
template <typename TCellInterface>
void
HexahedronCell<TCellInterface>::EvaluateLocation(int &                     itkNotUsed(subId),
                                                 PointsContainer *         points,
                                                 CoordRepType              pcoords[Self::CellDimension],
                                                 CoordRepType              x[Self::CellDimension],
                                                 InterpolationWeightType * weights)
{
  // Throw an exception if trying to EvaluatePosition for anything other than
  // a 3D point or cell dimension. This implementation is hard-coded to 3D.
  if ((Self::CellDimension3D != 3) || (Self::PointDimension3D != 3))
  {
    itkGenericExceptionMacro("ERROR: only 3D supported for HexahedronCell");
    // return false;
  }
  else
  {

    this->InterpolationFunctions(pcoords, weights);
    std::fill_n(x, Self::CellDimension3D, 0.0);

    // NOTE: Avoid compiler warning.  The code below only runs if PointType::Dimension == Self::PointDimension3D
    constexpr unsigned int PREVENT_OVERRUN_OF_INVALID_INSTANTIATIONS =
      hexahedron_constexpr_min(PointType::Dimension, Self::PointDimension3D);
    for (unsigned int i = 0; i < Self::NumberOfPoints; i++)
    {
      const PointType pt{ points->GetElement(m_PointIds[i]) };

      for (unsigned int j = 0; j < PREVENT_OVERRUN_OF_INVALID_INSTANTIATIONS; j++)
      {
        const CoordRepType t = pt[j] * weights[i];
        x[j] += t;
      }
    }
  }
}
} // end namespace itk

#endif
