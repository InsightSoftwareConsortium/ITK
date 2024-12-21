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
#ifndef itkQuadEdgeMeshParamMatrixCoefficients_h
#define itkQuadEdgeMeshParamMatrixCoefficients_h

#include "itkQuadEdgeMesh.h"
#include "itkTriangleHelper.h"
#include "itkMath.h"

namespace itk
{
/** \class MatrixCoefficients
 * \brief Superclass for all the matrix coefficients computation classes.
 * \note  Belongs to the parameterisation package.
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInputMesh>
class MatrixCoefficients
{
public:
  using InputMeshType = TInputMesh;
  using InputCoordinateType = typename InputMeshType::CoordinateType;
#ifndef ITK_FUTURE_LEGACY_REMOVE
  using InputCoordRepType ITK_FUTURE_DEPRECATED(
    "ITK 6 discourages using `InputCoordRepType`. Please use `InputCoordinateType` instead!") = InputCoordinateType;
#endif
  using InputQEType = typename InputMeshType::QEType;

  MatrixCoefficients() = default;
  virtual ~MatrixCoefficients() = default;

  virtual InputCoordinateType
  operator()(const InputMeshType * iMesh, InputQEType * iEdge) const = 0;
};

/** \class OnesMatrixCoefficients
 * \brief Compute a matrix filled by 1s wherever two vertices are connected
 *        by an edge.
 * \note  Belongs to the parameterisation package.
 * \note  See paper:
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInputMesh>
class ITK_TEMPLATE_EXPORT OnesMatrixCoefficients : public MatrixCoefficients<TInputMesh>
{
public:
  using Superclass = MatrixCoefficients<TInputMesh>;

  using InputMeshType = TInputMesh;
  using InputCoordinateType = typename InputMeshType::CoordinateType;
#ifndef ITK_FUTURE_LEGACY_REMOVE
  using InputCoordRepType ITK_FUTURE_DEPRECATED(
    "ITK 6 discourages using `InputCoordRepType`. Please use `InputCoordinateType` instead!") = InputCoordinateType;
#endif
  using InputQEType = typename InputMeshType::QEType;

  OnesMatrixCoefficients() = default;

  /**
   * \return \f$ 1 \f$
   */
  InputCoordinateType
  operator()(const InputMeshType * itkNotUsed(iMesh), InputQEType * itkNotUsed(iEdge)) const override
  {
    return 1.0;
  }
};

/** \class InverseEuclideanDistanceMatrixCoefficients
 * \brief Compute a matrix filed with the inverse of the euclidean distance
 *        wherever two vertices are connected by an edge.
 * \note  Belongs to the parameterisation package.
 * \note  See paper: ...
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInputMesh>
class ITK_TEMPLATE_EXPORT InverseEuclideanDistanceMatrixCoefficients : public MatrixCoefficients<TInputMesh>
{
public:
  using Superclass = MatrixCoefficients<TInputMesh>;

  using InputMeshType = TInputMesh;
  using InputCoordinateType = typename InputMeshType::CoordinateType;
#ifndef ITK_FUTURE_LEGACY_REMOVE
  using InputCoordRepType ITK_FUTURE_DEPRECATED(
    "ITK 6 discourages using `InputCoordRepType`. Please use `InputCoordinateType` instead!") = InputCoordinateType;
#endif
  using InputPointType = typename InputMeshType::PointType;
  using InputPointIdentifier = typename InputMeshType::PointIdentifier;
  using InputQEType = typename InputMeshType::QEType;
  using InputVectorType = typename InputMeshType::VectorType;

  InverseEuclideanDistanceMatrixCoefficients() = default;

  /**
   * \param[in] iMesh
   * \param[in] iEdge
   * \return \f$ \frac{1}{\|\boldsymbol{p1} - \boldsymbol{p2} \|} \f$
   */
  InputCoordinateType
  operator()(const InputMeshType * iMesh, InputQEType * iEdge) const override
  {
    const InputPointIdentifier id1 = iEdge->GetOrigin();
    const InputPointIdentifier id2 = iEdge->GetDestination();

    const InputPointType pt1 = iMesh->GetPoint(id1);
    const InputPointType pt2 = iMesh->GetPoint(id2);

    const InputCoordinateType oValue = 1.0 / pt1.EuclideanDistanceTo(pt2);

    return oValue;
  }
};

/** \class ConformalMatrixCoefficients
 * \brief Compute a matrix filed by Conformal Coefficients of the edge
 *        wherever two vertices are connected by an edge.
 * \note  Belongs to the parameterisation package.
 * \note  See paper ...
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInputMesh>
class ITK_TEMPLATE_EXPORT ConformalMatrixCoefficients : public MatrixCoefficients<TInputMesh>
{
public:
  using Superclass = MatrixCoefficients<TInputMesh>;

  using InputMeshType = TInputMesh;
  using InputCoordinateType = typename InputMeshType::CoordinateType;
#ifndef ITK_FUTURE_LEGACY_REMOVE
  using InputCoordRepType ITK_FUTURE_DEPRECATED(
    "ITK 6 discourages using `InputCoordRepType`. Please use `InputCoordinateType` instead!") = InputCoordinateType;
#endif
  using InputPointType = typename InputMeshType::PointType;
  using InputPointIdentifier = typename InputMeshType::PointIdentifier;
  using InputQEType = typename InputMeshType::QEType;

  ConformalMatrixCoefficients() = default;

  /**
   * \param[in] iMesh
   * \param[in] iEdge
   * \return \f$ \cot \alpha_{ij} + \cot \beta_{ij} \f$
   */
  InputCoordinateType
  operator()(const InputMeshType * iMesh, InputQEType * iEdge) const override
  {
    const InputPointIdentifier id1 = iEdge->GetOrigin();
    const InputPointIdentifier id2 = iEdge->GetDestination();
    const InputPointType       pt1 = iMesh->GetPoint(id1);
    const InputPointType       pt2 = iMesh->GetPoint(id2);

    InputCoordinateType oValue(0.0);

    if (iEdge->IsLeftSet())
    {
      const InputPointIdentifier idA = iEdge->GetLnext()->GetDestination();
      const InputPointType       ptA = iMesh->GetPoint(idA);
      oValue += TriangleHelper<InputPointType>::Cotangent(pt1, ptA, pt2);
    }
    if (iEdge->IsRightSet())
    {
      const InputPointIdentifier idB = iEdge->GetRnext()->GetOrigin();
      const InputPointType       ptB = iMesh->GetPoint(idB);
      oValue += TriangleHelper<InputPointType>::Cotangent(pt1, ptB, pt2);
    }

    return std::max(InputCoordinateType{}, oValue);
  }
};

/** \class AuthalicMatrixCoefficients
 *
 * \brief Compute a matrix filled with Authalic Coefficients of the edge,
 *        wherever two vertices are connected with an edge.
 * \note  Belongs to the Parameterisation package.
 * \note  See paper:
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInputMesh>
class ITK_TEMPLATE_EXPORT AuthalicMatrixCoefficients : public MatrixCoefficients<TInputMesh>
{
public:
  using Superclass = MatrixCoefficients<TInputMesh>;

  using InputMeshType = TInputMesh;
  using InputCoordinateType = typename InputMeshType::CoordinateType;
#ifndef ITK_FUTURE_LEGACY_REMOVE
  using InputCoordRepType ITK_FUTURE_DEPRECATED(
    "ITK 6 discourages using `InputCoordRepType`. Please use `InputCoordinateType` instead!") = InputCoordinateType;
#endif
  using InputPointType = typename InputMeshType::PointType;
  using InputPointIdentifier = typename InputMeshType::PointIdentifier;
  using InputQEType = typename InputMeshType::QEType;

  AuthalicMatrixCoefficients() = default;

  /**
   * \param[in] iMesh
   * \param[in] iEdge
   * \return \f$ \frac{\cot \gamma_{ij} + \cot
   \delta_{ij}}{\|\boldsymbol{p1} - \boldsymbol{p2} \|^2} \f$
   */
  InputCoordinateType
  operator()(const InputMeshType * iMesh, InputQEType * iEdge) const override
  {
    const InputPointIdentifier id1 = iEdge->GetOrigin();
    const InputPointType       pt1 = iMesh->GetPoint(id1);

    const InputPointIdentifier id2 = iEdge->GetDestination();
    const InputPointType       pt2 = iMesh->GetPoint(id2);

    InputCoordinateType oValue{};

    if (iEdge->IsLeftSet())
    {
      const InputPointIdentifier idA = iEdge->GetLnext()->GetDestination();
      const InputPointType       ptA = iMesh->GetPoint(idA);
      oValue += TriangleHelper<InputPointType>::Cotangent(pt1, pt2, ptA);
    }

    if (iEdge->IsRightSet())
    {
      const InputPointIdentifier idB = iEdge->GetRnext()->GetOrigin();
      const InputPointType       ptB = iMesh->GetPoint(idB);
      oValue += TriangleHelper<InputPointType>::Cotangent(pt1, pt2, ptB);
    }

    return oValue / pt1.SquaredEuclideanDistanceTo(pt2);
  }
};

/** \class IntrinsicMatrixCoefficients
 * \brief Compute a matrix filled by intrinsic Coefficients of the edge,
 *        wherever two vertices are connected by an edge.
 * \note  Belongs to the parameterization Package.
 * \note  See paper:
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInputMesh>
class ITK_TEMPLATE_EXPORT IntrinsicMatrixCoefficients : public MatrixCoefficients<TInputMesh>
{
public:
  using Superclass = MatrixCoefficients<TInputMesh>;

  using InputMeshType = TInputMesh;
  using InputCoordinateType = typename InputMeshType::CoordinateType;
#ifndef ITK_FUTURE_LEGACY_REMOVE
  using InputCoordRepType ITK_FUTURE_DEPRECATED(
    "ITK 6 discourages using `InputCoordRepType`. Please use `InputCoordinateType` instead!") = InputCoordinateType;
#endif
  using InputQEType = typename InputMeshType::QEType;

  InputCoordinateType m_Lambda;

  IntrinsicMatrixCoefficients(const InputCoordinateType & iLambda)
    : m_Lambda(iLambda)
  {}

  InputCoordinateType
  operator()(const InputMeshType * iMesh, InputQEType * iEdge) const override
  {
    const AuthalicMatrixCoefficients<TInputMesh>  authalic;
    const ConformalMatrixCoefficients<TInputMesh> conformal;

    const InputCoordinateType oValue = m_Lambda * conformal(iMesh, iEdge) + (1.0 - m_Lambda) * authalic(iMesh, iEdge);

    return oValue;
  }
};

/** \class HarmonicMatrixCoefficients
 * \brief Compute a matrix filled with Harmonic coefficients, wherever
 *        two vertices are connected by an edge.
 * \note  Belongs to the parameterization package.
 * \note  See paper:
 * \ingroup ITKQuadEdgeMeshFiltering
 */
template <typename TInputMesh>
class ITK_TEMPLATE_EXPORT HarmonicMatrixCoefficients : public MatrixCoefficients<TInputMesh>
{
public:
  using Superclass = MatrixCoefficients<TInputMesh>;

  using InputMeshType = TInputMesh;
  using InputCoordinateType = typename InputMeshType::CoordinateType;
#ifndef ITK_FUTURE_LEGACY_REMOVE
  using InputCoordRepType ITK_FUTURE_DEPRECATED(
    "ITK 6 discourages using `InputCoordRepType`. Please use `InputCoordinateType` instead!") = InputCoordinateType;
#endif
  using InputPointType = typename InputMeshType::PointType;
  using InputVectorType = typename InputPointType::VectorType;
  using InputPointIdentifier = typename InputMeshType::PointIdentifier;
  using InputQEType = typename InputMeshType::QEType;

  static constexpr unsigned int PointDimension = InputPointType::PointDimension;

  HarmonicMatrixCoefficients() = default;

  InputCoordinateType
  operator()(const InputMeshType * iMesh, InputQEType * iEdge) const override
  {
    const InputPointIdentifier id1 = iEdge->GetOrigin();
    const InputPointIdentifier id2 = iEdge->GetDestination();

    const InputPointIdentifier idA = iEdge->GetLnext()->GetDestination();
    const InputPointIdentifier idB = iEdge->GetRnext()->GetOrigin();

    const InputPointType pt1 = iMesh->GetPoint(id1);
    const InputPointType pt2 = iMesh->GetPoint(id2);
    const InputPointType ptA = iMesh->GetPoint(idA);
    const InputPointType ptB = iMesh->GetPoint(idB);

    const InputVectorType v1A = ptA - pt1;
    const InputVectorType v1B = ptB - pt1;
    const InputVectorType v12 = pt2 - pt1;

    const InputCoordinateType L1A = v1A * v1A;
    const InputCoordinateType L1B = v1B * v1B;
    const InputCoordinateType L12 = v12 * v12;

    const InputCoordinateType L2A = pt2.SquaredEuclideanDistanceTo(ptA);
    const InputCoordinateType L2B = pt2.SquaredEuclideanDistanceTo(ptB);

    const CrossHelper<InputVectorType> cross;

    const InputCoordinateType AreaA = 0.5 * (cross(v1A, v12).GetNorm());
    const InputCoordinateType AreaB = 0.5 * (cross(v1B, v12).GetNorm());

    const InputCoordinateType oValue = (L1A + L2A - L12) / AreaA + (L1B + L2B - L12) / AreaB;

    return oValue;
  }
};
} // namespace itk
#endif
