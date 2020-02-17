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
#ifndef itkLaplacianDeformationQuadEdgeMeshFilter_h
#define itkLaplacianDeformationQuadEdgeMeshFilter_h

#include "itkQuadEdgeMeshParamMatrixCoefficients.h"
#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"
#include "ITKQuadEdgeMeshFilteringExport.h"

#include "itkConceptChecking.h"

#include <unordered_map>

namespace itk
{
/**\class LaplacianDeformationQuadEdgeMeshFilterEnums
 * \brief Contains all enum classes used by LaplacianDeformationQuadEdgeMeshFilter class.
 * \ingroup ITKQuadEdgeMeshFiltering
 */
class LaplacianDeformationQuadEdgeMeshFilterEnums
{
public:
  /**\class Area
   * \ingroup ITKQuadEdgeMeshFiltering
   * Type of area*/
  enum class Area : uint8_t
  {
    /** Do not use any area information*/
    NONE = 0,
    /** Use a mixed area*/
    MIXEDAREA
  };
};
// Define how to print enumeration
extern ITKQuadEdgeMeshFiltering_EXPORT std::ostream &
                                       operator<<(std::ostream & out, const LaplacianDeformationQuadEdgeMeshFilterEnums::Area value);

/** \class LaplacianDeformationQuadEdgeMeshFilter
 *
 *  \brief (abstract) base class for laplacian surface mesh deformation.
 *
 * Laplacian mesh deformation offers the ability to deform 3D surface mesh
 * while preserving local details.
 *
 * Laplacian-based approaches represent the surface by the so-called
 * differential coordinates or Laplacian coordinates \f$ \boldsymbol{ \delta }_i \f$.
 * These coordinates are obtained by applying the Laplacian operator to the mesh vertices:
 *
 * \f[
 * \boldsymbol{ \delta }_i = \Delta_{S}( \boldsymbol{ p }_i ) = - H_i \cdot
 * \boldsymbol{ n_i }
 * \f]
 *
 * where \f$H_i\f$ is the mean curvature \f$ ( \kappa_1 + \kappa_2 ) \f$ at the
 * vertex \f$v_i\f$.
 *
 * The deformation can be formulated by minimizing the difference from the
 * input surface coordinates \f$\delta_i\f$. With a continuous formulation, this
 * would lead to the minimization of the following energy:
 * \f[
 * \min_{\boldsymbol{p'}} \int_{\Omega} \| \boldsymbol{\Delta p'} - \boldsymbol{\delta} \| du dv
 * \f]
 *
 * The Euler-Lagrange equation derived:
 * \f[
 * \Delta^2 \boldsymbol{p'} = \Delta \boldsymbol{\delta}
 * \f]
 *
 * When considering the input surface as the parameter domain, the Laplace operator turns out into the Laplace-Beltrami
 * operator \f$ \Delta_S \f$: \f[ L^2 \boldsymbol{p'} = L \boldsymbol{ \delta } \f]
 *
 * which can be separated into 3 coordinate components.
 *
 * Then users can add positional constraints on some vertices:
 * \f[
 * \boldsymbol{p'}_j = \boldsymbol{c}_j
 * \f]
 *
 * If output positions must exactly match set constraints, see
 * LaplacianDeformationQuadEdgeMeshFilterWithHardConstraints. Else see
 * LaplacianDeformationQuadEdgeMeshFilterWithSoftConstraints
 *
 *
 * To make the deformation as generic as possible, user can provide/choose:
 * \li their own way to compute Laplacian by the means of m_CoefficientsMethod
 * \li Laplacian order by the means of m_Order (according to the literature for
 * m_Order > 3, it can becomes numerically unstable)
 * \li one normalization per vertex based on the local area on the mesh (MIXED_AREA)
 * \li Sparser solver used by the mean of the template parameter TSolverTraits
 *
 * \tparam TInputMesh Input Mesh Type
 * \tparam TOutputMesh Output Mesh Type
 * \tparam TSolverTraits Linear Sparse Solver Traits see VNLIterativeSparseSolverTraits and VNLSparseLUSolverTraits
 *
 *  For details, see https://hdl.handle.net/10380/3410
 *
 *  \ingroup ITKQuadEdgeMeshFiltering
 */
template <class TInputMesh, class TOutputMesh, class TSolverTraits>
class ITK_TEMPLATE_EXPORT LaplacianDeformationQuadEdgeMeshFilter
  : public QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(LaplacianDeformationQuadEdgeMeshFilter);

  /** Basic types. */
  using Self = LaplacianDeformationQuadEdgeMeshFilter;
  using Superclass = QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkTypeMacro(LaplacianDeformationQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter)

    /** Input types. */
    using InputMeshType = TInputMesh;
  using InputPointType = typename Superclass::InputPointType;

  static constexpr unsigned int InputPointDimension = InputMeshType::PointDimension;

  /** Output types. */
  using OutputMeshType = TOutputMesh;
  using OutputCoordRepType = typename Superclass::OutputCoordRepType;
  using OutputPointType = typename Superclass::OutputPointType;
  using OutputPointIdentifier = typename Superclass::OutputPointIdentifier;
  using OutputQEPrimal = typename Superclass::OutputQEPrimal;
  using OutputVectorType = typename Superclass::OutputVectorType;
  using OutputQEIterator = typename Superclass::OutputQEIterator;

  static constexpr unsigned int OutputPointDimension = OutputMeshType::PointDimension;

  using SolverTraits = TSolverTraits;
  using ValueType = typename SolverTraits::ValueType;
  using MatrixType = typename SolverTraits::MatrixType;
  using VectorType = typename SolverTraits::VectorType;

  using CoefficientsComputationType = MatrixCoefficients<OutputMeshType>;

  /** Set the coefficient method to compute the Laplacian matrix of the input mesh*/
  void
  SetCoefficientsMethod(CoefficientsComputationType * iMethod)
  {
    this->m_CoefficientsMethod = iMethod;
    this->Modified();
  }

  using TriangleType = TriangleHelper<OutputPointType>;

  /** Constrain vertex vId to the given location iP */
  void
  SetConstrainedNode(OutputPointIdentifier vId, const OutputPointType & iP);

  /** Set the displacement vector iV for the vertex vId */
  void
  SetDisplacement(OutputPointIdentifier vId, const OutputVectorType & iV);

  /** Get the displacement vector oV for the vertex vId.
   * Returns true if the vertex vId is a constraint, else false.
   */
  bool
  GetDisplacement(OutputPointIdentifier vId, OutputVectorType & oV) const;

  /** Clear all constraints added by the means of SetConstrainedNode or SetDisplacement.*/
  void
  ClearConstraints();

  /** Set/Get the Laplacian order */
  itkSetMacro(Order, unsigned int);
  itkGetMacro(Order, unsigned int);

  using AreaEnum = LaplacianDeformationQuadEdgeMeshFilterEnums::Area;
#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr AreaEnum NONE = AreaEnum::NONE;
  static constexpr AreaEnum MIXEDAREA = AreaEnum::MIXEDAREA;
#endif

  /** Set/Get the area normalization type */
  itkSetEnumMacro(AreaComputationType, AreaEnum);
  itkGetMacro(AreaComputationType, AreaEnum);

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro(SameDimensionCheck1, (Concept::SameDimension<InputPointDimension, OutputPointDimension>));
  itkConceptMacro(SameDimensionCheck2, (Concept::SameDimension<InputPointDimension, 3>));
#endif

protected:
  /** Default constructor*/
  LaplacianDeformationQuadEdgeMeshFilter();
  ~LaplacianDeformationQuadEdgeMeshFilter() override = default;

  using OutputMapPointIdentifier = std::unordered_map<OutputPointIdentifier, OutputPointIdentifier>;
  using OutputMapPointIdentifierIterator = typename OutputMapPointIdentifier::iterator;
  using OutputMapPointIdentifierConstIterator = typename OutputMapPointIdentifier::const_iterator;

  using ConstraintMapType = std::unordered_map<OutputPointIdentifier, OutputVectorType>;
  using ConstraintMapConstIterator = typename ConstraintMapType::const_iterator;

  struct HashOutputQEPrimal
  {
    size_t
    operator()(OutputQEPrimal * qe) const
    {
      return reinterpret_cast<size_t>(qe);
    }
  };

  using CoefficientMapType = std::unordered_map<OutputQEPrimal *, OutputCoordRepType, HashOutputQEPrimal>;
  using CoefficientMapConstIterator = typename CoefficientMapType::const_iterator;

  using AreaMapType = std::unordered_map<OutputPointIdentifier, OutputCoordRepType>;
  using AreaMapConstIterator = typename AreaMapType::const_iterator;

  using RowType = std::unordered_map<OutputPointIdentifier, OutputCoordRepType>;
  using RowIterator = typename RowType::iterator;
  using RowConstIterator = typename RowType::const_iterator;

  OutputMapPointIdentifier m_InternalMap;
  ConstraintMapType        m_Constraints;
  CoefficientMapType       m_CoefficientMap;
  AreaMapType              m_MixedAreaMap;

  CoefficientsComputationType * m_CoefficientsMethod;

  unsigned int m_Order{ 1 };
  AreaEnum     m_AreaComputationType{ AreaEnum::NONE };

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  OutputCoordRepType
  ComputeMixedAreaForGivenVertex(OutputPointIdentifier vId);
  OutputCoordRepType
  ComputeMixedArea(OutputQEPrimal * iQE1, OutputQEPrimal * iQE2);

  virtual void
  ComputeVertexIdMapping();

  void
  ComputeLaplacianMatrix(MatrixType & ioL);

  void
  FillMatrixRow(OutputPointIdentifier iId, unsigned int iDegree, OutputCoordRepType iWeight, RowType & ioRow);

  /**
   *  \brief Fill matrix iM and vectors Bx, m_By and m_Bz depending on if one
   *  vertex is on the border or not.
   */
  void
  FillMatrix(MatrixType & iM, VectorType & iBx, VectorType & iBy, VectorType & iBz);

  /**
   *  \brief Solve linears systems : \f$ iM \cdot oX = iBx \f$ and
   * \f$ iM \cdot oY = iBy \f$ and \f$ iM \cdot oZ = iBz \f$
   *
   *  \param[in] iM
   *  \param[in] iBx
   *  \param[in] iBy
   *  \param[in] iBz
   *  \param[out] oX
   *  \param[out] oY
   *  \param[out] oZ
   */
  void
  SolveLinearSystems(const MatrixType & iM,
                     const VectorType & iBx,
                     const VectorType & iBy,
                     const VectorType & iBz,
                     VectorType &       oX,
                     VectorType &       oY,
                     VectorType &       oZ);


private:
  struct Triple
  {
    Triple() = default;
    Triple(OutputPointIdentifier iV, OutputCoordRepType iWeight, unsigned int iDegree)
      : m_Id(iV)
      , m_Weight(iWeight)
      , m_Degree(iDegree)
    {}

    OutputPointIdentifier m_Id;
    OutputCoordRepType    m_Weight;
    unsigned int          m_Degree;
  };
};
} // end namespace itk

#include "itkLaplacianDeformationQuadEdgeMeshFilter.hxx"

#endif
