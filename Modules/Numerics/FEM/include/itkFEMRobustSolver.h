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

#ifndef itkFEMRobustSolver_h
#define itkFEMRobustSolver_h

#include "itkFEMSolver.h"
#include "itkFEMElementBase.h"
#include "itkFEMMaterialBase.h"
#include "itkFEMLoadBase.h"
#include "itkFEMLoadNoisyLandmark.h"
#include "itkFEMLinearSystemWrapperVNL.h"
#include "itkFEMLinearSystemWrapperItpack.h"

#include <cmath>

namespace itk
{
namespace fem
{
/**
 * \class FEMRobustSolver
 * \brief A FEM solver characterized by accommodating outliers or
 * noises in landmarks and advancing approximation to interpolation.
 * This solver takes a FEMObject as input and outputs a deformed FEMObject.
 *
 * \code
 *       typedef itk::fem::FEMObject<3> FEMObjectType;
 *       FEMObjectObjectType::Pointer fem = FEMObjectObjectType::New();
 *       ...
 *       typedef itk::fem::RobustSolver<3> FEMSolverType;
 *       FEMSolverType::Pointer solver = FEMSolverType::New();
 *
 *       solver->SetInput( fem );
 *       solver->Update();
 *       FEMSolverType::Pointer defem = solver->GetOutput();
 *   ...
 * \endcode
 *
 * For simplicity reasons, a FEMScatteredDataPointSetToImageFilter is
 * developed to facilitate the use of this solver by hiding the details about
 * the FEMObject.
 * FEMScatteredDataPointSetToImageFilter takes a mesh and a feature point set as
 * inputs and converts them into a FEMObject, then calls this solver to find the
 * solution. Based on the solution and the user specified grid, a deformation
 * field is generated.
 *
 * \author Yixun Liu
 *
 * \par REFERENCE
 * O. Clatz, H. Delingette, I.-F. Talos, A. Golby, R. Kikinis, F. Jolesz,
 * N. Ayache, and S. Warfield, "Robust non-rigid registration to capture
 * brain shift from intra-operative MRI", IEEE Trans. Med. Imag., vol. 24,
 * no. 11, pp. 1417-1427, 2005.
 *
 *
 * \par REFERENCE
 * Yixun Liu, Andriy Fedorov, Ron Kikinis and Nikos Chrisochoides, "Real-time Non-rigid
 * Registration of Medical Images on a Cooperative Parallel Architecture",
 * IEEE International Conference on Bioinformatics & Biomedicine, pp. 401- 4,
 * November 2009.
 *
 *
 * \ingroup ITKFEM
 */

template <unsigned int VDimension = 3>
class ITK_TEMPLATE_EXPORT RobustSolver : public Solver<VDimension>
{
public:
  /** Standard class typedefs. */
  typedef RobustSolver              Self;
  typedef Solver<VDimension>        Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(RobustSolver, Solver);

  /** Inherit some types from the superclass. */
  typedef typename Superclass::VectorType                           VectorType;
  typedef typename Superclass::Float                                Float;
  typedef typename Superclass::InterpolationGridType                InterpolationGridType;
  typedef typename Superclass::InterpolationGridPointerType         InterpolationGridPointerType;
  typedef typename Superclass::InterpolationGridSizeType            InterpolationGridSizeType;
  typedef typename Superclass::InterpolationGridRegionType          InterpolationGridRegionType;
  typedef typename Superclass::InterpolationGridPointType           InterpolationGridPointType;
  typedef typename Superclass::InterpolationGridSpacingType         InterpolationGridSpacingType;
  typedef typename Superclass::InterpolationGridIndexType           InterpolationGridIndexType;
  typedef typename InterpolationGridType::DirectionType             InterpolationGridDirectionType;

  itkStaticConstMacro(FEMDimension, unsigned int, VDimension);

  typedef typename Superclass::FEMObjectType    FEMObjectType;

  /** Some convenient types */
  typedef typename Element::MatrixType                     MatrixType;
  typedef typename FEMObjectType::LoadContainerType        LoadContainerType;
  typedef typename FEMObjectType::NodeContainerType        NodeContainerType;
  typedef typename FEMObjectType::LoadContainerIterator    LoadContainerIterator;

  /**
   * Number of iterations used by the solver to compute approximations.
   */
  itkSetMacro(ApproximationSteps, unsigned int);
  itkGetMacro(ApproximationSteps, unsigned int);

  /**
   * Number of iterations to be run in which outliers will be removed.
   */
  itkSetMacro(OutlierRejectionSteps, unsigned int);
  itkGetMacro(OutlierRejectionSteps, unsigned int);

  /**
   * This variable set the largest error that will be tolerated when rejecting
   * outliers. It is expected to be a value from 0.0 to 1.0, and represents a
   * fraction of the largest error in the collection of feature points.
   */
  itkSetMacro(ToleranceToLargestDisplacement, double);
  itkGetMacro(ToleranceToLargestDisplacement, double);

  /**
   * Control the balance between the Mesh deformation energy and the matching
   * energy. The range of values is from 0.0 to 1.0. When set to zero, the matching
   * energy is not considered. When set to 1.0 the solver with consider equally the
   * Mesh energy and the matching energy.
   */
  itkSetMacro(TradeOffImageMeshEnergy, double);
  itkGetMacro(TradeOffImageMeshEnergy, double);

  /**
   * This variable control the fraction of landmarks that will be rejected as
   * outliers. It is expected to be a number between 0.0 and 1.0.
   */
  itkSetMacro(FractionErrorRejected, double);
  itkGetMacro(FractionErrorRejected, double);

  /**
   * When enabling the use of the interpolation grid, this accelerate the
   * proces of locating of the element containing the image pixels or
   * landmarks.
   */
  itkSetMacro(UseInterpolationGrid, bool);
  itkGetMacro(UseInterpolationGrid, bool);

protected:

  /**
   * Default constructor which sets the indices
   * for the matrix and vector storage.
   */
  RobustSolver();
  ~RobustSolver() ITK_OVERRIDE;

  /** Method invoked by the pipeline in order to trigger the computation of
   * the registration. */
  virtual void GenerateData() ITK_OVERRIDE;

  /** Run the solver and produce a warped FEM object. */
  virtual void RunSolver(void) ITK_OVERRIDE;

  /** Initialize matrix, vector, solution, interpolation grid, and landmark. */
  void Initialization();

  /**
   * Initialize the interpolation grid, which will be used to accelerate the
   * locating of the element containing the image pixels or landmarks.  This
   * will use the grid parameters provided in the SetOrigin, SetSpacing,
   * SetDirection and SetRegion methods.
   */
  void InitializeInterpolationGrid();

  /**
   * For each one of the landmarks, it record the element in which the landmark
   * is located, and its local coordinates.
   */
  void InitializeLandmarks();

  /**
   * Assemble the global mechanical stiffness matrix from the mesh contained in
   * the FEMObject
   */
  void AssembleMeshStiffnessMatrix();

  /**
   * Assemble element stiffness matrix, which will be used to assemble the
   * global stiffness matrix
   */
  virtual void AssembleElementMatrixWithID(const Element::Pointer & e, unsigned int matrixIndex);

  /**
   * Simulate the landmark as a physical point and
   * assemble its contribution matrix
   */
  void AssembleLandmarkStiffnessMatrix();

  /** Add global stiffness matrix with landmark stiffness matrix. */
  void AssembleGlobalMatrixFromLandmarksAndMeshMatrices();

  /** Assemble right side F vector based on the landmarks. */
  void AssembleF();

  /**
   * Solve iteratively, with outlier rejection,
   * from approximation to interpolation
   */
  void IncrementalSolverWithOutlierRejection();

  /**
   * Solve iteratively, without outlier rejection,
   * from approximation to interpolation
   */
  void IncrementalSolverWithoutOutlierRejection();

  /** Solve LS. */
  void SolveSystem();

  /**
   * Compute the approximation error for each landmark for subsequent outlier
   * rejection, by taking into account the weight set in the
   * ToleranceToLargestDisplacement.
   */
  void ComputeLandmarkSimulatedDisplacementAndWeightedError();

  /**
   * Compute the tensor associated with the landmark. The tensor is structural
   * weighted if a structural tensor point set is available
   */
  void ComputeLandmarkTensor();

  /** Get scaling factor. */
  float GetLandmarkTensorPonderation() const;

  /**
   * Sort the points in the decreasing order of error norm.  The argument
   * defines how many elements will be rejected at every step after they have
   * been sorted.
   */
  void NthElementWRTDisplacementError(unsigned int numberOfRejectedBlocksPerStep);

  /**
   * Unselect landmark from landmark array. The argument defines how many
   * landmarks will be unselected, after they have already been sorted.
   * This method must be called after NthElementWRTDisplacementError()
   * has been invoked to sort the landmarks.
   */
  void UnselectLandmarks(unsigned int numberOfRejectedBlocksPerStep);

  /**
   * Remove the contribution of the unselected landmarks
   * from the landmark stiffness matrix
   */
  void RemoveUnselectedLandmarkContributionInPointStiffnessMatrix();

  /**
   * Delete outliers. The argument define the number of outlier landmarks that
   * will be rejected at each one of the iterations.
   */
  void DeleteFromLandmarkBeginning(unsigned int numberOfRejectedLandmarksPerStep);

  /** Delete landmarks whose coordinates land outside of the mesh. */
  void DeleteLandmarksOutOfMesh();

  /**
   * Adjust the landmark stiffness matrix based on the change of the number of
   * the landmarks.  The argument depend on the number of landmarks and the
   * number of node in the mesh. This method update the value of the ponderation
   * based on a previous ponderation value and the computed Landmark Tensor
   * ponderation returned by the GetLandmarkTensorPonderation() method.
   */
  void RescaleLandmarkStiffnessMatrix(double oldPointTensorPonderation);

  /**
   * Calculate KU, which will  be added on the righ hand side to reach
   * the effect of zeroing mesh energy
   */
  void CalculateExternalForces();

  /**
   * Add exteranl force to set the mesh energy to be zero, which
   * is equivalent to starting FEM solver from the deformed mesh
   */
  void AddExternalForcesToSetMeshZeroEnergy();

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(RobustSolver);

  /**
   * The number of outlier rejection.
   * Note that outlier rejection is performed from approximation to interpolation
   */
  unsigned int m_OutlierRejectionSteps;

  /**
   * The number of approximation to interpolatioin without outlier rejection.
   */
  unsigned int m_ApproximationSteps;


  /** Represents the index of the vector and matrix. */
  typedef unsigned int FEMIndexType;

  FEMIndexType m_ForceIndex;
  FEMIndexType m_LandmarkForceIndex;
  FEMIndexType m_ExternalForceIndex;
  FEMIndexType m_SolutionIndex;
  FEMIndexType m_MeshStiffnessMatrixIndex;
  FEMIndexType m_LandmarkStiffnessMatrixIndex;
  FEMIndexType m_StiffnessMatrixIndex;

  double    m_TradeOffImageMeshEnergy;

  double    m_ToleranceToLargestDisplacement;
  double    m_ConjugateGradientPrecision;
  double    m_FractionErrorRejected;

  /**
   * Use interpolation grid to initialize the landmarks or not.  If use the
   * grid, make sure the landmark is the grid point.  The landmarks (feature
   * points) are usually the grid points if these landmarks come from a feature
   * point detection algorithm applied on an image.
   */
  bool m_UseInterpolationGrid;

  LinearSystemWrapperItpack m_Itpack;
};

/**
 * \class CompareLandmarkDisplacementError
 *  Comparison function object for sorting landmarks.
 *
 * \ingroup ITKFEM
*/
class CompareLandmarkDisplacementError :
    public std::binary_function<const Load::Pointer &, const Load::Pointer &, bool>
{
public:
    bool operator()(const Load::Pointer & L1 , const Load::Pointer & L2)
    {
    LoadNoisyLandmark * l1 = dynamic_cast<LoadNoisyLandmark*>(L1.GetPointer());
    LoadNoisyLandmark * l2 = dynamic_cast<LoadNoisyLandmark*>(L2.GetPointer());

    return l1->GetErrorNorm() > l2->GetErrorNorm();
    }
};

}  // end namespace fem
}  // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFEMRobustSolver.hxx"
#endif

#endif
