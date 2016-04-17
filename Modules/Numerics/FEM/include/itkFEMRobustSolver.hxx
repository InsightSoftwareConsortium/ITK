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
#ifndef itkFEMRobustSolver_hxx
#define itkFEMRobustSolver_hxx

#include "itkFEMRobustSolver.h"

#include "itkFEMLoadNode.h"
#include "itkFEMLoadElementBase.h"
#include "itkFEMLoadBC.h"
#include "itkFEMLoadBCMFC.h"
#include "itkFEMLoadLandmark.h"
#include "itkMath.h"

namespace itk
{
namespace fem
{

template <unsigned int VDimension>
RobustSolver<VDimension>
::RobustSolver()
{
  this->m_ForceIndex = 0;
  this->m_LandmarkForceIndex = 1;
  this->m_ExternalForceIndex = 2;
  this->m_SolutionIndex = 0;
  this->m_MeshStiffnessMatrixIndex = 1;
  this->m_LandmarkStiffnessMatrixIndex = 2;
  this->m_StiffnessMatrixIndex = 0;

  this->m_OutlierRejectionSteps = 5;
  this->m_ApproximationSteps = 5;

  this->m_ToleranceToLargestDisplacement = 1.0;
  this->m_ConjugateGradientPrecision = 1e-3;
  this->m_FractionErrorRejected =.25;

  this->m_TradeOffImageMeshEnergy = 1.0;

  this->m_UseInterpolationGrid = true;
}


template <unsigned int VDimension>
RobustSolver<VDimension>
::~RobustSolver()
{
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::Initialization()
{
  this->SetLinearSystemWrapper(&m_Itpack);

  const FEMIndexType maximumNonZeroMatrixEntriesFactor = 100;

  const FEMIndexType maxNumberOfNonZeroValues = this->m_NGFN * maximumNonZeroMatrixEntriesFactor;

  if( maxNumberOfNonZeroValues > NumericTraits< FEMIndexType >::max() / 2 )
    {
    itkExceptionMacro("Too large system of equations");
    }

  this->m_Itpack.SetMaximumNonZeroValuesInMatrix( maxNumberOfNonZeroValues );

  // The NGFN is determined once the FEMObject is finalized
  this->m_LinearSystem->SetSystemOrder(this->m_NGFN);
  this->m_LinearSystem->SetNumberOfVectors(3);
  this->m_LinearSystem->SetNumberOfSolutions(1);
  this->m_LinearSystem->SetNumberOfMatrices(3);
  this->m_LinearSystem->InitializeMatrix(m_MeshStiffnessMatrixIndex);
  this->m_LinearSystem->InitializeMatrix(m_LandmarkStiffnessMatrixIndex);
  this->m_LinearSystem->InitializeMatrix(m_StiffnessMatrixIndex);
  this->m_LinearSystem->InitializeVector(m_ForceIndex);
  this->m_LinearSystem->InitializeVector(m_LandmarkForceIndex);
  this->m_LinearSystem->InitializeVector(m_ExternalForceIndex);
  this->m_LinearSystem->InitializeSolution(m_SolutionIndex);

  this->InitializeInterpolationGrid();

  this->InitializeLandmarks();
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::InitializeLandmarks()
{
  // Record the element, in which the landmark is located, and the shape function
  // value.
  LoadContainerType *container = this->m_FEMObject->GetModifiableLoadContainer();

  if(!container)
    {
    itkExceptionMacro("Missing container");
    }

  LoadContainerIterator it = container->Begin();

  for(; it != container->End(); ++it)
    {
    Load::Pointer load = it.Value();

    LoadNoisyLandmark *landmark = dynamic_cast<LoadNoisyLandmark*>(load.GetPointer());

    itkAssertInDebugAndIgnoreInReleaseMacro(landmark != ITK_NULLPTR);

    const VectorType & globalPosition = landmark->GetSource();

    InterpolationGridPointType point;
    for( unsigned int i = 0; i < this->FEMDimension; i++ )
      {
      point[i] = globalPosition[i];
      }

    if(this->m_UseInterpolationGrid)
      {
      // Landmark is within the interpolation grid
      InterpolationGridIndexType index;
      if( this->m_InterpolationGrid->TransformPhysicalPointToIndex(point, index) )
        {
        const Element * element = this->m_InterpolationGrid->GetPixel(index);

        // Landmark is inside the mesh
        if(element != ITK_NULLPTR)
          {
          landmark->SetContainedElement( element );

          const FEMIndexType numberOfDimensions = element->GetNumberOfSpatialDimensions();
          VectorType localPos(numberOfDimensions);

          element->GetLocalFromGlobalCoordinates(globalPosition, localPos);
          landmark->SetShape(element->ShapeFunctions(localPos));
          landmark->SetIsOutOfMesh(false);
          }
        else
          {
          // Remove the landmark
          landmark->SetIsOutOfMesh(true);
          }

        }

      }
    else
      {
      landmark->AssignToElement(this->m_FEMObject->GetModifiableElementContainer() );
      Element::ConstPointer ep = landmark->GetElement(0);
      VectorType localPos = landmark->GetPoint();
      landmark->SetShape(ep->ShapeFunctions(localPos));
      }
    }

    // Remove landmarks outside of the mesh
    this->DeleteLandmarksOutOfMesh();
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::GenerateData()
{
  // Initialize matrix, vector, solution, interpolation grid, and landmark.
  this->Initialization();

  // LS solver, which can be a VNL solver or a PETSc solver
  this->RunSolver();

  // Copy the input to the output and add the
  // displacements to update the nodal coordinates
  FEMObjectType *femObject = this->GetOutput();
  femObject->DeepCopy(this->GetInput());

  // Create DOF
  femObject->FinalizeMesh();

  this->UpdateDisplacements();
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::RunSolver()
{
  // Solve the displacement vector U
  this->AssembleMeshStiffnessMatrix();

  this->ComputeLandmarkTensor();

  this->AssembleLandmarkStiffnessMatrix();

  this->AssembleGlobalMatrixFromLandmarksAndMeshMatrices();

  this->AssembleF();

  if( this->m_OutlierRejectionSteps != 0 )
    {
    this->IncrementalSolverWithOutlierRejection();
    }
  else
    {
    // do "interpolation" only (no points discarded)
    this->IncrementalSolverWithoutOutlierRejection();
    }
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::IncrementalSolverWithOutlierRejection()
{
  // Solve the displacement vector U with outlier rejection

  const unsigned int numberOfLoads = this->m_FEMObject->GetNumberOfLoads();
  const double rejectionRate = this->m_FractionErrorRejected / this->m_OutlierRejectionSteps;

  const unsigned int numberOfRejectedLandmarksPerStep =
    static_cast<unsigned int>( floor(numberOfLoads * rejectionRate) );

  itkDebugMacro("numberOfRejectedLandmarksPerStep " << numberOfRejectedLandmarksPerStep);

  for (unsigned int outlierRejectionIteration = 0;
       outlierRejectionIteration < m_OutlierRejectionSteps; ++outlierRejectionIteration)
    {
    // Get scaling parameter before outlier rejection
    const double oldPointTensorPonderation = this->GetLandmarkTensorPonderation();

    this->AddExternalForcesToSetMeshZeroEnergy();
    itkDebugMacro("external force added");

    // Solve linear system of equations
    // solver to find possible outliers
    this->SolveSystem();
    itkDebugMacro("System solved");

    // Compute simulated error for sorting
    this->ComputeLandmarkSimulatedDisplacementAndWeightedError();

    // Sort the points in the *decreasing* order of error norm
    this->NthElementWRTDisplacementError(numberOfRejectedLandmarksPerStep);

    // Set first n to "unselected", and decrease
    // numberOfSelectedBlocks accordingly
    this->UnselectLandmarks(numberOfRejectedLandmarksPerStep);

    this->RemoveUnselectedLandmarkContributionInPointStiffnessMatrix();
    itkDebugMacro("unselected points' contribution removed");

    this->DeleteFromLandmarkBeginning(numberOfRejectedLandmarksPerStep);
    itkDebugMacro("unselected points removed from the list");

    // Rescale the point stiffness matrix with the new pointTensorPonderation
    this->RescaleLandmarkStiffnessMatrix(oldPointTensorPonderation);
    itkDebugMacro("matrix rescaled");

    this->AssembleGlobalMatrixFromLandmarksAndMeshMatrices();
    itkDebugMacro("matrix reassembled");

    this->AssembleF();
    itkDebugMacro("vector rebuilt");

    this->AddExternalForcesToSetMeshZeroEnergy();
    itkDebugMacro("external force added");

    // Solver when some outliers are rejected
    this->SolveSystem();
    itkDebugMacro("system resolved");

    this->CalculateExternalForces();
    itkDebugMacro("approximation iteration with outlier rejection " << outlierRejectionIteration);
    }

  this->IncrementalSolverWithoutOutlierRejection();
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::IncrementalSolverWithoutOutlierRejection()
{
  // Solve the displacement vector U without outlier rejection

  for(unsigned int approximationStep = 0; approximationStep < this->m_ApproximationSteps; ++approximationStep)
    {
    this->AddExternalForcesToSetMeshZeroEnergy();
    itkDebugMacro("external force added");

    this->SolveSystem();

    this->CalculateExternalForces();
    itkDebugMacro("Approximation step without outlier rejection " << approximationStep);
    }
}

template <unsigned int VDimension>
void
RobustSolver<VDimension>
::ComputeLandmarkSimulatedDisplacementAndWeightedError()
{
  // Compute the approximation error for each landmark for subsequent outlier

  const double lambda = this->m_ToleranceToLargestDisplacement;

  LoadContainerType *container = this->m_FEMObject->GetModifiableLoadContainer();

  if(!container)
    {
    itkExceptionMacro("No container");
    }

  LoadContainerIterator it = container->Begin();

  while(it != container->End())
    {
    Load::Pointer load = it.Value();

    LoadNoisyLandmark *landmark = dynamic_cast<LoadNoisyLandmark*>(load.GetPointer());

    if(landmark == ITK_NULLPTR)
      {
      itkExceptionMacro("Encounter landmark that is not a LoadNoisyLandmark");
      }

    if(!landmark->IsOutlier())
      {
      const VectorType & shape = landmark->GetShape();

      const Element * element = landmark->GetContainedElement();

      const FEMIndexType numberOfDOFs = element->GetNumberOfDegreesOfFreedomPerNode();
      const FEMIndexType numberOfNodes = element->GetNumberOfNodes();

      VectorType error(numberOfDOFs, 0.0);
      VectorType nodeSolution(numberOfDOFs);

      for(FEMIndexType nodeId = 0; nodeId < numberOfNodes; ++nodeId)
        {
        for(FEMIndexType dofId = 0; dofId < numberOfDOFs; ++dofId)
          {
          const int degreeOfFreedom = element->GetDegreeOfFreedom(nodeId * numberOfDOFs + dofId);
          nodeSolution[dofId] = this->m_LinearSystem->GetSolutionValue(degreeOfFreedom, m_SolutionIndex);
          }

        error += shape[nodeId] * nodeSolution;

        }

      landmark->SetSimulatedDisplacement(error);
      const double displacementNorm = error.two_norm();
      error = landmark->GetRealDisplacement() - error;

      const double confidence = landmark->GetConfidence();
      const VectorType weightedError = error / ((1-lambda) * displacementNorm + lambda);

      if(landmark->HasStructureTensor())
        {
        MatrixType structureTensor = landmark->GetStructureTensor();
        VectorType structureTensorPonderatedError = structureTensor * confidence * weightedError;
        landmark->SetErrorNorm(structureTensorPonderatedError.two_norm());
        }
      else
        {
        VectorType nonStructureTensorponderatedError = confidence * weightedError;
        landmark->SetErrorNorm(nonStructureTensorponderatedError.two_norm());
        }
      }

    ++it;
    }
}

template <unsigned int VDimension>
void
RobustSolver<VDimension>
::ComputeLandmarkTensor()
{
  // Compute landmark tensor weighted by a structure tensor if exists

  LoadContainerType *container = this->m_FEMObject->GetModifiableLoadContainer();

  if(!container)
    {
    itkExceptionMacro("Missing container");
    }

  LoadContainerIterator it = container->Begin();

  for(; it != container->End(); ++it)
    {
    Load::Pointer load = it.Value();

    LoadNoisyLandmark *landmark = dynamic_cast<LoadNoisyLandmark*>(load.GetPointer());

    itkAssertInDebugAndIgnoreInReleaseMacro(landmark != ITK_NULLPTR);

    if(!landmark->IsOutlier())
      {
      const VectorType & shape = landmark->GetShape();

      const Element * element = landmark->GetContainedElement();

      const FEMIndexType numberOfDOFs = element->GetNumberOfDegreesOfFreedomPerNode();
      const FEMIndexType numberOfNodes = element->GetNumberOfNodes();

      MatrixType nodeTensor(numberOfDOFs, numberOfDOFs);
      MatrixType landmarkTensor(numberOfDOFs, numberOfDOFs);

      landmarkTensor.fill(0.0);

      for(FEMIndexType nodeId = 0; nodeId < numberOfNodes; ++nodeId)
        {
        for(FEMIndexType dofXId = 0; dofXId < numberOfDOFs; dofXId++)
          {
          for(FEMIndexType dofYId = 0; dofYId < numberOfDOFs; dofYId++)
            {
            unsigned nx = element->GetDegreeOfFreedom(nodeId * numberOfDOFs + dofXId);
            unsigned ny = element->GetDegreeOfFreedom(nodeId * numberOfDOFs + dofYId);
            nodeTensor[dofXId][dofYId] = this->m_LinearSystem->GetMatrixValue(nx, ny, m_MeshStiffnessMatrixIndex);
            }
          }

        landmarkTensor += nodeTensor * shape[nodeId];
        }

      if(landmark->HasStructureTensor())
        {
        landmarkTensor *= landmark->GetStructureTensor();
        }

      landmark->SetLandmarkTensor(landmarkTensor);

      }
    }
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::NthElementWRTDisplacementError(unsigned int nthPoint)
{
  // Sort the landmarks according to the error norm

  if(nthPoint == 0)
    {
    return;
    }

  typedef std::vector<Load::Pointer> LoadVectorType;

  LoadContainerType *container = this->m_FEMObject->GetModifiableLoadContainer();

  if(!container)
    {
    itkExceptionMacro("Missing container");
    }

  LoadVectorType &loadVector = container->CastToSTLContainer();

  LoadVectorType::iterator it = loadVector.begin();
  std::advance(it, nthPoint - 1);
  LoadVectorType::iterator nth = it;
  std::nth_element(loadVector.begin(), nth ,loadVector.end(), CompareLandmarkDisplacementError());
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::UnselectLandmarks(unsigned int nUnselected)
{
  if(nUnselected == 0)
    {
    return;
    }

  typedef std::vector<Load::Pointer> LoadVectorType;

  LoadContainerType *container = this->m_FEMObject->GetModifiableLoadContainer();

  if(!container)
    {
    itkExceptionMacro("Missing container");
    }

  LoadVectorType &loadVector = container->CastToSTLContainer();

  LoadVectorType::iterator it;
  it = loadVector.begin();
  std::advance(it, nUnselected - 1);
  LoadVectorType::iterator nth = it;

  for(it = loadVector.begin(); it <= nth; it++)
    {
    LoadNoisyLandmark * landmark = dynamic_cast<LoadNoisyLandmark*>((*it).GetPointer());
    itkAssertInDebugAndIgnoreInReleaseMacro(landmark != ITK_NULLPTR);

    landmark->SetOutlier(true);
    }
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::DeleteFromLandmarkBeginning(unsigned int nDeleted)
{
  if(nDeleted == 0)
    {
    return;
    }

  typedef std::vector<Load::Pointer> LoadVectorType;

  LoadContainerType *container = this->m_FEMObject->GetModifiableLoadContainer();

  if(!container)
    {
    itkExceptionMacro("Missing container");
    }

  LoadVectorType &loadVector = container->CastToSTLContainer();

  LoadVectorType::iterator it;
  it = loadVector.begin();
  std::advance(it, nDeleted);
  LoadVectorType::iterator nth = it;
  loadVector.erase(loadVector.begin(), nth);
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::DeleteLandmarksOutOfMesh()
{
  typedef typename FEMObjectType::LoadIdentifier LoadIdentifier;

  typedef itk::VectorContainer< LoadIdentifier, Load::Pointer> VectorContainerType;
  typename VectorContainerType::Pointer newLoadContainer = VectorContainerType::New();

  LoadIdentifier numToRemoveLoads = NumericTraits< LoadIdentifier >::ZeroValue();

  LoadContainerType * container = this->m_FEMObject->GetModifiableLoadContainer();

  if(!container)
    {
    itkExceptionMacro("Missing container");
    }

  for(LoadContainerIterator it = container->Begin(); it != container->End(); ++it)
    {
    Load::Pointer load = it.Value();

    LoadNoisyLandmark *landmark = dynamic_cast<LoadNoisyLandmark*>(load.GetPointer());

    if(landmark == ITK_NULLPTR)
      {
      itkExceptionMacro("Encounter landmark that is not a LoadNoisyLandmark");
      }

    if(landmark->IsOutOfMesh())
      {
      numToRemoveLoads++;
      }
    else
      {
      newLoadContainer->push_back(landmark);
      }
    }

  // If there were landmarks outside of the mesh, then the load container must
  // be updated to hold only the landmarks that are inside of the Mesh.
  if( numToRemoveLoads )
    {
    // Empty the load container first.
    container->clear();

    // Add the new loads to Load container
    for(LoadContainerIterator it = newLoadContainer->Begin(); it != newLoadContainer->End(); ++it)
      {
      this->m_FEMObject->AddNextLoad(it.Value());
      }
    }
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::RescaleLandmarkStiffnessMatrix(double oldPointTensorPonderation)
{
  // PointTensorPonderation is changing throughout outlier rejection.
  // This function scales the point stiffness matrix by the updated
  // pointTensorPonderation

  double newPointTensorPonderation =
    this->GetLandmarkTensorPonderation() / oldPointTensorPonderation;

  this->m_LinearSystem->ScaleMatrix(newPointTensorPonderation, m_LandmarkStiffnessMatrixIndex);
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::AssembleMeshStiffnessMatrix()
{
  // Assemble the mechanical stiffness matrix from the mesh

  // If no DOFs exist in a system, we have nothing to do
  if( this->m_NGFN <= 0 )
    {
    return;
    }

  // Assemble the mechanical matrix by stepping over all elements
  FEMIndexType numberOfElements = this->m_FEMObject->GetNumberOfElements();
  for( FEMIndexType i = 0; i < numberOfElements; i++)
    {
    // Call the function that actually moves the element matrix to the master matrix.
    Element::Pointer element = this->m_FEMObject->GetElement(i);

    this->AssembleElementMatrixWithID(element, this->m_MeshStiffnessMatrixIndex);
    }
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::AssembleElementMatrixWithID(const Element::Pointer & element, unsigned int matrixIndex)
{
  // Copy the element stiffness matrix for faster access.
  Element::MatrixType Ke;

  element->GetStiffnessMatrix(Ke);

  // Same for number of DOF
  const FEMIndexType numberOfDOFs = element->GetNumberOfDegreesOfFreedom();

  // Step over all rows in element matrix
  for( FEMIndexType j = 0; j < numberOfDOFs; j++ )
    {
    // Step over all columns in element matrix
    for( FEMIndexType k = 0; k < numberOfDOFs; k++ )
      {
      // Error checking. all GFN should be >= 0 and < NGFN
      const unsigned int dofj = element->GetDegreeOfFreedom(j);
      const unsigned int dofk = element->GetDegreeOfFreedom(k);
      if( dofj >= this->m_NGFN || dofk >= this->m_NGFN )
        {
        throw FEMExceptionSolution(__FILE__, __LINE__, "Solver::AssembleElementMatrix()", "Illegal GFN!");
        }

      if( Math::NotExactlyEquals(Ke[j][k], Float(0.0)) )
        {
        this->m_LinearSystem->AddMatrixValue(dofj, dofk, Ke[j][k], matrixIndex);
        }
      }
    }
}

template <unsigned int VDimension>
void
RobustSolver<VDimension>
::AssembleLandmarkStiffnessMatrix()
{
  // Assemble the contribution matrix of the landmarks

  const double pointTensorPonderation = this->GetLandmarkTensorPonderation();

  LoadContainerType *container = this->m_FEMObject->GetModifiableLoadContainer();

  if(!container)
    {
    itkExceptionMacro("Missing container");
    }

  LoadContainerIterator it = container->Begin();

  for(;it != container->End(); ++it)
    {
    Load::Pointer load = it.Value();

    LoadNoisyLandmark *landmark = dynamic_cast<LoadNoisyLandmark*>(load.GetPointer());

    if(landmark == ITK_NULLPTR)
      {
      itkExceptionMacro("Encounter landmark that is not a LoadNoisyLandmark");
      }

    if(!landmark->IsOutlier())
      {
      const double confidence = landmark->GetConfidence();

      const MatrixType & tens = landmark->GetLandmarkTensor();

      const VectorType & shape = landmark->GetShape();

      const Element * element = landmark->GetContainedElement();

      const FEMIndexType numberOfDOFs = element->GetNumberOfDegreesOfFreedomPerNode();
      const FEMIndexType numberOfNodes = element->GetNumberOfNodes();

      // Fill the diagonal matrices
      for(FEMIndexType k = 0; k < numberOfNodes; ++k)
        {
        const double barCoor = shape[k] * shape[k];

        for(FEMIndexType n = 0; n < numberOfDOFs; n++)
          {
          for(FEMIndexType m = 0; m < numberOfDOFs; m++)
            {
            const int dofn = element->GetDegreeOfFreedom(k * numberOfDOFs + n);
            const int dofm = element->GetDegreeOfFreedom(k * numberOfDOFs + m);
            const float value = static_cast<float>( barCoor * m_TradeOffImageMeshEnergy * pointTensorPonderation * (tens(n,m)) * confidence );

            this->m_LinearSystem->AddMatrixValue(dofn, dofm, value, m_LandmarkStiffnessMatrixIndex);
            }
          }
        }

      // Fill the extradiagonal matrices
      for(FEMIndexType i = 0; i < numberOfNodes - 1; i++)
        {
        for(FEMIndexType j = i + 1; j < numberOfNodes; j++)
          {
          const double barCoor = shape[i] * shape[j];

          for(FEMIndexType n = 0; n < numberOfDOFs; n++)
            {
            for(FEMIndexType m = 0; m < numberOfDOFs; m++)
              {
              const int dofn = element->GetDegreeOfFreedom(i * numberOfDOFs + n);
              const int dofm = element->GetDegreeOfFreedom(j * numberOfDOFs + m);
              const float value = static_cast<float>( barCoor * m_TradeOffImageMeshEnergy * pointTensorPonderation * (tens(n, m)) * confidence );

              this->m_LinearSystem->AddMatrixValue(dofn, dofm, value, m_LandmarkStiffnessMatrixIndex);
              this->m_LinearSystem->AddMatrixValue(dofm, dofn, value, m_LandmarkStiffnessMatrixIndex);

              }
            }
          }
        }
      }
    }
}

template <unsigned int VDimension>
void
RobustSolver<VDimension>
::RemoveUnselectedLandmarkContributionInPointStiffnessMatrix()
{
  // Remove the contribution of the unselected landmarks from the landmark
  // stiffness matrix

  const double pointTensorPonderation = GetLandmarkTensorPonderation();

  itkDebugMacro("Removing unselected blocks contribution, " << "pointTensorPonderation is " << pointTensorPonderation);

  LoadContainerType *container = this->m_FEMObject->GetModifiableLoadContainer();

  if(!container)
    {
    itkExceptionMacro("Missing container");
    }

  LoadContainerIterator it = container->Begin();

  for(;it != container->End(); ++it)
    {
    Load::Pointer load = it.Value();

    LoadNoisyLandmark *landmark = dynamic_cast<LoadNoisyLandmark*>(load.GetPointer());

    itkAssertInDebugAndIgnoreInReleaseMacro(landmark != ITK_NULLPTR);

    if(landmark->IsOutlier())
      {
      const float confidence = landmark->GetConfidence();

      const MatrixType & tens = landmark->GetLandmarkTensor();

      const VectorType & shape = landmark->GetShape();

      Element::ConstPointer element = landmark->GetElement(0);

      const FEMIndexType numberOfDOFs = element->GetNumberOfDegreesOfFreedomPerNode();
      const FEMIndexType numberOfNodes = element->GetNumberOfNodes();

      for(FEMIndexType k = 0; k < numberOfNodes; ++k)
        {
        const double barCoor = shape[k] * shape[k];

        for(FEMIndexType n = 0; n < numberOfDOFs; n++)
          {
          for(FEMIndexType m = 0; m < numberOfDOFs; m++)
            {
            const int dofn = element->GetDegreeOfFreedom(k * numberOfDOFs + n);
            const int dofm = element->GetDegreeOfFreedom(k * numberOfDOFs + m);
            const float value = static_cast<float>( -barCoor * m_TradeOffImageMeshEnergy * pointTensorPonderation * (tens(n, m)) * confidence );

            this->m_LinearSystem->AddMatrixValue(dofn, dofm, value, m_LandmarkStiffnessMatrixIndex);

            }
          }
        }

      for(FEMIndexType i = 0; i < numberOfNodes - 1; i++)
        {
        for(FEMIndexType j = i + 1; j < numberOfNodes; j++)
          {
          const double barCoor = shape[i] * shape[j];

          for(FEMIndexType n = 0; n < numberOfDOFs; n++)
            {
            for(FEMIndexType m = 0; m < numberOfDOFs; m++)
              {
              const int dofn = element->GetDegreeOfFreedom(i * numberOfDOFs + n);
              const int dofm = element->GetDegreeOfFreedom(j * numberOfDOFs + m);
              const float value = static_cast<float>( -barCoor * m_TradeOffImageMeshEnergy * pointTensorPonderation * (tens(n,m)) * confidence );

              this->m_LinearSystem->AddMatrixValue(dofn, dofm, value, m_LandmarkStiffnessMatrixIndex);
              this->m_LinearSystem->AddMatrixValue(dofm, dofn, value, m_LandmarkStiffnessMatrixIndex);

              }
            }
          }
        }
      }
    }
}


template <unsigned int VDimension>
float
RobustSolver<VDimension>
::GetLandmarkTensorPonderation(void) const
{
  const LoadContainerType * loadContainer = this->m_FEMObject->GetLoadContainer();

  if(!loadContainer)
    {
    itkExceptionMacro("Missing load container");
    }

  const NodeContainerType * nodeContainer = this->m_FEMObject->GetNodeContainer();

  if(!nodeContainer)
    {
    itkExceptionMacro("Missing node container");
    }

  const float ponderation =
    static_cast<float>( nodeContainer->Size() ) /
    static_cast<float>( loadContainer->Size() );

  return ponderation;
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::AssembleGlobalMatrixFromLandmarksAndMeshMatrices()
{
  this->m_LinearSystem->CopyMatrix( this->m_MeshStiffnessMatrixIndex, this->m_StiffnessMatrixIndex );
  this->m_LinearSystem->AddMatrixMatrix( this->m_StiffnessMatrixIndex, this->m_LandmarkStiffnessMatrixIndex);
}

template <unsigned int VDimension>
void
RobustSolver<VDimension>
::AssembleF()
{
  const double pointTensorPonderation = GetLandmarkTensorPonderation();

  this->m_LinearSystem->InitializeVector(m_LandmarkForceIndex);

  LoadContainerType *container = this->m_FEMObject->GetModifiableLoadContainer();

  if(!container)
    {
    itkExceptionMacro("Missing container");
    }

  LoadContainerIterator it = container->Begin();

  for(;it != container->End(); ++it)
    {
    Load::Pointer load = it.Value();

    LoadNoisyLandmark *landmark = dynamic_cast<LoadNoisyLandmark*>(load.GetPointer());

    itkAssertInDebugAndIgnoreInReleaseMacro(landmark != ITK_NULLPTR);

    if(!landmark->IsOutlier())
      {
      const double confidence = landmark->GetConfidence();

      const VectorType & realDisplacement = landmark->GetRealDisplacement();

      const MatrixType & tensor = landmark->GetLandmarkTensor();

      const VectorType & shape = landmark->GetShape();

      const Element * element = landmark->GetContainedElement();

      const FEMIndexType numberOfDOFs = element->GetNumberOfDegreesOfFreedomPerNode();
      const FEMIndexType numberOfNodes = element->GetNumberOfNodes();

      for(FEMIndexType m = 0;m < numberOfNodes; ++m)
        {
        double barCoor = shape[m];

        const VectorType weightedRealDisplacement = confidence * barCoor * pointTensorPonderation * m_TradeOffImageMeshEnergy * ((tensor) * realDisplacement);

        for(FEMIndexType j = 0; j < numberOfDOFs; ++j)
          {
          const int index = element->GetDegreeOfFreedom(m * numberOfDOFs + j);
          this->m_LinearSystem->AddVectorValue(index, weightedRealDisplacement[j], this->m_LandmarkForceIndex);
          }
        }
      }
    }
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::CalculateExternalForces()
{
  this->m_LinearSystem->MultiplyMatrixSolution( this->m_ExternalForceIndex, this->m_MeshStiffnessMatrixIndex, this->m_SolutionIndex);
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::AddExternalForcesToSetMeshZeroEnergy()
{
   // Add exteranl force to set the mesh energy to be zero, which
   // is equivalent to starting FEM solver from the deformed mesh
  this->m_LinearSystem->CopyVector( this->m_LandmarkForceIndex, this->m_ForceIndex );
  this->m_LinearSystem->AddVectorVector( this->m_ForceIndex, this->m_ExternalForceIndex );
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::SolveSystem()
{
  // Solve the linear system of equations

  // Note that VNL LS solver uses the matrix and vector with index zero to construct the LS
  this->m_LinearSystem->Solve();
}


template <unsigned int VDimension>
void
RobustSolver<VDimension>
::InitializeInterpolationGrid()
{
  const InterpolationGridRegionType & region = this->GetRegion();
  InterpolationGridSizeType size = region.GetSize();
  for( unsigned int i = 0; i < this->FEMDimension; i++ )
    {
    if( size[i] == 0 )
      {
      itkExceptionMacro("Size must be specified.");
      }
    }

  this->m_InterpolationGrid = InterpolationGridType::New();
  this->m_InterpolationGrid->SetOrigin( this->GetOrigin() );
  this->m_InterpolationGrid->SetSpacing( this->GetSpacing() );
  this->m_InterpolationGrid->SetDirection( this->GetDirection() );
  this->m_InterpolationGrid->SetRegions( this->GetRegion() );
  this->m_InterpolationGrid->Allocate();

   // Initialize all pointers in interpolation grid image to 0
  this->m_InterpolationGrid->FillBuffer(ITK_NULLPTR);

  // Fill the interpolation grid with proper pointers to elements
  FEMIndexType numberOfElements = this->m_FEMObject->GetNumberOfElements();
  for( FEMIndexType index = 0; index < numberOfElements; index++ )
    {
    const Element * element = this->m_FEMObject->GetElement( index );
    // Get square boundary box of an element
    VectorType v1 = element->GetNodeCoordinates(0);      // lower left corner
    VectorType v2 = v1;                            // upper right corner

    const FEMIndexType NumberOfDimensions = element->GetNumberOfSpatialDimensions();
    for( FEMIndexType n = 1; n < element->GetNumberOfNodes(); n++ )
      {
      const VectorType & v = element->GetNodeCoordinates(n);
      for( FEMIndexType d = 0; d < NumberOfDimensions; d++ )
        {
        if( v[d] < v1[d] )
          {
          v1[d] = v[d];
          }
        if( v[d] > v2[d] )
          {
          v2[d] = v[d];
          }
        }
      }

    // Convert boundary box corner points into discrete image indexes.
    InterpolationGridIndexType vi1;
    InterpolationGridIndexType vi2;

    typedef Point<Float, FEMDimension> PointType;
    PointType vp1;
    PointType vp2;
    PointType pt;
    for( unsigned int i = 0; i < FEMDimension; i++ )
      {
      vp1[i] = v1[i];
      vp2[i] = v2[i];
      }

    // Obtain the Index of BB corner and check whether it is within image.
    // If it is not, we ignore the entire element.
    if( !this->m_InterpolationGrid->TransformPhysicalPointToIndex(vp1, vi1) )
      {
      continue;
      }
    if( !this->m_InterpolationGrid->TransformPhysicalPointToIndex(vp2, vi2) )
      {
      continue;
      }

    InterpolationGridSizeType region_size;
    for( unsigned int i = 0; i < FEMDimension; i++ )
      {
      region_size[i] = vi2[i] - vi1[i] + 1;
      }

    InterpolationGridRegionType interRegion(vi1, region_size);

    // Initialize the iterator that will step over all grid points within
    // element boundary box.
    ImageRegionIterator<InterpolationGridType> iter(this->m_InterpolationGrid, interRegion);

    // Update the element pointers in the points defined within the region.
    VectorType global_point(NumberOfDimensions);

    VectorType local_point(NumberOfDimensions);

    // Step over all points within the region
    for( iter.GoToBegin(); !iter.IsAtEnd(); ++iter )
      {
      // Note: Iteratior is guarantied to be within image, since the
      // elements with BB outside are skipped before.
      this->m_InterpolationGrid->TransformIndexToPhysicalPoint(iter.GetIndex(), pt);
      for( FEMIndexType d = 0; d < NumberOfDimensions; d++ )
        {
        global_point[d] = pt[d];
        }

      // If the point is within the element, we update the pointer at
      // this point in the interpolation grid image.
      if( element->GetLocalFromGlobalCoordinates(global_point, local_point) )
        {
        iter.Set( element );
        }
      } // next point in region
    }   // next element
}

}  // end namespace fem
}  // end namespace itk
#endif // itkFEMRobustSolver_hxx
