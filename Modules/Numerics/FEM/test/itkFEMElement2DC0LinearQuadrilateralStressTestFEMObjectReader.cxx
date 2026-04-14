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

// Round-trip test for the FEM SpatialObject reader path:
//   1. Read a meta file containing an FEMObjectSpatialObject (produced by
//      itkFEMElement2DC0LinearQuadrilateralStressTest, which writes the
//      solved 2D C0 linear quadrilateral stress problem from Grandin's
//      "Fundamentals of the Finite Element Method").
//   2. Walk the SpatialObject hierarchy and verify the FEMObject is
//      structurally well-formed.
//   3. Re-solve the FEMObject through itk::fem::Solver and confirm the
//      solver completes without throwing.
//
// The previous version of this test had hard-coded Windows paths and
// called methods that do not exist on FEMObject (Solve(), GetSolution());
// it never compiled and was never registered in CMakeLists.txt.  See
// issue #4417.

#include "itkFEMSolver.h"
#include "itkFEMObjectSpatialObject.h"
#include "itkFEMSpatialObjectReader.h"
#include "itkGroupSpatialObject.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

#include <iostream>


int
itkFEMElement2DC0LinearQuadrilateralStressTestFEMObjectReader(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFEMObjectMetaFile" << std::endl;
    return EXIT_FAILURE;
  }

  // Register default FEM object types so the SpatialObject reader can
  // recognise them at parse time.
  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();

  constexpr unsigned int Dimension{ 2 };

  // ---- Read the FEMObjectSpatialObject from disk ----------------------
  using FEMSpatialObjectReaderType = itk::FEMSpatialObjectReader<Dimension>;
  auto spatialReader = FEMSpatialObjectReaderType::New();
  spatialReader->SetFileName(argv[1]);
  ITK_TRY_EXPECT_NO_EXCEPTION(spatialReader->Update());

  auto group = spatialReader->GetGroup();
  ITK_TEST_EXPECT_TRUE(group.IsNotNull());

  using FEMObjectSpatialObjectType = itk::FEMObjectSpatialObject<Dimension>;

  // ---- Find the FEMObjectSpatialObject child --------------------------
  auto * children = group->GetChildren();
  ITK_TEST_EXPECT_TRUE(children != nullptr);
  ITK_TEST_EXPECT_TRUE(!children->empty());

  auto firstChild = *(children->begin());
  if (std::strcmp(firstChild->GetTypeName().c_str(), "FEMObjectSpatialObject") != 0)
  {
    std::cerr << "Expected FEMObjectSpatialObject child, got " << firstChild->GetTypeName() << std::endl;
    delete children;
    return EXIT_FAILURE;
  }

  auto * femSO = dynamic_cast<FEMObjectSpatialObjectType *>(firstChild.GetPointer());
  delete children;
  if (femSO == nullptr)
  {
    std::cerr << "dynamic_cast to FEMObjectSpatialObjectType failed" << std::endl;
    return EXIT_FAILURE;
  }

  auto * femObject = femSO->GetFEMObject();
  if (femObject == nullptr)
  {
    std::cerr << "GetFEMObject() returned nullptr" << std::endl;
    return EXIT_FAILURE;
  }

  // The serialized form does not preserve the post-FinalizeMesh internal
  // state (DOF assignment, master-stiffness scaffolding); we have to
  // re-finalize before handing it to the solver.
  femObject->FinalizeMesh();

  // ---- Re-solve the FEMObject ----------------------------------------
  using SolverType = itk::fem::Solver<Dimension>;
  auto solver = SolverType::New();
  solver->SetInput(femObject);
  ITK_TRY_EXPECT_NO_EXCEPTION(solver->Update());

  // ---- Verify solution values -----------------------------------------
  // The 2D C0 linear quadrilateral has 4 nodes x 2 DOF/node = 8 DOFs.
  // Boundary conditions fix DOFs 0, 1 (node 0) and 6, 7 (node 3) to zero.
  // Nodal forces are applied to nodes 1 and 2 in the x-direction, so the
  // unconstrained DOFs (2..5) must have non-zero displacement.
  constexpr unsigned int NumDof = 8;
  for (unsigned int i = 0; i < NumDof; ++i)
  {
    const double soln = solver->GetSolution(i);
    std::cout << "Solution[" << i << "] = " << soln << std::endl;
  }

  // Constrained DOFs must be exactly zero.
  constexpr double   tolerance = 1e-7;
  const unsigned int constrainedDofs[] = { 0, 1, 6, 7 };
  for (const auto dof : constrainedDofs)
  {
    ITK_TEST_EXPECT_TRUE(itk::Math::abs(solver->GetSolution(dof)) < tolerance);
  }

  // Unconstrained DOFs must have non-zero displacement (forces applied).
  const unsigned int unconstrainedDofs[] = { 2, 3, 4, 5 };
  for (const auto dof : unconstrainedDofs)
  {
    ITK_TEST_EXPECT_TRUE(itk::Math::abs(solver->GetSolution(dof)) > tolerance);
  }

  std::cout << "Test PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
