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


#ifndef itkTriangleMeshCurvatureCalculator_hxx
#define itkTriangleMeshCurvatureCalculator_hxx

#include <memory> // For unique_ptr
#include "itkTriangleMeshCurvatureCalculator.h"
#include "itkObjectFactory.h"
#include "itkMath.h"
#include "vnl/vnl_cross.h"
#include "vnl/vnl_math.h"

namespace itk
{

template <typename TInputMesh>
void
TriangleMeshCurvatureCalculator<TInputMesh>::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "CurvatureType: " << m_CurvatureType << std::endl;
  itkPrintSelfObjectMacro(TriangleMesh);
  itkPrintSelfObjectMacro(GaussCurvatureData);
}

template <typename TInputMesh>
void
TriangleMeshCurvatureCalculator<TInputMesh>::Compute()
{
  MeshConstPointer inputMesh = this->m_TriangleMesh;
  if (inputMesh == nullptr)
  {
    itkExceptionMacro("First set the Input Triangle Mesh to perform computation");
  }

  if (this->GetCurvatureType() == TriangleMeshCurvatureCalculatorEnums::Curvatures::GaussCurvature)
  {
    this->GetGaussCurvature(inputMesh);
  }
  else
  {
    itkExceptionMacro("Only Gauss Curvature type available");
  }
}

template <typename TInputMesh>
void
TriangleMeshCurvatureCalculator<TInputMesh>::GetGaussCurvature(MeshConstPointer inputMesh)
{
  if (inputMesh->GetNumberOfCells())
  {
    this->ComputeGaussCurvature(inputMesh);
  }
}


template <typename TInputMesh>
void
TriangleMeshCurvatureCalculator<TInputMesh>::ComputeGaussCurvature(MeshConstPointer inputMesh)
{
  MeshPointType v0, v1, v2;
  MeshPointType e0, e1, e2;
  double        A, alpha0, alpha1, alpha2;

  const unsigned int Nv = inputMesh->GetNumberOfPoints();

  const std::unique_ptr<double[]> K(new double[Nv]);
  const std::unique_ptr<double[]> dA(new double[Nv]);
  double                          pi2 = 2.0 * itk::Math::pi;
  for (unsigned int k = 0; k < Nv; ++k)
  {
    K[k] = pi2;
    dA[k] = 0.0;
  }

  v0.Fill(0);
  v1.Fill(0);
  v2.Fill(0);

  CellsContainerConstPointer  outCells = inputMesh->GetCells();
  CellsContainerConstIterator cellsItr = outCells->Begin();

  while (cellsItr != outCells->End())
  {
    CellType *               cellPointer = cellsItr.Value();
    MeshPointIdConstIterator point_ids = cellPointer->GetPointIds();

    v0 = inputMesh->GetPoint(point_ids[0]);
    v1 = inputMesh->GetPoint(point_ids[1]);
    v2 = inputMesh->GetPoint(point_ids[2]);

    // Edges
    e0[0] = v1[0];
    e0[1] = v1[1];
    e0[2] = v1[2];
    e0[0] -= v0[0];
    e0[1] -= v0[1];
    e0[2] -= v0[2];

    e1[0] = v2[0];
    e1[1] = v2[1];
    e1[2] = v2[2];
    e1[0] -= v1[0];
    e1[1] -= v1[1];
    e1[2] -= v1[2];

    e2[0] = v0[0];
    e2[1] = v0[1];
    e2[2] = v0[2];
    e2[0] -= v2[0];
    e2[1] -= v2[1];
    e2[2] -= v2[2];

    alpha0 = itk::Math::pi - angle(e1.GetVnlVector(), e2.GetVnlVector());
    alpha1 = itk::Math::pi - angle(e2.GetVnlVector(), e0.GetVnlVector());
    alpha2 = itk::Math::pi - angle(e0.GetVnlVector(), e1.GetVnlVector());

    // Surface area
    A = double(std::abs(vnl_cross_3d((v1 - v0).GetVnlVector(), (v2 - v0).GetVnlVector()).two_norm() / 2.0));

    dA[point_ids[0]] += A;
    dA[point_ids[1]] += A;
    dA[point_ids[2]] += A;
    K[point_ids[0]] -= alpha1;
    K[point_ids[1]] -= alpha2;
    K[point_ids[2]] -= alpha0;

    ++cellsItr;
  }

  // Allocate Memory to store the curvature output.
  this->m_GaussCurvatureData = DoubleVectorContainer::New();
  this->m_GaussCurvatureData->Reserve(Nv);

  // Put curvature in gaussCurvatureData.
  for (unsigned int v = 0; v < Nv; ++v)
  {
    if (dA[v] > 0.0)
    {
      this->m_GaussCurvatureData->SetElement(v, 3.0 * K[v] / dA[v]);
    }
  }
}


} // end namespace itk

#endif
