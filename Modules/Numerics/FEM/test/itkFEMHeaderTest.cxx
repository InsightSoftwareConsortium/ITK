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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkFEMElements.h"
#include "itkFEMItpackSparseMatrix.h"
#include "itkFEMUtility.h"
#include "itkFEMImageMetricLoad.hxx"
#include "itkFEMElementBase.h"
#include "itkFEMElement2DC0LinearQuadrilateralMembrane.h"
#include "itkFEMSolver.h"
#include "itkFEMLinearSystemWrapperItpack.h"
#include "itkFEMElement3DMembrane.hxx"
#include "itkFEMElement2DC0LinearTriangularMembrane.h"
#include "itkFEMLoads.h"
#include "itkFEMElement2DC1Beam.h"
#include "itkFEMLoadBase.h"
#include "itkFEMLoadPoint.h"
#include "itkFEMLoadEdge.h"
#include "itkFEMElement3DC0LinearTetrahedronStrain.h"
#include "itkFEMLoadGrav.h"
#include "itkFEMMaterialBase.h"
#include "itkFEMElement3DC0LinearHexahedronMembrane.h"
#include "itkFEMElement1DStress.hxx"
#include "itkFEMElement3DC0LinearHexahedron.h"
#include "itkFEMElement2DC0QuadraticTriangular.h"
#include "itkFEMLoadNode.h"
#include "itkFEMElement2DMembrane.hxx"
#include "itkFEMLinearSystemWrapper.h"
#include "itkFEMElement2DC0LinearQuadrilateral.h"
#include "itkFEMLoadTest.h"
#include "itkFEMLinearSystemWrappers.h"
#include "itkFEMElement2DStress.hxx"
#include "itkFEMSolverCrankNicolson.h"
#include "itkFEMLightObject.h"
#include "itkFEMElement2DC0LinearTriangular.h"
#include "itkFEMLoadBCMFC.h"
#include "itkFEMLinearSystemWrapperDenseVNL.h"
#include "itkFEMLoadBC.h"

#include "itkFEMPArray.h"
#include "itkFEMElement3DC0LinearTetrahedron.h"
#include "itkFEMLoadLandmark.h"
#include "itkFEMP.h"
#include "itkFEMElement2DStrain.hxx"
#include "itpack.h"
#include "itkFEMSolution.h"
#include "itkFEMElement2DC0QuadraticTriangularStrain.h"
#include "itkFEMElement2DC0QuadraticTriangularStress.h"
#include "itkFEMElement3DC0LinearHexahedronStrain.h"
#include "itkFEMElement2DC0LinearQuadrilateralStrain.h"
#include "itkFEMElement2DC0LinearTriangularStrain.h"
#include "itkFEMElement2DC0LinearQuadrilateralStress.h"
#include "itkFEMElement2DC0LinearTriangularStress.h"
#include "itkFEMMaterialLinearElasticity.h"
#include "itkFEMElement3DC0LinearTetrahedronMembrane.h"
#include "itkFEMElement2DC0LinearLineStress.h"
#include "itkFEMMaterials.h"
#include "itkFEMLinearSystemWrapperVNL.h"
#include "itkFEMException.h"
#include "itkFEMElement3DStrain.hxx"
#include "itkFEMLoadElementBase.h"
#include "itkFEMElementStd.hxx"
#include "itkFEMElement2DC0LinearLine.h"
#include "itkFEMFactory.h"

int itkFEMHeaderTest( int, char * [] )
{

  return EXIT_SUCCESS;
}
