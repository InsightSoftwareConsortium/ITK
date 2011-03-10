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
// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkVisitorDispatcher.h"

#include "itkFEMLoadPoint.h"
#include "itkFEMLoadEdge.h"

#include "itkFEMLoadImplementationGenericBodyLoad.h"
#include "itkFEMLoadImplementationGenericLandmarkLoad.h"

#include "itkFEMElement2DC0LinearLineStress.h"
#include "itkFEMElement2DC1Beam.h"
#include "itkFEMElement2DC0LinearTriangularStress.h"
#include "itkFEMElement2DC0LinearTriangularStrain.h"
#include "itkFEMElement2DC0LinearTriangularMembrane.h"
#include "itkFEMElement2DC0LinearQuadrilateralStress.h"
#include "itkFEMElement2DC0LinearQuadrilateralStrain.h"
#include "itkFEMElement2DC0LinearQuadrilateralMembrane.h"
#include "itkFEMElement3DC0LinearTetrahedronStrain.h"
#include "itkFEMElement3DC0LinearHexahedronStrain.h"
#include "itkFEMElement3DC0LinearTetrahedronMembrane.h"
#include "itkFEMElement3DC0LinearHexahedronMembrane.h"

namespace itk {
namespace fem {

/* This macro makes registering Load implementations easier. */
#define REGISTER_LOAD_EX(ElementClass,LoadClass,FunctionName) \
  { ElementClass::LoadImplementationFunctionPointer fp=&FunctionName; \
    VisitorDispatcher<ElementClass, ElementClass::LoadType, ElementClass::LoadImplementationFunctionPointer> \
    ::RegisterVisitor((LoadClass*)0, fp); }
/* Use this macro to also automatically declare load implementation function. */
#define REGISTER_LOAD(ElementClass,LoadClass,FunctionName) \
  extern void FunctionName(ElementClass::ConstPointer, ElementClass::LoadPointer, ElementClass::VectorType& ); \
  REGISTER_LOAD_EX(ElementClass,LoadClass,FunctionName)

/**
 * Registers all Load classes in the FEM library with VisitorDispatcher.
 * This function must be called before the FEM library is functional!.
 */
void LoadImplementationsRegister(void)
{
  // Loads acting on LineStress element
  REGISTER_LOAD_EX(Element2DC0LinearLineStress,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);

  // Loads acting on Beam element
  REGISTER_LOAD_EX(Element2DC1Beam,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);

  // Loads acting on QuadrilateralStress element
  REGISTER_LOAD_EX(Element2DC0LinearQuadrilateralStress,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);
  REGISTER_LOAD_EX(Element2DC0LinearQuadrilateralStress,LoadLandmark,LoadImplementationGenericLandmarkLoad::HandleLoad);

  // Loads acting on QuadrilateralStrain element
  REGISTER_LOAD_EX(Element2DC0LinearQuadrilateralStrain,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);
  REGISTER_LOAD_EX(Element2DC0LinearQuadrilateralStrain,LoadLandmark,LoadImplementationGenericLandmarkLoad::HandleLoad);

  // Loads acting on QuadrilateralMembrane element
  REGISTER_LOAD_EX(Element2DC0LinearQuadrilateralMembrane,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);
  REGISTER_LOAD_EX(Element2DC0LinearQuadrilateralMembrane,LoadLandmark,LoadImplementationGenericLandmarkLoad::HandleLoad);

  // Loads acting on TriangularStress element
  REGISTER_LOAD_EX(Element2DC0LinearTriangularStress,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);
  REGISTER_LOAD_EX(Element2DC0LinearTriangularStress,LoadLandmark,LoadImplementationGenericLandmarkLoad::HandleLoad);

  // Loads acting on TriangularStrain element
  REGISTER_LOAD_EX(Element2DC0LinearTriangularStrain,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);
  REGISTER_LOAD_EX(Element2DC0LinearTriangularStrain,LoadLandmark,LoadImplementationGenericLandmarkLoad::HandleLoad);

  // Loads acting on TriangularMembrane element
  REGISTER_LOAD_EX(Element2DC0LinearTriangularMembrane,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);
  REGISTER_LOAD_EX(Element2DC0LinearTriangularMembrane,LoadLandmark,LoadImplementationGenericLandmarkLoad::HandleLoad);

  // Loads acting on HexahedronStrain element
  REGISTER_LOAD_EX(Element3DC0LinearHexahedronStrain,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);
  REGISTER_LOAD_EX(Element3DC0LinearHexahedronStrain,LoadLandmark,LoadImplementationGenericLandmarkLoad::HandleLoad);

  // Loads acting on HexahedronMembrane element
  REGISTER_LOAD_EX(Element3DC0LinearHexahedronMembrane,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);
  REGISTER_LOAD_EX(Element3DC0LinearHexahedronMembrane,LoadLandmark,LoadImplementationGenericLandmarkLoad::HandleLoad);

  // Loads acting on Tetrahedron element
  REGISTER_LOAD_EX(Element3DC0LinearTetrahedronStrain,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);
  REGISTER_LOAD_EX(Element3DC0LinearTetrahedronStrain,LoadLandmark,LoadImplementationGenericLandmarkLoad::HandleLoad);
  REGISTER_LOAD_EX(Element3DC0LinearTetrahedronMembrane,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);
  REGISTER_LOAD_EX(Element3DC0LinearTetrahedronMembrane,LoadLandmark,LoadImplementationGenericLandmarkLoad::HandleLoad);


  // Add any additional loads here in a similar fashion...
  // Make sure that the pointer to the visit function is the correct one!!!

}

}} // end namespace itk::fem
