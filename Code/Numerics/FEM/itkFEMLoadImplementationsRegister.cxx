/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementationsRegister.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkVisitorDispatcher.h"

#include "itkFEMLoadPoint.h"
#include "itkFEMLoadGrav.h"
#include "itkFEMLoadEdge.h"
#include "itkFEMLoadLandmark.h"

#include "itkFEMLoadImplementationGenericBodyLoad.h"
#include "itkFEMLoadImplementationGenericLandmarkLoad.h"

#include "itkFEMElement2DC0LinearLineStress.h"
#include "itkFEMElement2DC1Beam.h"
#include "itkFEMElement2DC0LinearTriangularStress.h"
#include "itkFEMElement2DC0LinearTriangularMembrane.h"
#include "itkFEMElement2DC0LinearQuadrilateralStress.h"
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
    /* NOTE: Borland compiler (bcc 5.5.1) crashes if the pointer to templated function is not stored in a variable, before it is used in a function call below. */ \
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

  // Loads acting on QuadrilateralMembrane element
  REGISTER_LOAD_EX(Element2DC0LinearQuadrilateralMembrane,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);
  REGISTER_LOAD_EX(Element2DC0LinearQuadrilateralMembrane,LoadLandmark,LoadImplementationGenericLandmarkLoad::HandleLoad);

  // Loads acting on TriangularStress element
  REGISTER_LOAD_EX(Element2DC0LinearTriangularStress,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);
  REGISTER_LOAD_EX(Element2DC0LinearTriangularStress,LoadLandmark,LoadImplementationGenericLandmarkLoad::HandleLoad);

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
