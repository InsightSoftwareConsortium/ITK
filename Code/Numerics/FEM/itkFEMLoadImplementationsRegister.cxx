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
#include "itkFEMElement2DC0LinearQuadrilateralStress.h"
#include "itkFEMElement3DC0LinearTetrahedronStrain.h"
#include "itkFEMElement3DC0LinearHexahedronStrain.h"

namespace itk {
namespace fem {




/* This macro makes registering Load implementations easier. */
#define REGISTER_LOAD_EX(ElementClass,LoadClass,FunctionName) \
  VisitorDispatcher<ElementClass, ElementClass::LoadElementType, ElementClass::VectorType (*)(ElementClass::ConstPointer,ElementClass::LoadElementPointer)> \
  ::RegisterVisitor((LoadClass*)0, &FunctionName);
/* Use this macro to also automatically declare load implementation function. */
#define REGISTER_LOAD(ElementClass,LoadClass,FunctionName) \
  extern ElementClass::VectorType FunctionName(ElementClass::ConstPointer, ElementClass::LoadElementPointer); \
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

  // Loads acting on TriangularStress element
  REGISTER_LOAD_EX(Element2DC0LinearTriangularStress,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);
  REGISTER_LOAD_EX(Element2DC0LinearTriangularStress,LoadLandmark,LoadImplementationGenericLandmarkLoad::HandleLoad);

  // Loads acting on HexahedronStrain element
  REGISTER_LOAD_EX(Element3DC0LinearHexahedronStrain,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);
  REGISTER_LOAD_EX(Element3DC0LinearHexahedronStrain,LoadLandmark,LoadImplementationGenericLandmarkLoad::HandleLoad);

  // Loads acting on TetrahedronStrain element
  REGISTER_LOAD_EX(Element3DC0LinearTetrahedronStrain,LoadGravConst,LoadImplementationGenericBodyLoad::HandleLoad);
  REGISTER_LOAD_EX(Element3DC0LinearTetrahedronStrain,LoadLandmark,LoadImplementationGenericLandmarkLoad::HandleLoad);


  // Add any additional loads here in a similar fashion...
  // Make sure that the pointer to the visit function is the correct one!!!

}




}} // end namespace itk::fem
