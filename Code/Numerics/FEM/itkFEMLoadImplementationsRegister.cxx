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

#include "itkFEMElement2DC0LinearLineStress.h"
#include "itkFEMElement2DC1Beam.h"
#include "itkFEMElement2DC0LinearTriangularStress.h"
#include "itkFEMElement2DC0LinearQuadrilateralStress.h"
#include "itkFEMElement3DC0LinearTetrahedronStrain.h"
#include "itkFEMElement3DC0LinearHexahedronStrain.h"

namespace itk {
namespace fem {




/**
 * \class GenericBodyLoad
 * \brief Templated class that holds a generic body load implementation.
 *
 * The only useful part of this class is a static function HandleLoad
 * This function is declared within a class only to avoid problems with
 * MS compiler. The real gravyty load implementation is in function
 * LoadImplementationGenericBodyLoad.
 *
 * \sa LoadImplementationGenericBodyLoad()
 */
extern Element::VectorType LoadImplementationGenericBodyLoad(Element::ConstPointer, LoadGrav::Pointer);

namespace {
template<class TElementClass>
class GenericBodyLoad
{
public:
  static Element::VectorType HandleBodyLoad(typename TElementClass::ConstPointer e, Element::LoadElementPointer l)
  {
    LoadGrav::Pointer l0=dynamic_cast<LoadGrav*>(&*l);
    if ( !l0 )
    {
      // Passed load object was not of class LoadGrav!
      throw FEMException(__FILE__, __LINE__, "FEM error");
    }
    Element::ConstPointer e0=e;

    return LoadImplementationGenericBodyLoad(e0,l0);
  }

};
} // end namespace

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
  REGISTER_LOAD_EX(Element2DC0LinearLineStress,LoadGravConst,GenericBodyLoad<Element2DC0LinearLineStress>::HandleBodyLoad);

  // Loads acting on Beam element
  REGISTER_LOAD_EX(Element2DC1Beam,LoadGravConst,GenericBodyLoad<Element2DC1Beam>::HandleBodyLoad);

  // Loads acting on QuadrilateralStress element
  REGISTER_LOAD_EX(Element2DC0LinearQuadrilateralStress,LoadGravConst,GenericBodyLoad<Element2DC0LinearQuadrilateralStress>::HandleBodyLoad);

  // Landmark loads acting on QuadrilateralStress elements
  REGISTER_LOAD(Element2DC0LinearQuadrilateralStress,LoadLandmark,LoadImplementationLandmarkLoadOnElement2DC0LinearQuadrilateralStress);

  // Loads acting on TriangularStress element
  REGISTER_LOAD_EX(Element2DC0LinearTriangularStress,LoadGravConst,GenericBodyLoad<Element2DC0LinearTriangularStress>::HandleBodyLoad);

  // Loads acting on HexahedronStrain element
  REGISTER_LOAD_EX(Element3DC0LinearHexahedronStrain,LoadGravConst,GenericBodyLoad<Element3DC0LinearHexahedronStrain>::HandleBodyLoad);

  // Loads acting on TetrahedronStrain element
  REGISTER_LOAD_EX(Element3DC0LinearTetrahedronStrain,LoadGravConst,GenericBodyLoad<Element3DC0LinearTetrahedronStrain>::HandleBodyLoad);
  

  // Add any additional loads here in a similar fashion...
  // Make sure that the pointer to the visit function is the correct one!!!

}




}} // end namespace itk::fem
