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
#include "itkFEMLoadImagePairBase.h"

#include "itkFEMElementBar2D.h"
#include "itkFEMElementBeam2D.h"
#include "itkFEMElementTriC02D.h"
#include "itkFEMElementQuadC02D.h"
#include "itkFEMElementMembraneC02D.h"
#include "itkFEMElementC1IsoCurve2D.h"
#include "itkFEMElementHexahedronC03D.h"
#include "itkFEMElementTetrahedronC03D.h"

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
extern ElementNew::VectorType LoadImplementationGenericBodyLoad(ElementNew::ConstPointer, LoadGrav::Pointer);
namespace {
template<class TElementClass>
class GenericBodyLoad
{
public:
  static ElementNew::VectorType HandleBodyLoad(typename TElementClass::ConstPointer e, ElementNew::LoadElementPointer l)
  {
    LoadGrav::Pointer l0=dynamic_cast<LoadGrav*>(&*l);
    if ( !l0 )
    {
      // Passed load object was not of class LoadGrav!
      throw FEMException(__FILE__, __LINE__, "FEM error");
    }
    ElementNew::ConstPointer e0=e;

    return LoadImplementationGenericBodyLoad(e0,l0);
  }

};
} // end namespace



/* This macro makes registering Load implementations easier. */
#define REGISTER_LOAD_EX(ElementClass,LoadClass,FunctionName) \
  VisitorDispatcher<ElementClass, ElementClass::LoadElementType, ElementClass::LoadVectorType>::RegisterVisitor((LoadClass*)0, &FunctionName);
/* Use this macro to also automatically declare load implementation function. */
#define REGISTER_LOAD(ElementClass,LoadClass,FunctionName) \
  extern ElementClass::LoadVectorType FunctionName(ElementClass::ConstPointer, ElementClass::LoadElementPointer); \
  REGISTER_LOAD_EX(ElementClass,LoadClass,FunctionName)




/**
 * Registers all Load classes in the FEM library with VisitorDispatcher.
 * This function must be called before the FEM library is functional!.
 */
void LoadImplementationsRegister(void)
{

  // Loads acting on Bar2D element
  REGISTER_LOAD( Bar2D,        LoadGravConst,    LoadGravImplementationBar2D    );
  REGISTER_LOAD( Bar2D,        LoadPoint,        LoadPointImplementationBar2D   );

  // Loads acting on Beam2D element
  REGISTER_LOAD( Beam2D,       LoadGravConst,    LoadGravImplementationBeam2D   );
  REGISTER_LOAD( Beam2D,       LoadPoint,        LoadPointImplementationBeam2D  );

  // Loads acting on TriC02D element
  REGISTER_LOAD( TriC02D,      LoadGravConst,    LoadGravImplementationTriC02D  );
  REGISTER_LOAD( TriC02D,      LoadEdge,         LoadEdgeImplementationTriC02D  );

  // Loads acting on QuadC02D element
  REGISTER_LOAD( QuadC02D,     LoadGravConst,    LoadGravImplementationQuadC02D );
  REGISTER_LOAD( QuadC02D,     LoadEdge,         LoadEdgeImplementationQuadC02D );

  // Loads acting on MembraneC02D element
  REGISTER_LOAD( MembraneC02D, LoadGrav,         LoadGravImplementationMembraneC02D );
  
  // Loads acting on C1IsoCurve2D element
  REGISTER_LOAD( C1IsoCurve2D, LoadElement,      LoadImplementationC1IsoCurve2D );

  // Loads acting on HexahedronC03D element
  REGISTER_LOAD( HexahedronC03D, LoadGravConst,  LoadGravImplementationHexahedronC03D );

  // Loads acting on TetrahedronC03D element
  REGISTER_LOAD( TetrahedronC03D, LoadGravConst, LoadGravImplementationTetrahedronC03D );

  // Loads acting on LineStress element
  REGISTER_LOAD_EX(Element2DC0LinearLineStress,LoadGravConst,GenericBodyLoad<Element2DC0LinearLineStress>::HandleBodyLoad);

  // Loads acting on Beam element
  REGISTER_LOAD_EX(Element2DC1Beam,LoadGravConst,GenericBodyLoad<Element2DC1Beam>::HandleBodyLoad);

  // Loads acting on QuadrilateralStress element
  REGISTER_LOAD_EX(Element2DC0LinearQuadrilateralStress,LoadGravConst,GenericBodyLoad<Element2DC0LinearQuadrilateralStress>::HandleBodyLoad);

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
