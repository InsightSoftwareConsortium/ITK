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
#include "itkFEMElements.h"
#include "itkFEMLoads.h"

namespace itk {
namespace fem {




/* This macro makes registering Load implementations easier. */
#ifndef FEM_USE_SMART_POINTERS
#define REGISTER_LOAD(ElementClass,LoadClass,FunctionName) \
  extern Element::LoadVectorType FunctionName(ElementClass::ConstPointer, LoadElement::Pointer); \
  VisitorDispatcher<ElementClass, LoadElement, Element::LoadVectorType>::RegisterVisitor((LoadClass*)0, &FunctionName);
#else
#define REGISTER_LOAD(ElementClass,LoadClass,FunctionName) \
  extern Element::LoadVectorType FunctionName(ElementClass::ConstPointer, FEMLightObject::Pointer); \
  VisitorDispatcher<ElementClass, FEMLightObject, Element::LoadVectorType>::RegisterVisitor((LoadClass*)0, &FunctionName);
#endif



/**
 * Registers all Load classes in the FEM library with VisitorDispatcher.
 * This function must be calles before the FEM library is functional!.
 */
void LoadImplementationsRegister(void)
{

  // Loads acting on Bar2D element
  REGISTER_LOAD( Bar2D,        LoadGrav,         LoadGravImplementation    );
  REGISTER_LOAD( Bar2D,        LoadGravConst,    LoadGravImplementation    );
  REGISTER_LOAD( Bar2D,        LoadPoint,        LoadPointImplementation   );

  // Loads acting on Beam2D element
  REGISTER_LOAD( Beam2D,       LoadGrav,         LoadGravImplementation    );
  REGISTER_LOAD( Beam2D,       LoadGravConst,    LoadGravImplementation    );
  REGISTER_LOAD( Beam2D,       LoadPoint,        LoadPointImplementation   );

  // Loads acting on TriC02D element
  REGISTER_LOAD( TriC02D,      LoadGrav,         LoadGravImplementation    );
  REGISTER_LOAD( TriC02D,      LoadGravConst,    LoadGravImplementation    );
  REGISTER_LOAD( TriC02D,      LoadEdge,         LoadEdgeImplementation    );

  // Loads acting on QuadC02D element
  REGISTER_LOAD( QuadC02D,     LoadGrav,         LoadGravImplementation    );
  REGISTER_LOAD( QuadC02D,     LoadGravConst,    LoadGravImplementation    );
  REGISTER_LOAD( QuadC02D,     LoadEdge,         LoadEdgeImplementation    );

  // Loads acting on C1IsoCurve2D element
  REGISTER_LOAD( C1IsoCurve2D, Load,             LoadImplementation        );


  // Add any additional loads here in a similar fashion...
  // Make sure that the pointer to visit function is the correct one!!!

}




}} // end namespace itk::fem
