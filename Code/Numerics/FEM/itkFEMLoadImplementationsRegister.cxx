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




/**
 * Registers all Load classes in the FEM library with VisitorDispatcher.
 * This function must be calles before the FEM library is functional!.
 */
void LoadImplementationsRegister(void)
{

  // Loads acting on Bar2D element
  extern Element::LoadVectorType LoadGravImplementation(Bar2D::ConstPointer, LoadElement::Pointer);
  extern Element::LoadVectorType LoadPointImplementation(Bar2D::ConstPointer, LoadElement::Pointer);
  VisitorDispatcher<Bar2D,LoadElement,Element::LoadVectorType>::RegisterVisitor((LoadGrav*)0, &LoadGravImplementation);
  VisitorDispatcher<Bar2D,LoadElement,Element::LoadVectorType>::RegisterVisitor((LoadGravConst*)0, &LoadGravImplementation);
  VisitorDispatcher<Bar2D,LoadElement,Element::LoadVectorType>::RegisterVisitor((LoadPoint*)0, &LoadPointImplementation);

  // Loads acting on Beam2D element
  extern Element::LoadVectorType LoadGravImplementation(Beam2D::ConstPointer, LoadElement::Pointer);
  extern Element::LoadVectorType LoadPointImplementation(Beam2D::ConstPointer, LoadElement::Pointer);
  VisitorDispatcher<Beam2D,LoadElement,Element::LoadVectorType>::RegisterVisitor((LoadGrav*)0, &LoadGravImplementation);
  VisitorDispatcher<Beam2D,LoadElement,Element::LoadVectorType>::RegisterVisitor((LoadGravConst*)0, &LoadGravImplementation);
  VisitorDispatcher<Beam2D,LoadElement,Element::LoadVectorType>::RegisterVisitor((LoadPoint*)0, &LoadPointImplementation);

  // Loads acting on TriC02D element
  extern Element::LoadVectorType LoadGravImplementation(TriC02D::ConstPointer, LoadElement::Pointer);
  extern Element::LoadVectorType LoadEdgeImplementation(TriC02D::ConstPointer, LoadElement::Pointer);
  VisitorDispatcher<TriC02D,LoadElement,Element::LoadVectorType>::RegisterVisitor((LoadGrav*)0, &LoadGravImplementation);
  VisitorDispatcher<TriC02D,LoadElement,Element::LoadVectorType>::RegisterVisitor((LoadGravConst*)0, &LoadGravImplementation);
  VisitorDispatcher<TriC02D,LoadElement,Element::LoadVectorType>::RegisterVisitor((LoadEdge*)0, &LoadEdgeImplementation);

  // Loads acting on QuadC02D element
  extern Element::LoadVectorType LoadGravImplementation(QuadC02D::ConstPointer, LoadElement::Pointer);
  extern Element::LoadVectorType LoadEdgeImplementation(QuadC02D::ConstPointer, LoadElement::Pointer);
  VisitorDispatcher<QuadC02D,LoadElement,Element::LoadVectorType>::RegisterVisitor((LoadGrav*)0, &LoadGravImplementation);
  VisitorDispatcher<QuadC02D,LoadElement,Element::LoadVectorType>::RegisterVisitor((LoadGravConst*)0, &LoadGravImplementation);
  VisitorDispatcher<QuadC02D,LoadElement,Element::LoadVectorType>::RegisterVisitor((LoadEdge*)0, &LoadEdgeImplementation);

  // Loads acting on C1IsoCurve2D element
  extern Element::LoadVectorType LoadImplementation(C1IsoCurve2D::ConstPointer, LoadElement::Pointer);
  VisitorDispatcher<C1IsoCurve2D,LoadElement,Element::LoadVectorType>::RegisterVisitor((Load*)0, &LoadImplementation);

  // Add any additional loads here in a similar fashion...
  // Make sure that the pointer to visit function is the correct one!!!

}




}} // end namespace itk::fem
