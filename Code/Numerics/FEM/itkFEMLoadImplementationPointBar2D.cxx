/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementationPointBar2D.cxx
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

#include "itkFEMElementBar2D.h"
#include "itkFEMLoadPoint.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk {
namespace fem {




/**
 * Handle LoadPoint in Bar2D element
 */
Element::LoadVectorType LoadPointImplementationBar2D(Bar2D::ConstPointer element, Element::LoadElementPointer load)
{

  typedef Element::Float Float;
  LoadPoint::Pointer l0=dynamic_cast<LoadPoint*>(&*load);
  if ( !l0 ) throw FEMException(__FILE__, __LINE__, "FEM error");

  vnl_vector_fixed<Float,2> n1(element->m_node[0]->X,element->m_node[0]->Y);
  vnl_vector_fixed<Float,2> n2(element->m_node[1]->X,element->m_node[1]->Y);
  Float l=(n1-n2).magnitude();
  Float l1=(l0->point-n1).magnitude();
  Float l2=(l0->point-n2).magnitude();
  vnl_vector_fixed<Float,2> F1=l0->Fp*l2/l;
  vnl_vector_fixed<Float,2> F2=l0->Fp*l1/l;

  vnl_vector<Float> F(4);
  F.update(F1,0);
  F.update(F2,2);
  return F;

}




}} // end namespace itk::fem
