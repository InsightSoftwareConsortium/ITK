/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementationPoint_Beam2D.cxx
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

#include "itkFEMElementBeam2D.h"
#include "itkFEMLoadPoint.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk {
namespace fem {




/**
 * Handle LoadPoint in Beam2D element
 */
Element::LoadVectorType LoadPointImplementation(Beam2D::ConstPointer element, LoadElement::Pointer load)
{

  typedef Element::Float Float;
  LoadPoint::Pointer l0=dynamic_cast<LoadPoint*>(&*load);
  if ( !l0 ) throw;

  vnl_vector_fixed<Float,2> n1(element->m_node1->X,element->m_node1->Y);
  vnl_vector_fixed<Float,2> n2(element->m_node2->X,element->m_node2->Y);
  Float l=(n1-n2).magnitude();
  Float l1=(l0->point-n1).magnitude();
  Float l2=(l0->point-n2).magnitude();
  vnl_vector_fixed<Float,2> F1=l0->Fp*l2/l;
  vnl_vector_fixed<Float,2> F2=l0->Fp*l1/l;

  vnl_vector<Float> F(6,0.0);
  F.update(F1,0);
  F.update(F2,3);
  return F;


}




}} // end namespace itk::fem
