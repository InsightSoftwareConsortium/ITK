/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementation_C1IsoCurve2D.cxx
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

#include "itkFEMElementC1IsoCurve2D.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk {
namespace fem {




/**
 * Handle gravity load in Bar2D element
 */
Element::LoadVectorType LoadImplementation(C1IsoCurve2D::ConstPointer element, Element::LoadElementPointer load)
{

  typedef Element::Float Float;

  vnl_vector<Float> fvec(element->N()*element->NI(),0.0);
  return fvec;

  //Superclass::Fe(L);  // we can't handle this load, pass it over to the parent class
/* FIX ME!!
  Float fx=0.;  
  Float fy=0.;
  const_cast<C1IsoCurve2D*>(this)->current_match_index=GN;
  ( dynamic_cast<itkActiveContourLoad*> (L))->DistForce(
    &(const_cast<C1IsoCurve2D*>(this)->current_match_index),
    cur_node->X,cur_node->Y, 0. , 0. , &fx,&fy);

  // apply only to cur_node -- all others zero
  fvec[2]=  cur_node->X + fx;
  fvec[3]=  cur_node->Y + fy;
  
  return fvec;
*/

}




}} // end namespace itk::fem
