/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementationGrav_Beam2D.cxx
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
#include "itkFEMLoadGrav.h"
#include "itkFEMUtility.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk {
namespace fem {




/**
 * Handle LoadGrav in Beam2D element
 */
Element::LoadVectorType LoadGravImplementation_Beam2D(Beam2D::ConstPointer element, Element::LoadElementPointer load)
{

  typedef Element::Float Float;
  LoadGrav::Pointer l0=dynamic_cast<LoadGrav*>(&*load);
  if ( !l0 ) throw;

  vnl_vector_fixed<Float,2> n1(element->m_node[0]->X,element->m_node[0]->Y);
  vnl_vector_fixed<Float,2> n2(element->m_node[1]->X,element->m_node[1]->Y);
  Float l=(n1-n2).magnitude();
  vnl_vector_fixed<Float,2> dn=(n2-n1)/l;

  double a=0;
  double b=l;
  int n=10;  /** number of integration points (even numbers only) */
  vnl_vector_fixed<Float,2> F1,F2;

  {
    double scale, t, tl, tu;
    int i, m, ibase;

    /**  Begin integration  */
    scale = (b - a)/2.0;
    m = n/2;
    ibase = m*m;
    F1.fill(0.0);
    F2.fill(0.0);
          
    for (i=1; i <= m; i++)  {
      t = GaussIntegrate::z[ibase + i - 1];
      tl = (a*(1.0 + t) + (1.0 - t)*b)/2.0;
      tu = (a*(1.0 - t) + (1.0 + t)*b)/2.0;
      F1 = F1 + GaussIntegrate::w[ibase + i - 1]*(l0->Fg(n1+dn*tl)*(l-tl)/l  +l0->Fg(n1+dn*tu)*(l-tu)/l);
      F2 = F2 + GaussIntegrate::w[ibase + i - 1]*(l0->Fg(n1+dn*tl)*tl/l  +l0->Fg(n1+dn*tu)*tu/l);
    }

    F1=scale*F1;
    F2=scale*F2;

  }

  vnl_vector<Float> F(6,0.0);
  F.update(F1,0);
  F.update(F2,3);
  return F;

}




}} // end namespace itk::fem
