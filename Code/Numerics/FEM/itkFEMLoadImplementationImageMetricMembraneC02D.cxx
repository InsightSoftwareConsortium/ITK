/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementationImageMetricMembraneC02D.cxx
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

#include "itkFEMElementMembraneC02D.h"
#include "itkFEMLoadGrav.h"
#include "vnl/vnl_math.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk {
namespace fem {



/**
 * Return the force vector for MembraneC02D element
 */
Element::LoadVectorType
LoadImageMetricImplementationMembraneC02D
(MembraneC02D::ConstPointer element, Element::LoadElementPointer load)
{
    
  typedef Element::Float Float;
  LoadGrav::Pointer l0=dynamic_cast<LoadGrav*>(&*load);
  if ( !l0 ) throw;
  // for gravity loads
  // 2 dimensional holders for points and vectors
  vnl_vector_fixed<Float,4> pt1(0.0); // (-1,-1) in local coords
  vnl_vector_fixed<Float,4> pt2(0.0); // ( 1,-1) in local coords
  vnl_vector_fixed<Float,4> pt3(0.0); // ( 1, 1) in local coords
  vnl_vector_fixed<Float,4> pt4(0.0); // (-1, 1) in local coords
 
  vnl_vector_fixed<Float,2> f(0.0);
  vnl_vector_fixed<Float,4> pt(0.0); // used for the current pt in global domain
  Solution::ConstPointer   S=l0->GetSolution(); // has current solution state



  // Computes point at which gravity load acts  
  // and also gives the current displacement
  pt1[0] = (element->m_node[0]->X); 
  pt1[1] = (element->m_node[0]->Y);

  pt2[0] = (element->m_node[1]->X); 
  pt2[1] = (element->m_node[1]->Y);

  pt3[0] = (element->m_node[2]->X); 
  pt3[1] = (element->m_node[2]->Y);

  pt4[0] = (element->m_node[3]->X); 
  pt4[1] = (element->m_node[3]->Y);

  
  pt1[2] = S->GetSolutionValue(element->GetDegreeOfFreedom(0));
  pt1[3] = S->GetSolutionValue(element->GetDegreeOfFreedom(1));

  pt2[2] = S->GetSolutionValue(element->GetDegreeOfFreedom(2));
  pt2[3] = S->GetSolutionValue(element->GetDegreeOfFreedom(3));

  pt3[2] = S->GetSolutionValue(element->GetDegreeOfFreedom(4));
  pt3[3] = S->GetSolutionValue(element->GetDegreeOfFreedom(5));

  pt4[2] = S->GetSolutionValue(element->GetDegreeOfFreedom(6));
  pt4[3] = S->GetSolutionValue(element->GetDegreeOfFreedom(7));

  typedef Element::Float Float;

  vnl_vector<Float> GL(8), shapeF(4);
  vnl_matrix<Float> J(2,2);
  Float detJ=1;

  /** Defines gaussian integration points */
  Float gpt = 1.0 / sqrt(3.0);
  Float GPoints[][2] = {{-gpt, -gpt}, {gpt, -gpt}, {gpt, gpt}, {-gpt, gpt}};

  /** Declares auxiliary variables */
  Float x[2];
  int p;

  /** Initialize load vector */
  GL.fill(0.0);

  /** Carry out numerical integration */
  for (int i=0; i<4; i++) {
    
    /** Gets integration point */
    x[0] = GPoints[i][0];
    x[1] = GPoints[i][1];

    /** Computes the shape function derivatives at integration point */
    shapeF = element->ComputeShapeFunctionsAt(x);

    /** Computes the Jacobian matrix and its determinant
      * at the i-th integration point
      */
    J = element->ComputeJacobianMatrixAt(x);
    detJ = element->JacobianMatrixDeterminant(J);

    /* Compute the global (cartesian) position and solution at local coordinate x. */
    pt[0]=pt1[0]*shapeF[0]+pt2[0]*shapeF[1]+pt3[0]*shapeF[2]+pt4[0]*shapeF[3];
    pt[1]=pt1[1]*shapeF[0]+pt2[1]*shapeF[1]+pt3[1]*shapeF[2]+pt4[1]*shapeF[3];
    pt[2]=pt1[2]*shapeF[0]+pt2[2]*shapeF[1]+pt3[2]*shapeF[2]+pt4[2]*shapeF[3];
    pt[3]=pt1[3]*shapeF[0]+pt2[3]*shapeF[1]+pt3[3]*shapeF[2]+pt4[3]*shapeF[3];

    Float GaussWeight=1.0;
    f=GaussWeight*l0->Fg(pt);    

    /** Calculates loads and associate with element nodal points */
    for (int j=0; j<4; j++) {
      p = j<<1;
      GL[  p] += f[0] * shapeF[j] * detJ;
      GL[p+1] += f[1] * shapeF[j] * detJ;
    }
  }
  
  return GL;
 
}




}} // end namespace itk::fem
