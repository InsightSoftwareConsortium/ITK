/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementationLandmarkLoadOnElement2DC0LinearQuadrilateralStress.cxx
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

#include "itkFEMElementBase.h"
#include "itkFEMElement2DC0LinearQuadrilateralStress.h"
#include "itkFEMLoadLandmark.h"
#include "itkFEMSolution.h"

namespace itk {
namespace fem {




/**
 * Handles LandmarkLoad over 2D linear quad stress element
 */
Element::VectorType LoadImplementationLandmarkLoadOnElement2DC0LinearQuadrilateralStress(Element2DC0LinearQuadrilateralStress::ConstPointer element, Element::LoadElementPointer load)
{
  // Order of integration
  // FIXME: Allow changing the order of integration by setting a 
  //        static member within an element base class.
  unsigned int order=0;

  const unsigned int Nip=element->GetNumberOfIntegrationPoints(order);
  const unsigned int Ndofs=element->GetNumberOfDegreesOfFreedomPerNode();
  const unsigned int Nnodes=element->GetNumberOfNodes();

  Element::VectorType Fe( element->GetNumberOfDegreesOfFreedom(), 0.0 );
  Element::VectorType force( Ndofs, 0.0 ), disp( Ndofs, 0.0 );
  Element::VectorType shapeF;
  Element::Float w, detJ;

  LoadLandmark::Pointer l0 = dynamic_cast<LoadLandmark*>( &*load );

  // Retrieve the local coordinate at which the force acts
  Element::VectorType pt = l0->GetPoint();

  // Retrieve the stored solution
  Solution::ConstPointer sol = l0->GetSolution();
  
  // Determine the new force vector
  disp = element->InterpolateSolution( pt, (*sol) );
  force = l0->GetForce() - disp;
  
  // "Integrate" at the location of the point load
  shapeF = element->ShapeFunctions(pt);
  detJ = element->JacobianDeterminant(pt);
  
  // Calculate the equivalent nodal loads
  for(unsigned int n=0; n < Nnodes; n++) {
    for(unsigned int d=0; d < Ndofs; d++) {
        Fe[n*Ndofs+d] += shapeF[n] * force[d] * w * detJ;
    }
  }
  
  return Fe;

}




}} // end namespace itk::fem
