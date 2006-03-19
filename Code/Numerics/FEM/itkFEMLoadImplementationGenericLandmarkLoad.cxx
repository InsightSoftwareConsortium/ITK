/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMLoadImplementationGenericLandmarkLoad.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMLoadImplementationGenericLandmarkLoad.h"

namespace itk {
namespace fem {




/*
 * Handles LandmarkLoad on 2D linear quad stress element
 */
void
LoadImplementationGenericLandmarkLoad
::Implementation(Element::ConstPointer element, LoadLandmark::Pointer load, Element::VectorType& Fe)
{
  const unsigned int NnDOF=element->GetNumberOfDegreesOfFreedomPerNode();
  const unsigned int Nnodes=element->GetNumberOfNodes();

  Element::VectorType force( NnDOF, 0.0 ), disp( NnDOF, 0.0 ), new_source (NnDOF, 0.0);
  Element::VectorType shapeF;

  Fe.set_size(element->GetNumberOfDegreesOfFreedom());
  Fe.fill(0.0);

  // Retrieve the local coordinate at which the force acts
  Element::VectorType pt = load->GetPoint();


  // Retrieve the stored solution
  Solution::ConstPointer sol = load->GetSolution();
  
  // Determine the displacement at point pt
  const unsigned int TotalSolutionIndex=1;
  disp = element->InterpolateSolution( pt, (*sol), TotalSolutionIndex );

  // Convert the source to global coordinates
  new_source = load->GetSource() + disp;

   // Calculate the new force
  
  load->m_force =  disp;
  force =  (load->m_target-new_source) / load->eta ;
 
//  std::cout << " disp " << disp <<  std::endl;
  //force /= vcl_sqrt(fmag);
  new_source = (load->GetTarget() - new_source);
//  std::cout << " force = " << force <<  " distance  " << new_source.magnitude() << std::endl;
  
  float curdist=new_source.magnitude();
  if (curdist < 1.0) force.fill(0.0);
  std::cout <<  " LM distance  " << curdist << std::endl;
  
  // "Integrate" at the location of the point load
  shapeF = element->ShapeFunctions(pt);
  
  // Calculate the equivalent nodal loads
  for(unsigned int n=0; n < Nnodes; n++)
  {      
    for(unsigned int d=0; d < NnDOF; d++)
    {
        Fe[n*NnDOF+d] += shapeF[n] * force[d];
    }
  }

 

  
}




}} // end namespace itk::fem
