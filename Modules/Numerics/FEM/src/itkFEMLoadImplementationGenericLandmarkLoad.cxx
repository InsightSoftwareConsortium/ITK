/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMLoadImplementationGenericLandmarkLoad.h"

namespace itk {
namespace fem {

/**
 * Handles LandmarkLoad on 2D linear quad stress element
 */
void
LoadImplementationGenericLandmarkLoad
::Implementation(Element::ConstPointer element, LoadLandmark::Pointer load, Element::VectorType& Fe)
{
  const unsigned int NnDOF=element->GetNumberOfDegreesOfFreedomPerNode();
  const unsigned int Nnodes=element->GetNumberOfNodes();

  Element::VectorType force( NnDOF, 0.0 );
  Element::VectorType disp( NnDOF, 0.0 );
  Element::VectorType new_source (NnDOF, 0.0);
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
  force =  (load->m_target-new_source) / load->eta;

//  std::cout << " disp " << disp <<  std::endl;
  //force /= vcl_sqrt(fmag);
  new_source = (load->GetTarget() - new_source);
//  std::cout << " force = " << force <<  " distance  " << new_source.magnitude() << std::endl;

  Element::Float curdist = new_source.magnitude();
  if ( curdist < 1.0 )
    {
    force.fill(0.0);
    }
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
