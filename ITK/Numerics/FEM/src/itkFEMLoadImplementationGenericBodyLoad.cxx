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

#include "itkFEMLoadImplementationGenericBodyLoad.h"

namespace itk {
namespace fem {

/**
 * Handle LoadGrav in element by integrating over the element domain.
 * This implementation requires that the element has the shape functions
 * and integration routines defined.
 *
 * It is also assumed, that element's local DOFs are numbered with respect
 * to node ID. If this is not the case, you should not use this function.
 */
void
LoadImplementationGenericBodyLoad
::Implementation(Element::ConstPointer element, LoadGrav::Pointer load, Element::VectorType& Fe)
{
  // Order of integration
  // FIXME: Allow changing the order of integration by setting a
  //        static member within an element base class.
  unsigned int order=0;

  const unsigned int Nip=element->GetNumberOfIntegrationPoints(order);
  const unsigned int Ndofs=element->GetNumberOfDegreesOfFreedomPerNode();
  const unsigned int Nnodes=element->GetNumberOfNodes();

  Element::VectorType force(Ndofs,0.0),
    ip,gip,force_tmp,shapeF;

  Fe.set_size(element->GetNumberOfDegreesOfFreedom());
  Fe.fill(0.0);

  Element::Float w,detJ;

  for(unsigned int i=0; i<Nip; i++)
    {
    element->GetIntegrationPointAndWeight(i,ip,w,order);
    gip=element->GetGlobalFromLocalCoordinates(ip);

    shapeF=element->ShapeFunctions(ip);
    detJ=element->JacobianDeterminant(ip);

    // Adjust the size of a force vector returned from the load object so
    // that it is equal to the number of DOFs per node. If the Fg returned
    // a vector with less dimensions, we add zero elements. If the Fg
    // returned a vector with more dimensions, we remove the extra dimensions.
    force.fill(0.0);
    // FIXME: Maybe Fg function should be declared as const in LoadGrav.
    force_tmp=load->Fg(gip);
    unsigned int Nd=Ndofs;
    if(force_tmp.size()<Nd)
      {
      Nd=force_tmp.size();
      }
    for(unsigned int d=0; d<Nd; d++)
      {
      force[d] = force_tmp[d];
      }

    // Claculate the equivalent nodal loads
    for(unsigned int n=0; n<Nnodes; n++)
      {
      for(unsigned int d=0; d<Ndofs; d++)
        {
        Fe[n*Ndofs+d] += shapeF[n]*force[d]*w*detJ;
        }
      }
    }

}

}} // end namespace itk::fem
