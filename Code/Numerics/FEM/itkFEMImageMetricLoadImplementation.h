/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMImageMetricLoadImplementation.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFEMImageMetricLoadImplementation_h
#define __itkFEMImageMetricLoadImplementation_h

#include "itkFEMImageMetricLoad.h"

#include "itkFEMElement2DC0LinearLineStress.h"
#include "itkFEMElement2DC1Beam.h"
#include "itkFEMElement2DC0LinearTriangularStress.h"
#include "itkFEMElement2DC0LinearQuadrilateralMembrane.h"
#include "itkFEMElement2DC0LinearQuadrilateralMembrane.h"
#include "itkFEMElement3DC0LinearTetrahedronStrain.h"
#include "itkFEMElement3DC0LinearHexahedronStrain.h"

namespace itk {
namespace fem {




/**
 * This is an example of how to define the implementation of a templated
 * Load class. Since the Load class is templated, its implementation must
 * also be templated. Due to limitations of MS compiler, we define this
 * implementation function as a static function inside a templated class.
 *
 * To make things easier to use, we template the class over the whole
 * templated load class and not only over the template parameters required
 * to define the templated Load class.
 *
 * You must manually instantiate this class to register the load
 * implementation function with the VisitorDispatcher. The
 * instantiation is normally done like:
 *     typedef LoadTest<...> MyLoadTestClass;
 *     template class LoadTestImplementationBar2D<MyLoadTestClass>;
 */
template<class TLoadClass>
class ImageMetricLoadImplementation
{
public:
  
  template<class TElementClassConstPointer>
  static void ImplementImageMetricLoad(TElementClassConstPointer element, Element::LoadPointer load, Element::VectorType& Fe )
  {
    // We must dynamically cast the given load pointer to the
    // correct templated load class, which is given as
    // template parameter.
    typename TLoadClass::Pointer l0=dynamic_cast<TLoadClass*>(&*load);
    if ( !l0 ) throw FEMException(__FILE__, __LINE__, "FEM error");

    Implementation(static_cast<Element::ConstPointer>(element),l0,Fe);
  }

private:
  
  static const bool registered;
  
  static void Implementation(typename Element::ConstPointer element, typename TLoadClass::Pointer l0, typename Element::VectorType& Fe)
  {
    const unsigned int TotalSolutionIndex=1;/* Need to change if the index changes in CrankNicolsonSolver */
    typename Solution::ConstPointer   S=l0->GetSolution(); // has current solution state

    // Order of integration
    // FIXME: Allow changing the order of integration by setting a 
    //        static member within an element base class.
    unsigned int order=l0->GetNumberOfIntegrationPoints();

    const unsigned int Nip=element->GetNumberOfIntegrationPoints(order);
    const unsigned int Ndofs=element->GetNumberOfDegreesOfFreedomPerNode();
    const unsigned int Nnodes=element->GetNumberOfNodes();
    unsigned int ImageDimension=Ndofs;

    Element::VectorType  force(Ndofs,0.0),
                         ip,gip,gsol,force_tmp,shapef;
    Element::Float w,detJ;

    Fe.resize(element->GetNumberOfDegreesOfFreedom());
    Fe.fill(0.0);
    shapef.resize(Nnodes);
    gsol.resize(Ndofs);
    gip.resize(Ndofs);

    for(unsigned int i=0; i<Nip; i++)
    {
      element->GetIntegrationPointAndWeight(i,ip,w,order);
/*      gip=element->GetGlobalFromLocalCoordinates(ip);
      gsol=element->InterpolateSolution(ip,*S,TotalSolutionIndex);
      //std::cout << gsol << std::endl;
      shapeF=element->ShapeFunctions(ip);
*/

  if (ImageDimension == 3){
#define FASTHEX
#ifdef FASTHEX
  float r=ip[0]; float s=ip[1]; float t=ip[2];
//FIXME temporarily using hexahedron shape f for speed
  shapef[0] = (1 - r) * (1 - s) * (1 - t) * 0.125;
  shapef[1] = (1 + r) * (1 - s) * (1 - t) * 0.125;
  shapef[2] = (1 + r) * (1 + s) * (1 - t) * 0.125;
  shapef[3] = (1 - r) * (1 + s) * (1 - t) * 0.125;
  shapef[4] = (1 - r) * (1 - s) * (1 + t) * 0.125;
  shapef[5] = (1 + r) * (1 - s) * (1 + t) * 0.125;
  shapef[6] = (1 + r) * (1 + s) * (1 + t) * 0.125;
  shapef[7] = (1 - r) * (1 + s) * (1 + t) * 0.125;
#else
        shapef = element->ShapeFunctions(ip);
#endif
}else if (ImageDimension==2) shapef = element->ShapeFunctions(ip);

        float solval,posval;
        detJ=element->JacobianDeterminant(ip);
        
        for(unsigned int f=0; f<ImageDimension; f++)
        {
          solval=0.0;
          posval=0.0;
          for(unsigned int n=0; n<Nnodes; n++)
          {
            posval+=shapef[n]*((element->GetNodeCoordinates(n))[f]);
            solval+=shapef[n] * S->GetSolutionValue( element->GetNode(n)->GetDegreeOfFreedom(f) , TotalSolutionIndex);
          }
          gsol[f]=solval;
          gip[f]=posval;
        }

      // Adjust the size of a force vector returned from the load object so
      // that it is equal to the number of DOFs per node. If the Fg returned
      // a vector with less dimensions, we add zero elements. If the Fg
      // returned a vector with more dimensions, we remove the extra dimensions.
      force.fill(0.0);
     
      force=l0->Fe(gip,gsol);
      // Calculate the equivalent nodal loads
      for(unsigned int n=0; n<Nnodes; n++)
      {
        for(unsigned int d=0; d<Ndofs; d++)
        {
          itk::fem::Element::Float temp=shapef[n]*force[d]*w*detJ;
          Fe[n*Ndofs+d]+=temp;
        }
      }

    }

  }

};


// When the templated load implementation function is instantiated,
// it will automatically be registered with the VisitorDispatcher so 
// that it is called as required.
// Instantiating the implementation function will also instantiate the
// corresponding Load class.
/*template<class TLoadClass>
const bool ImageMetricLoadImplementation<TLoadClass>::registered=
VisitorDispatcher<Element2DC0LinearQuadrilateralMembrane,Element::LoadType, Element2DC0LinearQuadrilateralMembrane::LoadImplementationFunctionPointer>
  ::RegisterVisitor((TLoadClass*)0, &ImageMetricLoadImplementation<TLoadClass>::ImplementImageMetricLoad);
*/



}} // end namespace itk::fem

#endif // #ifndef __itkFEMImageMetricLoadImplementation_h
