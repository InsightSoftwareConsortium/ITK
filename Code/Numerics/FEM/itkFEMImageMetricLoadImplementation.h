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

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEMImageMetricLoad.h"

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
  
  static Element::VectorType ImplementImageMetricLoad(ElementNew::ConstPointer element, ElementNew::LoadElementPointer load)
  {
    // We must dynamically cast the given load pointer to the
    // correct templated load class, which is given as
    // template parameter.
    typename TLoadClass::Pointer l0=dynamic_cast<TLoadClass*>(&*load);
    if ( !l0 ) throw FEMException(__FILE__, __LINE__, "FEM error");


    typename Solution::ConstPointer   S=l0->GetSolution(); // has current solution state

    // Order of integration
    // FIXME: Allow changing the order of integration by setting a 
    //        static member within an element base class.
    unsigned int order=0;

    const unsigned int Nip=element->GetNumberOfIntegrationPoints(order);
    const unsigned int Ndofs=element->GetNumberOfDegreesOfFreedomPerNode();
    const unsigned int Nnodes=element->GetNumberOfNodes();

    ElementNew::VectorType Fe(element->GetNumberOfDegreesOfFreedom()*2,0.0),
                         force(Ndofs,0.0),
                         ip,gip,gsol,force_tmp,shapeF;
    ElementNew::Float w,detJ;

    for(unsigned int i=0; i<Nip; i++)
    {
      element->GetIntegrationPointAndWeight(i,ip,w,order);
      gip=element->GetGlobalFromLocalCoordinates(ip);
      gsol=element->InterpolateSolution(ip,*S);

      shapeF=element->ShapeFunctions(ip);
      detJ=element->JacobianDeterminant(ip);

      // Adjust the size of a force vector returned from the load object so
      // that it is equal to the number of DOFs per node. If the Fg returned
      // a vector with less dimensions, we add zero elements. If the Fg
      // returned a vector with more dimensions, we remove the extra dimensions.
      force.fill(0.0);
     
      force_tmp=l0->Fe(gip,gsol);
      unsigned int Nd=Ndofs;
      if(force_tmp.size()<Nd) { Nd=force_tmp.size(); }
      for(unsigned int d=0; d<Nd; d++) { force[d] = force_tmp[d]; }

      // Calculate the equivalent nodal loads
      for(unsigned int n=0; n<Nnodes; n++)
      {
        for(unsigned int d=0; d<Ndofs; d++)
        {
          Fe[n*Ndofs+d]+=shapeF[n]*force[d]*w*detJ;
        }
      }

    }

    return Fe;

  }

//private:
  static const bool registered;
};

// When the templated load implementation function is instantiated,
// it will automatically be registered with the VisitorDispatcher so 
// that it is called as required.
// Instantiating the implementation function will also instantiate the
// corresponding Load class.
template<class TLoadClass>
const bool ImageMetricLoadImplementation<TLoadClass>::registered=
  VisitorDispatcher<ElementNew,Element::LoadElementType,Element::VectorType>
  ::RegisterVisitor((TLoadClass*)0, &ImageMetricLoadImplementation<TLoadClass>::ImplementImageMetricLoad);




}} // end namespace itk::fem
