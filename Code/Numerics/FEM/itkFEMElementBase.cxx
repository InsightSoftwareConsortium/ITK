/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementBase.cxx
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

namespace itk {
namespace fem {




vnl_matrix<Element::Float> Element::Me() const
{
  /*
   * If the function is not overiden, we return 0 matrix. This means that
   * by default the elements are static.
   */
  return vnl_matrix<Float>(GetNumberOfDegreesOfFreedom(),GetNumberOfDegreesOfFreedom(),0.0);
}




#ifdef FEM_BUILD_VISUALIZATION
/** Global scale factor for drawing on the DC */
double& Element::DC_Scale=Node::DC_Scale;
#endif




//////////////////////////////////////////////////////////////////////////
/*
 * Node management
 */

void Element::ClearDegreesOfFreedom(void)
{
  for(unsigned int i=0;i<GetNumberOfDegreesOfFreedom();i++)
  {
    SetDegreeOfFreedom(i,InvalidDegreeOfFreedomID);
  }

}




void Element::LinkDegreesOfFreedom(void)
{
  // FIXME: Write a code that checks if two elements are compatible, when they share a point.

  // Release any existing dofs that the element may be using.
  this->ClearDegreesOfFreedom();

  // Step over all points in current cell
  for( unsigned int pt=0;pt<this->GetNumberOfPoints();pt++ )
  {
    PointIDType n=this->GetPoint(pt);

    // Step over all elements that also use point pt
    // FIXME: This needs to be changed when the Mesh is used.
    for( Node::SetOfElements::const_iterator el_it=n->m_elements.begin();
         el_it!=n->m_elements.end();
         el_it++ )
    {
      Element::ConstPointer el=*el_it;
      if(el==this) // skip current element
      { 
        continue;
      }

      // Check if point ptj of the neighboring element is same as
      // the point pt of the current element
      for( unsigned int ptn=0;ptn<el->GetNumberOfPoints();ptn++ ) // Step over all points in the neighbor cell
      {
        // Skip all points that are not shared.
        if( n!=el->GetPoint(ptn) )
        {
          continue;
        }

        // Copy the DOFs from the neighboring element's point
        // since they have to be the same. Note that neighboring point
        // may contain more or less DOFs. If it has more, we simply ignore
        // the rest, if it has less, we'll get invalid DOF id from
        // GetDegreeOfFreedomAtNode function.
        for(unsigned int d=0; d<this->GetNumberOfDegreesOfFreedomPerNode(); d++)
        {
          // Get the DOF from the point at neighboring element
          DegreeOfFreedomIDType global_dof = el->GetDegreeOfFreedomAtNode(ptn,d);

          // Set the corresponding DOF in current element only if
          // we find a valid DOF id in the neighboring element
          if( global_dof!=InvalidDegreeOfFreedomID )
          {
            // Error checking
            if( this->GetDegreeOfFreedomAtNode(pt,d)!=InvalidDegreeOfFreedomID && 
                this->GetDegreeOfFreedomAtNode(pt,d)!=global_dof)
            {
              // Something got screwed.
              // FIXME: Write a better error handler or remove it completely,
              //        since this should never happen.
              throw FEMException(__FILE__, __LINE__, "FEM error");
            }
            this->SetDegreeOfFreedomAtNode(pt,d,global_dof);
          }

        } // end for d

      } // end for ptn

    } // end for el_it



    // Now all DOFs in current object for point i are matched with those
    // in the neghboring elements. However, if none of the neighboring
    // objects defines these DOFs, we need to create them.
    for(unsigned int d=0;d<this->GetNumberOfDegreesOfFreedomPerNode();d++) // step over all DOFs at point pt
    {
      if( this->GetDegreeOfFreedomAtNode(pt,d)==InvalidDegreeOfFreedomID )
      {
        /*
         * Found a undefined DOF. We need obtain a unique id,
         * which we set with the SetDegreeOfFreedom function.
         */
        SetDegreeOfFreedomAtNode(pt,d,Element::CreateNewGlobalDOF());
      }

    } // end for d

  } // end for pt


};


Element::DegreeOfFreedomIDType Element::m_DOFCounter;




}} // end namespace itk::fem
