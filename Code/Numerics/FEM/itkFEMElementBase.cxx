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
#include <algorithm>

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



/*
void Element::LinkDegreesOfFreedom(void)
{
  // FIXME: Write a code that checks if two elements are compatible, when they share a node.

  // Define some frequently used constants
  const unsigned int Nnodes=this->GetNumberOfNodes();
  const unsigned int Npoints=this->GetNumberOfPoints();
  const unsigned int NDOFsperNode=this->GetNumberOfDegreesOfFreedomPerNode();

  // Release any existing dof ids that the element may be using.
  this->ClearDegreesOfFreedom();


  // First we need to obtain a list of elements that are in the
  // neighborhood of current element

  // Step over all points in current element
  Node::SetOfElements nbhd_elements;
  for( unsigned int pt=0;pt<Npoints;pt++ )
  {
    PointIDType p=this->GetPoint(pt);
    std::copy(p->m_elements.begin(), p->m_elements.end(), std::inserter(nbhd_elements, nbhd_elements.begin()));
  }
  // remove current element from the neighborhood
  nbhd_elements.erase(this);

  // Then we step over all nodes in current element
  // and try to find a matching node in neighboring element
  for( unsigned int n=0; n<Nnodes; n++ )
  {
    // Get the definition of a current node
    NodeDefinitionType ndef;
    this->GetNodeDefinition(n,ndef);

    // Flag to exit subsequent for loops before they finish
    bool not_done=true;

    // Try to find the matching node definition in neighborhood elements

    // Step over all neighboring elements
    for( Node::SetOfElements::const_iterator el_it=nbhd_elements.begin();
         el_it!=nbhd_elements.end() && not_done;
         el_it++ )
    {
      Element::ConstPointer el=*el_it;

      // Step over all nodes in this neigboring element
      for( unsigned int nn=0; nn<el->GetNumberOfNodes() && not_done; nn++ )
      {
        // Get the definition of a node
        NodeDefinitionType nndef;
        el->GetNodeDefinition(nn,nndef);

        if(ndef==nndef)
        {
          // We found a node that is shared between elements.
          // Copy the DOFs from the neighboring element's node
          // since they have to be the same.
          //
          // Note that neighboring node may contain more or less DOFs.
          // If it has more, we simply ignore the rest, if it has less,
          // we'll get invalid DOF id from GetDegreeOfFreedomAtNode function.

          // If all DOF IDs are set from the neighboring elements,
          // we can terminate the loop over all nodes in
          // neighboring elements.
          not_done=false;

          for(unsigned int d=0; d<NDOFsperNode; d++)
          {
            // Get the DOF from the node at neighboring element
            DegreeOfFreedomIDType global_dof = el->GetDegreeOfFreedomAtNode(nn,d);

            // Set the corresponding DOF in current element only if
            // we find a valid DOF id in the neighboring element
            if( global_dof!=InvalidDegreeOfFreedomID )
            {
              // Error checking
              if( this->GetDegreeOfFreedomAtNode(n,d)!=InvalidDegreeOfFreedomID && 
                  this->GetDegreeOfFreedomAtNode(n,d)!=global_dof)
              {
                // Something got screwed.
                // FIXME: Write a better error handler or remove it completely,
                //        since this should never happen.
                throw FEMException(__FILE__, __LINE__, "FEM error");
              }

              this->SetDegreeOfFreedomAtNode(n,d,global_dof);

            }
            else
            {
              // Whenever we find an invalid DOF ID, we are not done yet.
              not_done=true;
            }

          } // end for d

        } // end if ndef==nndef

      } // end for nn
  
    } // end for el_it


    // Now all DOFs in current element for node n are matched with those
    // in the neghboring elements. However, if none of the neighboring
    // objects defines these DOFs, we need to assign new DOF IDs here.
    for(unsigned int d=0; d<NDOFsperNode; d++) // step over all DOFs at node n
    {
      if( this->GetDegreeOfFreedomAtNode(n,d)==InvalidDegreeOfFreedomID )
      {
        // Found a undefined DOF. We need obtain a unique id,
        // which we set with the SetDegreeOfFreedom function.
        SetDegreeOfFreedomAtNode(n,d,Element::CreateNewGlobalDOF());
      }

    } // end for d

  } // end for n

}
*/




// old link dofs
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
        // Found a undefined DOF. We need obtain a unique id,
        // which we set with the SetDegreeOfFreedom function.
        SetDegreeOfFreedomAtNode(pt,d,Element::CreateNewGlobalDOF());
      }

    } // end for d

  } // end for pt


};





Element::DegreeOfFreedomIDType Element::m_DOFCounter;




}} // end namespace itk::fem
