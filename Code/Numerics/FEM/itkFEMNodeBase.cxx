/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMNodeBase.cxx
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

#include "itkFEMNodeBase.h"
#include "itkFEMElementBase.h"

namespace itk {
namespace fem {




/** 
 * Windows MFC visualization routines
 */
#ifdef FEM_BUILD_VISUALIZATION

  /**
   * global scale for drawing on the DC
   */
  double Node::DC_Scale=1000.0;
#endif




unsigned int Node::GetDegreeOfFreedom(unsigned int i) const
{
  // First Find an element with the most DOFs that uses this node
  unsigned int ndof=0;
  ::itk::fem::Element::Pointer el=0;
  for( ::itk::fem::Node::SetOfElements::const_iterator e=this->m_elements.begin(); 
       e!=this->m_elements.end();
       e++ )
  {
    if ( (*e)->GetNumberOfDegreesOfFreedomPerPoint() > ndof )
    {
      ndof=(*e)->GetNumberOfDegreesOfFreedomPerPoint();
      el=(*e);
    }
  }

  if (el==0) return 0xFFFFFFFF;

  // find a point in an element that corresponds to this node
  unsigned int pt=0;
  for (pt=0; pt<el->GetNumberOfPoints(); pt++)
  {
    if(el->GetPoint(pt)==this)
    {
      break;
    }
  }

  // error checking
  if(pt>=el->GetNumberOfPoints())
  {
    // m_elements set was defined incorrectly
    throw;
  }

  return el->GetDegreeOfFreedomAtPoint(pt,i);

}



}} // end namespace itk::fem
