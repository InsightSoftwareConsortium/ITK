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




Element::DegreeOfFreedomIDType Element::m_DOFCounter;




}} // end namespace itk::fem
