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




Element::Displacement* Element::uDOF(int i) const
{
  /**
   * If we got here we throw an exception. This means that some
   * derived implementation of uDOF function called parent function
   * because the i was out of range.
   */
  throw FEMExceptionSolution(__FILE__,__LINE__,"Element::uDOF()","DOF number was out of range!");
}




vnl_matrix<Element::Float> Element::Me() const
{
  /*
   * If the function is not overiden, we return 0 matrix. This means that
   * by default the elements are static.
   */
  return vnl_matrix<Float>(N(),N(),0.0);
}




#ifdef FEM_BUILD_VISUALIZATION
/** Global scale factor for drawing on the DC */
double& Element::DC_Scale=Node::DC_Scale;
#endif




}} // end namespace itk::fem
