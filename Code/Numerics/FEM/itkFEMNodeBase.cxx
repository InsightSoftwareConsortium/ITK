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




}} // end namespace itk::fem
