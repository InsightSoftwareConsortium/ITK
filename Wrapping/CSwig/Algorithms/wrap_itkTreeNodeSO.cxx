/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkTreeNodeSO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImage.h"
#include "itkTreeNode.h"
#include "itkSpatialObject.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkTreeNodeSO);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(TreeNode, itk::SpatialObject<2>*,itkTreeNodeSO2);
    ITK_WRAP_OBJECT1(TreeNode, itk::SpatialObject<3>*,itkTreeNodeSO3);
  }
}

#endif
