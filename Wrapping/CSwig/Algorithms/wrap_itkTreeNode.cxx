/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrap_itkTreeNode.cxx
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
#include "itkHistogramMatchingImageFilter.h"
#include "itkSpatialObject.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
#include "itkCSwigImages.h"

namespace itkso
{
  typedef itk::SpatialObject<2> SO2;
  typedef itk::SpatialObject<3> SO3;
}

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkTreeNode);
  namespace wrappers
  {
    ITK_WRAP_OBJECT1(TreeNode,itkso::SO2 *,itkSOTreeNodeSO2);
    ITK_WRAP_OBJECT1(TreeNode,itkso::SO3 *,itkSOTreeNodeSO3);
  }
}

#endif
