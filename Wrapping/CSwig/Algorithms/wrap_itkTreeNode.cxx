/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
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
