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
#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
namespace _cable_
{
  const char* const package = ITK_WRAP_PACKAGE_NAME(ITK_WRAP_PACKAGE);
  const char* const groups[] =
  {
    ITK_WRAP_GROUP(ITKKernelDeformableTransforms),
    ITK_WRAP_GROUP(ITKRigidTransforms),
    ITK_WRAP_GROUP(ITKSimilarityTransforms),
    ITK_WRAP_GROUP(itkAffineTransform),
    ITK_WRAP_GROUP(itkAzimuthElevationToCartesianTransform),
    ITK_WRAP_GROUP(itkBSplineDeformableTransform),
    ITK_WRAP_GROUP(itkIdentityTransform),
    ITK_WRAP_GROUP(itkScaleTransform),
    ITK_WRAP_GROUP(itkTranslationTransform),
    ITK_WRAP_GROUP(itkTransform),
    ITK_WRAP_GROUP(itkMatrixOffsetTransformBase),
    ITK_WRAP_GROUP(itkVersorTransformGroup)
  };
}
#endif
