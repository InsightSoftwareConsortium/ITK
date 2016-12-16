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

#include <iostream>
#include <string.h>
#include <algorithm>
#include "itkVersion.h"
#include "itkTransformFactoryBase.h"

#define itkPushIfTransformDim(str,D) (D<= (ITK_TRANSFORM_FACTORY_MAX_DIM) )?defaultTransforms.push_back(str):((void)0)

int itkTransformFactoryBaseTest (int, char*[])
{
  // Call register default transforms
  itk::TransformFactoryBase::RegisterDefaultTransforms();

  // create the list of default transforms
  std::list<std::string> defaultTransforms;

  // Test doubles first (in alphabetical order)

  itkPushIfTransformDim("AffineTransform_double_2_2", 2);
  itkPushIfTransformDim("AffineTransform_double_3_3", 3);
  itkPushIfTransformDim("AffineTransform_double_4_4", 4);
  itkPushIfTransformDim("AffineTransform_double_5_5", 5);
  itkPushIfTransformDim("AffineTransform_double_6_6", 6);
  itkPushIfTransformDim("AffineTransform_double_7_7", 7);
  itkPushIfTransformDim("AffineTransform_double_8_8", 8);
  itkPushIfTransformDim("AffineTransform_double_9_9", 9);

  itkPushIfTransformDim("AzimuthElevationToCartesianTransform_double_3_3", 3);

  itkPushIfTransformDim("BSplineTransform_double_2_2", 2);
  itkPushIfTransformDim("BSplineTransform_double_3_3", 3);
#if !defined(ITK_FUTURE_LEGACY_REMOVE)
  itkPushIfTransformDim("BSplineDeformableTransform_double_2_2", 2);
  itkPushIfTransformDim("BSplineDeformableTransform_double_3_3", 3);
#endif

  itkPushIfTransformDim("CenteredAffineTransform_double_2_2", 2);
  itkPushIfTransformDim("CenteredAffineTransform_double_3_3", 3);
  itkPushIfTransformDim("CenteredEuler3DTransform_double_3_3", 3);
  itkPushIfTransformDim("CenteredRigid2DTransform_double_2_2", 2);
  itkPushIfTransformDim("CenteredSimilarity2DTransform_double_2_2", 2);

  itkPushIfTransformDim("CompositeTransform_double_2_2", 2);
  itkPushIfTransformDim("CompositeTransform_double_3_3", 3);
  itkPushIfTransformDim("CompositeTransform_double_4_4", 4);
  itkPushIfTransformDim("CompositeTransform_double_5_5", 5);
  itkPushIfTransformDim("CompositeTransform_double_6_6", 6);
  itkPushIfTransformDim("CompositeTransform_double_7_7", 7);
  itkPushIfTransformDim("CompositeTransform_double_8_8", 8);
  itkPushIfTransformDim("CompositeTransform_double_9_9", 9);

  itkPushIfTransformDim("Euler2DTransform_double_2_2", 2);
  itkPushIfTransformDim("Euler3DTransform_double_3_3", 3);

  itkPushIfTransformDim("FixedCenterOfRotationAffineTransform_double_3_3", 3);

  itkPushIfTransformDim("IdentityTransform_double_2_2", 2);
  itkPushIfTransformDim("IdentityTransform_double_3_3", 3);
  itkPushIfTransformDim("IdentityTransform_double_4_4", 4);
  itkPushIfTransformDim("IdentityTransform_double_5_5", 5);
  itkPushIfTransformDim("IdentityTransform_double_6_6", 6);
  itkPushIfTransformDim("IdentityTransform_double_7_7", 7);
  itkPushIfTransformDim("IdentityTransform_double_8_8", 8);
  itkPushIfTransformDim("IdentityTransform_double_9_9", 9);

  itkPushIfTransformDim("QuaternionRigidTransform_double_3_3", 3);

  itkPushIfTransformDim("Rigid2DTransform_double_2_2", 2);
  itkPushIfTransformDim("Rigid3DPerspectiveTransform_double_3_2", 3);
  itkPushIfTransformDim("Rigid3DTransform_double_3_3", 3);

  itkPushIfTransformDim("ScalableAffineTransform_double_3_3", 3);
  itkPushIfTransformDim("ScaleLogarithmicTransform_double_3_3", 3);
  itkPushIfTransformDim("ScaleSkewVersor3DTransform_double_3_3", 3);
  itkPushIfTransformDim("ScaleTransform_double_2_2", 2);
  itkPushIfTransformDim("ScaleTransform_double_3_3", 3);
  itkPushIfTransformDim("ScaleTransform_double_4_4", 4);
  itkPushIfTransformDim("ScaleVersor3DTransform_double_3_3", 3);

  itkPushIfTransformDim("Similarity2DTransform_double_2_2", 2);
  itkPushIfTransformDim("Similarity3DTransform_double_3_3", 3);

  itkPushIfTransformDim("TranslationTransform_double_2_2", 2);
  itkPushIfTransformDim("TranslationTransform_double_3_3", 3);
  itkPushIfTransformDim("TranslationTransform_double_4_4", 4);
  itkPushIfTransformDim("TranslationTransform_double_5_5", 5);
  itkPushIfTransformDim("TranslationTransform_double_6_6", 6);
  itkPushIfTransformDim("TranslationTransform_double_7_7", 7);
  itkPushIfTransformDim("TranslationTransform_double_8_8", 8);
  itkPushIfTransformDim("TranslationTransform_double_9_9", 9);

  itkPushIfTransformDim("VersorRigid3DTransform_double_3_3", 3);
  itkPushIfTransformDim("VersorTransform_double_3_3", 3);

  // Test float instances (in alphabetical order)

  itkPushIfTransformDim("AffineTransform_float_2_2", 2);
  itkPushIfTransformDim("AffineTransform_float_3_3", 3);
  itkPushIfTransformDim("AffineTransform_float_4_4", 4);
  itkPushIfTransformDim("AffineTransform_float_5_5", 5);
  itkPushIfTransformDim("AffineTransform_float_6_6", 6);
  itkPushIfTransformDim("AffineTransform_float_7_7", 7);
  itkPushIfTransformDim("AffineTransform_float_8_8", 8);
  itkPushIfTransformDim("AffineTransform_float_9_9", 9);

  itkPushIfTransformDim("AzimuthElevationToCartesianTransform_float_3_3", 3);

  itkPushIfTransformDim("BSplineTransform_float_2_2", 2);
  itkPushIfTransformDim("BSplineTransform_float_3_3", 3);
#if !defined(ITK_FUTURE_LEGACY_REMOVE)
  itkPushIfTransformDim("BSplineDeformableTransform_float_2_2", 2);
  itkPushIfTransformDim("BSplineDeformableTransform_float_3_3", 3);
#endif

  itkPushIfTransformDim("CenteredAffineTransform_float_2_2", 2);
  itkPushIfTransformDim("CenteredAffineTransform_float_3_3", 3);
  itkPushIfTransformDim("CenteredEuler3DTransform_float_3_3", 3);
  itkPushIfTransformDim("CenteredRigid2DTransform_float_2_2", 2);
  itkPushIfTransformDim("CenteredSimilarity2DTransform_float_2_2", 2);

  itkPushIfTransformDim("CompositeTransform_float_2_2", 2);
  itkPushIfTransformDim("CompositeTransform_float_3_3", 3);
  itkPushIfTransformDim("CompositeTransform_float_4_4", 4);
  itkPushIfTransformDim("CompositeTransform_float_5_5", 5);
  itkPushIfTransformDim("CompositeTransform_float_6_6", 6);
  itkPushIfTransformDim("CompositeTransform_float_7_7", 7);
  itkPushIfTransformDim("CompositeTransform_float_8_8", 8);
  itkPushIfTransformDim("CompositeTransform_float_9_9", 9);

  itkPushIfTransformDim("Euler2DTransform_float_2_2", 2);
  itkPushIfTransformDim("Euler3DTransform_float_3_3", 3);

  itkPushIfTransformDim("FixedCenterOfRotationAffineTransform_float_3_3", 3);

  itkPushIfTransformDim("IdentityTransform_float_2_2", 2);
  itkPushIfTransformDim("IdentityTransform_float_3_3", 3);
  itkPushIfTransformDim("IdentityTransform_float_4_4", 4);
  itkPushIfTransformDim("IdentityTransform_float_5_5", 5);
  itkPushIfTransformDim("IdentityTransform_float_6_6", 6);
  itkPushIfTransformDim("IdentityTransform_float_7_7", 7);
  itkPushIfTransformDim("IdentityTransform_float_8_8", 8);
  itkPushIfTransformDim("IdentityTransform_float_9_9", 9);

  itkPushIfTransformDim("QuaternionRigidTransform_float_3_3", 3);

  itkPushIfTransformDim("Rigid2DTransform_float_2_2", 2);
  itkPushIfTransformDim("Rigid3DPerspectiveTransform_float_3_2", 3);
  itkPushIfTransformDim("Rigid3DTransform_float_3_3", 3);

  itkPushIfTransformDim("ScalableAffineTransform_float_3_3", 3);
  itkPushIfTransformDim("ScaleLogarithmicTransform_float_3_3", 3);
  itkPushIfTransformDim("ScaleSkewVersor3DTransform_float_3_3", 3);
  itkPushIfTransformDim("ScaleTransform_float_2_2", 2);
  itkPushIfTransformDim("ScaleTransform_float_3_3", 3);
  itkPushIfTransformDim("ScaleTransform_float_4_4", 4);
  itkPushIfTransformDim("ScaleVersor3DTransform_float_3_3", 3);

  itkPushIfTransformDim("Similarity2DTransform_float_2_2", 2);
  itkPushIfTransformDim("Similarity3DTransform_float_3_3", 3);

  itkPushIfTransformDim("TranslationTransform_float_2_2", 2);
  itkPushIfTransformDim("TranslationTransform_float_3_3", 3);
  itkPushIfTransformDim("TranslationTransform_float_4_4", 4);
  itkPushIfTransformDim("TranslationTransform_float_5_5", 5);
  itkPushIfTransformDim("TranslationTransform_float_6_6", 6);
  itkPushIfTransformDim("TranslationTransform_float_7_7", 7);
  itkPushIfTransformDim("TranslationTransform_float_8_8", 8);
  itkPushIfTransformDim("TranslationTransform_float_9_9", 9);

  itkPushIfTransformDim("VersorRigid3DTransform_float_3_3", 3);
  itkPushIfTransformDim("VersorTransform_float_3_3", 3);
  itkPushIfTransformDim("DisplacementFieldTransform_float_2_2", 2);
  itkPushIfTransformDim("DisplacementFieldTransform_float_3_3", 3);
  itkPushIfTransformDim("BSplineSmoothingOnUpdateDisplacementFieldTransform_float_2_2", 2);
  itkPushIfTransformDim("BSplineSmoothingOnUpdateDisplacementFieldTransform_float_3_3", 3);
  itkPushIfTransformDim("ConstantVelocityFieldTransform_float_2_2", 2);
  itkPushIfTransformDim("ConstantVelocityFieldTransform_float_3_3", 3);
  itkPushIfTransformDim("VelocityFieldTransform_float_2_2", 2);
  itkPushIfTransformDim("VelocityFieldTransform_float_3_3", 3);
  itkPushIfTransformDim("TimeVaryingBSplineVelocityFieldTransform_float_2_2", 2);
  itkPushIfTransformDim("TimeVaryingBSplineVelocityFieldTransform_float_3_3", 3);
  itkPushIfTransformDim("TimeVaryingVelocityFieldTransform_float_2_2", 2);
  itkPushIfTransformDim("TimeVaryingVelocityFieldTransform_float_3_3", 3);
  itkPushIfTransformDim("GaussianExponentialDiffeomorphicTransform_float_2_2", 2);
  itkPushIfTransformDim("GaussianExponentialDiffeomorphicTransform_float_3_3", 3);
  itkPushIfTransformDim("GaussianSmoothingOnUpdateDisplacementFieldTransform_float_2_2", 2);
  itkPushIfTransformDim("GaussianSmoothingOnUpdateDisplacementFieldTransform_float_3_3", 3);
  itkPushIfTransformDim("GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform_float_2_2", 2);
  itkPushIfTransformDim("GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform_float_3_3", 3);

  itkPushIfTransformDim("DisplacementFieldTransform_double_2_2", 2);
  itkPushIfTransformDim("DisplacementFieldTransform_double_3_3", 3);
  itkPushIfTransformDim("BSplineSmoothingOnUpdateDisplacementFieldTransform_double_2_2", 2);
  itkPushIfTransformDim("BSplineSmoothingOnUpdateDisplacementFieldTransform_double_3_3", 3);
  itkPushIfTransformDim("ConstantVelocityFieldTransform_double_2_2", 2);
  itkPushIfTransformDim("ConstantVelocityFieldTransform_double_3_3", 3);
  itkPushIfTransformDim("VelocityFieldTransform_double_2_2", 2);
  itkPushIfTransformDim("VelocityFieldTransform_double_3_3", 3);
  itkPushIfTransformDim("TimeVaryingBSplineVelocityFieldTransform_double_2_2", 2);
  itkPushIfTransformDim("TimeVaryingBSplineVelocityFieldTransform_double_3_3", 3);
  itkPushIfTransformDim("TimeVaryingVelocityFieldTransform_double_2_2", 2);
  itkPushIfTransformDim("TimeVaryingVelocityFieldTransform_double_3_3", 3);
  itkPushIfTransformDim("GaussianExponentialDiffeomorphicTransform_double_2_2", 2);
  itkPushIfTransformDim("GaussianExponentialDiffeomorphicTransform_double_3_3", 3);
  itkPushIfTransformDim("GaussianExponentialDiffeomorphicTransform_float_2_2", 2);
  itkPushIfTransformDim("GaussianExponentialDiffeomorphicTransform_float_3_3", 3);
  itkPushIfTransformDim("GaussianSmoothingOnUpdateDisplacementFieldTransform_double_2_2", 2);
  itkPushIfTransformDim("GaussianSmoothingOnUpdateDisplacementFieldTransform_double_3_3", 3);
  itkPushIfTransformDim("GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform_double_2_2", 2);
  itkPushIfTransformDim("GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform_double_3_3", 3);
  int testReturnStatus = EXIT_SUCCESS;

  // check to make sure that all default transforms have been registered
  defaultTransforms.sort();
  // Print out the names of all the registered transforms
  std::list<std::string> names = itk::TransformFactoryBase::GetFactory()->GetClassOverrideWithNames();
  names.sort();
    {
    std::list<std::string>::iterator defaultsIt;
    for (defaultsIt = defaultTransforms.begin();
         defaultsIt != defaultTransforms.end();
      ++defaultsIt)
      {
      if (std::find(names.begin(), names.end(), *defaultsIt) == names.end())
        {
        std::cout << "[FAILED] " << *defaultsIt << " not registered properly with defaults" << std::endl;
        testReturnStatus = EXIT_FAILURE;
        }
      else
        {
        std::cout << "[SUCCESS] " << *defaultsIt << " registered properly" << std::endl;
        }
      }
    }

  // test other methods
  itk::TransformFactoryBase::Pointer base = itk::TransformFactoryBase::New();
  const char* itkVersion = base->GetITKSourceVersion();
  const char* description = base->GetDescription();
  const char* type = base->GetNameOfClass();
  if (strcmp(itkVersion, ITK_SOURCE_VERSION) != 0)
    {
    std::cout << "[FAILED] Did not report version correctly" << std::endl;
    testReturnStatus = EXIT_FAILURE;
    }
  else
    {
    std::cout << "[SUCCESS] Reported version correctly as " << itkVersion << std::endl;
    }
  if (strcmp(description, "Transform FactoryBase") != 0)
    {
    std::cout << "[FAILED] Did not report description correctly" << std::endl;
    testReturnStatus = EXIT_FAILURE;
    }
  else
    {
    std::cout << "[SUCCESS] Reported description correctly as " << description << std::endl;
    }
  if (strcmp(type, "TransformFactoryBase") != 0)
    {
    std::cout << "[FAILED] Did not report type correctly" << std::endl;
    testReturnStatus = EXIT_FAILURE;
    }
  else
    {
    std::cout << "[SUCCESS] Reported type correctly as " << type << std::endl;
    }

  // return successfully
  return testReturnStatus;
}
