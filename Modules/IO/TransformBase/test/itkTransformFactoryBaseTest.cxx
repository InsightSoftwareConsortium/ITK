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
#include "itkVersion.h"
#include "itkTransformFactoryBase.h"

int itkTransformFactoryBaseTest (int, char*[])
{
  // Call register default transforms
  itk::TransformFactoryBase::RegisterDefaultTransforms();

  // create the list of default transforms
  std::list<std::string> defaultTransforms;

  // Test doubles first (in alphabetical order)

  defaultTransforms.push_back("AffineTransform_double_2_2");
  defaultTransforms.push_back("AffineTransform_double_3_3");
  defaultTransforms.push_back("AffineTransform_double_4_4");
  defaultTransforms.push_back("AffineTransform_double_5_5");
  defaultTransforms.push_back("AffineTransform_double_6_6");
  defaultTransforms.push_back("AffineTransform_double_7_7");
  defaultTransforms.push_back("AffineTransform_double_8_8");
  defaultTransforms.push_back("AffineTransform_double_9_9");

  defaultTransforms.push_back("BSplineTransform_double_2_2");
  defaultTransforms.push_back("BSplineTransform_double_3_3");
#ifdef ITKV3_COMPATIBILITY
  defaultTransforms.push_back("BSplineDeformableTransform_double_2_2");
  defaultTransforms.push_back("BSplineDeformableTransform_double_3_3");
#endif

  defaultTransforms.push_back("CenteredAffineTransform_double_2_2");
  defaultTransforms.push_back("CenteredAffineTransform_double_3_3");
  defaultTransforms.push_back("CenteredEuler3DTransform_double_3_3");
  defaultTransforms.push_back("CenteredRigid2DTransform_double_2_2");
  defaultTransforms.push_back("CenteredSimilarity2DTransform_double_2_2");

  defaultTransforms.push_back("CompositeTransform_double_2_2");
  defaultTransforms.push_back("CompositeTransform_double_3_3");
  defaultTransforms.push_back("CompositeTransform_double_4_4");
  defaultTransforms.push_back("CompositeTransform_double_5_5");
  defaultTransforms.push_back("CompositeTransform_double_6_6");
  defaultTransforms.push_back("CompositeTransform_double_7_7");
  defaultTransforms.push_back("CompositeTransform_double_8_8");
  defaultTransforms.push_back("CompositeTransform_double_9_9");

  defaultTransforms.push_back("Euler2DTransform_double_2_2");
  defaultTransforms.push_back("Euler3DTransform_double_3_3");

  defaultTransforms.push_back("FixedCenterOfRotationAffineTransform_double_3_3");

  defaultTransforms.push_back("IdentityTransform_double_2_2");
  defaultTransforms.push_back("IdentityTransform_double_3_3");
  defaultTransforms.push_back("IdentityTransform_double_4_4");
  defaultTransforms.push_back("IdentityTransform_double_5_5");
  defaultTransforms.push_back("IdentityTransform_double_6_6");
  defaultTransforms.push_back("IdentityTransform_double_7_7");
  defaultTransforms.push_back("IdentityTransform_double_8_8");
  defaultTransforms.push_back("IdentityTransform_double_9_9");

  defaultTransforms.push_back("QuaternionRigidTransform_double_3_3");

  defaultTransforms.push_back("Rigid2DTransform_double_2_2");
  defaultTransforms.push_back("Rigid3DPerspectiveTransform_double_3_2");
  defaultTransforms.push_back("Rigid3DTransform_double_3_3");

  defaultTransforms.push_back("ScalableAffineTransform_double_3_3");
  defaultTransforms.push_back("ScaleLogarithmicTransform_double_3_3");
  defaultTransforms.push_back("ScaleSkewVersor3DTransform_double_3_3");
  defaultTransforms.push_back("ScaleTransform_double_2_2");
  defaultTransforms.push_back("ScaleTransform_double_3_3");
  defaultTransforms.push_back("ScaleTransform_double_4_4");
  defaultTransforms.push_back("ScaleVersor3DTransform_double_3_3");

  defaultTransforms.push_back("Similarity2DTransform_double_2_2");
  defaultTransforms.push_back("Similarity3DTransform_double_3_3");

  defaultTransforms.push_back("TranslationTransform_double_2_2");
  defaultTransforms.push_back("TranslationTransform_double_3_3");
  defaultTransforms.push_back("TranslationTransform_double_4_4");
  defaultTransforms.push_back("TranslationTransform_double_5_5");
  defaultTransforms.push_back("TranslationTransform_double_6_6");
  defaultTransforms.push_back("TranslationTransform_double_7_7");
  defaultTransforms.push_back("TranslationTransform_double_8_8");
  defaultTransforms.push_back("TranslationTransform_double_9_9");

  defaultTransforms.push_back("VersorRigid3DTransform_double_3_3");
  defaultTransforms.push_back("VersorTransform_double_3_3");

  // Test float instances (in alphabetical order)

  defaultTransforms.push_back("AffineTransform_float_2_2");
  defaultTransforms.push_back("AffineTransform_float_3_3");
  defaultTransforms.push_back("AffineTransform_float_4_4");
  defaultTransforms.push_back("AffineTransform_float_5_5");
  defaultTransforms.push_back("AffineTransform_float_6_6");
  defaultTransforms.push_back("AffineTransform_float_7_7");
  defaultTransforms.push_back("AffineTransform_float_8_8");
  defaultTransforms.push_back("AffineTransform_float_9_9");

  defaultTransforms.push_back("BSplineTransform_float_2_2");
  defaultTransforms.push_back("BSplineTransform_float_3_3");
#ifdef ITKV3_COMPATIBILITY
  defaultTransforms.push_back("BSplineDeformableTransform_float_2_2");
  defaultTransforms.push_back("BSplineDeformableTransform_float_3_3");
#endif

  defaultTransforms.push_back("CenteredAffineTransform_float_2_2");
  defaultTransforms.push_back("CenteredAffineTransform_float_3_3");
  defaultTransforms.push_back("CenteredEuler3DTransform_float_3_3");
  defaultTransforms.push_back("CenteredRigid2DTransform_float_2_2");
  defaultTransforms.push_back("CenteredSimilarity2DTransform_float_2_2");

  defaultTransforms.push_back("CompositeTransform_float_2_2");
  defaultTransforms.push_back("CompositeTransform_float_3_3");
  defaultTransforms.push_back("CompositeTransform_float_4_4");
  defaultTransforms.push_back("CompositeTransform_float_5_5");
  defaultTransforms.push_back("CompositeTransform_float_6_6");
  defaultTransforms.push_back("CompositeTransform_float_7_7");
  defaultTransforms.push_back("CompositeTransform_float_8_8");
  defaultTransforms.push_back("CompositeTransform_float_9_9");

  defaultTransforms.push_back("Euler2DTransform_float_2_2");
  defaultTransforms.push_back("Euler3DTransform_float_3_3");

  defaultTransforms.push_back("FixedCenterOfRotationAffineTransform_float_3_3");

  defaultTransforms.push_back("IdentityTransform_float_2_2");
  defaultTransforms.push_back("IdentityTransform_float_3_3");
  defaultTransforms.push_back("IdentityTransform_float_4_4");
  defaultTransforms.push_back("IdentityTransform_float_5_5");
  defaultTransforms.push_back("IdentityTransform_float_6_6");
  defaultTransforms.push_back("IdentityTransform_float_7_7");
  defaultTransforms.push_back("IdentityTransform_float_8_8");
  defaultTransforms.push_back("IdentityTransform_float_9_9");

  defaultTransforms.push_back("QuaternionRigidTransform_float_3_3");

  defaultTransforms.push_back("Rigid2DTransform_float_2_2");
  defaultTransforms.push_back("Rigid3DPerspectiveTransform_float_3_2");
  defaultTransforms.push_back("Rigid3DTransform_float_3_3");

  defaultTransforms.push_back("ScalableAffineTransform_float_3_3");
  defaultTransforms.push_back("ScaleLogarithmicTransform_float_3_3");
  defaultTransforms.push_back("ScaleSkewVersor3DTransform_float_3_3");
  defaultTransforms.push_back("ScaleTransform_float_2_2");
  defaultTransforms.push_back("ScaleTransform_float_3_3");
  defaultTransforms.push_back("ScaleTransform_float_4_4");
  defaultTransforms.push_back("ScaleVersor3DTransform_float_3_3");

  defaultTransforms.push_back("Similarity2DTransform_float_2_2");
  defaultTransforms.push_back("Similarity3DTransform_float_3_3");

  defaultTransforms.push_back("TranslationTransform_float_2_2");
  defaultTransforms.push_back("TranslationTransform_float_3_3");
  defaultTransforms.push_back("TranslationTransform_float_4_4");
  defaultTransforms.push_back("TranslationTransform_float_5_5");
  defaultTransforms.push_back("TranslationTransform_float_6_6");
  defaultTransforms.push_back("TranslationTransform_float_7_7");
  defaultTransforms.push_back("TranslationTransform_float_8_8");
  defaultTransforms.push_back("TranslationTransform_float_9_9");

  defaultTransforms.push_back("VersorRigid3DTransform_float_3_3");
  defaultTransforms.push_back("VersorTransform_float_3_3");

  defaultTransforms.push_back("DisplacementFieldTransform_double_2_2");
  defaultTransforms.push_back("DisplacementFieldTransform_double_3_3");

  defaultTransforms.push_back("DisplacementFieldTransform_float_2_2");
  defaultTransforms.push_back("DisplacementFieldTransform_float_3_3");


  // check to make sure that all default transforms have been registered
  defaultTransforms.sort();
  // Print out the names of all the registered transforms
  std::list<std::string> names = itk::TransformFactoryBase::GetFactory()->GetClassOverrideWithNames();
  names.sort();
  std::list<std::string>::iterator namesIt;
  std::list<std::string>::iterator defaultsIt;
  for (namesIt = names.begin(), defaultsIt = defaultTransforms.begin();
       namesIt != names.end() && defaultsIt != defaultTransforms.end();
       ++namesIt, ++defaultsIt)
    {
    if (strcmp((*namesIt).c_str(), (*defaultsIt).c_str()) != 0)
      {
      std::cout << "[FAILED] " <<*namesIt<<"   "<< *defaultsIt << " not registered properly with defaults" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cout << "[SUCCESS] " << *defaultsIt << " registered properly" << std::endl;
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
    }
  else
    {
    std::cout << "[SUCCESS] Reported version correctly as " << itkVersion << std::endl;
    }
  if (strcmp(description, "Transform FactoryBase") != 0)
    {
    std::cout << "[FAILED] Did not report description correctly" << std::endl;
    }
  else
    {
    std::cout << "[SUCCESS] Reported description correctly as " << description << std::endl;
    }
  if (strcmp(type, "TransformFactoryBase") != 0)
    {
    std::cout << "[FAILED] Did not report type correctly" << std::endl;
    }
  else
    {
    std::cout << "[SUCCESS] Reported type correctly as " << type << std::endl;
    }

  // return successfully
  return EXIT_SUCCESS;

}
