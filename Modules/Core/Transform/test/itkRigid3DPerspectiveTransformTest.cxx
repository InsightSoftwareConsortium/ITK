/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>

#include "itkRigid3DPerspectiveTransform.h"
#include "itkTestingMacros.h"

int
itkRigid3DPerspectiveTransformTest(int, char *[])
{


  using TransformType = itk::Rigid3DPerspectiveTransform<double>;

  constexpr double       epsilon = 1e-10;
  constexpr unsigned int N = 3;

  constexpr double focal = 100.0;

  bool Ok = true;

  // Test exceptions
  {
    auto transform = TransformType::New();

    const typename TransformType::InputVectorType vector = itk::MakeVector(1.0, 4.0, 9.0);
    ITK_TRY_EXPECT_EXCEPTION(transform->TransformVector(vector));

    typename TransformType::InputVnlVectorType vnlVector;
    vnlVector.fill(1.0);
    ITK_TRY_EXPECT_EXCEPTION(transform->TransformVector(vnlVector));

    auto covVector = itk::MakeFilled<typename TransformType::InputCovariantVectorType>(1.0);
    ITK_TRY_EXPECT_EXCEPTION(transform->TransformCovariantVector(covVector));

    auto                                         point = itk::MakeFilled<typename TransformType::InputPointType>(1.0);
    typename TransformType::JacobianPositionType jacobianPosition;
    ITK_TRY_EXPECT_EXCEPTION(transform->ComputeJacobianWithRespectToPosition(point, jacobianPosition));
  }

  // Exercise basic object methods and test Set/Get macros
  {
    auto transform = TransformType::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(transform, Rigid3DPerspectiveTransform, Transform);


    constexpr typename TransformType::OffsetType fixedOffset{};

    transform->SetFixedOffset(fixedOffset);
    ITK_TEST_SET_GET_VALUE(fixedOffset, transform->GetFixedOffset());

    constexpr typename TransformType::InputPointType centerOfRotation{};
    transform->SetCenterOfRotation(centerOfRotation);
    ITK_TEST_SET_GET_VALUE(centerOfRotation, transform->GetCenterOfRotation());

    // This transform has no fixed parameters; empty method body; called for coverage purposes
    const typename TransformType::FixedParametersType fixedParameters{};
    transform->SetFixedParameters(fixedParameters);
  }

  /* Create a 3D identity transformation and show its parameters */
  {
    auto identityTransform = TransformType::New();
    identityTransform->SetFocalDistance(focal);

    TransformType::OffsetType offset = identityTransform->GetOffset();
    std::cout << "Vector from instantiating an identity transform:  ";
    std::cout << offset << std::endl;

    for (unsigned int i = 0; i < N; ++i)
    {
      if (itk::Math::abs(offset[i] - 0.0) > epsilon)
      {
        Ok = false;
        break;
      }
    }
    if (!Ok)
    {
      std::cerr << "Identity doesn't have a null offset" << std::endl;
      return EXIT_FAILURE;
    }
  }

  /* Create a Rigid 3D transform with translation */
  {
    auto translation = TransformType::New();
    translation->SetFocalDistance(focal);

    TransformType::OffsetType ioffset{};

    translation->SetOffset(ioffset);
    ITK_TEST_SET_GET_VALUE(ioffset, translation->GetOffset());

    TransformType::OffsetType offset = translation->GetOffset();
    std::cout << "pure Translation test:  ";
    std::cout << offset << std::endl;

    for (unsigned int i = 0; i < N; ++i)
    {
      if (itk::Math::abs(offset[i] - ioffset[i]) > epsilon)
      {
        Ok = false;
        break;
      }
    }
    if (!Ok)
    {
      std::cerr << "Get Offset  differs from SetOffset value " << std::endl;
      return EXIT_FAILURE;
    }

    {
      // Projecting  an itk::Point
      auto                           p = itk::MakeFilled<TransformType::InputPointType>(10);
      TransformType::InputPointType  q = p + ioffset;
      TransformType::OutputPointType s;
      const double                   factor = focal / q[2];
      s[0] = q[0] * factor;
      s[1] = q[1] * factor;
      TransformType::OutputPointType r = translation->TransformPoint(p);
      for (unsigned int i = 0; i < N - 1; ++i)
      {
        if (itk::Math::abs(s[i] - r[i]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
      if (!Ok)
      {
        std::cerr << "Error translating point: " << p << std::endl;
        std::cerr << "Result should be       : " << s << std::endl;
        std::cerr << "Reported Result is     : " << r << std::endl;
        return EXIT_FAILURE;
      }

      std::cout << "Ok translating an itk::Point " << std::endl;
    }
  }

  /* Create a Rigid 3D transform with a rotation */
  {
    auto rigid = TransformType::New();
    rigid->SetFocalDistance(focal);

    constexpr TransformType::OffsetType ioffset{};

    rigid->SetOffset(ioffset);

    const TransformType::OffsetType offset = rigid->GetOffset();
    std::cout << "pure Translation test:  ";
    std::cout << offset << std::endl;

    using VersorType = TransformType::VersorType;
    VersorType                  rotation;
    VersorType::VectorType      axis;
    const VersorType::ValueType angle = 30.0f * std::atan(1.0f) / 45.0f;
    axis[0] = 1.0f;
    axis[1] = 1.0f;
    axis[2] = 1.0f;

    rotation.Set(axis, angle);
    rigid->SetRotation(rotation);
    ITK_TEST_SET_GET_VALUE(rotation, rigid->GetRotation());

    {
      // Project an itk::Point
      auto                           p = itk::MakeFilled<TransformType::InputPointType>(10.0);
      TransformType::InputPointType  q = p + ioffset;
      TransformType::OutputPointType s;
      const double                   factor = focal / q[2];
      s[0] = q[0] * factor;
      s[1] = q[1] * factor;
      TransformType::OutputPointType r = rigid->TransformPoint(p);
      for (unsigned int i = 0; i < N - 1; ++i)
      {
        if (itk::Math::abs(s[i] - r[i]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
      if (!Ok)
      {
        std::cerr << "Error rotating point: " << p << std::endl;
        std::cerr << "Result should be       : " << s << std::endl;
        std::cerr << "Reported Result is     : " << r << std::endl;
        return EXIT_FAILURE;
      }

      std::cout << "Ok rotating an itk::Point " << std::endl;
    }
  }


  std::cout << "Test successful" << std::endl;
  return EXIT_SUCCESS;
}
