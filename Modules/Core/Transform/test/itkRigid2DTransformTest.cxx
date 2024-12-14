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

#include "itkRigid2DTransform.h"
#include "itkTextOutput.h"
#include "itkTestingMacros.h"

namespace
{
bool
CheckEqual(itk::Point<double, 2> p1, itk::Point<double, 2> p2)
{
  const double epsilon = 1e-10;
  for (unsigned int i = 0; i < 2; ++i)
  {
    if (itk::Math::abs(p1[i] - p2[i]) > epsilon)
    {
      std::cout << p1 << " != " << p2 << ": FAILED" << '\n';
      return false;
    }
  }
  std::cout << p1 << " == " << p2 << ": PASSED" << '\n';
  return true;
}
} // namespace


int
itkRigid2DTransformTest(int, char *[])
{

  itk::OutputWindow::SetInstance(itk::TextOutput::New().GetPointer());

  using TransformType = itk::Rigid2DTransform<double>;

  const double           epsilon = 1e-10;
  constexpr unsigned int N = 2;


  bool Ok = true;


  /* Create a 2D identity transformation and show its parameters */
  {
    auto identityTransform = TransformType::New();
    identityTransform->SetIdentity();
    TransformType::OffsetType offset = identityTransform->GetOffset();
    std::cout << "Vector from instantiating an identity transform:  ";
    std::cout << offset << '\n';

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
      std::cerr << "Identity doesn't have a null offset" << '\n';
      return EXIT_FAILURE;
    }
  }

  /* Create a Rigid 2D transform with translation */
  {
    auto                                 translation = TransformType::New();
    TransformType::OffsetType::ValueType ioffsetInit[2] = { 1, 4 };
    TransformType::OffsetType            ioffset = ioffsetInit;

    translation->SetOffset(ioffset);

    auto translationInverse = TransformType::New();
    if (!translation->GetInverse(translationInverse))
    {
      std::cout << "Cannot create transform" << '\n';
      return EXIT_FAILURE;
    }
    std::cout << "translation: " << translation;
    std::cout << "translationInverse: " << translationInverse;

    translationInverse = dynamic_cast<TransformType *>(translation->GetInverseTransform().GetPointer());
    if (!translationInverse)
    {
      std::cout << "Cannot compute inverse" << '\n';
      return EXIT_FAILURE;
    }
    std::cout << "translation: " << translation;
    std::cout << "translationInverse: " << translationInverse;

    TransformType::OffsetType offset = translation->GetOffset();
    std::cout << "pure Translation test:  ";
    std::cout << offset << '\n';

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
      std::cerr << "Get Offset  differs from SetOffset value " << '\n';
      return EXIT_FAILURE;
    }

    {
      // Translate an itk::Point
      const TransformType::InputPointType::ValueType pInit[2] = { 10, 10 };
      const TransformType::InputPointType            p = pInit;
      TransformType::InputPointType                  q = p + ioffset;
      TransformType::OutputPointType                 r = translation->TransformPoint(p);
      for (unsigned int i = 0; i < N; ++i)
      {
        if (itk::Math::abs(q[i] - r[i]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
      if (!Ok)
      {
        std::cerr << "Error translating point: " << p << '\n';
        std::cerr << "Result should be       : " << q << '\n';
        std::cerr << "Reported Result is     : " << r << '\n';
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok translating an itk::Point " << '\n';
      }
    }

    {
      // Translate an itk::Vector
      TransformType::InputVectorType::ValueType pInit[2] = { 10, 10 };
      TransformType::InputVectorType            p = pInit;
      TransformType::OutputVectorType           q = translation->TransformVector(p);
      for (unsigned int i = 0; i < N; ++i)
      {
        if (itk::Math::abs(q[i] - p[i]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
      if (!Ok)
      {
        std::cerr << "Error translating vector: " << p << '\n';
        std::cerr << "Reported Result is      : " << q << '\n';
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok translating an itk::Vector " << '\n';
      }
    }

    {
      // Translate an itk::CovariantVector
      TransformType::InputCovariantVectorType::ValueType pInit[2] = { 10, 10 };
      TransformType::InputCovariantVectorType            p = pInit;
      TransformType::OutputCovariantVectorType           q = translation->TransformCovariantVector(p);
      for (unsigned int i = 0; i < N; ++i)
      {
        if (itk::Math::abs(q[i] - p[i]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
      if (!Ok)
      {
        std::cerr << "Error translating covariant vector: " << p << '\n';
        std::cerr << "Reported Result is      : " << q << '\n';
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok translating an itk::CovariantVector " << '\n';
      }
    }


    {
      // Translate a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 11;
      p[1] = 7;
      TransformType::OutputVnlVectorType q = translation->TransformVector(p);
      for (unsigned int i = 0; i < N; ++i)
      {
        if (itk::Math::abs(q[i] - p[i]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
      if (!Ok)
      {
        std::cerr << "Error translating vnl_vector: " << p << '\n';
        std::cerr << "Reported Result is      : " << q << '\n';
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok translating an vnl_Vector " << '\n';
      }
    }
  }

  /* Create a Rigid 2D transform with a rotation given by a Matrix */
  {
    auto                      rotation = TransformType::New();
    TransformType::MatrixType mrotation;

    mrotation.SetIdentity();

    // 15 degrees in radians
    const double angle = 15.0 * std::atan(1.0f) / 45.0;
    const double sinth = std::sin(angle);
    const double costh = std::cos(angle);

    // around the positive Z axis
    mrotation[0][0] = costh;
    mrotation[0][1] = sinth;
    mrotation[1][0] = -sinth;
    mrotation[1][1] = costh;

    rotation->SetMatrix(mrotation);

    ITK_TRY_EXPECT_NO_EXCEPTION(rotation->SetMatrix(mrotation, 1e-8));
    mrotation[0][0] += 1e-7;
    ITK_TRY_EXPECT_EXCEPTION(rotation->SetMatrix(mrotation, 1e-8));
    mrotation[0][0] -= 1e-7;

    std::cout.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
    if (!itk::Math::FloatAlmostEqual(-angle, rotation->GetRotation(), 10, epsilon))
    {
      std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
      std::cerr << "Test failed!" << '\n';
      std::cerr << "Error in GetRotation" << '\n';
      std::cerr << "Expected value " << -angle << '\n';
      std::cerr << " differs from " << rotation->GetRotation();
      std::cerr << " by more than " << epsilon << '\n';
      return EXIT_FAILURE;
    }

    TransformType::OffsetType ioffset{};

    rotation->SetOffset(ioffset);

    auto rotationInverse = TransformType::New();
    if (!rotation->GetInverse(rotationInverse))
    {
      std::cout << "Cannot create transform" << '\n';
      return EXIT_FAILURE;
    }
    std::cout << "rotation: " << rotation;
    std::cout << "rotationInverse: " << rotationInverse;

    rotationInverse = dynamic_cast<TransformType *>(rotation->GetInverseTransform().GetPointer());
    if (!rotationInverse)
    {
      std::cout << "Cannot compute inverse" << '\n';
      return EXIT_FAILURE;
    }
    std::cout << "rotation: " << rotation;
    std::cout << "rotationInverse: " << rotationInverse;


    // Verify the Offset content
    TransformType::OffsetType offset = rotation->GetOffset();
    std::cout << "pure Rotation test:  " << '\n';
    std::cout << "Offset = " << offset << '\n';

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
      std::cerr << "Get Offset  differs from SetOffset value " << '\n';
      return EXIT_FAILURE;
    }

    // Verify the Matrix content
    TransformType::MatrixType matrix = rotation->GetMatrix();
    std::cout << "Rotation matrix:  " << '\n';
    std::cout << matrix << '\n';

    for (unsigned int i = 0; i < N; ++i)
    {
      for (unsigned int j = 0; j < N; ++j)
      {
        if (itk::Math::abs(matrix[i][j] - mrotation[i][j]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
    }
    if (!Ok)
    {
      std::cerr << "Get Rotation Matrix  differs " << '\n';
      std::cerr << "from SetMatrix value " << '\n';
      return EXIT_FAILURE;
    }

    {
      // Rotate an itk::Point
      const TransformType::InputPointType::ValueType pInit[2] = { 10, 10 };
      TransformType::InputPointType                  p = pInit;
      TransformType::InputPointType                  q;

      q[0] = p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;

      TransformType::OutputPointType r = rotation->TransformPoint(p);
      for (unsigned int i = 0; i < N; ++i)
      {
        if (itk::Math::abs(q[i] - r[i]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
      if (!Ok)
      {
        std::cerr << "Error rotating point   : " << p << '\n';
        std::cerr << "Result should be       : " << q << '\n';
        std::cerr << "Reported Result is     : " << r << '\n';
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok translating an itk::Point " << '\n';
      }
    }

    {
      // Rotate an itk::Vector
      TransformType::InputVectorType::ValueType pInit[2] = { 10, 10 };
      TransformType::InputVectorType            p = pInit;

      TransformType::InputPointType q;
      q[0] = p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;

      TransformType::OutputVectorType r = rotation->TransformVector(p);
      for (unsigned int i = 0; i < N; ++i)
      {
        if (itk::Math::abs(q[i] - r[i]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
      if (!Ok)
      {
        std::cerr << "Error rotating vector  : " << p << '\n';
        std::cerr << "Result should be       : " << q << '\n';
        std::cerr << "Reported Result is     : " << r << '\n';
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an itk::Vector " << '\n';
      }
    }

    {
      // Rotate an itk::CovariantVector
      TransformType::InputCovariantVectorType::ValueType pInit[2] = { 10, 10 };
      TransformType::InputCovariantVectorType            p = pInit;
      TransformType::OutputCovariantVectorType           q;

      q[0] = p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;

      TransformType::OutputCovariantVectorType r = rotation->TransformCovariantVector(p);

      for (unsigned int i = 0; i < N; ++i)
      {
        if (itk::Math::abs(q[i] - r[i]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
      if (!Ok)
      {
        std::cerr << "Error Rotating covariant vector: " << p << '\n';
        std::cerr << "Result should be               : " << q << '\n';
        std::cerr << "Reported Result is             : " << r << '\n';
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok translating an itk::CovariantVector " << '\n';
      }
    }


    {
      // Translate a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 11;
      p[1] = 7;

      TransformType::OutputVnlVectorType q;

      q[0] = p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;


      TransformType::OutputVnlVectorType r = rotation->TransformVector(p);
      for (unsigned int i = 0; i < N; ++i)
      {
        if (itk::Math::abs(q[i] - r[i]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
      if (!Ok)
      {
        std::cerr << "Error translating vnl_vector : " << p << '\n';
        std::cerr << "Result should be             : " << q << '\n';
        std::cerr << "Reported Result is           : " << r << '\n';
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok translating an vnl_Vector " << '\n';
      }
    }

    {
      // Test instantiation, inverse computation, back transform etc.
      auto t1 = TransformType::New();

      // Set parameters
      double                        angle0 = -21.0 / 180.0 * itk::Math::pi;
      TransformType::InputPointType center;
      center[0] = 12.0;
      center[1] = -8.9;
      TransformType::OutputVectorType translation;
      translation[0] = 67.8;
      translation[1] = -0.2;

      t1->SetAngle(angle0);
      t1->SetCenter(center);
      t1->SetTranslation(translation);

      TransformType::InputPointType p1;
      p1[0] = 96.8;
      p1[1] = -3.2;

      TransformType::InputPointType p2;
      p2 = t1->TransformPoint(p1);

      // Test inverse
      TransformType::Pointer t2;
      t1->CloneInverseTo(t2);

      const TransformType::InputPointType p3 = t2->TransformPoint(p2);

      std::cout << "Test CloneInverseTo(): ";
      if (!CheckEqual(p1, p3))
      {
        return EXIT_FAILURE;
      }

      auto t2dash = TransformType::New();
      t1->GetInverse(t2dash);
      TransformType::InputPointType p3dash = t2dash->TransformPoint(p2);

      std::cout << "Test GetInverse(): ";
      if (!CheckEqual(p1, p3dash))
      {
        return EXIT_FAILURE;
      }

      t2dash = dynamic_cast<TransformType *>(t1->GetInverseTransform().GetPointer());
      if (!t2dash)
      {
        std::cout << "Cannot compute inverse transformation" << '\n';
        return EXIT_FAILURE;
      }
      p3dash = t2dash->TransformPoint(p2);

      std::cout << "Test GetInverseTransform(): ";
      if (!CheckEqual(p1, p3dash))
      {
        return EXIT_FAILURE;
      }


      // Test clone
      TransformType::Pointer t3;
      t1->CloneTo(t3);

      const TransformType::InputPointType p4 = t3->TransformPoint(p1);

      std::cout << "Test Clone(): ";
      if (!CheckEqual(p2, p4))
      {
        return EXIT_FAILURE;
      }

      // Test compose
      auto t4 = TransformType::New();

      angle0 = 14.7 / 180.0 * itk::Math::pi;
      center.Fill(4.0);
      translation.Fill(67.1);
      t4->SetAngle(angle0);
      t4->SetCenter(center);
      t4->SetTranslation(translation);

      TransformType::Pointer t5;
      t1->CloneTo(t5);
      t5->Compose(t4, false);

      TransformType::InputPointType p5 = t1->TransformPoint(p1);
      TransformType::InputPointType p6 = t4->TransformPoint(p5);
      TransformType::InputPointType p7 = t5->TransformPoint(p1);

      std::cout << "Test Compose(.,false): ";
      if (!CheckEqual(p6, p7))
      {
        return EXIT_FAILURE;
      }

      t1->CloneTo(t5);
      t5->Compose(t4, true);

      p5 = t4->TransformPoint(p1);
      p6 = t1->TransformPoint(p5);
      p7 = t5->TransformPoint(p1);

      std::cout << "Test Compose(.,true): ";
      if (!CheckEqual(p6, p7))
      {
        return EXIT_FAILURE;
      }
    }
  }


  return EXIT_SUCCESS;
}
