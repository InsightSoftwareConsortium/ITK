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

#include "itkRigid3DTransform.h"
#include "itkTestingMacros.h"

namespace itk
{
template <typename TScalar>
class Rigid3DTransformSurrogate : public Rigid3DTransform<TScalar>
{
public:
  /** Standard class type aliases. */
  using Self = Rigid3DTransformSurrogate;
  using Superclass = Rigid3DTransform<TScalar>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);

  /** Base inverse transform type. This type should not be changed to the
   * concrete inverse transform type or inheritance would be lost. */
  using InverseTransformBaseType = typename Superclass::InverseTransformBaseType;
  using InverseTransformBasePointer = typename InverseTransformBaseType::Pointer;

  InverseTransformBasePointer
  GetInverseTransform() const override
  {
    itkExceptionMacro("This is never called.");
  }

private:
  Rigid3DTransformSurrogate() = default;
  ~Rigid3DTransformSurrogate() override = default;
};

} // namespace itk

bool
TestSettingTranslation()
{

  itk::Matrix<double, 3, 3> R;
  R.SetIdentity();
  constexpr double alpha = itk::Math::pi / 180.0;
  R[0][0] = std::cos(alpha);
  R[0][1] = std::sin(alpha);
  R[1][0] = -1.0 * std::sin(alpha);
  R[1][1] = std::cos(alpha);

  itk::Vector<double, 3> T;
  T[0] = 100;
  T[1] = 200;
  T[2] = 300;

  using TransformType = itk::Rigid3DTransformSurrogate<double>;

  auto r1 = TransformType::New();
  // r1->SetIdentity();
  r1->SetMatrix(R);
  r1->Translate(T);

  TransformType::ParametersType p1;
  p1.set_size(12);
  p1 = r1->GetParameters();

  auto                          r2 = TransformType::New();
  TransformType::ParametersType p2;

  p2.set_size(12);
  for (int r = 0; r < 3; ++r)
  {
    for (int c = 0; c < 3; ++c)
    {
      p2[r * 3 + c] = R[r][c];
    }
  }
  p2[9] = T[0];
  p2[10] = T[1];
  p2[11] = T[2];
  r2->SetParameters(p2);
  auto r3 = TransformType::New();
  r3->SetFixedParameters(r1->GetFixedParameters());
  r3->SetParameters(r1->GetParameters());

  TransformType::ParametersType p3;
  p3.set_size(12);
  p3 = r3->GetParameters();
  if ((p1 == p2) && (p1 == p3))
  {
    return true;
  }

  std::cout << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << std::endl;
  std::cout << "r1\n" << r1 << std::endl;
  std::cout << "r2\n" << r2 << std::endl;
  std::cout << "r3\n" << r3 << std::endl;
  std::cout << p1 << '\n' << p2 << '\n' << p3 << std::endl;
  std::cout << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << std::endl;


  return false;
}

int
itkRigid3DTransformTest(int, char *[])
{

  using TransformType = itk::Rigid3DTransformSurrogate<double>;
  using ParametersType = TransformType::ParametersType;

  constexpr double       epsilon = 1e-10;
  constexpr unsigned int N = 3;

  bool Ok = true;

  /* Create a 3D identity transformation and show its parameters */
  {
    auto                      identityTransform = TransformType::New();
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
    auto                                 translation = TransformType::New();
    TransformType::OffsetType::ValueType ioffsetInit[3] = { 1, 4, 9 };
    TransformType::OffsetType            ioffset = ioffsetInit;

    translation->SetOffset(ioffset);

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
      // Translate an itk::Point
      constexpr TransformType::InputPointType::ValueType pInit[3] = { 10, 10, 10 };
      const TransformType::InputPointType                p = pInit;
      TransformType::InputPointType                      q;
      q = p + ioffset;
      TransformType::OutputPointType r;
      r = translation->TransformPoint(p);
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
        std::cerr << "Error translating point: " << p << std::endl;
        std::cerr << "Result should be       : " << q << std::endl;
        std::cerr << "Reported Result is     : " << r << std::endl;
        return EXIT_FAILURE;
      }

      std::cout << "Ok translating an itk::Point " << std::endl;
    }

    {
      // Translate an itk::Vector
      TransformType::InputVectorType::ValueType pInit[3] = { 10, 10, 10 };
      TransformType::InputVectorType            p = pInit;
      TransformType::OutputVectorType           q;
      q = translation->TransformVector(p);
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
        std::cerr << "Error translating vector: " << p << std::endl;
        std::cerr << "Reported Result is      : " << q << std::endl;
        return EXIT_FAILURE;
      }

      std::cout << "Ok translating an itk::Vector " << std::endl;
    }

    {
      // Translate an itk::CovariantVector
      TransformType::InputCovariantVectorType::ValueType pInit[3] = { 10, 10, 10 };
      TransformType::InputCovariantVectorType            p = pInit;
      TransformType::OutputCovariantVectorType           q;
      q = translation->TransformCovariantVector(p);
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
        std::cerr << "Error translating covariant vector: " << p << std::endl;
        std::cerr << "Reported Result is      : " << q << std::endl;
        return EXIT_FAILURE;
      }

      std::cout << "Ok translating an itk::CovariantVector " << std::endl;
    }

    {
      // Translate a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 11;
      p[1] = 7;
      p[2] = 15;
      TransformType::OutputVnlVectorType q;
      q = translation->TransformVector(p);
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
        std::cerr << "Error translating vnl_vector: " << p << std::endl;
        std::cerr << "Reported Result is      : " << q << std::endl;
        return EXIT_FAILURE;
      }

      std::cout << "Ok translating an vnl_Vector " << std::endl;
    }
  }

  /* Create a Rigid 3D transform with a rotation given by a Matrix */
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

    TransformType::OffsetType ioffset{};

    rotation->SetOffset(ioffset);

    // Verify the Offset content
    TransformType::OffsetType offset = rotation->GetOffset();
    std::cout << "pure Rotation test:  " << std::endl;
    std::cout << "Offset = " << offset << std::endl;

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

    // Verify the Matrix content
    TransformType::MatrixType matrix0 = rotation->GetMatrix();
    std::cout << "Rotation matrix:  " << std::endl;
    std::cout << matrix0 << std::endl;

    for (unsigned int i = 0; i < N; ++i)
    {
      for (unsigned int j = 0; j < N; ++j)
      {
        if (itk::Math::abs(matrix0[i][j] - mrotation[i][j]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
    }
    if (!Ok)
    {
      std::cerr << "Get Rotation Matrix  differs " << std::endl;
      std::cerr << "from SetMatrix value " << std::endl;
      return EXIT_FAILURE;
    }

    {
      // Rotate an itk::Point
      constexpr TransformType::InputPointType::ValueType pInit[3] = { 10, 10, 10 };
      TransformType::InputPointType                      p = pInit;
      TransformType::InputPointType                      q;

      q[0] = p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;
      q[2] = p[2];

      TransformType::OutputPointType r;
      r = rotation->TransformPoint(p);
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
        std::cerr << "Error rotating point   : " << p << std::endl;
        std::cerr << "Result should be       : " << q << std::endl;
        std::cerr << "Reported Result is     : " << r << std::endl;
        return EXIT_FAILURE;
      }

      std::cout << "Ok translating an itk::Point " << std::endl;
    }

    {
      // Rotate an itk::Vector
      TransformType::InputVectorType::ValueType pInit[3] = { 10, 10, 10 };
      TransformType::InputVectorType            p = pInit;

      TransformType::InputPointType q;
      q[0] = p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;
      q[2] = p[2];

      TransformType::OutputVectorType r;
      r = rotation->TransformVector(p);
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
        std::cerr << "Error rotating vector  : " << p << std::endl;
        std::cerr << "Result should be       : " << q << std::endl;
        std::cerr << "Reported Result is     : " << r << std::endl;
        return EXIT_FAILURE;
      }

      std::cout << "Ok rotating an itk::Vector " << std::endl;
    }

    {
      // Rotate an itk::CovariantVector
      TransformType::InputCovariantVectorType::ValueType pInit[3] = { 10, 10, 10 };
      TransformType::InputCovariantVectorType            p = pInit;
      TransformType::OutputCovariantVectorType           q;

      q[0] = p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;
      q[2] = p[2];

      TransformType::OutputCovariantVectorType r;
      r = rotation->TransformCovariantVector(p);

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
        std::cerr << "Error Rotating covariant vector: " << p << std::endl;
        std::cerr << "Result should be               : " << q << std::endl;
        std::cerr << "Reported Result is             : " << r << std::endl;
        return EXIT_FAILURE;
      }

      std::cout << "Ok translating an itk::CovariantVector " << std::endl;
    }

    {
      // Translate a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 11;
      p[1] = 7;
      p[2] = 15;

      TransformType::OutputVnlVectorType q;

      q[0] = p[0] * costh + p[1] * sinth;
      q[1] = -p[0] * sinth + p[1] * costh;
      q[2] = p[2];

      TransformType::OutputVnlVectorType r;
      r = rotation->TransformVector(p);
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
        std::cerr << "Error translating vnl_vector : " << p << std::endl;
        std::cerr << "Result should be             : " << q << std::endl;
        std::cerr << "Reported Result is           : " << r << std::endl;
        return EXIT_FAILURE;
      }

      std::cout << "Ok translating an vnl_Vector " << std::endl;
    }

    {
      // Testing SetParameters()
      std::cout << "Testing SetParameters() ... ";

      auto           t = TransformType::New();
      ParametersType p(t->GetNumberOfParameters());

      // attempt to set an non-orthogonal matrix
      for (unsigned int j = 0; j < t->GetNumberOfParameters(); ++j)
      {
        p[j] = static_cast<double>(j + 1);
        p[j] = itk::Math::sqr(p[j]);
      }

      Ok = false;
      try
      {
        t->SetParameters(p);
      }
      catch (const itk::ExceptionObject & itkNotUsed(err))
      {
        Ok = true;
      }
      catch (...)
      {
        std::cout << "Caught unknown exception" << std::endl;
      }

      if (!Ok)
      {
        std::cerr << "Error: expected to catch an exception when attempting";
        std::cerr << " to set an non-orthogonal matrix." << std::endl;
        return EXIT_FAILURE;
      }

      // attempt to set an orthogonal matrix
      using MatrixType = TransformType::MatrixType;

      MatrixType matrix;
      matrix.GetVnlMatrix().set_identity();

      constexpr double a = 1.0 / 180.0 * itk::Math::pi;
      matrix[0][0] = std::cos(a);
      matrix[0][1] = std::sin(a);
      matrix[1][0] = -1.0 * std::sin(a);
      matrix[1][1] = std::cos(a);

      unsigned int par = 0;
      for (unsigned int row = 0; row < 3; ++row)
      {
        for (unsigned int col = 0; col < 3; ++col)
        {
          p[par] = matrix[row][col];
          ++par;
        }
      }

      Ok = true;
      try
      {
        t->SetParameters(p);
      }
      catch (const itk::ExceptionObject & err)
      {
        std::cout << err << std::endl;
        Ok = false;
      }
      catch (...)
      {
        std::cout << "Caught unknown exception" << std::endl;
        Ok = false;
      }

      if (!Ok)
      {
        std::cerr << "Error: caught unexpected exception" << std::endl;
        return EXIT_FAILURE;
      }

      std::cout << "done." << std::endl;
    }

    {
      // Testing SetIdentity()
      std::cout << "Testing SetIdentity() ... ";

      auto           t = TransformType::New();
      ParametersType p(t->GetNumberOfParameters());

      t->SetIdentity();
      p = t->GetParameters();

      // check if all elements is the expected value to within tolerance
      ParametersType pIdeal(t->GetNumberOfParameters());
      pIdeal.Fill(0.0);
      pIdeal[0] = 1.0;
      pIdeal[4] = 1.0;
      pIdeal[8] = 1.0;

      Ok = true;
      for (unsigned int par = 0; par < t->GetNumberOfParameters(); ++par)
      {
        if (itk::Math::abs(p[par] - pIdeal[par]) > epsilon)
        {
          std::cerr << "Expected parameters: " << pIdeal << std::endl;
          std::cerr << "Actual parameters: " << p << std::endl;
          Ok = false;
          break;
        }
      }

      if (!Ok)
      {
        std::cerr << "Test failed." << std::endl;
        return EXIT_FAILURE;
      }

      std::cout << "done. " << std::endl;
    }

    {
      // Testing SetMatrix()
      std::cout << "Testing SetMatrix() ... ";


      using MatrixType = TransformType::MatrixType;
      MatrixType matrix;

      auto t = TransformType::New();

      // attempt to set an non-orthogonal matrix
      unsigned int par = 0;
      for (unsigned int row = 0; row < 3; ++row)
      {
        for (unsigned int col = 0; col < 3; ++col)
        {
          matrix[row][col] = static_cast<double>(par + 1);
          ++par;
        }
      }

      Ok = false;
      try
      {
        t->SetMatrix(matrix);
      }
      catch (const itk::ExceptionObject & itkNotUsed(err))
      {
        Ok = true;
      }
      catch (...)
      {
        std::cout << "Caught unknown exception" << std::endl;
      }

      if (!Ok)
      {
        std::cerr << "Error: expected to catch an exception when attempting";
        std::cerr << " to set an non-orthogonal matrix." << std::endl;
        return EXIT_FAILURE;
      }

      // attempt to set a non-orthogonal matrix but within tolerance
      Ok = true;
      try
      {
        t->SetMatrix(matrix, 1000);
      }
      catch (const itk::ExceptionObject & err)
      {
        std::cout << err << std::endl;
        Ok = false;
      }
      catch (...)
      {
        std::cout << "Caught unknown exception" << std::endl;
        Ok = false;
      }

      if (!Ok)
      {
        std::cerr << "Error: caught unexpected exception" << std::endl;
        return EXIT_FAILURE;
      }

      // attempt to set an orthogonal matrix
      matrix.GetVnlMatrix().set_identity();

      constexpr double a = 1.0 / 180.0 * itk::Math::pi;
      matrix[0][0] = std::cos(a);
      matrix[0][1] = std::sin(a);
      matrix[1][0] = -1.0 * std::sin(a);
      matrix[1][1] = std::cos(a);

      Ok = true;
      try
      {
        t->SetMatrix(matrix);
      }
      catch (const itk::ExceptionObject & err)
      {
        std::cout << err << std::endl;
        Ok = false;
      }
      catch (...)
      {
        std::cout << "Caught unknown exception" << std::endl;
        Ok = false;
      }

      if (!Ok)
      {
        std::cerr << "Error: caught unexpected exception" << std::endl;
        return EXIT_FAILURE;
      }
      const bool TranslationSettingOK = TestSettingTranslation();
      if (!TranslationSettingOK)
      {
        std::cerr << "Error:  SetTranslation() did not result in consistent internal state for Rigid3DTransform."
                  << std::endl;
        return EXIT_FAILURE;
      }

      std::cout << "done." << std::endl;
    }
  }

  // Test inverse of transform
  {
    auto transform = TransformType::New();

    // An identity transform should have an inverse
    transform->SetIdentity();
    auto inverse = TransformType::New();
    auto hasInverse = transform->GetInverse(inverse);
    if (!hasInverse)
    {
      std::cerr << "Transform does not have an inverse when expected:" << std::endl;
      std::cerr << "Transform:" << std::endl << transform;
      std::cerr << "Inverse:" << std::endl << inverse;
      return EXIT_FAILURE;
    }
    TransformType::MatrixType expectedInverse{};
    expectedInverse.SetIdentity();
    ITK_TEST_EXPECT_EQUAL(inverse->GetMatrix(), expectedInverse);

    // An orthogonal transform should have an inverse: use a rotation matrix
    transform->SetIdentity();
    // 15 degrees in radians
    const double angle = 15.0 * std::atan(1.0f) / 45.0;
    const double sinth = std::sin(angle);
    const double costh = std::cos(angle);

    // Around the positive Z axis
    TransformType::MatrixType mrotation;
    mrotation.SetIdentity();
    mrotation[0][0] = costh;
    mrotation[0][1] = sinth;
    mrotation[1][0] = -sinth;
    mrotation[1][1] = costh;
    transform->SetMatrix(mrotation);

    constexpr TransformType::OffsetType ioffset{};
    transform->SetOffset(ioffset);

    hasInverse = transform->GetInverse(inverse);
    if (!hasInverse)
    {
      std::cerr << "Transform does not have an inverse when expected:" << std::endl;
      std::cerr << "Transform:" << std::endl << transform;
      std::cerr << "Inverse:" << std::endl << inverse;
      return EXIT_FAILURE;
    }
    expectedInverse.SetIdentity();
    expectedInverse[0][0] = costh;
    expectedInverse[0][1] = -sinth;
    expectedInverse[1][0] = sinth;
    expectedInverse[1][1] = costh;
    ITK_TEST_EXPECT_EQUAL(inverse->GetMatrix(), expectedInverse);

    // Cannot test a singular matrix (i.e. not having an inverse) since ITK does not allow to set a
    // non-orthogonal rotation matrix to the transform
  }

  return EXIT_SUCCESS;
}
