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

#include "itkQuaternionRigidTransform.h"

int
itkQuaternionRigidTransformTest(int, char *[])
{

  using CoordinateType = double;
  using TransformType = itk::QuaternionRigidTransform<CoordinateType>;

  const double           epsilon = 1e-10;
  constexpr unsigned int N = 3;

  bool Ok = true;

  /* Create a 3D identity transformation and show its parameters */
  {
    auto                      identityTransform = TransformType::New();
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

  /* Create a Rigid 3D transform with translation */
  {
    auto                            translation = TransformType::New();
    TransformType::OutputVectorType itransVector;
    itransVector[0] = 1;
    itransVector[1] = 4;
    itransVector[2] = 9;

    translation->SetTranslation(itransVector);

    std::cout << "translation: " << translation;

    TransformType::OutputVectorType translationVector = translation->GetTranslation();
    std::cout << "pure Translation test:  ";
    std::cout << translationVector << '\n';
    for (unsigned int i = 0; i < N; ++i)
    {
      if (itk::Math::abs(translationVector[i] - itransVector[i]) > epsilon)
      {
        Ok = false;
        break;
      }
    }
    if (!Ok)
    {
      std::cerr << "GetTranslation differs from SetTranslation value " << '\n';
      return EXIT_FAILURE;
    }

    {
      // Translate an itk::Point
      const TransformType::InputPointType::ValueType pInit[3] = { 10, 10, 10 };
      const TransformType::InputPointType            p = pInit;
      TransformType::InputPointType                  q;
      q = p + itransVector;
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

  /* Create a Rigid 3D transform with a rotation given by a Matrix */
  {
    auto                             rotation = TransformType::New();
    TransformType::VnlQuaternionType qrotation;

    // 15 degrees in radians
    const double angle = 15.0 * std::atan(1.0f) / 45.0;
    const double sinth2 = std::sin(angle / 2.0);
    const double costh2 = std::cos(angle / 2.0);

    const double sinth = std::sin(angle);
    const double costh = std::cos(angle);

    // around the positive Z axis
    qrotation[0] = 0.0;
    qrotation[1] = 0.0;
    qrotation[2] = sinth2;
    qrotation[3] = costh2;

    rotation->SetIdentity();
    rotation->SetRotation(qrotation);

    TransformType::OffsetType ioffset{};

    // rotation->ComputeOffset();

    std::cout << "rotation: " << rotation;

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

    // VNL uses transposed matrices.
    vnl_matrix_fixed<double, 3, 3> mrotation = qrotation.rotation_matrix_transpose();

    // Verify the Matrix content
    TransformType::MatrixType matrix = rotation->GetMatrix();
    std::cout << "Rotation matrix:  " << '\n';
    std::cout << matrix << '\n';
    for (unsigned int i = 0; i < N; ++i)
    {
      for (unsigned int j = 0; j < N; ++j)
      {
        if (itk::Math::abs(matrix[i][j] - mrotation[j][i]) > epsilon)
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
      const TransformType::InputPointType::ValueType pInit[3] = { 10, 10, 10 };
      TransformType::InputPointType                  p = pInit;
      TransformType::InputPointType                  q;

      q[0] = p[0] * costh - p[1] * sinth;
      q[1] = p[0] * sinth + p[1] * costh;
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
      TransformType::InputVectorType::ValueType pInit[3] = { 10, 10, 10 };
      TransformType::InputVectorType            p = pInit;

      TransformType::InputPointType q;
      q[0] = p[0] * costh - p[1] * sinth;
      q[1] = p[0] * sinth + p[1] * costh;
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
      TransformType::InputCovariantVectorType::ValueType pInit[3] = { 10, 10, 10 };
      TransformType::InputCovariantVectorType            p = pInit;
      TransformType::OutputCovariantVectorType           q;

      q[0] = p[0] * costh - p[1] * sinth;
      q[1] = p[0] * sinth + p[1] * costh;
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
      p[2] = 15;

      TransformType::OutputVnlVectorType q;

      q[0] = p[0] * costh - p[1] * sinth;
      q[1] = p[0] * sinth + p[1] * costh;
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
  }

  {
    // Test the Jacobian
    std::cout << "Testing ComputeJacobianWithRespectToParameters()" << '\n';

    auto                          quaternionRigid = TransformType::New();
    TransformType::ParametersType parameters(quaternionRigid->GetNumberOfParameters());

    parameters.Fill(0.0);

    const double angle = 0.62 / 180.0 * itk::Math::pi;

    parameters[0] = 2.0 * std::sin(0.5 * angle);
    parameters[1] = 5.0 * std::sin(0.5 * angle);
    parameters[2] = -4.0 * std::sin(0.5 * angle);
    parameters[3] = std::cos(0.5 * angle);
    parameters[4] = 6.0;
    parameters[5] = 8.0;
    parameters[6] = 10.0;

    quaternionRigid->SetParameters(parameters);

    TransformType::InputPointType pInit;
    pInit[0] = 1.0;
    pInit[1] = 1.5;
    pInit[2] = 2.6;

    TransformType::JacobianType jacobian;
    quaternionRigid->ComputeJacobianWithRespectToParameters(pInit, jacobian);
    std::cout << jacobian << '\n';

    TransformType::JacobianType approxJacobian = jacobian;
    for (unsigned int k = 0; k < quaternionRigid->GetNumberOfParameters(); ++k)
    {
      constexpr double              delta = 0.001;
      TransformType::ParametersType plusParameters;
      TransformType::ParametersType minusParameters;

      plusParameters = parameters;
      minusParameters = parameters;
      plusParameters[k] += delta;
      minusParameters[k] -= delta;

      TransformType::OutputPointType plusPoint;
      TransformType::OutputPointType minusPoint;

      quaternionRigid->SetParameters(plusParameters);
      plusPoint = quaternionRigid->TransformPoint(pInit);
      quaternionRigid->SetParameters(minusParameters);
      minusPoint = quaternionRigid->TransformPoint(pInit);
      for (unsigned int j = 0; j < 3; ++j)
      {
        const double approxDerivative = (plusPoint[j] - minusPoint[j]) / (2.0 * delta);
        const double computedDerivative = jacobian[j][k];
        approxJacobian[j][k] = approxDerivative;
        if (itk::Math::abs(approxDerivative - computedDerivative) > 1e-5)
        {
          std::cerr << "Error computing Jacobian [" << j << "][" << k << ']' << '\n';
          std::cerr << "Result should be: " << approxDerivative << '\n';
          std::cerr << "Reported result is: " << computedDerivative << '\n';
          std::cerr << " [ FAILED ] " << '\n';
          return EXIT_FAILURE;
        } // if
      } // for j

    } // for k

    std::cout << approxJacobian << '\n';
    std::cout << " [ PASSED ] " << '\n';

    // Testing inverse transform
    std::cout << "Testing BackTransform()" << '\n';
    TransformType::OutputPointType pOut;
    quaternionRigid->SetParameters(parameters);
    {
      auto       inverseQuaternionRigid = TransformType::New();
      const bool inverseIsValid = quaternionRigid->GetInverse(inverseQuaternionRigid);
      if (!inverseIsValid)
      {
        std::cerr << "Error computing inverse transform" << '\n';
        std::cerr << " [ FAILED ] " << '\n';
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << " [ PASSED ] compute inverse transform" << '\n';
      }
      pOut = inverseQuaternionRigid->TransformPoint(quaternionRigid->TransformPoint(pInit));
    }
    // pOut should equate pInit
    for (unsigned int j = 0; j < 3; ++j)
    {
      if (itk::Math::abs(pOut[j] - pInit[j]) > 1e-5)
      {
        std::cerr << "Error computing back transform" << '\n';
        std::cerr << "Result should be: " << pInit << '\n';
        std::cerr << "Reported result is: " << pOut << '\n';
        std::cerr << " [ FAILED ] " << '\n';
        return EXIT_FAILURE;
      }
    }

    std::cout << " [ PASSED ] " << '\n';
  }

  /* Create a Rigid 3D transform with a defined center and a rotation given by a Matrix */
  {
    auto                             rotation = TransformType::New();
    TransformType::VnlQuaternionType qrotation;

    // 15 degrees in radians
    const double angle = 15.0 * std::atan(1.0f) / 45.0;
    const double sinth2 = std::sin(angle / 2.0);
    const double costh2 = std::cos(angle / 2.0);

    const double sinth = std::sin(angle);
    const double costh = std::cos(angle);

    // around the positive Z axis
    qrotation[0] = 0.0;
    qrotation[1] = 0.0;
    qrotation[2] = sinth2;
    qrotation[3] = costh2;

    rotation->SetIdentity();

    rotation->SetRotation(qrotation);

    TransformType::InputPointType center;
    center[0] = 17;
    center[1] = 19;
    center[2] = 23;

    TransformType::OutputVectorType itranslation;
    itranslation[0] = 13;
    itranslation[1] = 17;
    itranslation[2] = 19;

    rotation->SetTranslation(itranslation);
    rotation->SetCenter(center);

    // rotation->ComputeOffset();

    std::cout << "rotation: " << rotation;

    // Verify the Offset content
    TransformType::OffsetType offset = rotation->GetOffset();
    std::cout << "pure Rotation test:  " << '\n';
    std::cout << "Offset = " << offset << '\n';

    TransformType::OffsetType ioffset;

    ioffset[0] = center[0] + itranslation[0];
    ioffset[1] = center[1] + itranslation[1];
    ioffset[2] = center[2] + itranslation[2];

    ioffset[0] -= costh * center[0] - sinth * center[1];
    ioffset[1] -= sinth * center[0] + costh * center[1];
    ioffset[2] -= center[2];

    std::cout << "iOffset = " << ioffset << '\n';
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

    // VNL uses transposed matrices.
    vnl_matrix_fixed<double, 3, 3> mrotation = qrotation.rotation_matrix_transpose();

    // Verify the Matrix content
    TransformType::MatrixType matrix = rotation->GetMatrix();
    std::cout << "Rotation matrix:  " << '\n';
    std::cout << matrix << '\n';
    for (unsigned int i = 0; i < N; ++i)
    {
      for (unsigned int j = 0; j < N; ++j)
      {
        if (itk::Math::abs(matrix[i][j] - mrotation[j][i]) > epsilon)
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
      const TransformType::InputPointType::ValueType pInit[3] = { 10, 10, 10 };
      TransformType::InputPointType                  p = pInit;
      TransformType::InputPointType                  q;

      const CoordinateType x = p[0] - center[0];
      const CoordinateType y = p[1] - center[1];
      const CoordinateType z = p[2] - center[2];

      q[0] = x * costh - y * sinth + center[0] + itranslation[0];
      q[1] = x * sinth + y * costh + center[1] + itranslation[1];
      q[2] = z + center[2] + itranslation[2];

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
      TransformType::InputVectorType::ValueType pInit[3] = { 10, 10, 10 };
      TransformType::InputVectorType            p = pInit;

      TransformType::OutputVectorType q;
      q[0] = p[0] * costh - p[1] * sinth;
      q[1] = p[0] * sinth + p[1] * costh;
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
      TransformType::InputCovariantVectorType::ValueType pInit[3] = { 10, 10, 10 };
      TransformType::InputCovariantVectorType            p = pInit;
      TransformType::OutputCovariantVectorType           q;

      q[0] = p[0] * costh - p[1] * sinth;
      q[1] = p[0] * sinth + p[1] * costh;
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
      // Rotate a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 11;
      p[1] = 7;
      p[2] = 15;

      TransformType::OutputVnlVectorType q;

      q[0] = p[0] * costh - p[1] * sinth;
      q[1] = p[0] * sinth + p[1] * costh;
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
  }

  std::cout << '\n';
  std::cout << "Test PASSED !" << '\n';
  std::cout << '\n';

  {
    // Testing SetMatrix()
    std::cout << "Testing SetMatrix() ... ";
    unsigned int par;

    using MatrixType = TransformType::MatrixType;
    MatrixType matrix;

    auto t = TransformType::New();

    // attempt to set an non-orthogonal matrix
    par = 0;
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
      std::cout << "Caught unknown exception" << '\n';
    }

    if (!Ok)
    {
      std::cerr << "Error: expected to catch an exception when attempting";
      std::cerr << " to set an non-orthogonal matrix." << '\n';
      return EXIT_FAILURE;
    }

    t = TransformType::New();

    // attempt to set an orthogonal matrix
    matrix.GetVnlMatrix().set_identity();

    const double a = 1.0 / 180.0 * itk::Math::pi;
    matrix[0][0] = std::cos(a);
    matrix[0][1] = -1.0 * std::sin(a);
    matrix[1][0] = std::sin(a);
    matrix[1][1] = std::cos(a);

    Ok = true;
    try
    {
      t->SetMatrix(matrix);
    }
    catch (const itk::ExceptionObject & err)
    {
      std::cout << err << '\n';
      Ok = false;
    }
    catch (...)
    {
      std::cout << "Caught unknown exception" << '\n';
      Ok = false;
    }

    if (!Ok)
    {
      std::cerr << "Error: caught unexpected exception" << '\n';
      return EXIT_FAILURE;
    }

    // Check the computed parameters

    using ParametersType = TransformType::ParametersType;
    ParametersType e(t->GetNumberOfParameters());
    e.Fill(0.0);
    e[2] = std::sin(0.5 * a);
    e[3] = std::cos(0.5 * a);

    t = TransformType::New();
    t->SetParameters(e);

    auto t2 = TransformType::New();
    t2->SetMatrix(t->GetMatrix());

    ParametersType p = t2->GetParameters();
    for (unsigned int k = 0; k < e.GetSize(); ++k)
    {
      if (itk::Math::abs(e[k] - p[k]) > epsilon)
      {
        std::cout << " [ FAILED ] " << '\n';
        std::cout << "Expected parameters: " << e << '\n';
        std::cout << "but got: " << p << '\n';
        return EXIT_FAILURE;
      }
    }

    std::cout << "[ PASSED ]" << '\n';
  }

  return EXIT_SUCCESS;
}
