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

/**
 *
 *  This program illustrates the use of ScaleSkewVersor3DTransform
 *
 *  This transform performs: translation, rotation, scaling, and skewing.
 *
 *  Note that this transform's parameters do not do what their names imply.
 *  The scale and skew parameters actually have effects beyond simple scaling
 *  and skewing (e.g., they induce rotations and scalings) because they
 *  are added to the rotation matrix instead of composed with it.
 *
 */

#include "itkScaleSkewVersor3DTransform.h"
#include <iostream>

// -------------------------
//
//   Main code
//
// -------------------------
int
itkScaleSkewVersor3DTransformTest(int, char *[])
{

  using ValueType = double;

  const ValueType epsilon = 1e-12;

  //  Versor Transform type
  using TransformType = itk::ScaleSkewVersor3DTransform<ValueType>;

  //  Versor type
  using VersorType = TransformType::VersorType;

  //  Vector type
  using VectorType = TransformType::InputVectorType;

  //  Parameters type
  using ParametersType = TransformType::ParametersType;

  //  Jacobian type
  using JacobianType = TransformType::JacobianType;

  //  Rotation Matrix type
  using MatrixType = TransformType::MatrixType;

  {
    std::cout << "Test default constructor... ";

    auto transform = TransformType::New();

    auto axis = itk::MakeFilled<VectorType>(1.5);

    ValueType angle = 120.0 * std::atan(1.0) / 45.0;

    VersorType versor;
    versor.Set(axis, angle);

    ParametersType parameters(transform->GetNumberOfParameters()); // Number of parameters

    parameters[0] = versor.GetX();
    parameters[1] = versor.GetY();
    parameters[2] = versor.GetZ();
    parameters[3] = 0.0; // Translation
    parameters[4] = 0.0;
    parameters[5] = 0.0;
    parameters[6] = 1.0;
    parameters[7] = 1.0;
    parameters[8] = 1.0;

    transform->SetParameters(parameters);

    if (0.0 > epsilon)
    {
      std::cout << "Error ! " << std::endl;
      return EXIT_FAILURE;
    }
    std::cout << " PASSED !" << std::endl;
  }

  {
    std::cout << "Test initial rotation matrix " << std::endl;
    auto       transform = TransformType::New();
    MatrixType matrix = transform->GetMatrix();
    std::cout << "Matrix = " << std::endl;
    std::cout << matrix << std::endl;
  }

  /* Create a Rigid 3D transform with rotation */

  {
    bool Ok = true;

    auto rotation = TransformType::New();

    auto axis = itk::MakeFilled<itk::Vector<double, 3>>(1);

    const double angle = (std::atan(1.0) / 45.0) * 120.0; // turn 120 degrees

    // this rotation will permute the axis x->y, y->z, z->x
    rotation->SetRotation(axis, angle);

    TransformType::OffsetType offset = rotation->GetOffset();
    std::cout << "pure Rotation test:  ";
    std::cout << offset << std::endl;
    for (unsigned int i = 0; i < 3; ++i)
    {
      if (itk::Math::abs(offset[i] - 0.0) > epsilon)
      {
        Ok = false;
        break;
      }
    }

    if (!Ok)
    {
      std::cerr << "Get Offset  differs from null in rotation " << std::endl;
      return EXIT_FAILURE;
    }

    VersorType versor;
    versor.Set(axis, angle);

    {
      // Rotate an itk::Point
      TransformType::InputPointType::ValueType pInit[3] = { 1, 4, 9 };
      TransformType::InputPointType            p = pInit;
      TransformType::OutputPointType           q;
      q = versor.Transform(p);

      TransformType::OutputPointType r;
      r = rotation->TransformPoint(p);
      for (unsigned int i = 0; i < 3; ++i)
      {
        if (itk::Math::abs(q[i] - r[i]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
      if (!Ok)
      {
        std::cerr << "Error rotating point : " << p << std::endl;
        std::cerr << "Result should be     : " << q << std::endl;
        std::cerr << "Reported Result is   : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an itk::Point " << std::endl;
      }
    }

    {
      // Translate an itk::Vector
      TransformType::InputVectorType::ValueType pInit[3] = { 1, 4, 9 };
      TransformType::InputVectorType            p = pInit;
      TransformType::OutputVectorType           q;
      q = versor.Transform(p);

      TransformType::OutputVectorType r;
      r = rotation->TransformVector(p);
      for (unsigned int i = 0; i < 3; ++i)
      {
        if (itk::Math::abs(q[i] - r[i]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
      if (!Ok)
      {
        std::cerr << "Error rotating vector : " << p << std::endl;
        std::cerr << "Result should be      : " << q << std::endl;
        std::cerr << "Reported Result is    : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an itk::Vector " << std::endl;
      }
    }

    {
      // Translate an itk::CovariantVector
      TransformType::InputCovariantVectorType::ValueType pInit[3] = { 1, 4, 9 };
      TransformType::InputCovariantVectorType            p = pInit;
      TransformType::OutputCovariantVectorType           q;
      q = versor.Transform(p);

      TransformType::OutputCovariantVectorType r;
      r = rotation->TransformCovariantVector(p);
      for (unsigned int i = 0; i < 3; ++i)
      {
        if (itk::Math::abs(q[i] - r[i]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
      if (!Ok)
      {
        std::cerr << "Error rotating covariant vector : " << p << std::endl;
        std::cerr << "Result should be                : " << q << std::endl;
        std::cerr << "Reported Result is              : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an itk::CovariantVector " << std::endl;
      }
    }

    {
      // Translate a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 1;
      p[1] = 4;
      p[2] = 9;

      TransformType::OutputVnlVectorType q;
      q = versor.Transform(p);

      TransformType::OutputVnlVectorType r;
      r = rotation->TransformVector(p);
      for (unsigned int i = 0; i < 3; ++i)
      {
        if (itk::Math::abs(q[i] - r[i]) > epsilon)
        {
          Ok = false;
          break;
        }
      }
      if (!Ok)
      {
        std::cerr << "Error rotating vnl_vector : " << p << std::endl;
        std::cerr << "Result should be          : " << q << std::endl;
        std::cerr << "Reported Result is        : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an vnl_Vector " << std::endl;
      }
    }
  }

  /**  Exercise the SetCenter method  */
  {
    bool Ok = true;

    auto transform = TransformType::New();

    auto axis = itk::MakeFilled<itk::Vector<double, 3>>(1);

    const double angle = (std::atan(1.0) / 45.0) * 30.0; // turn 30 degrees

    transform->SetRotation(axis, angle);

    TransformType::InputPointType center;
    center[0] = 31;
    center[1] = 62;
    center[2] = 93;

    transform->SetCenter(center);

    TransformType::OutputPointType transformedPoint;
    transformedPoint = transform->TransformPoint(center);
    for (unsigned int i = 0; i < 3; ++i)
    {
      if (itk::Math::abs(center[i] - transformedPoint[i]) > epsilon)
      {
        Ok = false;
        break;
      }
    }

    if (!Ok)
    {
      std::cerr << "The center point was not invariant to rotation " << std::endl;
      return EXIT_FAILURE;
    }
    else
    {
      std::cout << "Ok center is invariant to rotation." << std::endl;
    }

    const unsigned int np = transform->GetNumberOfParameters();

    ParametersType parameters(np); // Number of parameters
    parameters.Fill(0.0);

    VersorType versor;

    // TODO: Test jacobian with non-zero skew

    parameters[0] = versor.GetX(); // Rotation axis * sin(t/2)
    parameters[1] = versor.GetY();
    parameters[2] = versor.GetZ();
    parameters[3] = 8.0; // Translation
    parameters[4] = 7.0;
    parameters[5] = 6.0;
    parameters[6] = 1.0; // Scale
    parameters[7] = 1.0;
    parameters[8] = 1.0;

    transform->SetParameters(parameters);

    ParametersType parameters2 = transform->GetParameters();

    const double tolerance = 1e-8;
    for (unsigned int p = 0; p < np; ++p)
    {
      if (itk::Math::abs(parameters[p] - parameters2[p]) > tolerance)
      {
        std::cerr << "Output parameter does not match input " << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << "Input/Output parameter check Passed !" << std::endl;

    // Try the ComputeJacobianWithRespectToParameters method
    TransformType::InputPointType aPoint;
    aPoint[0] = 10.0;
    aPoint[1] = 20.0;
    aPoint[2] = -10.0;
    JacobianType jacobian;
    transform->ComputeJacobianWithRespectToParameters(aPoint, jacobian);
    std::cout << "Jacobian: " << std::endl;
    std::cout << jacobian << std::endl;

    // copy the read one just for getting the right matrix size
    JacobianType TheoreticalJacobian = jacobian;

    TheoreticalJacobian[0][0] = 0.0;
    TheoreticalJacobian[1][0] = 206.0;
    TheoreticalJacobian[2][0] = -84.0;

    TheoreticalJacobian[0][1] = -206.0;
    TheoreticalJacobian[1][1] = 0.0;
    TheoreticalJacobian[2][1] = 42.0;

    TheoreticalJacobian[0][2] = 84.0;
    TheoreticalJacobian[1][2] = -42.0;
    TheoreticalJacobian[2][2] = 0.0;

    TheoreticalJacobian[0][3] = 1.0;
    TheoreticalJacobian[1][3] = 0.0;
    TheoreticalJacobian[2][3] = 0.0;

    TheoreticalJacobian[0][4] = 0.0;
    TheoreticalJacobian[1][4] = 1.0;
    TheoreticalJacobian[2][4] = 0.0;

    TheoreticalJacobian[0][5] = 0.0;
    TheoreticalJacobian[1][5] = 0.0;
    TheoreticalJacobian[2][5] = 1.0;

    TheoreticalJacobian[0][6] = -21.0;
    TheoreticalJacobian[1][6] = 0.0;
    TheoreticalJacobian[2][6] = 0.0;

    TheoreticalJacobian[0][7] = 0.0;
    TheoreticalJacobian[1][7] = -42.0;
    TheoreticalJacobian[2][7] = 0.0;

    TheoreticalJacobian[0][8] = 0.0;
    TheoreticalJacobian[1][8] = 0.0;
    TheoreticalJacobian[2][8] = -103.0;
    for (unsigned int ii = 0; ii < 3; ++ii)
    {
      for (unsigned int jj = 0; jj < 15; ++jj)
      {
        if (itk::Math::abs(TheoreticalJacobian[ii][jj] - jacobian[ii][jj]) > 1e-5)
        {
          std::cerr << "Jacobian components differ from expected values ";
          std::cerr << std::endl << std::endl;
          std::cerr << "Expected Jacobian = " << std::endl;
          std::cerr << TheoreticalJacobian << std::endl << std::endl;
          std::cerr << "Computed Jacobian = " << std::endl;
          std::cerr << jacobian << std::endl << std::endl;
          std::cerr << std::endl << "Test FAILED ! " << std::endl;
          return EXIT_FAILURE;
        }
      }
    }
  }

  {
    std::cout << " Exercise the SetIdentity() method " << std::endl;
    auto transform = TransformType::New();

    auto axis = itk::MakeFilled<itk::Vector<double, 3>>(1);

    const double angle = (std::atan(1.0) / 45.0) * 30.0; // turn 30 degrees

    transform->SetRotation(axis, angle);

    TransformType::InputPointType center;
    center[0] = 31;
    center[1] = 62;
    center[2] = 93;

    transform->SetCenter(center);

    transform->SetIdentity();

    const unsigned int np = transform->GetNumberOfParameters();

    ParametersType parameters(np); // Number of parameters

    VersorType versor;

    parameters[0] = versor.GetX(); // Rotation axis * sin(t/2)
    parameters[1] = versor.GetY();
    parameters[2] = versor.GetZ();
    parameters[3] = 0.0; // Translation
    parameters[4] = 0.0;
    parameters[5] = 0.0;
    parameters[6] = 1.0; // Scale
    parameters[7] = 1.0;
    parameters[8] = 1.0;
    parameters[9] = 0.0; // Skew
    parameters[10] = 0.0;
    parameters[11] = 0.0;
    parameters[12] = 0.0;
    parameters[13] = 0.0;
    parameters[14] = 0.0;

    ParametersType parameters2 = transform->GetParameters();

    const double tolerance = 1e-8;
    for (unsigned int p = 0; p < np; ++p)
    {
      if (itk::Math::abs(parameters[p] - parameters2[p]) > tolerance)
      {
        std::cerr << "Output parameter does not match input " << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << "Input/Output parameter check Passed !" << std::endl;
  }

  {
    std::cout << " Exercise the Scaling methods " << std::endl;
    auto transform = TransformType::New();

    auto axis = itk::MakeFilled<itk::Vector<double, 3>>(1);

    const double angle = (std::atan(1.0) / 45.0) * 30.0; // turn 30 degrees

    transform->SetRotation(axis, angle);

    TransformType::InputPointType center;
    center[0] = 31;
    center[1] = 62;
    center[2] = 93;

    transform->SetCenter(center);

    TransformType::OutputVectorType translation;
    translation[0] = 17;
    translation[1] = 19;
    translation[2] = 23;

    transform->SetTranslation(translation);

    TransformType::ScaleVectorType scale;
    scale.Fill(2.5);

    transform->SetScale(scale);

    TransformType::ScaleVectorType rscale = transform->GetScale();

    const double tolerance = 1e-8;
    for (unsigned int j = 0; j < 3; ++j)
    {
      if (itk::Math::abs(rscale[j] - scale[j]) > tolerance)
      {
        std::cerr << "Error in Set/Get Scale() " << std::endl;
        std::cerr << "Input scale: " << scale << std::endl;
        std::cerr << "Output scale: " << rscale << std::endl;
        return EXIT_FAILURE;
      }
    }

    const unsigned int np = transform->GetNumberOfParameters();

    ParametersType parameters(np); // Number of parameters

    VersorType versor;
    versor.Set(axis, angle);

    parameters.Fill(0.0);
    parameters[0] = versor.GetX(); // Rotation axis * sin(t/2)
    parameters[1] = versor.GetY();
    parameters[2] = versor.GetZ();
    parameters[3] = translation[0];
    parameters[4] = translation[1];
    parameters[5] = translation[2];
    parameters[6] = scale[0];
    parameters[7] = scale[1];
    parameters[8] = scale[2];

    ParametersType parameters2 = transform->GetParameters();
    for (unsigned int p = 0; p < np; ++p)
    {
      if (itk::Math::abs(parameters[p] - parameters2[p]) > tolerance)
      {
        std::cerr << "Output parameter does not match input " << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << "Input/Output parameter check Passed !" << std::endl;
#if 0 // TODO: Need to instrument inverse of ScaleVersor3DTransform
      {
      auto tInverse = TransformType::New();
      if(!transform->GetInverse(tInverse))
        {
        std::cout << "Cannot create inverse transform" << std::endl;
        return EXIT_FAILURE;
        }
      std::cout << "translation: " << transform;
      std::cout << "translationInverse: " << tInverse;
      }
#endif
  }
  std::cout << std::endl << "Test PASSED ! " << std::endl;

  return EXIT_SUCCESS;
}
