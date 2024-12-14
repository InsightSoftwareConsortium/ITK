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

#include "itkScaleVersor3DTransform.h"
#include <iostream>

class TransformHelperType : public itk::ScaleVersor3DTransform<double>
{
public:
  using Self = TransformHelperType;
  using Superclass = itk::ScaleVersor3DTransform<double>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(TransformHelperType);

  void
  TriggerExceptionFromComputeMatrixParameters()
  {
    this->ComputeMatrixParameters();
  }
};

int
itkScaleVersor3DTransformTest(int, char *[])
{
  using ValueType = double;

  const ValueType epsilon = 1e-12;

  using TransformType = itk::ScaleVersor3DTransform<ValueType>;
  using VersorType = TransformType::VersorType;
  using VectorType = TransformType::InputVectorType;
  using ParametersType = TransformType::ParametersType;
  using JacobianType = TransformType::JacobianType;
  using MatrixType = TransformType::MatrixType;

  {
    std::cout << "Test default constructor... ";

    auto transform = TransformType::New();

    std::cout << transform->GetNameOfClass() << '\n';

    transform->Print(std::cout);

    auto axis = itk::MakeFilled<VectorType>(1.5);

    const ValueType angle = 120.0 * std::atan(1.0) / 45.0;

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
      std::cout << "Error ! " << '\n';
      return EXIT_FAILURE;
    }

    transform->Print(std::cout);

    std::cout << " PASSED !" << '\n';
  }

  {
    std::cout << "Test initial rotation matrix " << '\n';
    auto             transform = TransformType::New();
    const MatrixType matrix = transform->GetMatrix();
    std::cout << "Matrix = " << '\n';
    std::cout << matrix << '\n';

    MatrixType identity;
    identity.SetIdentity();

    try
    {
      // SetMatrix is not fully implemented, and it is expected
      // to throw an exception at this point.
      transform->SetMatrix(identity);
      std::cerr << "ERROR: Missed expected exception when calling SetMatrix() " << '\n';
      return EXIT_FAILURE;
    }
    catch (const itk::ExceptionObject & itkNotUsed(excp))
    {
      std::cerr << "Got Normal expected exception when calling SetMatrix() " << '\n';
    }
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
    std::cout << offset << '\n';
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
      std::cerr << "Get Offset  differs from null in rotation " << '\n';
      return EXIT_FAILURE;
    }

    VersorType versor;
    versor.Set(axis, angle);

    {
      // Rotate an itk::Point
      const TransformType::InputPointType::ValueType pInit[3] = { 1, 4, 9 };
      const TransformType::InputPointType            p = pInit;
      TransformType::OutputPointType                 q;
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
        std::cerr << "Error rotating point : " << p << '\n';
        std::cerr << "Result should be     : " << q << '\n';
        std::cerr << "Reported Result is   : " << r << '\n';
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an itk::Point " << '\n';
      }
    }

    {
      // Translate an itk::Vector
      TransformType::InputVectorType::ValueType pInit[3] = { 1, 4, 9 };
      const TransformType::InputVectorType      p = pInit;
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
        std::cerr << "Error rotating vector : " << p << '\n';
        std::cerr << "Result should be      : " << q << '\n';
        std::cerr << "Reported Result is    : " << r << '\n';
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an itk::Vector " << '\n';
      }
    }

    {
      // Translate an itk::CovariantVector
      TransformType::InputCovariantVectorType::ValueType pInit[3] = { 1, 4, 9 };
      const TransformType::InputCovariantVectorType      p = pInit;
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
        std::cerr << "Error rotating covariant vector : " << p << '\n';
        std::cerr << "Result should be                : " << q << '\n';
        std::cerr << "Reported Result is              : " << r << '\n';
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an itk::CovariantVector " << '\n';
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
        std::cerr << "Error rotating vnl_vector : " << p << '\n';
        std::cerr << "Result should be          : " << q << '\n';
        std::cerr << "Reported Result is        : " << r << '\n';
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an vnl_Vector " << '\n';
      }
    }
  }

  //  Exercise the SetCenter method
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
      std::cerr << "The center point was not invariant to rotation " << '\n';
      return EXIT_FAILURE;
    }
    else
    {
      std::cout << "Ok center is invariant to rotation." << '\n';
    }

    const unsigned int np = transform->GetNumberOfParameters();

    ParametersType parameters(np); // Number of parameters
    parameters.Fill(0.0);

    const VersorType versor;

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
        std::cerr << "Output parameter does not match input " << '\n';
        return EXIT_FAILURE;
      }
    }
    std::cout << "Input/Output parameter check Passed !" << '\n';

    // Try the ComputeJacobianWithRespectToParameters method
    TransformType::InputPointType aPoint;
    aPoint[0] = 10.0;
    aPoint[1] = 20.0;
    aPoint[2] = -10.0;
    JacobianType jacobian;
    transform->ComputeJacobianWithRespectToParameters(aPoint, jacobian);
    std::cout << "Jacobian: " << '\n';
    std::cout << jacobian << '\n';

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
      for (unsigned int jj = 0; jj < 9; ++jj)
      {
        if (itk::Math::abs(TheoreticalJacobian[ii][jj] - jacobian[ii][jj]) > 1e-5)
        {
          std::cerr << "Jacobian components differ from expected values ";
          std::cerr << '\n' << '\n';
          std::cerr << "Expected Jacobian = " << '\n';
          std::cerr << TheoreticalJacobian << '\n' << '\n';
          std::cerr << "Computed Jacobian = " << '\n';
          std::cerr << jacobian << '\n' << '\n';
          std::cerr << '\n' << "Test FAILED ! " << '\n';
          return EXIT_FAILURE;
        }
      }
    }
  }

  {
    std::cout << " Exercise the SetIdentity() method " << '\n';
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

    const VersorType versor;

    parameters[0] = versor.GetX(); // Rotation axis * sin(t/2)
    parameters[1] = versor.GetY();
    parameters[2] = versor.GetZ();
    parameters[3] = 0.0; // Translation
    parameters[4] = 0.0;
    parameters[5] = 0.0;
    parameters[6] = 1.0; // Scale
    parameters[7] = 1.0;
    parameters[8] = 1.0;

    ParametersType parameters2 = transform->GetParameters();

    const double tolerance = 1e-8;
    for (unsigned int p = 0; p < np; ++p)
    {
      if (itk::Math::abs(parameters[p] - parameters2[p]) > tolerance)
      {
        std::cerr << "Output parameter does not match input " << '\n';
        return EXIT_FAILURE;
      }
    }
    std::cout << "Input/Output parameter check Passed !" << '\n';
  }

  {
    std::cout << " Exercise the Scaling methods " << '\n';
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

    auto scale = itk::MakeFilled<TransformType::ScaleVectorType>(2.5);

    transform->SetScale(scale);

    TransformType::ScaleVectorType rscale = transform->GetScale();

    const double tolerance = 1e-8;
    for (unsigned int j = 0; j < 3; ++j)
    {
      if (itk::Math::abs(rscale[j] - scale[j]) > tolerance)
      {
        std::cerr << "Error in Set/Get Scale() " << '\n';
        std::cerr << "Input scale: " << scale << '\n';
        std::cerr << "Output scale: " << rscale << '\n';
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
        std::cerr << "Output parameter does not match input " << '\n';
        return EXIT_FAILURE;
      }
    }
    std::cout << "Input/Output parameter check Passed !" << '\n';
  }

  { // Exercise exceptions
    std::cout << " Exercise Exceptions " << '\n';

    auto transform = TransformHelperType::New();

    // At this point the method ComputeMatrixParameters() is expected to throw
    // an exception.
    try
    {
      transform->TriggerExceptionFromComputeMatrixParameters();
      std::cerr << "ERROR: Missed an expected exceptions from ComputeMatrixParameters() " << '\n';
      return EXIT_FAILURE;
    }
    catch (const itk::ExceptionObject & itkNotUsed(excp))
    {
      std::cout << "Got Correct expected exception from ComputeMatrixParameters() " << '\n';
    }
  }

  {
    std::cout << "Exercise SetParameters with Versor norm > 1.0 - epsilon" << '\n';

    auto transform = TransformType::New();

    const unsigned int np = transform->GetNumberOfParameters();

    ParametersType parameters(np); // Number of parameters
    parameters.Fill(0.0);

    parameters[0] = 1.0; // Set the Versor to norm 1.0
    parameters[1] = 0.0;
    parameters[2] = 0.0;

    transform->SetParameters(parameters);
#if 0 // TODO: Need to instrument inverse of ScaleVersor3DTransform
      {
      auto tInverse = TransformType::New();
      if(!transform->GetInverse(tInverse))
        {
        std::cout << "Cannot create inverse transform" << '\n';
        return EXIT_FAILURE;
        }
      std::cout << "translation: " << transform;
      std::cout << "translationInverse: " << tInverse;
      }
#endif
  }

  std::cout << '\n' << "Test PASSED ! " << '\n';

  return EXIT_SUCCESS;
}
