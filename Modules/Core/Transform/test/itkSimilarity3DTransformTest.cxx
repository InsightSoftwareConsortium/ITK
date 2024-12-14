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
 *  This program illustrates the use of Similarity3DTransform
 *
 *  This transform performs: translation, rotation and uniform scaling.
 *
 */

#include "itkSimilarity3DTransform.h"
// -------------------------
//
//   Main code
//
// -------------------------
int
itkSimilarity3DTransformTest(int, char *[])
{

  using ValueType = double;

  const ValueType epsilon = 1e-12;

  //  Versor Transform type
  using TransformType = itk::Similarity3DTransform<ValueType>;

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
    if (itk::Math::abs(transform->GetScale() - 1.0) > itk::NumericTraits<TransformType::ScaleType>::min())
    {
      std::cout << "Error: Scale: Expected 1.0, got " << transform->GetScale() << " ! " << '\n';
      return EXIT_FAILURE;
    }
    // SetIdentity supposed to reset scale as well.
    transform->SetScale(2.0);
    transform->SetIdentity();
    if (itk::Math::abs(transform->GetScale() - 1.0) > itk::NumericTraits<TransformType::ScaleType>::min())
    {
      std::cout << "Error: Scale: Expected 1.0 after SetIdentity, got " << transform->GetScale() << " ! " << '\n';
      return EXIT_FAILURE;
    }

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

    transform->SetParameters(parameters);

    if (0.0 > epsilon)
    {
      std::cout << "Error ! " << '\n';
      return EXIT_FAILURE;
    }
    std::cout << " PASSED !" << '\n';
  }
  {
    std::cout << "Test initial rotation matrix " << '\n';
    auto             transform = TransformType::New();
    const MatrixType matrix = transform->GetMatrix();
    std::cout << "Matrix = " << '\n';
    std::cout << matrix << '\n';
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
        std::cout << "Error rotating point : " << p << '\n';
        std::cout << "Result should be     : " << q << '\n';
        std::cout << "Reported Result is   : " << r << '\n';
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
        std::cout << "Error rotating vector : " << p << '\n';
        std::cout << "Result should be      : " << q << '\n';
        std::cout << "Reported Result is    : " << r << '\n';
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
        std::cout << "Error rotating covariant vector : " << p << '\n';
        std::cout << "Result should be                : " << q << '\n';
        std::cout << "Reported Result is              : " << r << '\n';
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
        std::cout << "Error rotating vnl_vector : " << p << '\n';
        std::cout << "Result should be          : " << q << '\n';
        std::cout << "Reported Result is        : " << r << '\n';
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an vnl_Vector " << '\n';
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
      std::cout << "The center point was not invariant to rotation " << '\n';
      return EXIT_FAILURE;
    }
    else
    {
      std::cout << "Ok center is invariant to rotation." << '\n';
    }

    const unsigned int np = transform->GetNumberOfParameters();

    ParametersType parameters(np); // Number of parameters

    const VersorType versor;

    parameters[0] = versor.GetX(); // Rotation axis * sin(t/2)
    parameters[1] = versor.GetY();
    parameters[2] = versor.GetZ();
    parameters[3] = 8.0; // Translation
    parameters[4] = 7.0;
    parameters[5] = 6.0;
    parameters[6] = 10.0; // Scale

    transform->SetParameters(parameters);

    ParametersType parameters2 = transform->GetParameters();

    const double tolerance = 1e-8;
    for (unsigned int p = 0; p < np; ++p)
    {
      if (itk::Math::abs(parameters[p] - parameters2[p]) > tolerance)
      {
        std::cout << "Output parameter does not match input " << '\n';
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
    TheoreticalJacobian[1][0] = 2060.0;
    TheoreticalJacobian[2][0] = -840.0;

    TheoreticalJacobian[0][1] = -2060.0;
    TheoreticalJacobian[1][1] = 0.0;
    TheoreticalJacobian[2][1] = 420.0;

    TheoreticalJacobian[0][2] = 840.0;
    TheoreticalJacobian[1][2] = -420.0;
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
    TheoreticalJacobian[1][6] = -42.0;
    TheoreticalJacobian[2][6] = -103.0;
    for (unsigned int ii = 0; ii < 3; ++ii)
    {
      for (unsigned int jj = 0; jj < 7; ++jj)
      {
        if (itk::Math::abs(TheoreticalJacobian[ii][jj] - jacobian[ii][jj]) > 1e-5)
        {
          std::cout << "Jacobian components differ from expected values ";
          std::cout << '\n' << '\n';
          std::cout << "Expected Jacobian = " << '\n';
          std::cout << TheoreticalJacobian << '\n' << '\n';
          std::cout << "Computed Jacobian = " << '\n';
          std::cout << jacobian << '\n' << '\n';
          std::cout << '\n' << "Test FAILED ! " << '\n';
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
    parameters[6] = 1.0;

    ParametersType parameters2 = transform->GetParameters();

    const double tolerance = 1e-8;
    for (unsigned int p = 0; p < np; ++p)
    {
      if (itk::Math::abs(parameters[p] - parameters2[p]) > tolerance)
      {
        std::cout << "Output parameter does not match input " << '\n';
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

    constexpr double scale = 2.5;

    transform->SetScale(scale);

    const double rscale = transform->GetScale();

    const double tolerance = 1e-8;

    if (itk::Math::abs(rscale - scale) > tolerance)
    {
      std::cout << "Error in Set/Get Scale() " << '\n';
      return EXIT_FAILURE;
    }

    const unsigned int np = transform->GetNumberOfParameters();

    ParametersType parameters(np); // Number of parameters

    VersorType versor;
    versor.Set(axis, angle);

    parameters[0] = versor.GetX(); // Rotation axis * sin(t/2)
    parameters[1] = versor.GetY();
    parameters[2] = versor.GetZ();
    parameters[3] = translation[0];
    parameters[4] = translation[1];
    parameters[5] = translation[2];
    parameters[6] = scale;

    ParametersType parameters2 = transform->GetParameters();
    for (unsigned int p = 0; p < np; ++p)
    {
      if (itk::Math::abs(parameters[p] - parameters2[p]) > tolerance)
      {
        std::cout << "Output parameter does not match input " << '\n';
        return EXIT_FAILURE;
      }
    }
    std::cout << "Input/Output parameter check Passed !" << '\n';
  }
  {
    // Testing SetMatrix()
    std::cout << "Testing SetMatrix() ... ";
    bool         Ok;
    unsigned int par;

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

    std::cout << "Setting non-orthogonal matrix = " << '\n';
    std::cout << matrix << '\n';

    try
    {
      t->SetMatrix(matrix);
    }
    catch (const itk::ExceptionObject & err)
    {
      std::cout << "Caught expected exception" << err << '\n';
      Ok = true;
    }
    catch (...)
    {
      std::cout << "Caught unknown exception" << '\n';
    }

    if (!Ok)
    {
      std::cout << "Error: expected to catch an exception when attempting";
      std::cout << " to set an non-orthogonal matrix." << '\n';
      return EXIT_FAILURE;
    }

    t = TransformType::New();

    // attempt to set an (orthogonal + scale) matrix
    matrix.GetVnlMatrix().set_identity();

    const double a = 1.0 / 180.0 * itk::Math::pi;
    const double s = 0.5;
    matrix[0][0] = std::cos(a) * s;
    matrix[0][1] = -1.0 * std::sin(a) * s;
    matrix[1][0] = std::sin(a) * s;
    matrix[1][1] = std::cos(a) * s;
    matrix[2][2] = s;

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
      std::cout << "Error: caught unexpected exception" << '\n';
      return EXIT_FAILURE;
    }

    // Check the computed parameters
    TransformType::InputPointType center;
    center[0] = 15.0;
    center[1] = 16.0;
    center[2] = 17.0;

    ParametersType e(t->GetNumberOfParameters());
    e.Fill(0.0);
    e[2] = std::sin(0.5 * a);
    e[6] = 0.5;

    t = TransformType::New();
    t->SetCenter(center);
    t->SetParameters(e);

    auto t2 = TransformType::New();
    t2->SetCenter(center);
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

  std::cout << '\n' << "Test PASSED ! " << '\n';

  return EXIT_SUCCESS;
}
