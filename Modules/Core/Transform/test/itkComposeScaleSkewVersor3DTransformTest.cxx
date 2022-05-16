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
 *  This program illustrates the use of ComposeScaleSkewVersor3D transform.
 *
 *  This transform performs: translation, rotation scaling, and skewing.
 *
 */

#include "itkComposeScaleSkewVersor3DTransform.h"
#include <iostream>

#include <vnl/vnl_sample.h>

// -------------------------
//
//   Main code
//
// -------------------------
int
itkComposeScaleSkewVersor3DTransformTest(int, char *[])
{

  using ValueType = double;

  const ValueType epsilon = 1e-12;

  //  Versor Transform type
  using TransformType = itk::ComposeScaleSkewVersor3DTransform<ValueType>;

  //  Versor type
  using VersorType = TransformType::VersorType;

  //  Vector type
  using VectorType = TransformType::InputVectorType;

  //  Parameters type
  using ParametersType = TransformType::ParametersType;

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
    parameters[9] = 0.0;
    parameters[10] = 0.0;
    parameters[11] = 0.0;

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
    parameters[9] = 0.1; // Skew
    parameters[10] = 0.1;
    parameters[11] = 0.0;

    transform->SetParameters(parameters);

    ParametersType parameters2 = transform->GetParameters();

    const double tolerance = 1e-8;
    for (unsigned int p = 0; p < np; ++p)
    {
      if (itk::Math::abs(parameters[p] - parameters2[p]) > tolerance)
      {
        std::cerr << "Get/Set parameters: parameters do not match " << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << "Get/Set parameter check Passed !" << std::endl;
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

    ParametersType parameters2 = transform->GetParameters();

    const double tolerance = 1e-8;
    for (unsigned int p = 0; p < np; ++p)
    {
      std::cout << parameters[p] << " = " << parameters2[p] << std::endl;
      if (itk::Math::abs(parameters[p] - parameters2[p]) > tolerance)
      {
        std::cerr << "Identity parameters do not match" << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << "Identity parameters check Passed !" << std::endl;
  }

  {
    std::cout << " Exercise the Scaling methods " << std::endl;
    auto transform = TransformType::New();

    auto         axis = itk::MakeFilled<itk::Vector<double, 3>>(1);
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

    TransformType::SkewVectorType skew;
    skew.Fill(0);
    skew[0] = 0.1;
    skew[1] = 0.1;
    transform->SetSkew(skew);

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
    parameters[9] = skew[0];
    parameters[10] = skew[1];
    parameters[11] = skew[2];

    ParametersType parameters2 = transform->GetParameters();
    for (unsigned int p = 0; p < np; ++p)
    {
      std::cout << parameters[p] << " = " << parameters2[p] << std::endl;
      if (itk::Math::abs(parameters[p] - parameters2[p]) > tolerance)
      {
        std::cerr << "Scale parameters do not match input " << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << "Scale parameters check Passed !" << std::endl;

    std::cout << " Exercise the SetMatrix() method" << std::endl;
    auto transform2 = TransformType::New();
    transform2->SetFixedParameters(transform->GetFixedParameters());
    transform2->SetMatrix(transform->GetMatrix());
    transform2->SetOffset(transform->GetOffset());

    auto transform3 = TransformType::New();
    transform3->SetFixedParameters(transform2->GetFixedParameters());
    transform3->SetParameters(transform2->GetParameters());

    ParametersType parameters3 = transform3->GetParameters();
    for (unsigned int p = 0; p < np; ++p)
    {
      std::cout << parameters[p] << " = " << parameters3[p] << std::endl;
      if (itk::Math::abs(parameters[p] - parameters3[p]) > tolerance)
      {
        std::cerr << "SetMatrix parameters do not match input " << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << "SetMatrix parameters do match!" << std::endl;

    int diff = 0;
    for (unsigned int p = 0; p < 100; ++p)
    {
      TransformType::InputPointType pnt;
      for (unsigned int i = 0; i < 3; ++i)
      {
        pnt[i] = vnl_sample_uniform(-100, 100);
      }

      TransformType::OutputPointType tPnt;
      tPnt = transform->TransformPoint(pnt);
      TransformType::OutputPointType tPnt2;
      tPnt2 = transform2->TransformPoint(pnt);
      TransformType::OutputPointType tPnt3;
      tPnt3 = transform3->TransformPoint(pnt);

      for (unsigned int i = 0; i < 3; ++i)
      {
        if (itk::Math::abs(tPnt[i] - tPnt2[i]) > 1e-7)
        {
          ++diff;
        }
        if (itk::Math::abs(tPnt[i] - tPnt3[i]) > 1e-7)
        {
          ++diff;
        }
      }
      if (diff != 0)
      {
        std::cerr << "SetMatrix() points do not match" << std::endl;
        std::cout << "Point #" << p << std::endl;
        std::cout << "idea = " << tPnt << std::endl;
        std::cout << "t2 = " << tPnt2 << std::endl;
        std::cout << "t3 = " << tPnt3 << std::endl;
        std::cerr << "**************************************" << std::endl;
        std::cerr << "Transform = " << transform << std::endl;
        std::cerr << "**************************************" << std::endl;
        std::cerr << "Transform2 = " << transform2 << std::endl;
        std::cerr << "**************************************" << std::endl;
        return EXIT_FAILURE;
      }
    }
    std::cout << "SetMatrix() points check Passed !" << std::endl;

    auto tInverse = TransformType::New();
    if (!transform->GetInverse(tInverse))
    {
      std::cout << "Cannot create inverse transform" << std::endl;
      return EXIT_FAILURE;
    }
    std::cout << "translation: " << transform;
    std::cout << "translationInverse: " << tInverse;

    std::cout << "Testing jacobian" << std::endl;
    TransformType::InputPointType pnt;
    pnt[0] = 10;
    pnt[1] = 20;
    pnt[2] = -10;
    auto idT = TransformType::New();
    for (unsigned int mc = 0; mc < np; ++mc)
    {
      std::cout << "Testing parameter #" << mc << std::endl;
      idT->SetIdentity();
      ParametersType params = idT->GetParameters();
      params[mc] = 0.1;
      idT->SetParameters(params);
      TransformType::JacobianType jacob;
      idT->ComputeJacobianWithRespectToParameters(pnt, jacob);
      for (unsigned int i = 0; i < np; ++i)
      {
        ParametersType params1 = params;
        params1[i] += epsilon;
        idT->SetParameters(params1);
        TransformType::InputPointType pnt1 = idT->TransformPoint(pnt);
        ParametersType                params2 = params;
        params2[i] -= epsilon;
        idT->SetParameters(params2);
        TransformType::InputPointType pnt2 = idT->TransformPoint(pnt);
        for (unsigned int d = 0; d < 3; ++d)
        {
          double pntDiff = (pnt1[d] - pnt2[d]) / (2 * epsilon);
          if (itk::Math::abs(pntDiff - jacob[d][i]) > itk::Math::abs(0.1 * pntDiff))
          {
            std::cout << "Ideal = " << pntDiff << "  Jacob = " << jacob[d][i] << std::endl;
            std::cout << "Jacobian not matching finite difference." << std::endl;
            return EXIT_FAILURE;
          }
        }
      }
    }
  }
  std::cout << std::endl << "Test PASSED ! " << std::endl;

  return EXIT_SUCCESS;
}
