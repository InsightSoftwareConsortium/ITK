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
#include "itkAffineTransform.h"
#include "itkAzimuthElevationToCartesianTransform.h"
#include "itkBSplineBaseTransform.h"
#include "itkBSplineTransform.h"
#include "itkBSplineTransformInitializer.h"
#include "itkCenteredAffineTransform.h"
#include "itkCenteredEuler3DTransform.h"
#include "itkCenteredRigid2DTransform.h"
#include "itkCenteredSimilarity2DTransform.h"
#include "itkComposeScaleSkewVersor3DTransform.h"
#include "itkCompositeTransform.h"
#include "itkElasticBodyReciprocalSplineKernelTransform.h"
#include "itkElasticBodySplineKernelTransform.h"
#include "itkEuler2DTransform.h"
#include "itkEuler3DTransform.h"
#include "itkFixedCenterOfRotationAffineTransform.h"
#include "itkIdentityTransform.h"
#include "itkKernelTransform.h"
#include "itkMatrixOffsetTransformBase.h"
#include "itkMultiTransform.h"
#include "itkQuaternionRigidTransform.h"
#include "itkRigid2DTransform.h"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkRigid3DTransform.h"
#include "itkScalableAffineTransform.h"
#include "itkScaleLogarithmicTransform.h"
#include "itkScaleSkewVersor3DTransform.h"
#include "itkScaleTransform.h"
#include "itkScaleVersor3DTransform.h"
#include "itkSimilarity2DTransform.h"
#include "itkSimilarity3DTransform.h"
#include "itkThinPlateR2LogRSplineKernelTransform.h"
#include "itkThinPlateSplineKernelTransform.h"
#include "itkTranslationTransform.h"
#include "itkVersorRigid3DTransform.h"
#include "itkVersorTransform.h"
#include "itkVolumeSplineKernelTransform.h"
#include "itkMultiThreaderBase.h"
#include "itkMath.h"
#include <cmath>
#include <iostream>
#include <string>

template <typename TTransform>
struct ThreadData
{
  typename TTransform::Pointer m_Transform;
  typename TTransform::Pointer m_Inverse;
};

template <typename TTransform>
ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
TestGetInverseThreadFunction(void * perThreadData)
{
  auto * ti = static_cast<itk::MultiThreaderBase::WorkUnitInfo *>(perThreadData);
  auto * td = static_cast<ThreadData<TTransform> *>(ti->UserData);
  for (unsigned int i = 0; i < 100000; ++i)
  {
    td->m_Transform->GetInverse(td->m_Inverse.GetPointer());
  }

  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}

template <typename TTransform>
unsigned
TransformTest(bool expectSameType)
{
  const typename itk::MultiThreaderBase::Pointer threader = itk::MultiThreaderBase::New();

  ThreadData<TTransform> td;
  td.m_Transform = TTransform::New();
  td.m_Inverse = TTransform::New();
  std::cout << "Testing " << td.m_Transform->GetNameOfClass() << std::endl;

  if (expectSameType)
  {
    const typename TTransform::InverseTransformBasePointer inverseBase = td.m_Transform->GetInverseTransform();
    if (inverseBase.IsNull())
    {
      std::cerr << "ERROR: GetInverseTransform() returned null for " << td.m_Transform->GetNameOfClass() << std::endl;
      return 1;
    }
    if (dynamic_cast<const TTransform *>(inverseBase.GetPointer()) == nullptr)
    {
      std::cerr << "ERROR: GetInverseTransform() did not preserve concrete type for "
                << td.m_Transform->GetNameOfClass() << "; got " << inverseBase->GetNameOfClass() << std::endl;
      return 1;
    }
  }

  itk::ThreadFunctionType pFunc = TestGetInverseThreadFunction<TTransform>;
  threader->SetSingleMethod(pFunc, &td);
  try
  {
    threader->SingleMethodExecute();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "ITK Exception " << excp.what() << std::endl;
    return 1;
  }
  catch (...)
  {
    std::cerr << "Unknown exception" << std::endl;
  }
  return 0;
}

// Assert the inverse of a non-identity transform maps points back in both directions, and that the inverse
// rebuilt from its fixed and regular parameters, as a transform file stores it, maps them back too.
template <typename TTransform>
unsigned
InverseRoundTripTest(const TTransform * forward, bool expectSameType = true)
{
  constexpr double                                 tolerance = 1e-6;
  const std::string                                name = forward->GetNameOfClass();
  typename TTransform::InverseTransformBasePointer inverseBase;
  try
  {
    inverseBase = forward->GetInverseTransform();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "ERROR: " << name << " GetInverseTransform() threw: " << excp.GetDescription() << std::endl;
    return 1;
  }
  if (inverseBase.IsNull())
  {
    std::cerr << "ERROR: " << name << " GetInverseTransform() returned null for a non-identity instance" << std::endl;
    return 1;
  }
  const auto * inverse = inverseBase.GetPointer();
  if (expectSameType && dynamic_cast<const TTransform *>(inverse) == nullptr)
  {
    std::cerr << "ERROR: " << name << " inverse is not the concrete type (got " << inverseBase->GetNameOfClass() << ')'
              << std::endl;
    return 1;
  }

  const itk::LightObject::Pointer rebuiltObject = inverse->CreateAnother();
  auto *                          rebuilt = dynamic_cast<itk::Transform<double, 3, 3> *>(rebuiltObject.GetPointer());
  if (rebuilt == nullptr)
  {
    std::cerr << "ERROR: " << name << " inverse " << inverse->GetNameOfClass() << " could not be recreated"
              << std::endl;
    return 1;
  }
  rebuilt->SetFixedParameters(inverse->GetFixedParameters());
  rebuilt->SetParameters(inverse->GetParameters());

  const double samples[3][3]{ { 5.0, -3.0, 7.0 }, { -11.0, 2.0, 4.0 }, { 0.5, 9.0, -6.0 } };
  for (const auto & coords : samples)
  {
    itk::Point<double, 3> p;
    p[0] = coords[0];
    p[1] = coords[1];
    p[2] = coords[2];
    const auto inverseOfForward = inverse->TransformPoint(forward->TransformPoint(p));
    const auto forwardOfInverse = forward->TransformPoint(inverse->TransformPoint(p));
    const auto rebuiltOfForward = rebuilt->TransformPoint(forward->TransformPoint(p));
    for (unsigned int d = 0; d < 3; ++d)
    {
      if (itk::Math::Absolute(inverseOfForward[d] - p[d]) > tolerance ||
          itk::Math::Absolute(forwardOfInverse[d] - p[d]) > tolerance ||
          itk::Math::Absolute(rebuiltOfForward[d] - p[d]) > tolerance)
      {
        std::cerr << "ERROR: " << name << " inverse round-trip failed for " << p << ": inverse(forward(p)) is "
                  << inverseOfForward << ", forward(inverse(p)) is " << forwardOfInverse
                  << ", inverse rebuilt from its parameters gives " << rebuiltOfForward << std::endl;
        return 1;
      }
    }
  }
  return 0;
}

// Non-trivial coverage for some concrete-type inverse overrides.
unsigned
NonIdentityInverseTests()
{
  unsigned errorCount = 0;

  itk::Vector<double, 3> translation;
  translation[0] = 4.0;
  translation[1] = -6.0;
  translation[2] = 2.0;

  itk::Versor<double>::VectorType axis;
  axis[0] = 0.0;
  axis[1] = 0.0;
  axis[2] = 1.0;
  itk::Versor<double> versor;
  versor.Set(axis, 0.6);

  {
    auto t = itk::Euler3DTransform<double>::New();
    t->SetRotation(0.2, -0.35, 0.5);
    t->SetTranslation(translation);
    errorCount += InverseRoundTripTest(t.GetPointer());
  }
  {
    auto t = itk::VersorTransform<double>::New();
    t->SetRotation(versor);
    errorCount += InverseRoundTripTest(t.GetPointer());
  }
  {
    auto t = itk::VersorRigid3DTransform<double>::New();
    t->SetRotation(versor);
    t->SetTranslation(translation);
    errorCount += InverseRoundTripTest(t.GetPointer());
  }
  {
    auto                   t = itk::QuaternionRigidTransform<double>::New();
    vnl_quaternion<double> q(0.0, 0.0, std::sin(0.3), std::cos(0.3));
    t->SetRotation(q);
    t->SetTranslation(translation);
    errorCount += InverseRoundTripTest(t.GetPointer());
  }
  {
    auto t = itk::Similarity3DTransform<double>::New();
    t->SetRotation(versor);
    t->SetScale(2.0);
    t->SetTranslation(translation);
    errorCount += InverseRoundTripTest(t.GetPointer());
  }
  {
    auto                      t = itk::FixedCenterOfRotationAffineTransform<double, 3>::New();
    itk::Matrix<double, 3, 3> matrix;
    matrix.SetIdentity();
    matrix(0, 0) = 2.0; // anisotropic scale + shear -> non-orthogonal, invertible
    matrix(1, 2) = -0.5;
    itk::Point<double, 3> center;
    center[0] = 1.0;
    center[1] = -2.0;
    center[2] = 3.0;
    t->SetCenter(center);
    t->SetMatrix(matrix);
    t->SetTranslation(translation);
    errorCount += InverseRoundTripTest(t.GetPointer());
  }
  {
    auto                                                 t = itk::ScaleLogarithmicTransform<double, 3>::New();
    itk::ScaleLogarithmicTransform<double, 3>::ScaleType scale;
    scale[0] = 2.0;
    scale[1] = 4.0;
    scale[2] = 0.5;
    t->SetScale(scale);
    errorCount += InverseRoundTripTest(t.GetPointer());
  }

  // Scaled and skewed versor transforms invert to a MatrixOffsetTransformBase.
  itk::Vector<double, 3> anisotropicScale;
  anisotropicScale[0] = 1.5;
  anisotropicScale[1] = 0.75;
  anisotropicScale[2] = 2.0;
  {
    auto t = itk::ScaleVersor3DTransform<double>::New();
    t->SetRotation(versor);
    t->SetScale(anisotropicScale);
    t->SetTranslation(translation);
    errorCount += InverseRoundTripTest(t.GetPointer(), false);
  }
  {
    auto                                                    t = itk::ScaleSkewVersor3DTransform<double>::New();
    itk::ScaleSkewVersor3DTransform<double>::SkewVectorType skew;
    skew.Fill(0.1);
    t->SetRotation(versor);
    t->SetScale(anisotropicScale);
    t->SetSkew(skew);
    t->SetTranslation(translation);
    errorCount += InverseRoundTripTest(t.GetPointer(), false);
  }
  {
    auto t = itk::ComposeScaleSkewVersor3DTransform<double>::New();
    itk::ComposeScaleSkewVersor3DTransform<double>::SkewVectorType skew;
    skew.Fill(0.2);
    t->SetRotation(versor);
    t->SetScale(anisotropicScale);
    t->SetSkew(skew);
    t->SetTranslation(translation);
    errorCount += InverseRoundTripTest(t.GetPointer(), false);
  }

  return errorCount;
}

int
itkTestTransformGetInverse(int, char *[])
{
  unsigned int errorCount = TransformTest<itk::AffineTransform<double, 3>>(true);
  // Nonlinear: inverse is an affine approximation, not the same concrete type.
  errorCount += TransformTest<itk::AzimuthElevationToCartesianTransform<double, 3>>(false);
  // No closed-form global inverse: GetInverseTransform() returns null.
  errorCount += TransformTest<itk::BSplineTransform<double, 3>>(false);
  errorCount += TransformTest<itk::CenteredAffineTransform<double, 3>>(true);
  errorCount += TransformTest<itk::CenteredEuler3DTransform<double>>(true);
  errorCount += TransformTest<itk::CenteredAffineTransform<double, 3>>(true);
  errorCount += TransformTest<itk::CenteredRigid2DTransform<double>>(true);
  errorCount += TransformTest<itk::CenteredSimilarity2DTransform<double>>(true);
  errorCount += TransformTest<itk::CompositeTransform<double, 3>>(true);
  errorCount += TransformTest<itk::ElasticBodyReciprocalSplineKernelTransform<double, 3>>(true);
  errorCount += TransformTest<itk::ElasticBodySplineKernelTransform<double, 3>>(true);
  errorCount += TransformTest<itk::Euler2DTransform<double>>(true);
  errorCount += TransformTest<itk::Euler3DTransform<double>>(true);
  errorCount += TransformTest<itk::FixedCenterOfRotationAffineTransform<double, 3>>(true);
  errorCount += TransformTest<itk::IdentityTransform<double, 3>>(true);
  errorCount += TransformTest<itk::QuaternionRigidTransform<double>>(true);
  errorCount += TransformTest<itk::Rigid2DTransform<double>>(true);
  errorCount += TransformTest<itk::ScalableAffineTransform<double, 3>>(true);
  errorCount += TransformTest<itk::ScaleLogarithmicTransform<double, 3>>(true);
  errorCount += TransformTest<itk::ScaleTransform<double, 3>>(true);
  // ScaleVersor3D and ScaleSkewVersor3D cannot be set from a matrix, so only NonIdentityInverseTests() covers them.
  errorCount += TransformTest<itk::Similarity2DTransform<double>>(true);
  errorCount += TransformTest<itk::Similarity3DTransform<double>>(true);
  errorCount += TransformTest<itk::ThinPlateR2LogRSplineKernelTransform<double, 3>>(true);
  errorCount += TransformTest<itk::ThinPlateSplineKernelTransform<double, 3>>(true);
  errorCount += TransformTest<itk::TranslationTransform<double, 3>>(true);
  errorCount += TransformTest<itk::VersorRigid3DTransform<double>>(true);
  errorCount += TransformTest<itk::VersorTransform<double>>(true);
  errorCount += TransformTest<itk::VolumeSplineKernelTransform<double, 3>>(true);
  errorCount += NonIdentityInverseTests();
  if (errorCount > 0)
  {
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
