/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkTransformFactory.h"
#include "itkVersion.h"

#include "itkAzimuthElevationToCartesianTransform.h"
#include "itkCenteredAffineTransform.h"
#include "itkCenteredEuler3DTransform.h"
#include "itkCenteredRigid2DTransform.h"
#include "itkCenteredSimilarity2DTransform.h"
#include "itkEuler2DTransform.h"
#include "itkFixedCenterOfRotationAffineTransform.h"
#include "itkIdentityTransform.h"
#include "itkQuaternionRigidTransform.h"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkScaleLogarithmicTransform.h"
#include "itkScaleVersor3DTransform.h"
#include "itkScaleSkewVersor3DTransform.h"
#include "itkSimilarity2DTransform.h"
#include "itkSimilarity3DTransform.h"
#include "itkTranslationTransform.h"
#include "itkBSplineTransform.h"
#include "itkCompositeTransform.h"

// Transforms from Filtering/DisplacementField/include
#include "itkBSplineExponentialDiffeomorphicTransform.h"
#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransform.h"
#include "itkConstantVelocityFieldTransform.h"
#include "itkDisplacementFieldTransform.h"
#include "itkGaussianExponentialDiffeomorphicTransform.h"
#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"
#include "itkGaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform.h"
#include "itkTimeVaryingBSplineVelocityFieldTransform.h"
#include "itkTimeVaryingVelocityFieldTransform.h"
#include "itkVelocityFieldTransform.h"

#if !defined(ITK_LEGACY_REMOVE)
#  include "itkBSplineDeformableTransform.h"
#endif

namespace itk
{
TransformFactoryBase * TransformFactoryBase::m_Factory = nullptr;

namespace TransformFactoryBasePrivate
{
bool DefaultTransformsRegistered = false;
}

TransformFactoryBase::TransformFactoryBase() = default;

TransformFactoryBase::~TransformFactoryBase() = default;

void
TransformFactoryBase::RegisterDefaultTransforms()
{
  //
  // make sure that the the factory instance has
  // been created. All normal paths to this method
  // already do this but this makes certain sure it's done
  (void)TransformFactoryBase::GetFactory();

  if (!TransformFactoryBasePrivate::DefaultTransformsRegistered)
  {
    //
    // double Parameters, double FixedParameters instances (in alphabetical order)
    //
    RegisterTransformFactoryDoubleParameters();

    ////////////////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////////
    //
    // float Parameters, double FixedParamters instances (in alphabetical order)
    //
    RegisterTransformFactoryFloatParameters();
  }
  TransformFactoryBasePrivate::DefaultTransformsRegistered = true;
}

TransformFactoryBase *
TransformFactoryBase::GetFactory()
{
  if (m_Factory == nullptr)
  {
    // Make and register the factory
    Pointer p = New();
    m_Factory = p.GetPointer();
    ObjectFactoryBase::RegisterFactory(p);
    p->RegisterDefaultTransforms();
  }
  return m_Factory;
}

const char *
TransformFactoryBase::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
TransformFactoryBase::GetDescription() const
{
  return "Transform FactoryBase";
}
} // end namespace itk
