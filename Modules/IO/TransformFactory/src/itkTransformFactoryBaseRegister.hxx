/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkTransformFactoryBaseRegister_hxx
#define itkTransformFactoryBaseRegister_hxx
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
#include "itkv3Rigid3DTransform.h"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkScaleLogarithmicTransform.h"
#include "itkScaleVersor3DTransform.h"
#include "itkScaleSkewVersor3DTransform.h"
#include "itkSimilarity2DTransform.h"
#include "itkSimilarity3DTransform.h"
#include "itkTranslationTransform.h"
#include "itkBSplineTransform.h"
#include "itkCompositeTransform.h"

//Transforms from Filtering/DisplacementField/include
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

#if !defined( ITK_FUTURE_LEGACY_REMOVE )
#include "itkBSplineDeformableTransform.h"
#endif

#ifndef ITK_TRANSFORM_FACTORY_MAX_DIM
#define ITK_TRANSFORM_FACTORY_MAX_DIM 9
#endif


namespace itk
{


namespace
{

// Class to register generic transform for dimension D and lower
template< typename TParameterType, unsigned int D>
struct RegisterTransformsD
{
  static void Register(void)
    {
      TransformFactory< AffineTransform<TParameterType, D> >::RegisterTransform ();
      TransformFactory< CompositeTransform<TParameterType, D> >::RegisterTransform();
      TransformFactory< IdentityTransform<TParameterType, D> >::RegisterTransform ();
      TransformFactory< TranslationTransform<TParameterType, D> >::RegisterTransform ();

      // register transforms of one less dimension
      RegisterTransformsD<TParameterType, D-1>::Register();
    }
};

// Template specialized class to stop registering transform.
template<typename TParameterType>
struct RegisterTransformsD<TParameterType, 1>
{
  static void Register(void) {}
};
}

template <typename TParameterType>
void TransformFactoryBase::RegisterTransformFactory()
{
  // All generic transforms for any dimension, are registered in a
  // recursive class 2-MAX_TRANSFORM_DIM
  RegisterTransformsD<TParameterType, ITK_TRANSFORM_FACTORY_MAX_DIM>::Register();

  //
  //   TParameterType FixedParameters instances (in alphabetical order)
  //

  TransformFactory< AzimuthElevationToCartesianTransform<TParameterType, 3> >::RegisterTransform ();

  TransformFactory< BSplineTransform<TParameterType, 2, 3> >::RegisterTransform ();
  TransformFactory< BSplineTransform<TParameterType, 3, 3> >::RegisterTransform ();
#if !defined( ITK_FUTURE_LEGACY_REMOVE )
  TransformFactory< BSplineDeformableTransform<TParameterType, 2, 2> >::RegisterTransform ();
  TransformFactory< BSplineDeformableTransform<TParameterType, 3, 3> >::RegisterTransform ();
#endif

  TransformFactory< CenteredAffineTransform<TParameterType, 2> >::RegisterTransform ();
  TransformFactory< CenteredAffineTransform<TParameterType, 3> >::RegisterTransform ();
  TransformFactory< CenteredEuler3DTransform<TParameterType > >::RegisterTransform ();
  TransformFactory< CenteredRigid2DTransform<TParameterType > >::RegisterTransform();
  TransformFactory< CenteredSimilarity2DTransform<TParameterType > >::RegisterTransform ();


  TransformFactory< Euler2DTransform<TParameterType > >::RegisterTransform ();
  TransformFactory< Euler3DTransform<TParameterType > >::RegisterTransform ();

  TransformFactory< FixedCenterOfRotationAffineTransform<TParameterType, 3> >::RegisterTransform ();

  TransformFactory< QuaternionRigidTransform<TParameterType > >::RegisterTransform ();

  TransformFactory< Rigid2DTransform<TParameterType > >::RegisterTransform ();
  // We cannot register both Rigid3DTransform and
  // itkv3::Rigid3DTransform because they both have the same name
#ifdef ITKV3_COMPATIBILITY
  TransformFactory< Rigid3DTransform<TParameterType > >::RegisterTransform ();
#else
  TransformFactory< itkv3::Rigid3DTransform<TParameterType > >::RegisterTransform ();
#endif
  TransformFactory< Rigid3DPerspectiveTransform<TParameterType > >::RegisterTransform ();

  TransformFactory< ScalableAffineTransform<TParameterType, 3> >::RegisterTransform ();
  TransformFactory< ScaleLogarithmicTransform<TParameterType, 3> >::RegisterTransform ();
  TransformFactory< ScaleSkewVersor3DTransform<TParameterType > >::RegisterTransform ();
  TransformFactory< ScaleTransform<TParameterType, 2> >::RegisterTransform ();
  TransformFactory< ScaleTransform<TParameterType, 3> >::RegisterTransform ();
  TransformFactory< ScaleTransform<TParameterType, 4> >::RegisterTransform ();
  TransformFactory< ScaleVersor3DTransform<TParameterType > >::RegisterTransform ();

  TransformFactory< Similarity2DTransform<TParameterType > >::RegisterTransform ();
  TransformFactory< Similarity3DTransform<TParameterType > >::RegisterTransform ();

  TransformFactory< VersorRigid3DTransform<TParameterType > >::RegisterTransform ();
  TransformFactory< VersorTransform<TParameterType > >::RegisterTransform ();

  TransformFactory< BSplineSmoothingOnUpdateDisplacementFieldTransform<TParameterType,2> >::RegisterTransform ();
  TransformFactory< BSplineSmoothingOnUpdateDisplacementFieldTransform<TParameterType,3> >::RegisterTransform ();
  TransformFactory< ConstantVelocityFieldTransform<TParameterType,2> >::RegisterTransform ();
  TransformFactory< ConstantVelocityFieldTransform<TParameterType,3> >::RegisterTransform ();
  TransformFactory< DisplacementFieldTransform<TParameterType, 2> >::RegisterTransform ();
  TransformFactory< DisplacementFieldTransform<TParameterType, 3> >::RegisterTransform ();
  TransformFactory< GaussianExponentialDiffeomorphicTransform<TParameterType,2> >::RegisterTransform ();
  TransformFactory< GaussianExponentialDiffeomorphicTransform<TParameterType,3> >::RegisterTransform ();
  TransformFactory< GaussianSmoothingOnUpdateDisplacementFieldTransform<TParameterType,2> >::RegisterTransform ();
  TransformFactory< GaussianSmoothingOnUpdateDisplacementFieldTransform<TParameterType,3> >::RegisterTransform ();
  TransformFactory< GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform<TParameterType,2> >::RegisterTransform ();
  TransformFactory< GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform<TParameterType,3> >::RegisterTransform ();
  TransformFactory< TimeVaryingBSplineVelocityFieldTransform<TParameterType,2> >::RegisterTransform ();
  TransformFactory< TimeVaryingBSplineVelocityFieldTransform<TParameterType,3> >::RegisterTransform ();
  TransformFactory< TimeVaryingVelocityFieldTransform<TParameterType,2> >::RegisterTransform ();
  TransformFactory< TimeVaryingVelocityFieldTransform<TParameterType,3> >::RegisterTransform ();
  TransformFactory< VelocityFieldTransform<TParameterType,2> >::RegisterTransform ();
  TransformFactory< VelocityFieldTransform<TParameterType,3> >::RegisterTransform ();

}

} // end namespace itk

#endif
