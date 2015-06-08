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

namespace itk
{
TransformFactoryBase *TransformFactoryBase:: m_Factory = ITK_NULLPTR;

namespace TransformFactoryBasePrivate
{
bool DefaultTransformsRegistered = false;
}

TransformFactoryBase::TransformFactoryBase()
{}

TransformFactoryBase::~TransformFactoryBase()
{}

void TransformFactoryBase::RegisterDefaultTransforms()
{
  //
  // make sure that the the factory instance has
  // been created. All normal paths to this method
  // already do this but this makes certain sure it's done
  (void)TransformFactoryBase::GetFactory();

  if ( !TransformFactoryBasePrivate::DefaultTransformsRegistered )
    {
    //
    // double instances (in alphabetical order)
    //
    TransformFactory< AffineTransform< double, 2 > >::RegisterTransform ();
    TransformFactory< AffineTransform< double, 3 > >::RegisterTransform ();
    TransformFactory< AffineTransform< double, 4 > >::RegisterTransform ();
    TransformFactory< AffineTransform< double, 5 > >::RegisterTransform ();
    TransformFactory< AffineTransform< double, 6 > >::RegisterTransform ();
    TransformFactory< AffineTransform< double, 7 > >::RegisterTransform ();
    TransformFactory< AffineTransform< double, 8 > >::RegisterTransform ();
    TransformFactory< AffineTransform< double, 9 > >::RegisterTransform ();

    TransformFactory< AzimuthElevationToCartesianTransform< double, 3 > >::RegisterTransform ();

    TransformFactory< BSplineTransform< double, 2, 2 > >::RegisterTransform ();
    TransformFactory< BSplineTransform< double, 3, 3 > >::RegisterTransform ();
#if !defined( ITK_FUTURE_LEGACY_REMOVE )
    TransformFactory< BSplineDeformableTransform< double, 2, 2 > >::RegisterTransform ();
    TransformFactory< BSplineDeformableTransform< double, 3, 3 > >::RegisterTransform ();
#endif

    TransformFactory< CenteredAffineTransform< double, 2 > >::RegisterTransform ();
    TransformFactory< CenteredAffineTransform< double, 3 > >::RegisterTransform ();
    TransformFactory< CenteredEuler3DTransform< double > >::RegisterTransform ();
    TransformFactory< CenteredRigid2DTransform< double > >::RegisterTransform();
    TransformFactory< CenteredSimilarity2DTransform< double > >::RegisterTransform ();

    TransformFactory< CompositeTransform<double, 2> >::RegisterTransform();
    TransformFactory< CompositeTransform<double, 3> >::RegisterTransform();
    TransformFactory< CompositeTransform<double, 4> >::RegisterTransform();
    TransformFactory< CompositeTransform<double, 5> >::RegisterTransform();
    TransformFactory< CompositeTransform<double, 6> >::RegisterTransform();
    TransformFactory< CompositeTransform<double, 7> >::RegisterTransform();
    TransformFactory< CompositeTransform<double, 8> >::RegisterTransform();
    TransformFactory< CompositeTransform<double, 9> >::RegisterTransform();

    TransformFactory< Euler2DTransform< double > >::RegisterTransform ();
    TransformFactory< Euler3DTransform< double > >::RegisterTransform ();

    TransformFactory< FixedCenterOfRotationAffineTransform< double > >::RegisterTransform ();

    TransformFactory< IdentityTransform< double, 2 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< double, 3 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< double, 4 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< double, 5 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< double, 6 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< double, 7 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< double, 8 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< double, 9 > >::RegisterTransform ();

    TransformFactory< QuaternionRigidTransform< double > >::RegisterTransform ();

    TransformFactory< Rigid2DTransform< double > >::RegisterTransform ();
    // We cannot register both Rigid3DTransform and
    // itkv3::Rigid3DTransform because they both have the same name
#ifdef ITKV3_COMPATIBILITY
    TransformFactory< Rigid3DTransform< double > >::RegisterTransform ();
#else
    TransformFactory< itkv3::Rigid3DTransform< double > >::RegisterTransform ();
#endif
    TransformFactory< Rigid3DPerspectiveTransform< double > >::RegisterTransform ();

    TransformFactory< ScalableAffineTransform< double > >::RegisterTransform ();
    TransformFactory< ScaleLogarithmicTransform< double > >::RegisterTransform ();
    TransformFactory< ScaleSkewVersor3DTransform< double > >::RegisterTransform ();
    TransformFactory< ScaleTransform< double, 2 > >::RegisterTransform ();
    TransformFactory< ScaleTransform< double, 3 > >::RegisterTransform ();
    TransformFactory< ScaleTransform< double, 4 > >::RegisterTransform ();
    TransformFactory< ScaleVersor3DTransform< double > >::RegisterTransform ();

    TransformFactory< Similarity2DTransform< double > >::RegisterTransform ();
    TransformFactory< Similarity3DTransform< double > >::RegisterTransform ();

    TransformFactory< TranslationTransform< double, 2 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< double, 3 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< double, 4 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< double, 5 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< double, 6 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< double, 7 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< double, 8 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< double, 9 > >::RegisterTransform ();

    TransformFactory< VersorRigid3DTransform< double > >::RegisterTransform ();
    TransformFactory< VersorTransform< double > >::RegisterTransform ();

    //
    // float instances (in alphabetical order)
    //
    TransformFactory< AffineTransform< float, 2 > >::RegisterTransform ();
    TransformFactory< AffineTransform< float, 3 > >::RegisterTransform ();
    TransformFactory< AffineTransform< float, 4 > >::RegisterTransform ();
    TransformFactory< AffineTransform< float, 5 > >::RegisterTransform ();
    TransformFactory< AffineTransform< float, 6 > >::RegisterTransform ();
    TransformFactory< AffineTransform< float, 7 > >::RegisterTransform ();
    TransformFactory< AffineTransform< float, 8 > >::RegisterTransform ();
    TransformFactory< AffineTransform< float, 9 > >::RegisterTransform ();

    TransformFactory< AzimuthElevationToCartesianTransform< float, 3 > >::RegisterTransform ();

    TransformFactory< BSplineTransform< float, 2, 2 > >::RegisterTransform ();
    TransformFactory< BSplineTransform< float, 3, 3 > >::RegisterTransform ();
#if !defined(ITK_FUTURE_LEGACY_REMOVE)
    TransformFactory< BSplineDeformableTransform< float, 2, 2 > >::RegisterTransform ();
    TransformFactory< BSplineDeformableTransform< float, 3, 3 > >::RegisterTransform ();
#endif

    TransformFactory< CenteredAffineTransform< float, 2 > >::RegisterTransform ();
    TransformFactory< CenteredAffineTransform< float, 3 > >::RegisterTransform ();
    TransformFactory< CenteredEuler3DTransform< float > >::RegisterTransform ();
    TransformFactory< CenteredRigid2DTransform< float > >::RegisterTransform();
    TransformFactory< CenteredSimilarity2DTransform< float > >::RegisterTransform ();

    TransformFactory< CompositeTransform<float, 2> >::RegisterTransform();
    TransformFactory< CompositeTransform<float, 3> >::RegisterTransform();
    TransformFactory< CompositeTransform<float, 4> >::RegisterTransform();
    TransformFactory< CompositeTransform<float, 5> >::RegisterTransform();
    TransformFactory< CompositeTransform<float, 6> >::RegisterTransform();
    TransformFactory< CompositeTransform<float, 7> >::RegisterTransform();
    TransformFactory< CompositeTransform<float, 8> >::RegisterTransform();
    TransformFactory< CompositeTransform<float, 9> >::RegisterTransform();

    TransformFactory< Euler2DTransform< float > >::RegisterTransform ();
    TransformFactory< Euler3DTransform< float > >::RegisterTransform ();

    TransformFactory< FixedCenterOfRotationAffineTransform< float > >::RegisterTransform ();

    TransformFactory< IdentityTransform< float, 2 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< float, 3 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< float, 4 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< float, 5 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< float, 6 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< float, 7 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< float, 8 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< float, 9 > >::RegisterTransform ();

    TransformFactory< QuaternionRigidTransform< float > >::RegisterTransform ();

    TransformFactory< Rigid2DTransform< float > >::RegisterTransform ();
    // We cannot register both Rigid3DTransform and
    // itkv3::Rigid3DTransform because they both have the same name
#ifdef ITKV3_COMPATIBILITY
    TransformFactory< Rigid3DTransform< float > >::RegisterTransform ();
#else
    TransformFactory< itkv3::Rigid3DTransform< float > >::RegisterTransform ();
#endif
    TransformFactory< Rigid3DPerspectiveTransform< float > >::RegisterTransform ();

    TransformFactory< ScalableAffineTransform< float > >::RegisterTransform ();
    TransformFactory< ScaleLogarithmicTransform< float > >::RegisterTransform ();
    TransformFactory< ScaleSkewVersor3DTransform< float > >::RegisterTransform ();
    TransformFactory< ScaleTransform< float, 2 > >::RegisterTransform ();
    TransformFactory< ScaleTransform< float, 3 > >::RegisterTransform ();
    TransformFactory< ScaleTransform< float, 4 > >::RegisterTransform ();
    TransformFactory< ScaleVersor3DTransform< float > >::RegisterTransform ();

    TransformFactory< Similarity2DTransform< float > >::RegisterTransform ();
    TransformFactory< Similarity3DTransform< float > >::RegisterTransform ();

    TransformFactory< TranslationTransform< float, 2 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< float, 3 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< float, 4 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< float, 5 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< float, 6 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< float, 7 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< float, 8 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< float, 9 > >::RegisterTransform ();

    TransformFactory< VersorRigid3DTransform< float > >::RegisterTransform ();
    TransformFactory< VersorTransform< float > >::RegisterTransform ();

    TransformFactory< BSplineSmoothingOnUpdateDisplacementFieldTransform<float,2> >::RegisterTransform ();
    TransformFactory< BSplineSmoothingOnUpdateDisplacementFieldTransform<float,3> >::RegisterTransform ();
    TransformFactory< BSplineSmoothingOnUpdateDisplacementFieldTransform<double,2> >::RegisterTransform ();
    TransformFactory< BSplineSmoothingOnUpdateDisplacementFieldTransform<double,3> >::RegisterTransform ();

    TransformFactory< ConstantVelocityFieldTransform<float,2> >::RegisterTransform ();
    TransformFactory< ConstantVelocityFieldTransform<float,3> >::RegisterTransform ();
    TransformFactory< ConstantVelocityFieldTransform<double,2> >::RegisterTransform ();
    TransformFactory< ConstantVelocityFieldTransform<double,3> >::RegisterTransform ();

    TransformFactory< DisplacementFieldTransform<float, 2> >::RegisterTransform ();
    TransformFactory< DisplacementFieldTransform<float, 3> >::RegisterTransform ();
    TransformFactory< DisplacementFieldTransform<double, 2> >::RegisterTransform ();
    TransformFactory< DisplacementFieldTransform<double, 3> >::RegisterTransform ();

    TransformFactory< GaussianExponentialDiffeomorphicTransform<float,2> >::RegisterTransform ();
    TransformFactory< GaussianExponentialDiffeomorphicTransform<float,3> >::RegisterTransform ();
    TransformFactory< GaussianExponentialDiffeomorphicTransform<double,2> >::RegisterTransform ();
    TransformFactory< GaussianExponentialDiffeomorphicTransform<double,3> >::RegisterTransform ();

    TransformFactory< GaussianSmoothingOnUpdateDisplacementFieldTransform<float,2> >::RegisterTransform ();
    TransformFactory< GaussianSmoothingOnUpdateDisplacementFieldTransform<float,3> >::RegisterTransform ();
    TransformFactory< GaussianSmoothingOnUpdateDisplacementFieldTransform<double,2> >::RegisterTransform ();
    TransformFactory< GaussianSmoothingOnUpdateDisplacementFieldTransform<double,3> >::RegisterTransform ();

    TransformFactory< GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform<float,2> >::RegisterTransform ();
    TransformFactory< GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform<float,3> >::RegisterTransform ();
    TransformFactory< GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform<double,2> >::RegisterTransform ();
    TransformFactory< GaussianSmoothingOnUpdateTimeVaryingVelocityFieldTransform<double,3> >::RegisterTransform ();

    TransformFactory< TimeVaryingBSplineVelocityFieldTransform<float,2> >::RegisterTransform ();
    TransformFactory< TimeVaryingBSplineVelocityFieldTransform<float,3> >::RegisterTransform ();
    TransformFactory< TimeVaryingBSplineVelocityFieldTransform<double,2> >::RegisterTransform ();
    TransformFactory< TimeVaryingBSplineVelocityFieldTransform<double,3> >::RegisterTransform ();

    TransformFactory< TimeVaryingVelocityFieldTransform<float,2> >::RegisterTransform ();
    TransformFactory< TimeVaryingVelocityFieldTransform<float,3> >::RegisterTransform ();
    TransformFactory< TimeVaryingVelocityFieldTransform<double,2> >::RegisterTransform ();
    TransformFactory< TimeVaryingVelocityFieldTransform<double,3> >::RegisterTransform ();

    TransformFactory< VelocityFieldTransform<float,2> >::RegisterTransform ();
    TransformFactory< VelocityFieldTransform<float,3> >::RegisterTransform ();
    TransformFactory< VelocityFieldTransform<double,2> >::RegisterTransform ();
    TransformFactory< VelocityFieldTransform<double,3> >::RegisterTransform ();
    }
  TransformFactoryBasePrivate::DefaultTransformsRegistered = true;
}

TransformFactoryBase * TransformFactoryBase::GetFactory()
{
  if ( m_Factory == ITK_NULLPTR )
    {
    // Make and register the factory
    Pointer p = New();
    m_Factory = p.GetPointer();
    ObjectFactoryBase::RegisterFactory (p);
    p->RegisterDefaultTransforms ();
    }
  return m_Factory;
}

const char *
TransformFactoryBase::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
TransformFactoryBase::GetDescription() const
{
  return "Transform FactoryBase";
}
} // end namespace itk
