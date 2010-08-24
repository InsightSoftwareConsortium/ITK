/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkTransformFactoryBase.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkTransformFactoryBase.h"
#include "itkTransformFactory.h"
#include "itkVersion.h"

#include "itkAffineTransform.h"
#include "itkBSplineDeformableTransform.h"
#include "itkCenteredAffineTransform.h"
#include "itkCenteredEuler3DTransform.h"
#include "itkCenteredEuler3DTransform.h"
#include "itkCenteredRigid2DTransform.h"
#include "itkCenteredSimilarity2DTransform.h"
#include "itkSimilarity2DTransform.h"
#include "itkEuler2DTransform.h"
#include "itkEuler3DTransform.h"
#include "itkFixedCenterOfRotationAffineTransform.h"
#include "itkIdentityTransform.h"
#include "itkQuaternionRigidTransform.h"
#include "itkRigid2DTransform.h"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkRigid3DTransform.h"
#include "itkScalableAffineTransform.h"
#include "itkScaleLogarithmicTransform.h"
#include "itkScaleVersor3DTransform.h"
#include "itkScaleSkewVersor3DTransform.h"
#include "itkScaleTransform.h"
#include "itkTranslationTransform.h"
#include "itkVersorRigid3DTransform.h"
#include "itkVersorTransform.h"

namespace itk
{
TransformFactoryBase *TransformFactoryBase:: m_Factory = 0;

TransformFactoryBase::TransformFactoryBase()
{}

TransformFactoryBase::~TransformFactoryBase()
{}

void TransformFactoryBase::RegisterDefaultTransforms()
{
  if ( !m_Factory )
    {
    // BSpline purposely not registered!
    TransformFactory< AffineTransform< double, 2 > >::RegisterTransform ();
    TransformFactory< AffineTransform< double, 3 > >::RegisterTransform ();
    TransformFactory< BSplineDeformableTransform< double, 2, 2 > >::RegisterTransform ();
    TransformFactory< BSplineDeformableTransform< double, 3, 3 > >::RegisterTransform ();
    TransformFactory< CenteredAffineTransform< double, 2 > >::RegisterTransform ();
    TransformFactory< CenteredAffineTransform< double, 3 > >::RegisterTransform ();
    TransformFactory< CenteredEuler3DTransform< double > >::RegisterTransform ();
    TransformFactory< CenteredRigid2DTransform< double > >::RegisterTransform();
    TransformFactory< CenteredSimilarity2DTransform< double > >::RegisterTransform ();
    TransformFactory< Similarity2DTransform< double > >::RegisterTransform ();
    TransformFactory< Euler2DTransform< double > >::RegisterTransform ();
    TransformFactory< Euler3DTransform< double > >::RegisterTransform ();
    TransformFactory< FixedCenterOfRotationAffineTransform< double > >::RegisterTransform ();
    TransformFactory< IdentityTransform< double, 2 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< double, 3 > >::RegisterTransform ();
    TransformFactory< QuaternionRigidTransform< double > >::RegisterTransform ();
    TransformFactory< Rigid2DTransform< double > >::RegisterTransform ();
    TransformFactory< Rigid3DPerspectiveTransform< double > >::RegisterTransform ();
    TransformFactory< Rigid3DTransform< double > >::RegisterTransform ();
    TransformFactory< ScalableAffineTransform< double > >::RegisterTransform ();
    TransformFactory< ScaleLogarithmicTransform< double > >::RegisterTransform ();
    TransformFactory< ScaleVersor3DTransform< double > >::RegisterTransform ();
    TransformFactory< ScaleSkewVersor3DTransform< double > >::RegisterTransform ();
    TransformFactory< ScaleTransform< double, 2 > >::RegisterTransform ();
    TransformFactory< ScaleTransform< double, 2 > >::RegisterTransform ();
    TransformFactory< ScaleTransform< double, 3 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< double > >::RegisterTransform ();
    TransformFactory< VersorRigid3DTransform< double > >::RegisterTransform ();
    TransformFactory< VersorTransform< double > >::RegisterTransform ();

    TransformFactory< AffineTransform< float, 2 > >::RegisterTransform ();
    TransformFactory< AffineTransform< float, 3 > >::RegisterTransform ();
    TransformFactory< BSplineDeformableTransform< float, 2, 2 > >::RegisterTransform ();
    TransformFactory< BSplineDeformableTransform< float, 3, 3 > >::RegisterTransform ();
    TransformFactory< CenteredAffineTransform< float, 2 > >::RegisterTransform ();
    TransformFactory< CenteredAffineTransform< float, 3 > >::RegisterTransform ();
    TransformFactory< CenteredEuler3DTransform< float > >::RegisterTransform ();
    TransformFactory< CenteredRigid2DTransform< float > >::RegisterTransform();
    TransformFactory< CenteredSimilarity2DTransform< float > >::RegisterTransform ();
    TransformFactory< Similarity2DTransform< float > >::RegisterTransform ();
    TransformFactory< Euler2DTransform< float > >::RegisterTransform ();
    TransformFactory< Euler3DTransform< float > >::RegisterTransform ();
    TransformFactory< FixedCenterOfRotationAffineTransform< float > >::RegisterTransform ();
    TransformFactory< IdentityTransform< float, 2 > >::RegisterTransform ();
    TransformFactory< IdentityTransform< float, 3 > >::RegisterTransform ();
    TransformFactory< QuaternionRigidTransform< float > >::RegisterTransform ();
    TransformFactory< Rigid2DTransform< float > >::RegisterTransform ();
    TransformFactory< Rigid3DPerspectiveTransform< float > >::RegisterTransform ();
    TransformFactory< Rigid3DTransform< float > >::RegisterTransform ();
    TransformFactory< ScalableAffineTransform< float > >::RegisterTransform ();
    TransformFactory< ScaleLogarithmicTransform< float > >::RegisterTransform ();
    TransformFactory< ScaleVersor3DTransform< float > >::RegisterTransform ();
    TransformFactory< ScaleSkewVersor3DTransform< float > >::RegisterTransform ();
    TransformFactory< ScaleTransform< float, 2 > >::RegisterTransform ();
    TransformFactory< ScaleTransform< float, 2 > >::RegisterTransform ();
    TransformFactory< ScaleTransform< float, 3 > >::RegisterTransform ();
    TransformFactory< TranslationTransform< float > >::RegisterTransform ();
    TransformFactory< VersorRigid3DTransform< float > >::RegisterTransform ();
    TransformFactory< VersorTransform< float > >::RegisterTransform ();
    }
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
