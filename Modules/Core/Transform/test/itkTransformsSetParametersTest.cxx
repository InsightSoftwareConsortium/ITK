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

#include "itkCenteredAffineTransform.h"
#include "itkCenteredEuler3DTransform.h"
#include "itkCenteredRigid2DTransform.h"
#include "itkCenteredSimilarity2DTransform.h"
#include "itkEuler2DTransform.h"
#include "itkFixedCenterOfRotationAffineTransform.h"
#include "itkQuaternionRigidTransform.h"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkScaleLogarithmicTransform.h"
#include "itkScaleSkewVersor3DTransform.h"
#include "itkSimilarity3DTransform.h"
#include "itkTranslationTransform.h"
#include "itkAzimuthElevationToCartesianTransform.h"
#include "itkElasticBodyReciprocalSplineKernelTransform.h"
#include "itkElasticBodySplineKernelTransform.h"
#include "itkThinPlateR2LogRSplineKernelTransform.h"
#include "itkThinPlateSplineKernelTransform.h"
#include "itkVolumeSplineKernelTransform.h"
#include "itkIntTypes.h"


// Generic Kernel Transform Tester
template<typename KernelType> int TestKernelTransform(const char *name, KernelType *)
{
  std::cout << name << std::flush;

  typedef typename KernelType::PointSetType KernelPointSetType;

  typename KernelType::Pointer kernel = KernelType::New();
  typename KernelPointSetType::Pointer targetLandmarks =
    KernelPointSetType::New ();
  typename KernelPointSetType::Pointer sourceLandmarks =
    KernelPointSetType::New ();

  targetLandmarks->GetPoints()->Reserve( 4 );
  sourceLandmarks->GetPoints()->Reserve( 4 );

  // Generate some random coordinates
  typename KernelPointSetType::CoordRepType randomCoords[3];
  for(int i=0; i < 4; ++i)
    {
    randomCoords[0] = (typename KernelPointSetType::CoordRepType)
      vnl_sample_uniform((double)-1.0,(double)1.0);
    randomCoords[1] = (typename KernelPointSetType::CoordRepType)
      vnl_sample_uniform((double)-1.0,(double)1.0);
    randomCoords[2] = (typename KernelPointSetType::CoordRepType)
      vnl_sample_uniform((double)-1.0,(double)1.0);
    targetLandmarks->GetPoints()->SetElement(i, randomCoords);

    randomCoords[0] = (typename KernelPointSetType::CoordRepType)
      vnl_sample_uniform((double)-1.0,(double)1.0);
    randomCoords[1] = (typename KernelPointSetType::CoordRepType)
      vnl_sample_uniform((double)-1.0,(double)1.0);
    randomCoords[2] = (typename KernelPointSetType::CoordRepType)
      vnl_sample_uniform((double)-1.0,(double)1.0);
    sourceLandmarks->GetPoints()->SetElement(i, randomCoords);
    }

  kernel->SetSourceLandmarks( sourceLandmarks );
  kernel->SetTargetLandmarks( targetLandmarks );

  itk::ModifiedTimeType beginMTime;
  itk::ModifiedTimeType endMTime;
  beginMTime = kernel->GetMTime();
  typename KernelType::ParametersType kernelParams = kernel->GetParameters();
  kernelParams[0] = 1.0;
  kernel->SetParameters( kernelParams );
  endMTime = kernel->GetMTime();
  if ( endMTime > beginMTime)
    {
    std::cout << "PASS" << std::endl;
    return 0;
    }
  else
    {
    std::cout << "FAIL" << std::endl;
    return 1;
    }
}

// Main Program
int itkTransformsSetParametersTest( int , char *[] )
{


  itk::ModifiedTimeType beginMTime;
  itk::ModifiedTimeType endMTime;


  std::cout << "Begin testing of SetParameters() method for all itkTransforms"
            << std::endl << std::endl;

  std::cout << "AffineTransform->SetParameters() - " << std::flush;
  typedef itk::AffineTransform< double, 3 > Affine;
  Affine::Pointer affine = Affine::New();
  beginMTime = affine->GetMTime();
  Affine::ParametersType affineParams = affine->GetParameters();
  affineParams[0] = 1.0;
  affine->SetParameters( affineParams );
  endMTime = affine->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;


  std::cout << "CenteredAffineTransform->SetParameters() - " << std::flush;
  typedef itk::CenteredAffineTransform< double, 3 > CenteredAffine;
  CenteredAffine::Pointer centeredAffine =
    CenteredAffine::New();
  beginMTime = centeredAffine->GetMTime();
  CenteredAffine::ParametersType centeredAffineParams =
    centeredAffine->GetParameters();
  centeredAffineParams[0] = 1.0;
  centeredAffine->SetParameters( centeredAffineParams );
  endMTime = centeredAffine->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "CenteredEuler3DTransform->SetParameters() - " << std::flush;
  typedef itk::CenteredEuler3DTransform< double > CenteredEuler3D;
  CenteredEuler3D::Pointer centeredEuler3D =
    CenteredEuler3D::New();
  beginMTime = centeredEuler3D->GetMTime();
  CenteredEuler3D::ParametersType centeredEuler3DParams =
    centeredEuler3D->GetParameters();
  centeredEuler3DParams[0] = 1.0;
  centeredEuler3D->SetParameters( centeredEuler3DParams );
  endMTime = centeredEuler3D->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "CenteredRigid2DTransform->SetParameters() - " << std::flush;
  typedef itk::CenteredRigid2DTransform< double > CenteredRigid2D;
  CenteredRigid2D::Pointer centeredRigid2D =
    CenteredRigid2D::New();
  beginMTime = centeredRigid2D->GetMTime();
  CenteredRigid2D::ParametersType centeredRigid2DParams =
    centeredRigid2D->GetParameters();
  centeredRigid2DParams[0] = 1.0;
  centeredRigid2D->SetParameters( centeredRigid2DParams );
  endMTime = centeredRigid2D->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "CenteredSimilarity2DTransform->SetParameters() - " << std::flush;
  typedef itk::CenteredSimilarity2DTransform< double > CenteredSimilarity2D;
  CenteredSimilarity2D::Pointer centeredSimilarity2D =
    CenteredSimilarity2D::New();
  beginMTime = centeredSimilarity2D->GetMTime();
  CenteredSimilarity2D::ParametersType centeredSimilarity2DParams =
    centeredSimilarity2D->GetParameters();
  centeredSimilarity2DParams[0] = 1.0;
  centeredSimilarity2D->SetParameters( centeredSimilarity2DParams );
  endMTime = centeredSimilarity2D->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "Euler2DTransform->SetParameters() - " << std::flush;
  typedef itk::Euler2DTransform< double > Euler2D;
  Euler2D::Pointer euler2D =
    Euler2D::New();
  beginMTime = euler2D->GetMTime();
  Euler2D::ParametersType euler2DParams =
    euler2D->GetParameters();
  euler2DParams[0] = 1.0;
  euler2D->SetParameters( euler2DParams );
  endMTime = euler2D->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "Euler3DTransform->SetParameters() - " << std::flush;
  typedef itk::Euler3DTransform< double > Euler3D;
  Euler3D::Pointer euler3D =
    Euler3D::New();
  beginMTime = euler3D->GetMTime();
  Euler3D::ParametersType euler3DParams =
    euler3D->GetParameters();
  euler3DParams[0] = 1.0;
  euler3D->SetParameters( euler3DParams );
  endMTime = euler3D->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "FixedCenteredAffineTransform->SetParameters() - " << std::flush;
  typedef itk::FixedCenterOfRotationAffineTransform< double, 3 > FixedCenteredAffine;
  FixedCenteredAffine::Pointer fixedCenteredAffine =
    FixedCenteredAffine::New();
  beginMTime = fixedCenteredAffine->GetMTime();
  FixedCenteredAffine::ParametersType fixedCenteredAffineParams =
    fixedCenteredAffine->GetParameters();
  fixedCenteredAffineParams[0] = 1.0;
  fixedCenteredAffine->SetParameters( fixedCenteredAffineParams );
  endMTime = fixedCenteredAffine->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;


  std::cout << "QuaternionRigidTransform->SetParameters() - " << std::flush;
  typedef itk::QuaternionRigidTransform< double > QuaternionRigid;
  QuaternionRigid::Pointer quaternionRigid =
    QuaternionRigid::New();
  beginMTime = quaternionRigid->GetMTime();
  QuaternionRigid::ParametersType quaternionRigidParams =
    quaternionRigid->GetParameters();
  quaternionRigidParams[0] = 1.0;
  quaternionRigid->SetParameters( quaternionRigidParams );
  endMTime = quaternionRigid->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "Rigid2DTransform->SetParameters() - " << std::flush;
  typedef itk::Rigid2DTransform< double > Rigid2D;
  Rigid2D::Pointer rigid2D =
    Rigid2D::New();
  beginMTime = rigid2D->GetMTime();
  Rigid2D::ParametersType rigid2DParams =
    rigid2D->GetParameters();
  rigid2DParams[0] = 1.0;
  rigid2D->SetParameters( rigid2DParams );
  endMTime = rigid2D->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;


  std::cout << "Rigid3DPerspectiveTransform->SetParameters() - " << std::flush;
  typedef itk::Rigid3DPerspectiveTransform< double > Rigid3DPerspective;
  Rigid3DPerspective::Pointer rigid3DPerspective =
    Rigid3DPerspective::New();
  beginMTime = rigid3DPerspective->GetMTime();
  Rigid3DPerspective::ParametersType rigid3DPerspectiveParams =
    rigid3DPerspective->GetParameters();
  rigid3DPerspectiveParams[0] = 1.0;
  rigid3DPerspective->SetParameters( rigid3DPerspectiveParams );
  endMTime = rigid3DPerspective->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "ScalableAffineTransform->SetParameters() - " << std::flush;
  typedef itk::ScalableAffineTransform< double, 3 > ScalableAffine;
  ScalableAffine::Pointer scalableAffine =
    ScalableAffine::New();
  beginMTime = scalableAffine->GetMTime();
  ScalableAffine::ParametersType scalableAffineParams =
    scalableAffine->GetParameters();
  scalableAffineParams[0] = 1.0;
  scalableAffine->SetParameters( scalableAffineParams );
  endMTime = scalableAffine->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "ScaleLogarithmicTransform->SetParameters() - " << std::flush;
  typedef itk::ScaleLogarithmicTransform< double, 3 > ScaleLogarithmic;
  ScaleLogarithmic::Pointer scaleLogarithmic =
    ScaleLogarithmic::New();
  beginMTime = scaleLogarithmic->GetMTime();
  ScaleLogarithmic::ParametersType scaleLogarithmicParams =
    scaleLogarithmic->GetParameters();
  scaleLogarithmicParams[0] = 1.0;
  scaleLogarithmic->SetParameters( scaleLogarithmicParams );
  endMTime = scaleLogarithmic->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "ScaleSkewVersor3DTransform->SetParameters() - " << std::flush;
  typedef itk::ScaleSkewVersor3DTransform< double > ScaleSkewVersor3D;
  ScaleSkewVersor3D::Pointer scaleSkewVersor3D =
    ScaleSkewVersor3D::New();
  beginMTime = scaleSkewVersor3D->GetMTime();
  ScaleSkewVersor3D::ParametersType scaleSkewVersor3DParams =
    scaleSkewVersor3D->GetParameters();
  scaleSkewVersor3DParams[0] = 1.0;
  scaleSkewVersor3D->SetParameters( scaleSkewVersor3DParams );
  endMTime = scaleSkewVersor3D->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "ScaleTransform->SetParameters() - " << std::flush;
  typedef itk::ScaleTransform< double, 3 > Scale;
  Scale::Pointer scale =
    Scale::New();
  beginMTime = scale->GetMTime();
  Scale::ParametersType scaleParams =
    scale->GetParameters();
  scaleParams[0] = 1.0;
  scale->SetParameters( scaleParams );
  endMTime = scale->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "Similarity2DTransform->SetParameters() - " << std::flush;
  typedef itk::Similarity2DTransform< double > Similarity2D;
  Similarity2D::Pointer similarity2D =
    Similarity2D::New();
  beginMTime = similarity2D->GetMTime();
  Similarity2D::ParametersType similarity2DParams =
    similarity2D->GetParameters();
  similarity2DParams[0] = 1.0;
  similarity2D->SetParameters( similarity2DParams );
  endMTime = similarity2D->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "Similarity3DTransform->SetParameters() - " << std::flush;
  typedef itk::Similarity3DTransform< double > Similarity3D;
  Similarity3D::Pointer similarity3D =
    Similarity3D::New();
  beginMTime = similarity3D->GetMTime();
  Similarity3D::ParametersType similarity3DParams =
    similarity3D->GetParameters();
  similarity3DParams[0] = 1.0;
  similarity3D->SetParameters( similarity3DParams );
  endMTime = similarity3D->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "TranslationTransform->SetParameters() - " << std::flush;
  typedef itk::TranslationTransform< double, 3 > Translation;
  Translation::Pointer translation =
    Translation::New();
  beginMTime = translation->GetMTime();
  Translation::ParametersType translationParams =
    translation->GetParameters();
  translationParams[0] = 1.0;
  translation->SetParameters( translationParams );
  endMTime = translation->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "VersorTransform->SetParameters() - " << std::flush;
  typedef itk::VersorTransform< double > Versor;
  Versor::Pointer versor =
    Versor::New();
  beginMTime = versor->GetMTime();
  Versor::ParametersType versorParams =
    versor->GetParameters();
  versorParams[0] = 1.0;
  versor->SetParameters( versorParams );
  endMTime = versor->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "AzimuthElevationToCartesianTransform->SetParameters() - "
            << std::flush;
  typedef itk::AzimuthElevationToCartesianTransform< double, 3 >
    AzimuthElevationToCartesian;
  AzimuthElevationToCartesian::Pointer azimuthElevation =
    AzimuthElevationToCartesian::New();
  beginMTime = azimuthElevation->GetMTime();
  AzimuthElevationToCartesian::ParametersType azimuthElevationParams =
    azimuthElevation->GetParameters();
  azimuthElevationParams[0] = 1.0;
  azimuthElevation->SetParameters( azimuthElevationParams );
  endMTime = azimuthElevation->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;

  std::cout << "VersorRigid3DTransform->SetParameters() - " << std::flush;
  typedef itk::VersorRigid3DTransform< double > VersorRigid3D;
  VersorRigid3D::Pointer versorRigid3D =
    VersorRigid3D::New();
  beginMTime = versorRigid3D->GetMTime();
  VersorRigid3D::ParametersType versorRigid3DParams =
    versorRigid3D->GetParameters();
  versorRigid3DParams[0] = 1.0;
  versorRigid3D->SetParameters( versorRigid3DParams );
  endMTime = versorRigid3D->GetMTime();
  if ( endMTime > beginMTime)
    std::cout << "PASS" << std::endl;
  else
    std::cout << "FAIL" << std::endl;


  std::cout << "BSplineTransform->SetParameters() - Not Tested (manual check indicates PASS)"
            << std::endl;
//    typedef itk::BSplineTransform< double > BSplineDeformable;
//    BSplineDeformable::Pointer bSplineDeformable = BSplineDeformable::New();
//    beginMTime = bSplineDeformable->GetMTime();
//    bSplineDeformable->SetIdentity();
//    BSplineDeformable::ParametersType bSplineDeformableParams; = bSplineDeformable->GetParameters();
//    bSplineDeformableParams[0] = 1.0;
//    bSplineDeformable->SetParameters( bSplineDeformableParams );
//   endMTime = bSplineDeformable->GetMTime();
//   if ( endMTime > beginMTime)
//     std::cout << "PASS" << std::endl;
//   else
//     std::cout << "FAIL" << std::endl;

  TestKernelTransform
    ("ElasticBodyReciprocalSplineKernelTransform->SetParameters() -",
     static_cast<itk::ElasticBodyReciprocalSplineKernelTransform<double,3> *>(ITK_NULLPTR));
  TestKernelTransform
    ("ElasticBodySplineKernelTransform->SetParameters() - ",
     static_cast<itk::ElasticBodySplineKernelTransform< double, 3 > *>(ITK_NULLPTR));

  TestKernelTransform
    ("KernelTransform->SetParameters() - ",
     static_cast<itk::KernelTransform< double, 3 > *>(ITK_NULLPTR));

  TestKernelTransform
    ("ThinPlateR2LogRSplineKernelTransform->SetParameters() - ",
     static_cast<itk::ThinPlateR2LogRSplineKernelTransform< double, 3 > *>(ITK_NULLPTR));

  TestKernelTransform
    ("ThinPlateSplineKernelTransform->SetParameters() - ",
     static_cast<itk::ThinPlateSplineKernelTransform< double, 3 > *>(ITK_NULLPTR));

  TestKernelTransform
    ("VolumeSplineKernelTransform->SetParameters() - ",
     static_cast<itk::VolumeSplineKernelTransform< double, 3 > *>(ITK_NULLPTR));

  std::cout << std::endl << "Done." << std::endl;

  return EXIT_SUCCESS;

}
