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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkAffineTransform.h"
#include "itkAffineTransform.txx"
#include "itkAzimuthElevationToCartesianTransform.h"
#include "itkAzimuthElevationToCartesianTransform.txx"
#include "itkBSplineDeformableTransform.h"
#include "itkBSplineDeformableTransform.txx"
#include "itkCenteredAffineTransform.h"
#include "itkCenteredAffineTransform.txx"
#include "itkCenteredEuler3DTransform.h"
#include "itkCenteredEuler3DTransform.txx"
#include "itkCenteredRigid2DTransform.h"
#include "itkCenteredRigid2DTransform.txx"
#include "itkCenteredSimilarity2DTransform.h"
#include "itkCenteredSimilarity2DTransform.txx"
//#include "itkCenteredTransformInitializer.h"   BUG 11909
//#include "itkCenteredTransformInitializer.txx"
//#include "itkCenteredVersorTransformInitializer.h"
//#include "itkCenteredVersorTransformInitializer.txx"
#include "itkElasticBodyReciprocalSplineKernelTransform.h"
#include "itkElasticBodyReciprocalSplineKernelTransform.txx"
#include "itkElasticBodySplineKernelTransform.h"
#include "itkElasticBodySplineKernelTransform.txx"
#include "itkEuler2DTransform.h"
#include "itkEuler2DTransform.txx"
#include "itkEuler3DTransform.h"
#include "itkEuler3DTransform.txx"
#include "itkFixedCenterOfRotationAffineTransform.h"
#include "itkFixedCenterOfRotationAffineTransform.txx"
#include "itkIdentityTransform.h"
#include "itkKernelTransform.h"
#include "itkKernelTransform.txx"
#include "itkLandmarkBasedTransformInitializer.h"
#include "itkLandmarkBasedTransformInitializer.txx"
#include "itkMatlabTransformIO.h"
#include "itkMatlabTransformIOFactory.h"
#include "itkMatrixOffsetTransformBase.h"
#include "itkMatrixOffsetTransformBase.txx"
#include "itkQuaternionRigidTransform.h"
#include "itkQuaternionRigidTransform.txx"
#include "itkRigid2DTransform.h"
#include "itkRigid2DTransform.txx"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkRigid3DPerspectiveTransform.txx"
#include "itkRigid3DTransform.h"
#include "itkRigid3DTransform.txx"
#include "itkScalableAffineTransform.h"
#include "itkScalableAffineTransform.txx"
#include "itkScaleLogarithmicTransform.h"
#include "itkScaleLogarithmicTransform.txx"
#include "itkScaleSkewVersor3DTransform.h"
#include "itkScaleSkewVersor3DTransform.txx"
#include "itkScaleTransform.h"
#include "itkScaleTransform.txx"
#include "itkScaleVersor3DTransform.h"
#include "itkScaleVersor3DTransform.txx"
#include "itkSimilarity2DTransform.h"
#include "itkSimilarity2DTransform.txx"
#include "itkSimilarity3DTransform.h"
#include "itkSimilarity3DTransform.txx"
#include "itkThinPlateR2LogRSplineKernelTransform.h"
#include "itkThinPlateR2LogRSplineKernelTransform.txx"
#include "itkThinPlateSplineKernelTransform.h"
#include "itkThinPlateSplineKernelTransform.txx"
#include "itkTransform.h"
#include "itkTransform.txx"
#include "itkTransformBase.h"
#include "itkTransformFactory.h"
#include "itkTransformFactoryBase.h"
#include "itkTransformFileReader.h"
#include "itkTransformFileWriter.h"
#include "itkTransformIOBase.h"
#include "itkTransformIOFactory.h"
#include "itkTranslationTransform.h"
#include "itkTranslationTransform.txx"
#include "itkTxtTransformIO.h"
#include "itkTxtTransformIOFactory.h"
#include "itkv3Rigid3DTransform.h"
#include "itkVersorRigid3DTransform.h"
#include "itkVersorRigid3DTransform.txx"
#include "itkVersorTransform.h"
#include "itkVersorTransform.txx"
#include "itkVolumeSplineKernelTransform.h"
#include "itkVolumeSplineKernelTransform.txx"



int itkTransformHeaderTest ( int , char ** )
{

  return EXIT_SUCCESS;
}
