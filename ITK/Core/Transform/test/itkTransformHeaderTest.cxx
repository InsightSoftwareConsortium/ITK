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

#include "itkAffineTransform.txx"
#include "itkAzimuthElevationToCartesianTransform.txx"
#include "itkBSplineDeformableTransform.txx"
#include "itkCenteredAffineTransform.txx"
#include "itkCenteredEuler3DTransform.txx"
#include "itkCenteredRigid2DTransform.txx"
#include "itkCenteredSimilarity2DTransform.txx"
//BUG 11909
//#include "itkCenteredTransformInitializer.txx"
//#include "itkCenteredVersorTransformInitializer.txx"
#include "itkElasticBodyReciprocalSplineKernelTransform.txx"
#include "itkElasticBodySplineKernelTransform.txx"
#include "itkEuler2DTransform.txx"
#include "itkEuler3DTransform.txx"
#include "itkFixedCenterOfRotationAffineTransform.txx"
#include "itkIdentityTransform.h"
#include "itkKernelTransform.txx"
#include "itkLandmarkBasedTransformInitializer.txx"
#include "itkMatlabTransformIO.h"
#include "itkMatlabTransformIOFactory.h"
#include "itkMatrixOffsetTransformBase.txx"
#include "itkQuaternionRigidTransform.txx"
#include "itkRigid2DTransform.txx"
#include "itkRigid3DPerspectiveTransform.txx"
#include "itkRigid3DTransform.txx"
#include "itkScalableAffineTransform.txx"
#include "itkScaleLogarithmicTransform.txx"
#include "itkScaleSkewVersor3DTransform.txx"
#include "itkScaleTransform.txx"
#include "itkScaleVersor3DTransform.txx"
#include "itkSimilarity2DTransform.txx"
#include "itkSimilarity3DTransform.txx"
#include "itkThinPlateR2LogRSplineKernelTransform.txx"
#include "itkThinPlateSplineKernelTransform.txx"
#include "itkTransform.txx"
#include "itkTransformBase.h"
#include "itkTransformFactory.h"
#include "itkTransformFactoryBase.h"
#include "itkTransformFileReader.h"
#include "itkTransformFileWriter.h"
#include "itkTransformIOBase.h"
#include "itkTransformIOFactory.h"
#include "itkTranslationTransform.txx"
#include "itkTxtTransformIO.h"
#include "itkTxtTransformIOFactory.h"
#include "itkv3Rigid3DTransform.h"
#include "itkVersorRigid3DTransform.txx"
#include "itkVersorTransform.txx"
#include "itkVolumeSplineKernelTransform.txx"



int itkTransformHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
