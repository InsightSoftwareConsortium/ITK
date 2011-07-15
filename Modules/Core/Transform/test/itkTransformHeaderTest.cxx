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

#include "itkAffineTransform.hxx"
#include "itkAzimuthElevationToCartesianTransform.hxx"
#include "itkBSplineDeformableTransform.hxx"
#include "itkCenteredAffineTransform.hxx"
#include "itkCenteredEuler3DTransform.hxx"
#include "itkCenteredRigid2DTransform.hxx"
#include "itkCenteredSimilarity2DTransform.hxx"
//BUG 11909
//#include "itkCenteredTransformInitializer.hxx"
//#include "itkCenteredVersorTransformInitializer.hxx"
#include "itkElasticBodyReciprocalSplineKernelTransform.hxx"
#include "itkElasticBodySplineKernelTransform.hxx"
#include "itkEuler2DTransform.hxx"
#include "itkEuler3DTransform.hxx"
#include "itkFixedCenterOfRotationAffineTransform.hxx"
#include "itkIdentityTransform.h"
#include "itkKernelTransform.hxx"
#include "itkLandmarkBasedTransformInitializer.hxx"
#include "itkMatlabTransformIO.h"
#include "itkMatlabTransformIOFactory.h"
#include "itkMatrixOffsetTransformBase.hxx"
#include "itkQuaternionRigidTransform.hxx"
#include "itkRigid2DTransform.hxx"
#include "itkRigid3DPerspectiveTransform.hxx"
#include "itkRigid3DTransform.hxx"
#include "itkScalableAffineTransform.hxx"
#include "itkScaleLogarithmicTransform.hxx"
#include "itkScaleSkewVersor3DTransform.hxx"
#include "itkScaleTransform.hxx"
#include "itkScaleVersor3DTransform.hxx"
#include "itkSimilarity2DTransform.hxx"
#include "itkSimilarity3DTransform.hxx"
#include "itkThinPlateR2LogRSplineKernelTransform.hxx"
#include "itkThinPlateSplineKernelTransform.hxx"
#include "itkTransform.hxx"
#include "itkTransformBase.h"
#include "itkTransformFactory.h"
#include "itkTransformFactoryBase.h"
#include "itkTransformFileReader.h"
#include "itkTransformFileWriter.h"
#include "itkTransformIOBase.h"
#include "itkTransformIOFactory.h"
#include "itkTranslationTransform.hxx"
#include "itkTxtTransformIO.h"
#include "itkTxtTransformIOFactory.h"
#include "itkv3Rigid3DTransform.h"
#include "itkVersorRigid3DTransform.hxx"
#include "itkVersorTransform.hxx"
#include "itkVolumeSplineKernelTransform.hxx"



int itkTransformHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
