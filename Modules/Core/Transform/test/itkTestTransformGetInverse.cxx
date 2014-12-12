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
#include "itkAffineTransform.h"
#include "itkAzimuthElevationToCartesianTransform.h"
#include "itkBSplineBaseTransform.h"
#include "itkBSplineTransform.h"
#include "itkBSplineTransformInitializer.h"
#include "itkCenteredAffineTransform.h"
#include "itkCenteredEuler3DTransform.h"
#include "itkCenteredRigid2DTransform.h"
#include "itkCenteredSimilarity2DTransform.h"
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
#include "itkv3Rigid3DTransform.h"
#include "itkVersorRigid3DTransform.h"
#include "itkVersorTransform.h"
#include "itkVolumeSplineKernelTransform.h"
#include "itkMultiThreader.h"
#include <iostream>

template <typename TTransform>
struct ThreadData
{
  typename TTransform::Pointer m_Transform;
  typename TTransform::Pointer m_Inverse;
};

template <typename TTransform>
ITK_THREAD_RETURN_TYPE
TestGetInverseThreadFunction(void *perThreadData)
{
  itk::MultiThreader::ThreadInfoStruct *ti =
    static_cast<itk::MultiThreader::ThreadInfoStruct *>(perThreadData);
  ThreadData<TTransform> *td = static_cast<ThreadData<TTransform> *>(ti->UserData);
  for(unsigned int i = 0; i < 100000; ++i)
    {
    td->m_Transform->GetInverse(td->m_Inverse.GetPointer());
    }

  return ITK_THREAD_RETURN_VALUE;
}

template <typename TTransform>
unsigned
TransformTest()
{
  typename itk::MultiThreader::Pointer threader = itk::MultiThreader::New();

  ThreadData<TTransform> td;
  td.m_Transform  = TTransform::New();
  td.m_Inverse = TTransform::New();
  std::cout << "Testing " << td.m_Transform->GetNameOfClass() << std::endl;
  itk::ThreadFunctionType pFunc = TestGetInverseThreadFunction<TTransform>;
  threader->SetSingleMethod( pFunc, &td);
  try
    {
    threader->SingleMethodExecute();
    }
  catch( itk::ExceptionObject excp )
    {
    std::cerr << "ITK Exception " << excp.what() << std::endl;
    return 1;
    }
  catch(...)
    {
    std::cerr << "Unknown exception" << std::endl;
    }
  return 0;
}

int itkTestTransformGetInverse(int, char *[])
{
  unsigned errorCount;
  errorCount = TransformTest< itk::AffineTransform<double, 3> >();
  errorCount += TransformTest< itk::AzimuthElevationToCartesianTransform<double,3> >();
  errorCount += TransformTest< itk::BSplineTransform<double, 3> >();
  errorCount += TransformTest< itk::CenteredAffineTransform<double, 3> >();
  errorCount += TransformTest< itk::CenteredEuler3DTransform<double> >();
  errorCount += TransformTest< itk::CenteredAffineTransform<double, 3> >();
  errorCount += TransformTest< itk::CenteredRigid2DTransform<double> >();
  errorCount += TransformTest< itk::CenteredSimilarity2DTransform<double> >();
  errorCount += TransformTest< itk::CompositeTransform<double, 3> >();
  errorCount += TransformTest< itk::ElasticBodyReciprocalSplineKernelTransform<double,3> >();
  errorCount += TransformTest< itk::ElasticBodySplineKernelTransform<double,3> >();
  errorCount += TransformTest< itk::Euler2DTransform<double> >();
  errorCount += TransformTest< itk::Euler3DTransform<double> >();
  errorCount += TransformTest< itk::FixedCenterOfRotationAffineTransform<double,3> >();
  errorCount += TransformTest< itk::IdentityTransform<double,3> >();
  errorCount += TransformTest< itk::QuaternionRigidTransform<double> >();
  errorCount += TransformTest< itk::Rigid2DTransform<double> >();
  errorCount += TransformTest< itk::ScalableAffineTransform<double,3> >();
  errorCount += TransformTest< itk::ScaleLogarithmicTransform<double,3> >();
  errorCount += TransformTest< itk::ScaleTransform<double,3> >();
  //
  // ScaleVersor3DTransform can't apparently get an inverse. Gets this
  // error message:
  // /scratch/kent/itktest/ITK/Modules/Core/Transform/include/itkScaleVersor3DTransform.hxx:236:
  // itk::ERROR: ScaleVersor3DTransform(0x1757820): Setting the matrix
  // of a ScaleVersor3D
  // transform is not supported at this time.
  // errorCount += TransformTest< itk::ScaleVersor3DTransform<double> >();
  errorCount += TransformTest< itk::Similarity2DTransform<double> >();
  errorCount += TransformTest< itk::Similarity3DTransform<double> >();
  errorCount += TransformTest< itk::ThinPlateR2LogRSplineKernelTransform<double,3> >();
  errorCount += TransformTest< itk::ThinPlateSplineKernelTransform<double,3> >();
  errorCount += TransformTest< itk::TranslationTransform<double,3> >();
  errorCount += TransformTest< itk::VersorRigid3DTransform<double> >();
  errorCount += TransformTest< itk::VersorTransform<double> >();
  errorCount += TransformTest< itk::VolumeSplineKernelTransform<double,3> >();
  if(errorCount > 0)
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
