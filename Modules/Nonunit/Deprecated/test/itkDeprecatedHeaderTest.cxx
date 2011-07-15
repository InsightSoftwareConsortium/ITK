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

#include "itkAnalyzeDbh.h"
#include "itkAnalyzeImageIO.h"
#include "itkAnalyzeImageIOFactory.h"
#include "itkAnalyzeImageIOTest.h"
#include "itkBalloonForceFilter.hxx"
#include "itkCompose2DCovariantVectorImageFilter.h"
#include "itkCompose2DVectorImageFilter.h"
#include "itkCompose3DCovariantVectorImageFilter.h"
#include "itkCompose3DVectorImageFilter.h"
#include "itkComposeRGBImageFilter.h"
#include "itkDefaultImageTraits.h"
#include "itkDeformableMesh3DFilter.hxx"
#include "itkDicomImageIO.h"
#include "itkDicomImageIOFactory.h"
#include "itkDICOMImageIO2.h"
#include "itkDICOMImageIO2Factory.h"
#include "itkDICOMSeriesFileNames.h"
#include "itkGradientToMagnitudeImageFilter.h"
#include "itkImageToVectorImageFilter.h"
#include "itkNonThreadedShrinkImageFilter.h"
#include "itkNonUniformBSpline.hxx"
#include "itkQuaternionOrientationAdapter.h"
#include "itkReflectImageFilter.hxx"
#include "itkScalarToArrayCastImageFilter.h"
#include "itkScalarVector.h"
#include "itkSemaphore.h"
#include "itkShiftScaleInPlaceImageFilter.hxx"
#include "itkTwoOutputExampleImageFilter.hxx"

#include "itkTestDriverIncludeAnalyzeIOFactory.h"
#include "itkTestDriverIncludeDeprecatedIOFactories.h"

int itkDeprecatedHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
