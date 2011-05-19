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

#include "itkTestDriverIncludeAnalyzeIOFactory.h"
#include "itkAnalyzeDbh.h"
#include "itkScalarVector.h"
#include "itkDICOMImageIO2Factory.h"
#include "itkTestDriverIncludeDeprecatedIOFactories.h"
#include "itkNonThreadedShrinkImageFilter.h"
#include "itkDefaultImageTraits.h"
#include "itkDICOMSeriesFileNames.h"
#include "itkReflectImageFilter.txx"
#include "itkTwoOutputExampleImageFilter.txx"
#include "itkDicomImageIOFactory.h"
#include "itkShiftScaleInPlaceImageFilter.txx"
#include "itkAnalyzeImageIOTest.h"
#include "itkDeformableMesh3DFilter.txx"
#include "itkAnalyzeImageIO.h"
#include "itkNonUniformBSpline.txx"
#include "itkDICOMImageIO2.h"
#include "itkBalloonForceFilter.txx"
#include "itkDicomImageIO.h"
#include "itkAnalyzeImageIOFactory.h"
#include "itkQuaternionOrientationAdapter.h"
#include "itkCompose2DCovariantVectorImageFilter.h"
#include "itkCompose2DVectorImageFilter.h"
#include "itkCompose3DCovariantVectorImageFilter.h"
#include "itkCompose3DVectorImageFilter.h"
#include "itkComposeRGBImageFilter.h"
#include "itkImageToVectorImageFilter.h"
#include "itkScalarToArrayCastImageFilter.h"



int itkDeprecatedHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
