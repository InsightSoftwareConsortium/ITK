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

#include "itkNthElementPixelAccessor.h"
#include "itkExpImageAdaptor.h"
#include "itkBluePixelAccessor.h"
#include "itkRGBToVectorImageAdaptor.h"
#include "itkVectorImageToImageAdaptor.h"
#include "itkPixelAccessor.h"
#include "itkLog10ImageAdaptor.h"
#include "itkRGBToLuminanceImageAdaptor.h"
#include "itkImageAdaptor.h"
#include "itkComplexToModulusImageAdaptor.h"
#include "itkAbsImageAdaptor.h"
#include "itkAsinImageAdaptor.h"
#include "itkRGBToVectorPixelAccessor.h"
#include "itkVectorToRGBPixelAccessor.h"
#include "itkSqrtImageAdaptor.h"
#include "itkVectorToRGBImageAdaptor.h"
#include "itkAcosImageAdaptor.h"
#include "itkSinImageAdaptor.h"
#include "itkComplexToPhaseImageAdaptor.h"
#include "itkAtanImageAdaptor.h"
#include "itkRedPixelAccessor.h"
#include "itkAddImageAdaptor.h"
#include "itkCosImageAdaptor.h"
#include "itkGreenPixelAccessor.h"
#include "itkTanImageAdaptor.h"
#include "itkComplexToImaginaryImageAdaptor.h"
#include "itkComplexToRealImageAdaptor.h"
#include "itkLogImageAdaptor.h"
#include "itkExpNegativeImageAdaptor.h"
#include "itkAddPixelAccessor.h"
#include "itkNthElementImageAdaptor.h"



int itkImageAdaptorsHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
