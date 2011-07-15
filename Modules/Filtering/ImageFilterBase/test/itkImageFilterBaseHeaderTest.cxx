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

#include "itkBinaryFunctorImageFilter.hxx"
#include "itkBoxImageFilter.hxx"
#include "itkCastImageFilter.h"
#include "itkClampImageFilter.h"
#include "itkFilterWatcher.h"
#include "itkGaussianImageSource.hxx"
#include "itkMaskNeighborhoodOperatorImageFilter.hxx"
#include "itkNeighborhoodOperatorImageFilter.hxx"
#include "itkNoiseImageFilter.hxx"
#include "itkNullImageToImageFilterDriver.hxx"
#include "itkRecursiveSeparableImageFilter.hxx"
#include "itkTernaryFunctorImageFilter.hxx"
#include "itkVectorCastImageFilter.h"
#include "itkVectorNeighborhoodOperatorImageFilter.hxx"

#include "itkPipelineMonitorImageFilter.h"

int itkImageFilterBaseHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
