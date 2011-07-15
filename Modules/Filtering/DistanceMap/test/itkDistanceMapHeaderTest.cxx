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

#include "itkReflectiveImageRegionConstIterator.hxx"
#include "itkReflectiveImageRegionIterator.hxx"
#include "itkApproximateSignedDistanceMapImageFilter.hxx"
#include "itkHausdorffDistanceImageFilter.hxx"
#include "itkSignedDanielssonDistanceMapImageFilter.hxx"
#include "itkDanielssonDistanceMapImageFilter.hxx"
#include "itkContourMeanDistanceImageFilter.hxx"
#include "itkDirectedHausdorffDistanceImageFilter.hxx"
#include "itkContourDirectedMeanDistanceImageFilter.hxx"
#include "itkIsoContourDistanceImageFilter.hxx"
#include "itkFastChamferDistanceImageFilter.h"
#include "itkSignedMaurerDistanceMapImageFilter.hxx"
#include "itkFastChamferDistanceImageFilter.hxx"



int itkDistanceMapHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
