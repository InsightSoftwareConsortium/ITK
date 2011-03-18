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

#include "itkBloxImage.txx"
#include "itkBloxPixel.txx"
#include "itkBloxItem.h"
#include "itkBinaryMedialNodeMetric.txx"
#include "itkBloxCoreAtomItem.txx"
#include "itkCoreAtomImageToDistanceMatrixProcess.txx"
#include "itkCorrespondingList.txx"
#include "itkBloxBoundaryPointToCoreAtomImageFilter.h"
#include "itkMedialNodeTripletCorrespondenceProcess.txx"
#include "itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter.txx"
#include "itkBloxBoundaryPointItem.txx"
#include "itkCorrespondenceDataStructure.txx"
#include "itkBloxCoreAtomImage.txx"
#include "itkBloxBoundaryPointImage.txx"
#include "itkUnaryMedialNodeMetric.h"
#include "itkSecondaryNodeList.txx"
#include "itkBloxCoreAtomPixel.txx"
#include "itkBloxBoundaryPointPixel.txx"
#include "itkGradientImageToBloxBoundaryPointImageFilter.txx"
#include "itkCorrespondingMedialNodeClique.txx"
#include "itkUnaryMedialNodeMetric.txx"
#include "itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilter.txx"
#include "itkNodeList.txx"
#include "itkBloxBoundaryPointToCoreAtomImageFilter.txx"
#include "itkBloxBoundaryProfileItem.h"
#include "itkBloxBoundaryPointImage.h"
#include "itkBloxBoundaryPointPixel.h"
#include "itkMedialNodePairCorrespondenceProcess.txx"
#include "itkBloxBoundaryProfileImage.txx"
#include "itkBloxBoundaryProfilePixel.txx"
#include "itkCoreAtomImageToUnaryCorrespondenceMatrixProcess.txx"
#include "itkBloxBoundaryProfileItem.txx"



int itkBloxHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
