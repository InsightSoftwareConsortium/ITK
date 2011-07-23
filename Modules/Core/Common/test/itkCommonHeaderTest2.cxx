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

#include "itkImage.hxx"
#include "itkImageBase.hxx"
#include "itkImageBoundaryCondition.h"
#include "itkImageConstIterator.hxx"
#include "itkImageConstIteratorWithIndex.hxx"
#include "itkImageContainerInterface.h"
#include "itkImageDuplicator.hxx"
#include "itkImageHelper.h"
#include "itkImageIterator.hxx"
#include "itkImageIteratorWithIndex.hxx"
#include "itkImageKernelOperator.hxx"
#include "itkImageLinearConstIteratorWithIndex.hxx"
#include "itkImageLinearIteratorWithIndex.hxx"
//BUG 11903
//#include "itkImageRandomConstIteratorWithIndex.hxx"
//#include "itkImageRandomIteratorWithIndex.hxx"
//#include "itkImageRandomNonRepeatingConstIteratorWithIndex.hxx"
//#include "itkImageRandomNonRepeatingIteratorWithIndex.hxx"
#include "itkImageRegion.hxx"
#include "itkImageRegionConstIterator.hxx"
#include "itkImageRegionConstIteratorWithIndex.hxx"
#include "itkImageRegionExclusionConstIteratorWithIndex.hxx"
#include "itkImageRegionExclusionIteratorWithIndex.hxx"
#include "itkImageRegionIterator.hxx"
#include "itkImageRegionIteratorWithIndex.hxx"
#include "itkImageRegionMultidimensionalSplitter.hxx"
#include "itkImageRegionReverseConstIterator.hxx"
#include "itkImageRegionReverseIterator.hxx"
#include "itkImageRegionSplitter.hxx"
#include "itkImageReverseConstIterator.hxx"
#include "itkImageReverseIterator.hxx"
#include "itkImageSliceConstIteratorWithIndex.hxx"
#include "itkImageSliceIteratorWithIndex.hxx"
#include "itkImageSource.hxx"
#include "itkImageToImageFilter.hxx"
#include "itkImageToImageFilterDetail.h"
#include "itkImageTransformHelper.h"
#include "itkImportImageContainer.hxx"
#include "itkImportImageFilter.hxx"
#include "itkIndent.h"
#include "itkIndex.h"
#include "itkIndexedContainerInterface.h"
#include "itkInOrderTreeIterator.h"
#include "itkInPlaceImageFilter.hxx"
#include "itkInteriorExteriorSpatialFunction.hxx"
#include "itkIntTypes.h"
#include "itkIterationReporter.h"

int itkCommonHeaderTest2 ( int , char * [] )
{

  return EXIT_SUCCESS;
}
