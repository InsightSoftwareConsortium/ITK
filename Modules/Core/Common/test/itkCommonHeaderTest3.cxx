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

#include "itkKernelFunction.h"
#include "itkLaplacianOperator.hxx"
#include "itkLeafTreeIterator.h"
#include "itkLevelOrderTreeIterator.hxx"
#include "itkLightObject.h"
#include "itkLightProcessObject.h"
#include "itkLineCell.hxx"
#include "itkLineConstIterator.hxx"
#include "itkLineIterator.hxx"
#include "itkLogger.h"
#include "itkLoggerBase.h"
#include "itkLoggerManager.h"
#include "itkLoggerOutput.h"
#include "itkLoggerThreadWrapper.hxx"
#include "itkLogOutput.h"
#include "itkMacro.h"
#include "itkMapContainer.hxx"
#include "itkMath.h"
#include "itkMathDetail.h"
#include "itkMatrix.hxx"
#include "itkMatrixResizeableDataObject.hxx"
#include "itkMemoryProbe.h"
#include "itkMemoryProbesCollectorBase.h"
#include "itkMemoryUsageObserver.h"
#include "itkMetaDataDictionary.h"
#include "itkMetaDataObject.hxx"
#include "itkMetaDataObjectBase.h"
#include "itkMinimumMaximumImageCalculator.hxx"
#include "itkMultipleLogOutput.h"
#include "itkMultiThreader.h"
#include "itkMutexLock.h"
#include "itkMutexLockHolder.h"
#include "itkNeighborhood.hxx"
#include "itkNeighborhoodAccessorFunctor.h"
#include "itkNeighborhoodAlgorithm.hxx"
#include "itkNeighborhoodAllocator.h"
#include "itkNeighborhoodInnerProduct.hxx"
#include "itkNeighborhoodIterator.hxx"
#include "itkNeighborhoodIteratorTestCommon.hxx"
#include "itkNeighborhoodOperator.hxx"
#include "itkNumericTraits.h"
#include "itkNumericTraitsArrayPixel.h"
#include "itkNumericTraitsCovariantVectorPixel.h"
#include "itkNumericTraitsDiffusionTensor3DPixel.h"
#include "itkNumericTraitsFixedArrayPixel.h"
#include "itkNumericTraitsPointPixel.h"
#include "itkNumericTraitsRGBAPixel.h"
#include "itkNumericTraitsRGBPixel.h"
#include "itkNumericTraitsStdVector.h"
#include "itkNumericTraitsTensorPixel.h"
#include "itkNumericTraitsVariableLengthVectorPixel.h"
#include "itkNumericTraitsVectorPixel.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkObjectFactoryBase.h"
#include "itkObjectStore.hxx"
#include "itkOctree.hxx"
#include "itkOctreeNode.h"
#include "itkOffset.h"
#include "itkOrientationAdapterBase.h"
#include "itkOutputWindow.h"
#include "itkParametricImageSource.hxx"
#include "itkPeriodicBoundaryCondition.hxx"
#include "itkPhasedArray3DSpecialCoordinatesImage.hxx"
#include "itkPixelTraits.h"
#include "itkPoint.hxx"
#include "itkPointSet.hxx"
#include "itkPointSetToImageFilter.hxx"
#include "itkPolygonCell.hxx"
#include "itkPostOrderTreeIterator.h"
#include "itkPreOrderTreeIterator.h"
#include "itkPriorityQueueContainer.hxx"
#include "itkProcessObject.h"
#include "itkProgressAccumulator.h"
#include "itkProgressReporter.h"

int itkCommonHeaderTest3 ( int , char * [] )
{

  return EXIT_SUCCESS;
}
