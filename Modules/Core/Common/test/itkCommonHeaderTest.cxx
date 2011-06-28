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

#include "itkAnnulusOperator.hxx"
#include "itkArray.hxx"
#include "itkArray2D.hxx"
#include "itkAutoPointer.h"
#include "itkAutoPointerDataObjectDecorator.hxx"
#include "itkBackwardDifferenceOperator.hxx"
#include "itkBarrier.h"
#include "itkBinaryThresholdSpatialFunction.hxx"
#include "itkBoundingBox.hxx"
#include "itkBresenhamLine.hxx"
#include "itkBSplineDerivativeKernelFunction.h"
#include "itkBSplineInterpolationWeightFunction.hxx"
#include "itkBSplineKernelFunction.h"
#include "itkByteSwapper.hxx"
#include "itkCellInterface.hxx"
#include "itkCellInterfaceVisitor.h"
#include "itkChildTreeIterator.hxx"
#include "itkColorTable.hxx"
#include "itkCommand.h"
#include "itkConceptChecking.h"
#include "itkConditionalConstIterator.hxx"
#include "itkConditionVariable.h"
#include "itkConicShellInteriorExteriorSpatialFunction.hxx"
#include "itkConstantBoundaryCondition.h"
#include "itkConstNeighborhoodIterator.hxx"
#include "itkConstShapedNeighborhoodIterator.hxx"
#include "itkConstSliceIterator.h"
#include "itkContinuousIndex.h"
#include "itkCorrespondenceDataStructureIterator.hxx"
#include "itkCovariantVector.hxx"
#include "itkCreateObjectFunction.h"
#include "itkCrossHelper.h"
#include "itkDataObject.h"
#include "itkDataObjectDecorator.hxx"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkDefaultPixelAccessor.h"
#include "itkDefaultPixelAccessorFunctor.h"
#include "itkDefaultStaticMeshTraits.h"
#include "itkDefaultVectorPixelAccessor.h"
#include "itkDefaultVectorPixelAccessorFunctor.h"
#include "itkDerivativeOperator.hxx"
#include "itkDiffusionTensor3D.hxx"
#include "itkDirectory.h"
#include "itkDynamicLoader.h"
#include "itkEllipsoidInteriorExteriorSpatialFunction.hxx"
#include "itkEquivalencyTable.h"
#include "itkEventObject.h"
#include "itkExceptionObject.h"
#include "itkExtractImageFilter.hxx"
#include "itkExtractImageFilterRegionCopier.h"
#include "itkFactoryTestLib.h"
#include "itkFastMutexLock.h"
#include "itkFileOutputWindow.h"
#include "itkFiniteCylinderSpatialFunction.hxx"
#include "itkFixedArray.hxx"
#include "itkFloatingPointExceptions.h"
#include "itkFloodFilledFunctionConditionalConstIterator.hxx"
#include "itkFloodFilledImageFunctionConditionalConstIterator.hxx"
#include "itkFloodFilledImageFunctionConditionalIterator.h"
#include "itkFloodFilledSpatialFunctionConditionalConstIterator.hxx"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"
#include "itkForwardDifferenceOperator.hxx"
#include "itkFrustumSpatialFunction.hxx"
#include "itkFunctionBase.h"
#include "itkGaussianDerivativeSpatialFunction.hxx"
#include "itkGaussianKernelFunction.h"
#include "itkGaussianOperator.hxx"
#include "itkGaussianSpatialFunction.hxx"
#include "itkHexahedronCell.hxx"
#include "itkHexahedronCellTopology.h"

int itkCommonHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
