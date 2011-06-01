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

#include "itkGeodesicActiveContourShapePriorLevelSetFunction.txx"
#include "itkShapePriorSegmentationLevelSetFunction.txx"
#include "itkBinaryMaskToNarrowBandPointSetFilter.txx"
#include "itkVectorThresholdSegmentationLevelSetFunction.txx"
#include "itkParallelSparseFieldLevelSetImageFilter.txx"
#include "itkSegmentationLevelSetImageFilter.txx"
#include "itkThresholdSegmentationLevelSetFunction.txx"
#include "itkImplicitManifoldNormalVectorFilter.txx"
#include "itkCurvesLevelSetImageFilter.txx"
#include "itkShapeDetectionLevelSetImageFilter.h"
#include "itkVectorThresholdSegmentationLevelSetImageFilter.txx"
#include "itkShapeDetectionLevelSetFunction.h"
#include "itkSparseFieldFourthOrderLevelSetImageFilter.txx"
#include "itkCurvesLevelSetFunction.txx"
#include "itkLevelSetNode.h"
#include "itkLevelSetFunction.txx"
#include "itkNarrowBandLevelSetImageFilter.txx"
#include "itkIsotropicFourthOrderLevelSetImageFilter.h"
#include "itkLaplacianSegmentationLevelSetImageFilter.txx"
#include "itkShapePriorMAPCostFunctionBase.txx"
#include "itkGeodesicActiveContourLevelSetImageFilter.txx"
#include "itkExtensionVelocitiesImageFilter.txx"
#include "itkGeodesicActiveContourLevelSetFunction.txx"
#include "itkLevelSetNeighborhoodExtractor.txx"
#include "itkNormalVectorFunctionBase.h"
#include "itkCannySegmentationLevelSetFunction.h"
#include "itkCollidingFrontsImageFilter.txx"
#include "itkShapeDetectionLevelSetImageFilter.txx"
#include "itkLaplacianSegmentationLevelSetFunction.txx"
#include "itkAnisotropicFourthOrderLevelSetImageFilter.txx"
#include "itkThresholdSegmentationLevelSetImageFilter.txx"
#include "itkNormalVectorFunctionBase.txx"
#include "itkLevelSet.h"
#include "itkSparseFieldLevelSetImageFilter.h"
#include "itkNormalVectorDiffusionFunction.txx"
#include "itkGeodesicActiveContourShapePriorLevelSetImageFilter.txx"
#include "itkSegmentationLevelSetFunction.txx"
#include "itkShapePriorMAPCostFunction.txx"
#include "itkCannySegmentationLevelSetFunction.txx"
#include "itkLevelSetFunctionWithRefitTerm.txx"
#include "itkReinitializeLevelSetImageFilter.txx"
#include "itkIsotropicFourthOrderLevelSetImageFilter.txx"
#include "itkNarrowBandThresholdSegmentationLevelSetImageFilter.txx"
#include "itkShapePriorSegmentationLevelSetImageFilter.txx"
#include "itkLevelSetVelocityNeighborhoodExtractor.txx"
#include "itkSparseFieldLevelSetImageFilter.txx"
#include "itkNarrowBandCurvesLevelSetImageFilter.txx"
#include "itkShapeDetectionLevelSetFunction.txx"
#include "itkCannySegmentationLevelSetImageFilter.txx"
#include "itkImplicitManifoldNormalVectorFilter.h"
#include "itkUnsharpMaskLevelSetImageFilter.txx"



int itkLevelSetsHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
