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

#include "itkGeodesicActiveContourShapePriorLevelSetFunction.hxx"
#include "itkShapePriorSegmentationLevelSetFunction.hxx"
#include "itkBinaryMaskToNarrowBandPointSetFilter.hxx"
#include "itkVectorThresholdSegmentationLevelSetFunction.hxx"
#include "itkParallelSparseFieldLevelSetImageFilter.hxx"
#include "itkSegmentationLevelSetImageFilter.hxx"
#include "itkThresholdSegmentationLevelSetFunction.hxx"
#include "itkImplicitManifoldNormalVectorFilter.hxx"
#include "itkCurvesLevelSetImageFilter.hxx"
#include "itkShapeDetectionLevelSetImageFilter.h"
#include "itkVectorThresholdSegmentationLevelSetImageFilter.hxx"
#include "itkShapeDetectionLevelSetFunction.h"
#include "itkSparseFieldFourthOrderLevelSetImageFilter.hxx"
#include "itkCurvesLevelSetFunction.hxx"
#include "itkLevelSetNode.h"
#include "itkLevelSetFunction.hxx"
#include "itkNarrowBandLevelSetImageFilter.hxx"
#include "itkIsotropicFourthOrderLevelSetImageFilter.h"
#include "itkLaplacianSegmentationLevelSetImageFilter.hxx"
#include "itkShapePriorMAPCostFunctionBase.hxx"
#include "itkGeodesicActiveContourLevelSetImageFilter.hxx"
#include "itkExtensionVelocitiesImageFilter.hxx"
#include "itkGeodesicActiveContourLevelSetFunction.hxx"
#include "itkLevelSetNeighborhoodExtractor.hxx"
#include "itkNormalVectorFunctionBase.h"
#include "itkCannySegmentationLevelSetFunction.h"
#include "itkCollidingFrontsImageFilter.hxx"
#include "itkShapeDetectionLevelSetImageFilter.hxx"
#include "itkLaplacianSegmentationLevelSetFunction.hxx"
#include "itkAnisotropicFourthOrderLevelSetImageFilter.hxx"
#include "itkThresholdSegmentationLevelSetImageFilter.hxx"
#include "itkNormalVectorFunctionBase.hxx"
#include "itkLevelSet.h"
#include "itkSparseFieldLevelSetImageFilter.h"
#include "itkNormalVectorDiffusionFunction.hxx"
#include "itkGeodesicActiveContourShapePriorLevelSetImageFilter.hxx"
#include "itkSegmentationLevelSetFunction.hxx"
#include "itkShapePriorMAPCostFunction.hxx"
#include "itkCannySegmentationLevelSetFunction.hxx"
#include "itkLevelSetFunctionWithRefitTerm.hxx"
#include "itkReinitializeLevelSetImageFilter.hxx"
#include "itkIsotropicFourthOrderLevelSetImageFilter.hxx"
#include "itkNarrowBandThresholdSegmentationLevelSetImageFilter.hxx"
#include "itkShapePriorSegmentationLevelSetImageFilter.hxx"
#include "itkLevelSetVelocityNeighborhoodExtractor.hxx"
#include "itkSparseFieldLevelSetImageFilter.hxx"
#include "itkNarrowBandCurvesLevelSetImageFilter.hxx"
#include "itkShapeDetectionLevelSetFunction.hxx"
#include "itkCannySegmentationLevelSetImageFilter.hxx"
#include "itkImplicitManifoldNormalVectorFilter.h"
#include "itkUnsharpMaskLevelSetImageFilter.hxx"



int itkLevelSetsHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
