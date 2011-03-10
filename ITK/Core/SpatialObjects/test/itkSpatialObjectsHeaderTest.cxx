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

#include "itkSpatialObjectFactory.h"
#include "itkMetaMeshConverter.txx"
#include "itkMetaImageConverter.txx"
#include "itkMetaBlobConverter.txx"
#include "itkMetaVesselTubeConverter.txx"
#include "itkSurfaceSpatialObjectPoint.txx"
#include "itkDTITubeSpatialObject.txx"
#include "itkPolygonGroupSpatialObject.txx"
#include "itkCylinderSpatialObject.h"
#include "itkPlaneSpatialObject.txx"
#include "itkLineSpatialObject.txx"
#include "itkMetaGaussianConverter.txx"
#include "itkPointBasedSpatialObject.txx"
#include "itkSpatialObjectToPointSetFilter.h"
#include "itkSpatialObjectToImageFilter.txx"
#include "itkMetaGroupConverter.txx"
#include "itkSpatialObjectToImageStatisticsCalculator.h"
#include "itkBlobSpatialObject.h"
#include "itkMetaLandmarkConverter.txx"
#include "itkMetaEvent.h"
#include "itkEllipseSpatialObject.h"
#include "itkAffineGeometryFrame.h"
#include "itkSpatialObjectTreeContainer.h"
#include "itkMetaDTITubeConverter.txx"
#include "itkMetaSurfaceConverter.txx"
#include "itkSceneSpatialObject.txx"
#include "itkMeshSpatialObject.txx"
#include "itkSpatialObjectToImageStatisticsCalculator.txx"
#include "itkTubeSpatialObjectPoint.txx"
#include "itkSpatialObjectProperty.txx"
#include "itkSpatialObject.txx"
#include "itkSpatialObjectTreeNode.txx"
#include "itkMetaEllipseConverter.h"
#include "itkMetaContourConverter.h"
#include "itkLandmarkSpatialObject.txx"
#include "itkSpatialObjectPoint.h"
#include "itkSceneSpatialObject.h"
#include "itkLineSpatialObjectPoint.txx"
#include "itkContourSpatialObject.txx"
#include "itkVesselTubeSpatialObject.txx"
#include "itkPolygonSpatialObject.h"
#include "itkSurfaceSpatialObject.txx"
#include "itkImageSpatialObject.txx"
#include "itkSpatialObjectTreeContainer.txx"
#include "itkMetaSceneConverter.txx"
#include "itkEllipseSpatialObject.txx"
#include "itkDTITubeSpatialObjectPoint.txx"
#include "itkSpatialObjectDuplicator.h"
#include "itkBoxSpatialObject.txx"
#include "itkPolygonSpatialObject.txx"
#include "itkSpatialObjectFactoryBase.h"
#include "itkContourSpatialObjectPoint.txx"
#include "itkGaussianSpatialObject.txx"
#include "itkSpatialObjectToPointSetFilter.txx"
#include "itkImageMaskSpatialObject.txx"
#include "itkSpatialObjectDuplicator.txx"
#include "itkSpatialObjectProperty.h"
#include "itkGroupSpatialObject.txx"
#include "itkArrowSpatialObject.txx"
#include "itkMetaContourConverter.txx"
#include "itkMetaTubeConverter.txx"
#include "itkBlobSpatialObject.txx"
#include "itkTubeSpatialObject.txx"
#include "itkMetaEllipseConverter.txx"
#include "itkMetaArrowConverter.txx"
#include "itkSpatialObjectPoint.txx"
#include "itkMetaLineConverter.txx"
#include "itkAffineGeometryFrame.txx"
#include "itkVesselTubeSpatialObjectPoint.txx"



int itkSpatialObjectsHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
