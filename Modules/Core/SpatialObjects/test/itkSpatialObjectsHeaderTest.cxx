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
#include "itkMetaMeshConverter.hxx"
#include "itkMetaImageConverter.hxx"
#include "itkMetaBlobConverter.hxx"
#include "itkMetaVesselTubeConverter.hxx"
#include "itkSurfaceSpatialObjectPoint.hxx"
#include "itkDTITubeSpatialObject.hxx"
#include "itkPolygonGroupSpatialObject.hxx"
#include "itkCylinderSpatialObject.h"
#include "itkPlaneSpatialObject.hxx"
#include "itkLineSpatialObject.hxx"
#include "itkMetaGaussianConverter.hxx"
#include "itkPointBasedSpatialObject.hxx"
#include "itkSpatialObjectToPointSetFilter.h"
#include "itkSpatialObjectToImageFilter.hxx"
#include "itkMetaGroupConverter.hxx"
#include "itkSpatialObjectToImageStatisticsCalculator.h"
#include "itkBlobSpatialObject.h"
#include "itkMetaLandmarkConverter.hxx"
#include "itkMetaEvent.h"
#include "itkEllipseSpatialObject.h"
#include "itkAffineGeometryFrame.h"
#include "itkSpatialObjectTreeContainer.h"
#include "itkMetaDTITubeConverter.hxx"
#include "itkMetaSurfaceConverter.hxx"
#include "itkSceneSpatialObject.hxx"
#include "itkMeshSpatialObject.hxx"
#include "itkSpatialObjectToImageStatisticsCalculator.hxx"
#include "itkTubeSpatialObjectPoint.hxx"
#include "itkSpatialObjectProperty.hxx"
#include "itkSpatialObject.hxx"
#include "itkSpatialObjectTreeNode.hxx"
#include "itkMetaEllipseConverter.h"
#include "itkMetaContourConverter.h"
#include "itkLandmarkSpatialObject.hxx"
#include "itkSpatialObjectPoint.h"
#include "itkSceneSpatialObject.h"
#include "itkLineSpatialObjectPoint.hxx"
#include "itkContourSpatialObject.hxx"
#include "itkVesselTubeSpatialObject.hxx"
#include "itkPolygonSpatialObject.h"
#include "itkSurfaceSpatialObject.hxx"
#include "itkImageSpatialObject.hxx"
#include "itkSpatialObjectTreeContainer.hxx"
#include "itkMetaSceneConverter.hxx"
#include "itkEllipseSpatialObject.hxx"
#include "itkDTITubeSpatialObjectPoint.hxx"
#include "itkSpatialObjectDuplicator.h"
#include "itkBoxSpatialObject.hxx"
#include "itkPolygonSpatialObject.hxx"
#include "itkSpatialObjectFactoryBase.h"
#include "itkContourSpatialObjectPoint.hxx"
#include "itkGaussianSpatialObject.hxx"
#include "itkSpatialObjectToPointSetFilter.hxx"
#include "itkImageMaskSpatialObject.hxx"
#include "itkSpatialObjectDuplicator.hxx"
#include "itkSpatialObjectProperty.h"
#include "itkGroupSpatialObject.hxx"
#include "itkArrowSpatialObject.hxx"
#include "itkMetaContourConverter.hxx"
#include "itkMetaTubeConverter.hxx"
#include "itkBlobSpatialObject.hxx"
#include "itkTubeSpatialObject.hxx"
#include "itkMetaEllipseConverter.hxx"
#include "itkMetaArrowConverter.hxx"
#include "itkSpatialObjectPoint.hxx"
#include "itkMetaLineConverter.hxx"
#include "itkAffineGeometryFrame.hxx"
#include "itkVesselTubeSpatialObjectPoint.hxx"



int itkSpatialObjectsHeaderTest ( int , char * [] )
{

  return EXIT_SUCCESS;
}
