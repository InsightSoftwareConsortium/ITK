/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectPrintTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkBlobSpatialObject.h"
#include "itkEllipseSpatialObject.h"
#include "itkGroupSpatialObject.h"
#include "itkImageSpatialObject.h"
#include "itkLandmarkSpatialObject.h"
#include "itkLineSpatialObject.h"
#include "itkLineSpatialObjectPoint.h"
#include "itkPlaneSpatialObject.h"
#include "itkPolygonGroupOrientation.h"
#include "itkPolygonGroupSpatialObject.h"
#include "itkPolygonSpatialObject.h"
#include "itkScene.h"
#include "itkSpatialObject.h"
#include "itkSpatialObjectPoint.h"
#include "itkSpatialObjectProperty.h"
#include "itkSurfaceSpatialObject.h"
#include "itkSurfaceSpatialObjectPoint.h"
#include "itkTubeSpatialObject.h"
#include "itkTubeSpatialObjectPoint.h"

int itkSpatialObjectPrintTest(int , char* [])
{
  itk::BlobSpatialObject<3>::Pointer BlobSpatialObjectObj =
    itk::BlobSpatialObject<3>::New();
  std::cout << "----------BlobSpatialObject " << BlobSpatialObjectObj;

  itk::EllipseSpatialObject<3>::Pointer EllipseSpatialObjectObj =
    itk::EllipseSpatialObject<3>::New();
  std::cout << "----------EllipseSpatialObject " << EllipseSpatialObjectObj;

  itk::GroupSpatialObject<3>::Pointer GroupSpatialObjectObj =
    itk::GroupSpatialObject<3>::New();
  std::cout << "----------GroupSpatialObject " << GroupSpatialObjectObj;

  typedef unsigned short Pixel;
  itk::ImageSpatialObject<3,Pixel>::Pointer ImageSpatialObjectObj =
    itk::ImageSpatialObject<3,Pixel>::New();
  std::cout << "----------ImageSpatialObject " << ImageSpatialObjectObj;

#if 0
  itk::LandmarkSpatialObject<foo>::Pointer LandmarkSpatialObjectObj =
    itk::LandmarkSpatialObject<foo>::New();
  std::cout << "----------LandmarkSpatialObject " << LandmarkSpatialObjectObj;

  itk::LineSpatialObject<foo>::Pointer LineSpatialObjectObj =
    itk::LineSpatialObject<foo>::New();
  std::cout << "----------LineSpatialObject " << LineSpatialObjectObj;

  itk::LineSpatialObjectPoint<foo>::Pointer LineSpatialObjectPointObj =
    itk::LineSpatialObjectPoint<foo>::New();
  std::cout << "----------LineSpatialObjectPoint " << LineSpatialObjectPointObj;

  itk::PlaneSpatialObject<foo>::Pointer PlaneSpatialObjectObj =
    itk::PlaneSpatialObject<foo>::New();
  std::cout << "----------PlaneSpatialObject " << PlaneSpatialObjectObj;

  itk::PolygonGroupOrientation<foo>::Pointer PolygonGroupOrientationObj =
    itk::PolygonGroupOrientation<foo>::New();
  std::cout << "----------PolygonGroupOrientation " << PolygonGroupOrientationObj;

  itk::PolygonGroupSpatialObject<foo>::Pointer PolygonGroupSpatialObjectObj =
    itk::PolygonGroupSpatialObject<foo>::New();
  std::cout << "----------PolygonGroupSpatialObject " << PolygonGroupSpatialObjectObj;

  itk::PolygonSpatialObject<foo>::Pointer PolygonSpatialObjectObj =
    itk::PolygonSpatialObject<foo>::New();
  std::cout << "----------PolygonSpatialObject " << PolygonSpatialObjectObj;

  itk::Scene<foo>::Pointer SceneObj =
    itk::Scene<foo>::New();
  std::cout << "----------Scene " << SceneObj;

  itk::SpatialObject<foo>::Pointer SpatialObjectObj =
    itk::SpatialObject<foo>::New();
  std::cout << "----------SpatialObject " << SpatialObjectObj;

  itk::SpatialObjectPoint<foo>::Pointer SpatialObjectPointObj =
    itk::SpatialObjectPoint<foo>::New();
  std::cout << "----------SpatialObjectPoint " << SpatialObjectPointObj;

  itk::SpatialObjectProperty<foo>::Pointer SpatialObjectPropertyObj =
    itk::SpatialObjectProperty<foo>::New();
  std::cout << "----------SpatialObjectProperty " << SpatialObjectPropertyObj;

  itk::SurfaceSpatialObject<foo>::Pointer SurfaceSpatialObjectObj =
    itk::SurfaceSpatialObject<foo>::New();
  std::cout << "----------SurfaceSpatialObject " << SurfaceSpatialObjectObj;

  itk::SurfaceSpatialObjectPoint<foo>::Pointer SurfaceSpatialObjectPointObj =
    itk::SurfaceSpatialObjectPoint<foo>::New();
  std::cout << "----------SurfaceSpatialObjectPoint " << SurfaceSpatialObjectPointObj;

  itk::TubeSpatialObject<foo>::Pointer TubeSpatialObjectObj =
    itk::TubeSpatialObject<foo>::New();
  std::cout << "----------TubeSpatialObject " << TubeSpatialObjectObj;

  itk::TubeSpatialObjectPoint<foo>::Pointer TubeSpatialObjectPointObj =
    itk::TubeSpatialObjectPoint<foo>::New();
  std::cout << "----------TubeSpatialObjectPoint " << TubeSpatialObjectPointObj;
#endif

  return 0;
}
