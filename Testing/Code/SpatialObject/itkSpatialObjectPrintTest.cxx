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
#include "itkSceneSpatialObject.h"
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

  itk::LandmarkSpatialObject<3>::Pointer LandmarkSpatialObjectObj =
    itk::LandmarkSpatialObject<3>::New();
  std::cout << "----------LandmarkSpatialObject " << LandmarkSpatialObjectObj;

  itk::LineSpatialObject<3>::Pointer LineSpatialObjectObj =
    itk::LineSpatialObject<3>::New();
  std::cout << "----------LineSpatialObject " << LineSpatialObjectObj;
  
  itk::LineSpatialObjectPoint<3> * LineSpatialObjectPointObj =
    new itk::LineSpatialObjectPoint<3>;
  std::cout << "----------LineSpatialObjectPoint " << LineSpatialObjectPointObj;
  delete LineSpatialObjectPointObj;
  
  itk::PlaneSpatialObject<3>::Pointer PlaneSpatialObjectObj =
    itk::PlaneSpatialObject<3>::New();
  std::cout << "----------PlaneSpatialObject " << PlaneSpatialObjectObj;
#if 0
  itk::PolygonGroupOrientation::Pointer PolygonGroupOrientationObj =
    itk::PolygonGroupOrientation::New();
  std::cout << "----------PolygonGroupOrientation " << PolygonGroupOrientationObj;
#endif
  itk::PolygonGroupSpatialObject<3>::Pointer PolygonGroupSpatialObjectObj =
    itk::PolygonGroupSpatialObject<3>::New();
  std::cout << "----------PolygonGroupSpatialObject " << PolygonGroupSpatialObjectObj;

  itk::PolygonSpatialObject<3>::Pointer PolygonSpatialObjectObj =
    itk::PolygonSpatialObject<3>::New();
  std::cout << "----------PolygonSpatialObject " << PolygonSpatialObjectObj;

  itk::SceneSpatialObject<3>::Pointer SceneObj =
    itk::SceneSpatialObject<3>::New();
  std::cout << "----------Scene " << SceneObj;

  itk::SpatialObject<3>::Pointer SpatialObjectObj =
    itk::SpatialObject<3>::New();
  std::cout << "----------SpatialObject " << SpatialObjectObj;

  itk::SpatialObjectPoint<3> * SpatialObjectPointObj =
    new itk::SpatialObjectPoint<3>;
  std::cout << "----------SpatialObjectPoint " << SpatialObjectPointObj;
  delete SpatialObjectPointObj;

  itk::SpatialObjectProperty<float>::Pointer SpatialObjectPropertyObj =
    itk::SpatialObjectProperty<float>::New();
  std::cout << "----------SpatialObjectProperty " << SpatialObjectPropertyObj;

  itk::SurfaceSpatialObject<3>::Pointer SurfaceSpatialObjectObj =
    itk::SurfaceSpatialObject<3>::New();
  std::cout << "----------SurfaceSpatialObject " << SurfaceSpatialObjectObj;

  itk::SurfaceSpatialObjectPoint<3> * SurfaceSpatialObjectPointObj =
    new itk::SurfaceSpatialObjectPoint<3>;
  std::cout << "----------SurfaceSpatialObjectPoint " << SurfaceSpatialObjectPointObj;
  delete SurfaceSpatialObjectPointObj;

  itk::TubeSpatialObject<3>::Pointer TubeSpatialObjectObj =
    itk::TubeSpatialObject<3>::New();
  std::cout << "----------TubeSpatialObject " << TubeSpatialObjectObj;

  itk::TubeSpatialObjectPoint<3> *  TubeSpatialObjectPointObj =
    new itk::TubeSpatialObjectPoint<3>;
  std::cout << "----------TubeSpatialObjectPoint " << TubeSpatialObjectPointObj;
  delete TubeSpatialObjectPointObj;

  return 0;
}
