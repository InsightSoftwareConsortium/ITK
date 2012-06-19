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

#include "itkEllipseSpatialObject.h"
#include "itkImageSpatialObject.h"
#include "itkLandmarkSpatialObject.h"
#include "itkLineSpatialObject.h"
#include "itkMeshSpatialObject.h"
#include "itkPlaneSpatialObject.h"
#include "itkPolygonGroupSpatialObject.h"
#include "itkSceneSpatialObject.h"
#include "itkSurfaceSpatialObject.h"
#include "itkTubeSpatialObject.h"

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

  itk::MeshSpatialObject<>::Pointer MeshSpatialObjectObj =
    itk::MeshSpatialObject<>::New();
  std::cout << "----------MeshSpatialObject " << MeshSpatialObjectObj;

  itk::PlaneSpatialObject<3>::Pointer PlaneSpatialObjectObj =
    itk::PlaneSpatialObject<3>::New();
  std::cout << "----------PlaneSpatialObject " << PlaneSpatialObjectObj;

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
