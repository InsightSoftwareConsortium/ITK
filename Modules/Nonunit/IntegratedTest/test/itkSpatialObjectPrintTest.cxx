/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkArrowSpatialObject.h"
#include "itkBlobSpatialObject.h"
#include "itkBoxSpatialObject.h"
#include "itkContourSpatialObject.h"
#include "itkDTITubeSpatialObject.h"
#include "itkEllipseSpatialObject.h"
#include "itkGaussianSpatialObject.h"
#include "itkGroupSpatialObject.h"
#include "itkImageMaskSpatialObject.h"
#include "itkImageSpatialObject.h"
#include "itkLandmarkSpatialObject.h"
#include "itkLineSpatialObject.h"
#include "itkMeshSpatialObject.h"
#include "itkPolygonSpatialObject.h"
#include "itkSurfaceSpatialObject.h"
#include "itkTubeSpatialObject.h"
#include "itkTubeSpatialObjectPoint.h"

int
itkSpatialObjectPrintTest(int, char *[])
{
  const itk::ArrowSpatialObject<3>::Pointer ArrowSpatialObjectObj = itk::ArrowSpatialObject<3>::New();
  std::cout << "----------ArrowSpatialObject " << ArrowSpatialObjectObj;

  const itk::BlobSpatialObject<3>::Pointer BlobSpatialObjectObj = itk::BlobSpatialObject<3>::New();
  std::cout << "----------BlobSpatialObject " << BlobSpatialObjectObj;

  const itk::BoxSpatialObject<3>::Pointer BoxSpatialObjectObj = itk::BoxSpatialObject<3>::New();
  std::cout << "----------BoxSpatialObject " << BoxSpatialObjectObj;

  const itk::ContourSpatialObject<3>::Pointer ContourSpatialObjectObj = itk::ContourSpatialObject<3>::New();
  std::cout << "----------ContourSpatialObject " << ContourSpatialObjectObj;

  const itk::DTITubeSpatialObject<3>::Pointer DTITubeSpatialObjectObj = itk::DTITubeSpatialObject<3>::New();
  std::cout << "----------DTITubeSpatialObject " << DTITubeSpatialObjectObj;

  const itk::EllipseSpatialObject<3>::Pointer EllipseSpatialObjectObj = itk::EllipseSpatialObject<3>::New();
  std::cout << "----------EllipseSpatialObject " << EllipseSpatialObjectObj;

  const itk::GaussianSpatialObject<3>::Pointer GaussianSpatialObjectObj = itk::GaussianSpatialObject<3>::New();
  std::cout << "----------GaussianSpatialObject " << GaussianSpatialObjectObj;

  const itk::GroupSpatialObject<3>::Pointer GroupSpatialObjectObj = itk::GroupSpatialObject<3>::New();
  std::cout << "----------GroupSpatialObject " << GroupSpatialObjectObj;

  const itk::ImageMaskSpatialObject<3>::Pointer ImageMaskSpatialObjectObj = itk::ImageMaskSpatialObject<3>::New();
  std::cout << "----------ImageMaskSpatialObject " << ImageMaskSpatialObjectObj;

  using Pixel = unsigned short;
  const itk::ImageSpatialObject<3, Pixel>::Pointer ImageSpatialObjectObj = itk::ImageSpatialObject<3, Pixel>::New();
  std::cout << "----------ImageSpatialObject " << ImageSpatialObjectObj;

  const itk::LandmarkSpatialObject<3>::Pointer LandmarkSpatialObjectObj = itk::LandmarkSpatialObject<3>::New();
  std::cout << "----------LandmarkSpatialObject " << LandmarkSpatialObjectObj;

  const itk::LineSpatialObject<3>::Pointer LineSpatialObjectObj = itk::LineSpatialObject<3>::New();
  std::cout << "----------LineSpatialObject " << LineSpatialObjectObj;

  auto * LineSpatialObjectPointObj = new itk::LineSpatialObjectPoint<3>;
  std::cout << "----------LineSpatialObjectPoint " << LineSpatialObjectPointObj;
  delete LineSpatialObjectPointObj;

  const itk::MeshSpatialObject<>::Pointer MeshSpatialObjectObj = itk::MeshSpatialObject<>::New();
  std::cout << "----------MeshSpatialObject " << MeshSpatialObjectObj;

  const itk::PolygonSpatialObject<3>::Pointer PolygonSpatialObjectObj = itk::PolygonSpatialObject<3>::New();
  std::cout << "----------PolygonSpatialObject " << PolygonSpatialObjectObj;

  const itk::SpatialObject<3>::Pointer SpatialObjectObj = itk::SpatialObject<3>::New();
  std::cout << "----------SpatialObject " << SpatialObjectObj;

  auto * SpatialObjectPointObj = new itk::SpatialObjectPoint<3>;
  std::cout << "----------SpatialObjectPoint ";
  SpatialObjectPointObj->Print(std::cout);
  delete SpatialObjectPointObj;

  const itk::SpatialObjectProperty SpatialObjectPropertyObj;
  std::cout << "----------SpatialObjectProperty ";
  SpatialObjectPropertyObj.Print(std::cout);

  const itk::SurfaceSpatialObject<3>::Pointer SurfaceSpatialObjectObj = itk::SurfaceSpatialObject<3>::New();
  std::cout << "----------SurfaceSpatialObject " << SurfaceSpatialObjectObj;

  const itk::SurfaceSpatialObjectPoint<3> SurfaceSpatialObjectPointObj;
  std::cout << "----------SurfaceSpatialObjectPoint ";
  SurfaceSpatialObjectPointObj.Print(std::cout);

  const itk::TubeSpatialObject<3>::Pointer TubeSpatialObjectObj = itk::TubeSpatialObject<3>::New();
  std::cout << "----------TubeSpatialObject " << TubeSpatialObjectObj;

  const itk::TubeSpatialObjectPoint<3> TubeSpatialObjectPointObj;
  std::cout << "----------TubeSpatialObjectPoint ";
  TubeSpatialObjectPointObj.Print(std::cout);

  return 0;
}
