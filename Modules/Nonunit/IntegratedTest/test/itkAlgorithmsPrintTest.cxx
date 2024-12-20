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


#include "itkAntiAliasBinaryImageFilter.h"
#include "itkBinaryMask3DMeshSource.h"
#include "itkBinaryMinMaxCurvatureFlowImageFilter.h"
#include "itkCannySegmentationLevelSetImageFilter.h"
#include "itkConnectedRegionsMeshFilter.h"
#include "itkDemonsRegistrationFilter.h"
#include "itkExtensionVelocitiesImageFilter.h"

int
main(int, char *[])
{
  using InputType = itk::Image<float, 2>;
  using InputType3D = itk::Image<float, 3>;
  using OutputType = itk::Image<float, 2>;

  using MeshType = itk::Mesh<double>;

  using VectorType = itk::Vector<float, 2>;
  using VectorImageType = itk::Image<VectorType, 2>;

  const itk::AntiAliasBinaryImageFilter<InputType, OutputType>::Pointer AntiAliasBinaryImageFilterObj =
    itk::AntiAliasBinaryImageFilter<InputType, OutputType>::New();
  std::cout << "-------------AntiAliasBinaryImageFilter " << AntiAliasBinaryImageFilterObj;

  const itk::BinaryMask3DMeshSource<InputType3D, MeshType>::Pointer BinaryMask3DMeshSourceObj =
    itk::BinaryMask3DMeshSource<InputType3D, MeshType>::New();
  std::cout << "-------------BinaryMask3DMeshSource " << BinaryMask3DMeshSourceObj;

  const itk::BinaryMinMaxCurvatureFlowFunction<InputType>::Pointer BinaryMinMaxCurvatureFlowFunctionObj =
    itk::BinaryMinMaxCurvatureFlowFunction<InputType>::New();
  std::cout << "-------------BinaryMinMaxCurvatureFlowFunction " << BinaryMinMaxCurvatureFlowFunctionObj;

  const itk::BinaryMinMaxCurvatureFlowImageFilter<InputType, OutputType>::Pointer
    BinaryMinMaxCurvatureFlowImageFilterObj = itk::BinaryMinMaxCurvatureFlowImageFilter<InputType, OutputType>::New();
  std::cout << "-------------BinaryMinMaxCurvatureFlowImageFilter " << BinaryMinMaxCurvatureFlowImageFilterObj;

  const itk::CannySegmentationLevelSetFunction<InputType, InputType>::Pointer CannySegmentationLevelSetFunctionObj =
    itk::CannySegmentationLevelSetFunction<InputType, InputType>::New();
  std::cout << "-------------CannySegmentationLevelSetFunction " << CannySegmentationLevelSetFunctionObj;

  const itk::CannySegmentationLevelSetImageFilter<InputType, OutputType>::Pointer
    CannySegmentationLevelSetImageFilterObj = itk::CannySegmentationLevelSetImageFilter<InputType, OutputType>::New();
  std::cout << "-------------CannySegmentationLevelSetImageFilter " << CannySegmentationLevelSetImageFilterObj;

  const itk::ConnectedRegionsMeshFilter<MeshType, MeshType>::Pointer ConnectedRegionsMeshFilterObj =
    itk::ConnectedRegionsMeshFilter<MeshType, MeshType>::New();
  std::cout << "-------------ConnectedRegionsMeshFilter " << ConnectedRegionsMeshFilterObj;

  const itk::CurvatureFlowFunction<InputType>::Pointer CurvatureFlowFunctionObj =
    itk::CurvatureFlowFunction<InputType>::New();
  std::cout << "-------------CurvatureFlowFunction " << CurvatureFlowFunctionObj;

  const itk::CurvatureFlowImageFilter<InputType, OutputType>::Pointer CurvatureFlowImageFilterObj =
    itk::CurvatureFlowImageFilter<InputType, OutputType>::New();
  std::cout << "-------------CurvatureFlowImageFilter " << CurvatureFlowImageFilterObj;

  const itk::DemonsRegistrationFilter<InputType, OutputType, VectorImageType>::Pointer DemonsRegistrationFilterObj =
    itk::DemonsRegistrationFilter<InputType, OutputType, VectorImageType>::New();
  std::cout << "-------------DemonsRegistrationFilter " << DemonsRegistrationFilterObj;

  const itk::DemonsRegistrationFunction<InputType, OutputType, VectorImageType>::Pointer DemonsRegistrationFunctionObj =
    itk::DemonsRegistrationFunction<InputType, OutputType, VectorImageType>::New();
  std::cout << "-------------DemonsRegistrationFunction " << DemonsRegistrationFunctionObj;

  const itk::ExtensionVelocitiesImageFilter<InputType, float, 1>::Pointer ExtensionVelocitiesImageFilterObj =
    itk::ExtensionVelocitiesImageFilter<InputType, float, 1>::New();
  std::cout << "-------------ExtensionVelocitiesImageFilter " << ExtensionVelocitiesImageFilterObj;

  //  itk::fem::FEMRegistrationFilter<InputType,InputType>::Pointer FEMRegistrationFilterObj =
  //    itk::fem::FEMRegistrationFilter<InputType,InputType>::New();
  //  std:: cout << "-------------FEMRegistrationFilter " << FEMRegistrationFilterObj;

  const itk::FastMarchingExtensionImageFilter<InputType, float>::Pointer FastMarchingExtensionImageFilterObj =
    itk::FastMarchingExtensionImageFilter<InputType, float>::New();
  std::cout << "-------------FastMarchingExtensionImageFilter " << FastMarchingExtensionImageFilterObj;

  const itk::FastMarchingImageFilter<InputType>::Pointer FastMarchingImageFilterObj =
    itk::FastMarchingImageFilter<InputType>::New();
  std::cout << "-------------FastMarchingImageFilter " << FastMarchingImageFilterObj;

  return EXIT_SUCCESS;
}
