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


#include "itkAntiAliasBinaryImageFilter.h"
#include "itkBinaryMask3DMeshSource.h"
#include "itkBinaryMinMaxCurvatureFlowImageFilter.h"
#include "itkCannySegmentationLevelSetImageFilter.h"
#include "itkConnectedRegionsMeshFilter.h"
#include "itkDemonsRegistrationFilter.h"
#include "itkExtensionVelocitiesImageFilter.h"

int main (int , char* [])
{
  typedef itk::Image<float,2>          InputType;
  typedef itk::Image<float,3>          InputType3D;
  typedef itk::Image<float,2>          OutputType;

  typedef itk::Mesh<double>  MeshType;

  typedef itk::Vector<float,2>      VectorType;
  typedef itk::Image<VectorType, 2> VectorImageType;

  itk::AntiAliasBinaryImageFilter<InputType,OutputType>::Pointer AntiAliasBinaryImageFilterObj =
    itk::AntiAliasBinaryImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------AntiAliasBinaryImageFilter " << AntiAliasBinaryImageFilterObj;

  itk::BinaryMask3DMeshSource<InputType3D,MeshType>::Pointer BinaryMask3DMeshSourceObj =
    itk::BinaryMask3DMeshSource<InputType3D,MeshType>::New();
  std:: cout << "-------------BinaryMask3DMeshSource " << BinaryMask3DMeshSourceObj;

  itk::BinaryMinMaxCurvatureFlowFunction<InputType>::Pointer BinaryMinMaxCurvatureFlowFunctionObj =
    itk::BinaryMinMaxCurvatureFlowFunction<InputType>::New();
  std:: cout << "-------------BinaryMinMaxCurvatureFlowFunction " << BinaryMinMaxCurvatureFlowFunctionObj;

  itk::BinaryMinMaxCurvatureFlowImageFilter<InputType,OutputType>::Pointer BinaryMinMaxCurvatureFlowImageFilterObj =
    itk::BinaryMinMaxCurvatureFlowImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------BinaryMinMaxCurvatureFlowImageFilter " << BinaryMinMaxCurvatureFlowImageFilterObj;

  itk::CannySegmentationLevelSetFunction<InputType,InputType>::Pointer CannySegmentationLevelSetFunctionObj =
    itk::CannySegmentationLevelSetFunction<InputType,InputType>::New();
  std:: cout << "-------------CannySegmentationLevelSetFunction " << CannySegmentationLevelSetFunctionObj;

  itk::CannySegmentationLevelSetImageFilter<InputType,OutputType>::Pointer CannySegmentationLevelSetImageFilterObj =
    itk::CannySegmentationLevelSetImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------CannySegmentationLevelSetImageFilter " << CannySegmentationLevelSetImageFilterObj;

  itk::ConnectedRegionsMeshFilter<MeshType,MeshType>::Pointer ConnectedRegionsMeshFilterObj =
    itk::ConnectedRegionsMeshFilter<MeshType,MeshType>::New();
  std:: cout << "-------------ConnectedRegionsMeshFilter " << ConnectedRegionsMeshFilterObj;

  itk::CurvatureFlowFunction<InputType>::Pointer CurvatureFlowFunctionObj =
    itk::CurvatureFlowFunction<InputType>::New();
  std:: cout << "-------------CurvatureFlowFunction " << CurvatureFlowFunctionObj;

  itk::CurvatureFlowImageFilter<InputType,OutputType>::Pointer CurvatureFlowImageFilterObj =
    itk::CurvatureFlowImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------CurvatureFlowImageFilter " << CurvatureFlowImageFilterObj;

  itk::DemonsRegistrationFilter<InputType,OutputType,VectorImageType>::Pointer DemonsRegistrationFilterObj =
    itk::DemonsRegistrationFilter<InputType,OutputType,VectorImageType>::New();
  std:: cout << "-------------DemonsRegistrationFilter " << DemonsRegistrationFilterObj;

  itk::DemonsRegistrationFunction<InputType,OutputType,VectorImageType>::Pointer DemonsRegistrationFunctionObj =
    itk::DemonsRegistrationFunction<InputType,OutputType,VectorImageType>::New();
  std:: cout << "-------------DemonsRegistrationFunction " << DemonsRegistrationFunctionObj;

  itk::ExtensionVelocitiesImageFilter<InputType,float,1>::Pointer ExtensionVelocitiesImageFilterObj =
    itk::ExtensionVelocitiesImageFilter<InputType,float,1>::New();
  std:: cout << "-------------ExtensionVelocitiesImageFilter " << ExtensionVelocitiesImageFilterObj;

//  itk::fem::FEMRegistrationFilter<InputType,InputType>::Pointer FEMRegistrationFilterObj =
//    itk::fem::FEMRegistrationFilter<InputType,InputType>::New();
//  std:: cout << "-------------FEMRegistrationFilter " << FEMRegistrationFilterObj;

  itk::FastMarchingExtensionImageFilter<InputType,float>::Pointer FastMarchingExtensionImageFilterObj =
    itk::FastMarchingExtensionImageFilter<InputType,float>::New();
  std:: cout << "-------------FastMarchingExtensionImageFilter " << FastMarchingExtensionImageFilterObj;

  itk::FastMarchingImageFilter<InputType>::Pointer FastMarchingImageFilterObj =
    itk::FastMarchingImageFilter<InputType>::New();
  std:: cout << "-------------FastMarchingImageFilter " << FastMarchingImageFilterObj;

  return EXIT_SUCCESS;

}
