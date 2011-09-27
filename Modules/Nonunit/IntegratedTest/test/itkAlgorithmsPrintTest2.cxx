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

#include "itkVector.h"
#include "itkMesh.h"

#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkMeanSquaresPointSetToImageMetric.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkMinMaxCurvatureFlowImageFilter.h"
#include "itkMultiResolutionImageRegistrationMethod.h"
#include "itkMultiResolutionPDEDeformableRegistration.h"
#include "itkMutualInformationImageToImageMetric.h"
#include "itkNormalizedCorrelationImageToImageMetric.h"
#include "itkNormalizedCorrelationPointSetToImageMetric.h"
#include "itkOtsuThresholdImageCalculator.h"
#include "itkPDEDeformableRegistrationFilter.h"
#include "itkRGBGibbsPriorFilter.h"
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkReinitializeLevelSetImageFilter.h"

#include "itkDistanceToCentroidMembershipFunction.h"
#include "itkMahalanobisDistanceMembershipFunction.h"
#include "itkGroupSpatialObject.h"
#include "itkRGBPixel.h"

int main(int , char* [])
{
  typedef itk::Image<float,2> InputType;
  typedef itk::Image<float,2> OutputType;
  typedef itk::Image<bool,2> BinaryImageType;
  typedef itk::Image<unsigned short,2> UShortImageType;
  typedef itk::Image<unsigned short,3> UShortImageType3D;
  typedef itk::Image<unsigned char,2> CharType;

  typedef itk::Mesh<double>  MeshType;

  typedef itk::Vector<float,2> VectorType;
  typedef itk::Image<VectorType, 2> VectorImageType;
  typedef itk::Image<VectorType, 3> VectorImageType3D;

  // Used for NormalizedCorrelationPointSetToImageMetric
  typedef itk::PointSet<float,2> PointSetType;

  // Used for GradientVectorFlowImageFilter
  typedef itk::CovariantVector<double,2> GradientType;
  typedef itk::Image<GradientType,2>   GradientImageType;

  itk::MattesMutualInformationImageToImageMetric<InputType,InputType>::Pointer MattesMutualInformationImageToImageMetricObj =
    itk::MattesMutualInformationImageToImageMetric<InputType,InputType>::New();
  std:: cout << "-------------MattesMutualInformationImageToImageMetric " << MattesMutualInformationImageToImageMetricObj;

  /*itk::MeanSquaresPointSetToImageMetric<InputType,OutputType>::Pointer MeanSquaresPointSetToImageMetricObj =
    itk::MeanSquaresPointSetToImageMetric<InputType,OutputType>::New();
  std:: cout << "-------------MeanSquaresPointSetToImageMetric " << MeanSquaresPointSetToImageMetricObj;*/
  itk::MeanSquaresImageToImageMetric<InputType,InputType>::Pointer MeanSquaresImageToImageMetricObj =
    itk::MeanSquaresImageToImageMetric<InputType,InputType>::New();
  std:: cout << "-------------MeanSquaresImageToImageMetric " << MeanSquaresImageToImageMetricObj;
  itk::MinMaxCurvatureFlowFunction<InputType>::Pointer MinMaxCurvatureFlowFunctionObj =
    itk::MinMaxCurvatureFlowFunction<InputType>::New();
  std:: cout << "-------------MinMaxCurvatureFlowFunction " << MinMaxCurvatureFlowFunctionObj;

  itk::MinMaxCurvatureFlowImageFilter<InputType,OutputType>::Pointer MinMaxCurvatureFlowImageFilterObj =
    itk::MinMaxCurvatureFlowImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------MinMaxCurvatureFlowImageFilter " << MinMaxCurvatureFlowImageFilterObj;

  itk::MultiResolutionImageRegistrationMethod<InputType,InputType>::Pointer MultiResolutionImageRegistrationMethodObj =
    itk::MultiResolutionImageRegistrationMethod<InputType,InputType>::New();
  std:: cout << "-------------MultiResolutionImageRegistrationMethod " << MultiResolutionImageRegistrationMethodObj;

  itk::MultiResolutionPDEDeformableRegistration<InputType,OutputType,VectorImageType>::Pointer MultiResolutionPDEDeformableRegistrationObj =
    itk::MultiResolutionPDEDeformableRegistration<InputType,OutputType,VectorImageType>::New();
  std:: cout << "-------------MultiResolutionPDEDeformableRegistration " << MultiResolutionPDEDeformableRegistrationObj;

  itk::MultiResolutionPyramidImageFilter<InputType,OutputType>::Pointer MultiResolutionPyramidImageFilterObj =
    itk::MultiResolutionPyramidImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------MultiResolutionPyramidImageFilter " << MultiResolutionPyramidImageFilterObj;

  itk::MutualInformationImageToImageMetric<InputType,InputType>::Pointer MutualInformationImageToImageMetricObj =
    itk::MutualInformationImageToImageMetric<InputType,InputType>::New();
  std:: cout << "-------------MutualInformationImageToImageMetric " << MutualInformationImageToImageMetricObj;

  itk::NormalizedCorrelationImageToImageMetric<InputType,InputType>::Pointer NormalizedCorrelationImageToImageMetricObj =
    itk::NormalizedCorrelationImageToImageMetric<InputType,InputType>::New();
  std:: cout << "-------------NormalizedCorrelationImageToImageMetric " << NormalizedCorrelationImageToImageMetricObj;

  itk::NormalizedCorrelationPointSetToImageMetric<PointSetType,InputType>::Pointer NormalizedCorrelationPointSetToImageMetricObj =
    itk::NormalizedCorrelationPointSetToImageMetric<PointSetType,InputType>::New();
  std:: cout << "-------------NormalizedCorrelationPointSetToImageMetric " << NormalizedCorrelationPointSetToImageMetricObj;

  itk::OtsuThresholdImageCalculator<InputType>::Pointer OtsuThresholdImageCalculatorObj =
    itk::OtsuThresholdImageCalculator<InputType>::New();
  std:: cout << "-------------OtsuThresholdImageCalculator " << OtsuThresholdImageCalculatorObj;

  itk::PDEDeformableRegistrationFilter<InputType,InputType,VectorImageType>::Pointer PDEDeformableRegistrationFilterObj =
    itk::PDEDeformableRegistrationFilter<InputType,InputType,VectorImageType>::New();
  std:: cout << "-------------PDEDeformableRegistrationFilter " << PDEDeformableRegistrationFilterObj;

  //NOTE:  RGBGibbsPriorFilter only works in 3D
  itk::RGBGibbsPriorFilter<VectorImageType3D,UShortImageType3D>::Pointer RGBGibbsPriorFilterObj =
    itk::RGBGibbsPriorFilter<VectorImageType3D,UShortImageType3D>::New();
  std:: cout << "-------------RGBGibbsPriorFilter " << RGBGibbsPriorFilterObj;

  itk::RecursiveMultiResolutionPyramidImageFilter<InputType,OutputType>::Pointer RecursiveMultiResolutionPyramidImageFilterObj =
    itk::RecursiveMultiResolutionPyramidImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------RecursiveMultiResolutionPyramidImageFilter " << RecursiveMultiResolutionPyramidImageFilterObj;

  itk::ReinitializeLevelSetImageFilter<InputType>::Pointer ReinitializeLevelSetImageFilterObj =
    itk::ReinitializeLevelSetImageFilter<InputType>::New();
  std:: cout << "-------------ReinitializeLevelSetImageFilter " << ReinitializeLevelSetImageFilterObj;

  return EXIT_SUCCESS;

}
