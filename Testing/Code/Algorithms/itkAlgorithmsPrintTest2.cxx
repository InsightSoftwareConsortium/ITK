/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAlgorithmsPrintTest2.cxx
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

#include "itkImage.h"
#include "itkVector.h"
#include "itkPoint.h"
#include "itkMesh.h"
#include "itkEllipseSpatialObject.h"

#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkMeanSquaresPointSetToImageMetric.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkMinMaxCurvatureFlowFunction.h"
#include "itkMinMaxCurvatureFlowImageFilter.h"
#include "itkMultiResolutionImageRegistrationMethod.h"
#include "itkMultiResolutionPDEDeformableRegistration.h"
#include "itkMultiResolutionPyramidImageFilter.h"
#include "itkMutualInformationImageToImageMetric.h"
#include "itkNormalizedCorrelationImageToImageMetric.h"
#include "itkNormalizedCorrelationPointSetToImageMetric.h"
#include "itkOtsuThresholdImageCalculator.h"
#include "itkPDEDeformableRegistrationFilter.h"
#include "itkMeanReciprocalSquareDifferenceImageToImageMetric.h"
#if 0
#include "itkMeanReciprocalSquareDifferencePointSetToImageMetric.h"
#endif
#include "itkRGBGibbsPriorFilter.h"
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkRegionGrowImageFilter.h"
#include "itkRegistrationMethod.h"
#include "itkReinitializeLevelSetImageFilter.h"

#include "itkDistanceToCentroidMembershipFunction.h"
#include "itkMahalanobisDistanceMembershipFunction.h"
#include "itkGroupSpatialObject.h"
#include "itkRGBPixel.h"

int itkAlgorithmsPrintTest2(int , char* [])
{
  typedef itk::Image<float,2> InputType; 
  typedef itk::Image<float,2> OutputType;
  typedef itk::Image<bool,2> BinaryImageType;
  typedef itk::Image<unsigned short,2> UShortImageType;
  typedef itk::Image<unsigned char,2> CharType;
  
  typedef itk::Mesh<double>  MeshType;
  
  typedef itk::Vector<float,2> VectorType;
  typedef itk::Image<VectorType, 2> VectorImageType;
  
  // Used for NormalizedCorrelationPointSetToImageMetric
  typedef itk::PointSet<float,2> PointSetType;
  
  // Used for GradientVectorFlowImageFilter
  typedef itk::CovariantVector<double,2> GradientType;
  typedef itk::Image<GradientType,2>   GradientImageType;

  //Used for ImageKMeansModelEstimator
  typedef itk::Statistics::DistanceToCentroidMembershipFunction<VectorType> KMeansMemFuncType;
  
  // Used for ImageGaussianModelEstimator
  typedef itk::Statistics::MahalanobisDistanceMembershipFunction<VectorType> GaussianMemFuncType;
  
  // Used for ImageToSpatialObjectRegistrationMethod
  typedef itk::GroupSpatialObject<2>   GroupType;

  // Used for ImageToSpatialObjectMetric
  typedef itk::EllipseSpatialObject<2> SpatialObjectType;

  itk::MattesMutualInformationImageToImageMetric<InputType,InputType>::Pointer MattesMutualInformationImageToImageMetricObj =
    itk::MattesMutualInformationImageToImageMetric<InputType,InputType>::New();
  std:: cout << "-------------MattesMutualInformationImageToImageMetric " << MattesMutualInformationImageToImageMetricObj;

#if 0
  itk::MeanSquaresPointSetToImageMetric<InputType,OutputType>::Pointer MeanSquaresPointSetToImageMetricObj =
    itk::MeanSquaresPointSetToImageMetric<InputType,OutputType>::New();
  std:: cout << "-------------MeanSquaresPointSetToImageMetric " << MeanSquaresPointSetToImageMetricObj;
#endif
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

  itk::MeanReciprocalSquareDifferenceImageToImageMetric<InputType,InputType>::Pointer MeanReciprocalSquareDifferenceImageToImageMetricObj =
    itk::MeanReciprocalSquareDifferenceImageToImageMetric<InputType,InputType>::New();
  std:: cout << "-------------MeanReciprocalSquareDifferenceImageToImageMetric " << MeanReciprocalSquareDifferenceImageToImageMetricObj;
#if 0
  itk::MeanReciprocalSquareDifferencePointSetToImageMetric<InputType,OutputType>::Pointer MeanReciprocalSquareDifferencePointSetToImageMetricObj =
    itk::MeanReciprocalSquareDifferencePointSetToImageMetric<InputType,OutputType>::New();
  std:: cout << "-------------MeanReciprocalSquareDifferencePointSetToImageMetric " << MeanReciprocalSquareDifferencePointSetToImageMetricObj;
#endif
  itk::RGBGibbsPriorFilter<VectorImageType,UShortImageType>::Pointer RGBGibbsPriorFilterObj =
    itk::RGBGibbsPriorFilter<VectorImageType,UShortImageType>::New();
  std:: cout << "-------------RGBGibbsPriorFilter " << RGBGibbsPriorFilterObj;
  
  itk::RecursiveMultiResolutionPyramidImageFilter<InputType,OutputType>::Pointer RecursiveMultiResolutionPyramidImageFilterObj =
    itk::RecursiveMultiResolutionPyramidImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------RecursiveMultiResolutionPyramidImageFilter " << RecursiveMultiResolutionPyramidImageFilterObj;

  itk::RegionGrowImageFilter<InputType,OutputType>::Pointer RegionGrowImageFilterObj =
    itk::RegionGrowImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------RegionGrowImageFilter " << RegionGrowImageFilterObj;
#if 0
  itk::RegistrationMethod<InputType,OutputType>::Pointer RegistrationMethodObj =
    itk::RegistrationMethod<InputType,OutputType>::New();
  std:: cout << "-------------RegistrationMethod " << RegistrationMethodObj;
#endif
  itk::ReinitializeLevelSetImageFilter<InputType>::Pointer ReinitializeLevelSetImageFilterObj =
    itk::ReinitializeLevelSetImageFilter<InputType>::New();
  std:: cout << "-------------ReinitializeLevelSetImageFilter " << ReinitializeLevelSetImageFilterObj;

  return 0;

}
