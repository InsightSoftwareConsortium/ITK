/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatisticsTests.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

// this file defines the itkStatisticsTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(itkStatisticsPrintTest);
  REGISTER_TEST(itkCovarianceCalculatorTest);
  REGISTER_TEST(itkDenseFrequencyContainerTest);
  REGISTER_TEST(itkExpectationMaximizationMixtureModelEstimatorTest);
  REGISTER_TEST(itkGaussianDensityFunctionTest);
  REGISTER_TEST(itkGoodnessOfFitMixtureModelCostFunctionTest);
  REGISTER_TEST(itkGreyLevelCooccurrenceMatrixTextureCoefficientsCalculatorTest);
  REGISTER_TEST(itkHistogramTest);
  REGISTER_TEST(itkImageToListAdaptorTest);
  REGISTER_TEST(itkImageToCooccurrenceListAdaptorTest);
  REGISTER_TEST(itkImageToHistogramGeneratorTest);
  REGISTER_TEST(itkKdTreeBasedKmeansEstimatorTest);
  REGISTER_TEST(itkKdTreeGeneratorTest);
  REGISTER_TEST(itkListSampleTest);
  REGISTER_TEST(itkListSampleToHistogramFilterTest);
  REGISTER_TEST(itkListSampleToHistogramGeneratorTest);
  REGISTER_TEST(itkMaskedScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest);
  REGISTER_TEST(itkMembershipSampleTest);
  REGISTER_TEST(itkMembershipSampleGeneratorTest);
  REGISTER_TEST(itkMersenneTwisterRandomVariateGeneratorTest);
  REGISTER_TEST(itkMeanCalculatorTest);
  REGISTER_TEST(itkNeighborhoodSamplerTest) ;
  REGISTER_TEST(itkNormalVariateGeneratorTest);
  REGISTER_TEST(itkSampleClassifierTest) ;
  REGISTER_TEST(itkSampleClassifierWithMaskTest) ;
  REGISTER_TEST(itkSampleMeanShiftClusteringFilterTest) ;
  REGISTER_TEST(itkSampleSelectiveMeanShiftBlurringFilterTest) ;
  REGISTER_TEST(itkSelectiveSubsampleGeneratorTest) ;
  REGISTER_TEST(itkScalarImageToHistogramGeneratorTest);
  REGISTER_TEST(itkScalarImageTextureCalculatorTest);
  REGISTER_TEST(itkScalarImageToGreyLevelCooccurrenceMatrixGeneratorTest);
  REGISTER_TEST(itkStatisticsAlgorithmTest);
  REGISTER_TEST(itkSubsampleTest);
  REGISTER_TEST(itkVariableDimensionHistogramTest);
  REGISTER_TEST(itkWeightedMeanCalculatorTest);
  REGISTER_TEST(itkWeightedCovarianceCalculatorTest);
}

