/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGreyLevelCooccurrenceMatrixTextureCoefficientsCalculatorTest.cxx
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
// Insight classes
#include "itkHistogram.h"
#include "vnl/vnl_math.h"

#include "itkGreyLevelCooccurrenceMatrixTextureCoefficientsCalculator.h"

// Un-comment to run this test standalone:
//int itkGreyLevelCooccurrenceMatrixTextureCoefficientsCalculatorTest(int, char* [] );
//int main(int c, char * v[])
//  {
//  return itkGreyLevelCooccurrenceMatrixTextureCoefficientsCalculator(c, v);
//  }

int itkGreyLevelCooccurrenceMatrixTextureCoefficientsCalculatorTest(int, char* [] )
{

  try { // the rest of the function is in the try block...
  
  //Data definitions 
  const unsigned int  HISTOGRAM_AXIS_LEN =  25;


  //------------------------------------------------------
  // Create a simple test histogram. The histogram must be
  // symmetric and normalized.
  //------------------------------------------------------
  typedef float MeasurementType ;
  typedef itk::Statistics::Histogram< MeasurementType, 2 > HistogramType ;
  HistogramType::Pointer histogram = HistogramType::New() ;
  HistogramType::SizeType size ;
  size.Fill(HISTOGRAM_AXIS_LEN) ;
  HistogramType::MeasurementVectorType lowerBound ;
  HistogramType::MeasurementVectorType upperBound ;
  lowerBound[0] = 0 ;
  lowerBound[1] = 0 ;
  upperBound[0] = HISTOGRAM_AXIS_LEN + 1 ;
  upperBound[1] = HISTOGRAM_AXIS_LEN + 1 ;
  histogram->Initialize(size, lowerBound, upperBound ) ; 

  HistogramType::IndexType index ;
  index[0] = 0 ;
  index[1] = 0 ;
  histogram->SetFrequency(index, 0.1);
  index[0] = 3 ;
  index[1] = 3 ;
  histogram->SetFrequency(index, 0.5);
  index[0] = 2 ;
  index[1] = 1 ;
  histogram->SetFrequency(index, 0.05);
  index[0] = 1 ;
  index[1] = 2 ;
  histogram->SetFrequency(index, 0.05);
  index[0] = 7 ;
  index[1] = 6 ;
  histogram->SetFrequency(index, 0.1);
  index[0] = 6 ;
  index[1] = 7 ;
  histogram->SetFrequency(index, 0.1);
  index[0] = 10 ;
  index[1] = 10 ;
  histogram->SetFrequency(index, 0.1);
  
  typedef itk::Statistics::GreyLevelCooccurrenceMatrixTextureCoefficientsCalculator<
    HistogramType > GLCMCalcType;
  GLCMCalcType::Pointer glcmCalc = GLCMCalcType::New();
  
  glcmCalc->SetHistogram(histogram);
  glcmCalc->Compute();
  
  double trueEnergy = 0.295;
  double trueEntropy = 2.26096;
  double trueCorrelation = 0.12819;
  double trueInverseDifferenceMoment = 0.85;
  double trueInertia = 0.3;
  double trueClusterShade = 139.1879;
  double trueClusterProminence = 2732.557;
  double trueHaralickCorrelation = 2264.549;
  
  double energy = glcmCalc->GetEnergy();
  double entropy = glcmCalc->GetEntropy();
  double correlation = glcmCalc->GetCorrelation();
  double inverseDifferenceMoment = glcmCalc->GetInverseDifferenceMoment();
  // Now try the other way of getting features.
  double inertia = glcmCalc->GetFeature(itk::Statistics::Inertia);
  double clusterShade = glcmCalc->GetFeature(itk::Statistics::ClusterShade);
  double clusterProminence = glcmCalc->GetFeature(itk::Statistics::ClusterProminence);
  double haralickCorrelation = glcmCalc->GetFeature(itk::Statistics::HaralickCorrelation);
  
  bool passed = true;
  
  if( vnl_math_abs(energy - trueEnergy) > 0.001 )
    {
    std::cerr << "Error:" << std::endl;
    std::cerr << "Energy calculated wrong. Expected: " << trueEnergy << ", got: " 
      << energy << std::endl;
    passed = false;
    }
   
  if( vnl_math_abs(entropy - trueEntropy) > 0.001 )
    {
    std::cerr << "Error:" << std::endl;
    std::cerr << "Entropy calculated wrong. Expected: " << trueEntropy << ", got: "  
      << entropy << std::endl;
    passed = false;
    }
  
  if( vnl_math_abs(correlation - trueCorrelation) > 0.001 )
    {
    std::cerr << "Error:" << std::endl;
    std::cerr << "Correlation calculated wrong. Expected: " << trueCorrelation << 
      ", got: "  << correlation << std::endl;
    passed = false;
    }
  
  if( vnl_math_abs(inverseDifferenceMoment - trueInverseDifferenceMoment) > 0.001 )
    {
    std::cerr << "Error:" << std::endl;
    std::cerr << "InverseDifferenceMoment calculated wrong. Expected: " << 
      trueInverseDifferenceMoment <<  ", got: "  << inverseDifferenceMoment << std::endl;
    passed = false;
    }
  
  if( vnl_math_abs(inertia - trueInertia) > 0.001 )
    {
    std::cerr << "Error:" << std::endl;
    std::cerr << "Inertia calculated wrong. Expected: " << trueInertia << ", got: " 
      << inertia << std::endl;
    passed = false;
    }
  
  if( vnl_math_abs(clusterShade - trueClusterShade) > 0.001 )
    {
    std::cerr << "Error:" << std::endl;
    std::cerr << "ClusterShade calculated wrong. Expected: " << trueClusterShade << 
      ", got: "  << clusterShade << std::endl;
    passed = false;
    }

  if( vnl_math_abs(clusterProminence - trueClusterProminence) > 0.001 )
    {
    std::cerr << "Error:" << std::endl;
    std::cerr << "ClusterProminence calculated wrong. Expected: " 
      << trueClusterProminence << ", got: "  << clusterProminence << std::endl;
    passed = false;
    }
  
  if( vnl_math_abs(haralickCorrelation - trueHaralickCorrelation) > 0.001 )
    {
    std::cerr << "Error:" << std::endl;
    std::cerr << "Haralick's Correlation calculated wrong. Expected: "
      << trueHaralickCorrelation << ", got: "  << haralickCorrelation << std::endl;
    passed = false;
    }
  
  
  if (!passed)
    {
    std::cerr << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cerr << "Test succeeded" << std::endl;
    return EXIT_SUCCESS;
    }
  
  } catch( itk::ExceptionObject & err ) { 
    std::cerr << "ExceptionObject caught !" << std::endl; 
    std::cerr << err << std::endl; 
    std::cerr << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }
}

