/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    StatisticalClusteringValidationAppBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __StatisticalClusteringValidationAppBase_h
#define __StatisticalClusteringValidationAppBase_h

#include "OptionList.h"
#include "ValidationSampleGenerator.h"
#include "ParameterTable.h"

#include "itkEuclideanDistance.h"
#include "ClusteringOutputEvaluator.h"
#include "KdTreeBasedKmeansClusteringMethod.h"

#define IMAGE_DIMENSION 3

template< class TPixel, unsigned int VMeasurementVectorSize >
class StatisticalClusteringValidationAppBase
{
public:
  StatisticalClusteringValidationAppBase() ;
  ~StatisticalClusteringValidationAppBase() ;

  typedef itk::Image< short, IMAGE_DIMENSION > ImageType ;
  typedef itk::Image< unsigned char , IMAGE_DIMENSION > MaskImageType ;
  typedef itk::FixedArray< short, VMeasurementVectorSize > VectorPixelType ;
  typedef itk::Image< VectorPixelType, IMAGE_DIMENSION > VectorImageType ;

  typedef ValidationSampleGenerator< ImageType, MaskImageType, 
                                     VectorImageType > SampleGeneratorType ;
  
  typedef itk::Array< double > ParametersType ;
  typedef itk::Vector< double, VMeasurementVectorSize > VectorType ;
  typedef itk::Statistics::EuclideanDistance< VectorType > DistanceMetricType ;
  
  void SetCommandLineOptions(int argc, char* argv[]) ;
  void SetSelectedClasses(const std::vector< unsigned int >& classLabels) ;

  void Run() ;

protected:
  virtual void GenerateSample() = 0 ; 
  virtual void PrepareInputParameterFilter() = 0 ; 
  virtual void PrepareOutputParameterTableHeader() = 0 ; 
  virtual void StartClustering(ParametersType& params) = 0 ;
  virtual void MapClusterToClass() = 0 ;
  virtual void PutResult(unsigned int caseNo) = 0 ;

  SampleGeneratorType m_SampleGenerator ;
  ParametersType m_InitialProportions ;

  ParameterTable::HeaderType m_InputFilter ;
  ParameterTable::HeaderType m_OutputHeader ;
  ParameterTable m_InputParameterTable ;
  ParameterTable m_OutputParameterTable ;

  unsigned int m_NumberOfCases ;
  unsigned int m_NumberOfClasses ;
  std::vector< unsigned int > m_ClassLabels ;
  
  std::vector< std::string > m_ImageFileNames ;
  std::string m_MaskFileName ;
  int m_MaskImageSliceOffset ;
  std::string m_ParameterFileName ;
  std::string m_ResultFileName ;
  int m_KdTreeBucketSize ;
  int m_MaximumIteration ;
  bool m_NormalizedSample ;
  std::vector< unsigned int > m_SelectedClasses ;

  ParametersType m_EstimatedParameters ;
  itk::hash_map< unsigned long, unsigned int >* m_ClusterLabels ;
  std::vector< unsigned int > m_ClusterMap ;
  DistanceMetricType::Pointer m_DistanceMetric ;
  ClusteringOutputEvaluator m_Evaluator ;
} ; // end of class

#ifndef ITK_MANUAL_INSTANTIATION
#include "StatisticalClusteringValidationAppBase.txx"
#endif

#endif
