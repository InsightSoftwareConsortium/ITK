/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ExpectationMaximizationValidationApp.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __ExpectationMaximizationValidationApp_txx
#define __ExpectationMaximizationValidationApp_txx

#include "ExpectationMaximizationValidationApp.h"

template< class TPixel, unsigned int VMeasurementVectorSize >
ExpectationMaximizationValidationApp< TPixel, VMeasurementVectorSize >
::ExpectationMaximizationValidationApp()
{
}

template< class TPixel, unsigned int VMeasurementVectorSize >
ExpectationMaximizationValidationApp< TPixel, VMeasurementVectorSize >
::~ExpectationMaximizationValidationApp()
{
}

template< class TPixel, unsigned int VMeasurementVectorSize >
void 
ExpectationMaximizationValidationApp< TPixel, VMeasurementVectorSize >
::GenerateSample()
{
  m_SampleGenerator.SetOutputSampleType(SampleGeneratorType::HISTOGRAM) ;
  m_SampleGenerator.SetImageFileNames(m_ImageFileNames) ;
  if ( m_MaskImageSliceOffset == -1 )
    {
      // do not use SliceFiller
      m_SampleGenerator.SetClassMaskImageFileName(m_MaskFileName.c_str(), 0) ;
    }
  else
    {
      m_SampleGenerator.SetClassMaskImageFileName(m_MaskFileName.c_str(), 0, m_MaskImageSliceOffset) ;
    }

  m_SampleGenerator.SetSelectedClasses(m_SelectedClasses) ;
  m_SampleGenerator.SetOutputNormalized(m_NormalizedSample) ;
  m_SampleGenerator.GenerateData() ;
  m_ClusteringMethod.SetSample(m_SampleGenerator.GetListSample()) ;
  m_ClusteringMethod.SetHistogram(m_SampleGenerator.GetHistogram()) ;
  m_ClusteringMethod.Initialize(m_SelectedClasses.size()) ;
}

template< class TPixel, unsigned int VMeasurementVectorSize >
void 
ExpectationMaximizationValidationApp< TPixel, VMeasurementVectorSize >
::PrepareInputParameterFilter()
{
  std::vector< std::string > filter ;
  for ( unsigned int i = 0 ; i < VMeasurementVectorSize ; i++ )
    {
      itk::OStringStream field ;
      field << "mean." << i + 1;
      m_InputFilter.push_back(field.str()) ;
    }

  for ( unsigned int i = 0 ; i < VMeasurementVectorSize * VMeasurementVectorSize ; i++ )
    {
      itk::OStringStream field ;
      field << "sigma." << i + 1;
      m_InputFilter.push_back(field.str()) ;
    }
}

template< class TPixel, unsigned int VMeasurementVectorSize >
void 
ExpectationMaximizationValidationApp< TPixel, VMeasurementVectorSize >
::PrepareOutputParameterTableHeader()
{
  m_OutputHeader.push_back("mapped class") ;
  for ( unsigned int i = 0 ; i < VMeasurementVectorSize ; i++ )
    {
      itk::OStringStream field ;
      field << "mean." << i + 1 ;
      m_OutputHeader.push_back(field.str()) ;
    }

  for ( unsigned int i = 0 ; i < VMeasurementVectorSize * VMeasurementVectorSize ; i++ )
    {
      itk::OStringStream field ;
      field << "sigma." << i + 1;
      m_OutputHeader.push_back(field.str()) ;
    }

  m_OutputHeader.push_back("proportion") ;

  for ( unsigned int i = 0 ; i < m_NumberOfClasses ; i++)
    {
      itk::OStringStream field ;
      field << m_ClassLabels[i] ;
      m_OutputHeader.push_back(field.str()) ;
    }
  m_OutputHeader.push_back("iterations") ;
  m_OutputHeader.push_back("time estimation") ;
  m_OutputHeader.push_back("time total") ;

} 

template< class TPixel, unsigned int VMeasurementVectorSize >
void
ExpectationMaximizationValidationApp< TPixel, VMeasurementVectorSize >
::StartClustering(ParametersType& params)
{
  unsigned int numberOfParametersPerClass = 
    VMeasurementVectorSize + 
    VMeasurementVectorSize * VMeasurementVectorSize ;

  ParametersType tempParams((numberOfParametersPerClass + 1) * m_NumberOfClasses) ;
  unsigned int paramIndex = 0 ;
  unsigned int tempParamIndex = 0 ;
  for ( unsigned int i = 0 ; i < m_NumberOfClasses ; i++ )
    {
      for ( unsigned int j = 0 ; j < numberOfParametersPerClass ; j++ )
        {
          tempParams[tempParamIndex] = params[paramIndex] ;
          ++tempParamIndex ;
          ++ paramIndex ;
        }
      tempParams[tempParamIndex] = m_InitialProportions[i] ;
      ++tempParamIndex ;
    }

  m_ClusteringMethod.SetMaximumIteration(m_MaximumIteration) ;
  m_ClusteringMethod.SetInitialParameters(tempParams) ;
 
  m_ClusteringMethod.Run() ;
  m_EstimatedParameters =  m_ClusteringMethod.GetEstimatedParameters() ;
  m_ClusterLabels = m_ClusteringMethod.GetClusterLabels() ;
}

template< class TPixel, unsigned int VMeasurementVectorSize >
void
ExpectationMaximizationValidationApp< TPixel, VMeasurementVectorSize >
::MapClusterToClass() 
{
  VectorType x ;
  double temp, minDistance ;
  unsigned int paramIndex = 0 ;
  unsigned int numberOfParametersPerClass = 
    VMeasurementVectorSize + 
    VMeasurementVectorSize * VMeasurementVectorSize + 1 ;

  ParametersType tempParameters = m_EstimatedParameters ;
  unsigned int classLabelIndex ;
  m_ClusterMap.resize(m_ClassLabels.size()) ;
  for ( unsigned int i = 0 ; i < m_NumberOfClasses ; i++ )
    {
      minDistance = itk::NumericTraits< double >::max() ;
      for ( unsigned int k = 0 ; k < VMeasurementVectorSize ; k++ )
        {
          x[k] = m_EstimatedParameters[paramIndex] ;
          ++paramIndex ;
        }
      paramIndex += numberOfParametersPerClass - VMeasurementVectorSize ;
      
      for ( unsigned int j = 0 ; j < m_NumberOfClasses ; j++ )
        {
          temp = m_DistanceMetric->Evaluate(m_SampleGenerator.GetClassMean(m_ClassLabels[j]), 
                                  x) ;
          if ( temp < minDistance )
            {
              //               std::cout << "DEBUG: swapping between " << i << " and " << j << std::endl ;
              minDistance = temp ;
              classLabelIndex = j ;
            }
          
        }
      
      m_ClusterMap[i] = m_ClassLabels[classLabelIndex] ;
    }
  
//   for (unsigned int k = 0 ; k < numberOfParametersPerClass ; k++ )
//     {
//       //           std::cout << "DEBUG: i begins at " << numberOfParametersPerClass*i
//       //                     << " j begins at " << numberOfParametersPerClass * classLabelIndex <<std::endl ;
//       tempParameters[numberOfParametersPerClass * i + k] = 
//         m_EstimatedParameters[numberOfParametersPerClass * classLabelIndex + k] ;
//     }

  m_EstimatedParameters = tempParameters ;
}

template< class TPixel, unsigned int VMeasurementVectorSize >
void 
ExpectationMaximizationValidationApp< TPixel, VMeasurementVectorSize >
::PutResult(unsigned int caseNo)
{
  unsigned int numberOfParametersPerClass = 
    VMeasurementVectorSize + 
    VMeasurementVectorSize * VMeasurementVectorSize + 1 ;

  ParameterTable::ParametersType tempOutputParams(m_OutputParameterTable.GetHeaderSize()) ;
  unsigned int classIndex ;
  unsigned int i ;
  unsigned int classLabel ;
  unsigned int outputParamIndex ;
  unsigned int paramIndex = 0 ;
  for ( classIndex = 0 ; classIndex < m_NumberOfClasses ; classIndex++)
    {
      classLabel = m_ClassLabels[classIndex] ;
      outputParamIndex = 0 ;
      tempOutputParams[outputParamIndex] = m_ClusterMap[classIndex] ;
      ++outputParamIndex ;
      for ( i = 0 ; i < numberOfParametersPerClass ; i++ )
        {
          tempOutputParams[outputParamIndex] = m_EstimatedParameters[paramIndex] ;
          ++outputParamIndex ;
          ++paramIndex ;
        }
      
      for ( i = 0 ; i < m_NumberOfClasses ; i++)
        {
          tempOutputParams[outputParamIndex] = 
            (m_Evaluator.GetComposition(classLabel))[i] ;
          ++outputParamIndex ;
        }
      tempOutputParams[outputParamIndex] = m_ClusteringMethod.GetLastIteration() ;
      ++outputParamIndex ;
      
      tempOutputParams[outputParamIndex] = m_ClusteringMethod.GetEstimationElapsedTime() ;
      ++outputParamIndex ;
      
      tempOutputParams[outputParamIndex] = m_ClusteringMethod.GetTotalElapsedTime() ;
      ++outputParamIndex ;
      
      m_OutputParameterTable.SetParameters(caseNo, classLabel, tempOutputParams) ; 
    }
}

#endif
