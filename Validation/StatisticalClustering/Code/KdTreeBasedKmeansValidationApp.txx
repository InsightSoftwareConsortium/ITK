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
#ifndef __KdTreeBasedKmeansValidationApp_txx
#define __KdTreeBasedKmeansValidationApp_txx

#include "KdTreeBasedKmeansValidationApp.h"

template< class TPixel, unsigned int VMeasurementVectorSize >
KdTreeBasedKmeansValidationApp< TPixel, VMeasurementVectorSize >
::KdTreeBasedKmeansValidationApp()
{
}

template< class TPixel, unsigned int VMeasurementVectorSize >
KdTreeBasedKmeansValidationApp< TPixel, VMeasurementVectorSize >
::~KdTreeBasedKmeansValidationApp()
{
}

template< class TPixel, unsigned int VMeasurementVectorSize >
void
KdTreeBasedKmeansValidationApp< TPixel, VMeasurementVectorSize >
::GenerateSample()
{
  m_SampleGenerator.SetOutputSampleType(SampleGeneratorType::WEIGHTED_CENTROID_KD_TREE) ;
  m_SampleGenerator.SetKdTreeBucketSize(m_KdTreeBucketSize) ;
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
}

template< class TPixel, unsigned int VMeasurementVectorSize >
void
KdTreeBasedKmeansValidationApp< TPixel, VMeasurementVectorSize >
::PrepareInputParameterFilter()
{
  std::vector< std::string > filter ;
  for ( unsigned int i = 0 ; i < VMeasurementVectorSize ; i++ )
    {
      itk::OStringStream field ;
      field << "mean." << i + 1;
      m_InputFilter.push_back(field.str()) ;
    }

}

template< class TPixel, unsigned int VMeasurementVectorSize >
void
KdTreeBasedKmeansValidationApp< TPixel, VMeasurementVectorSize >
::PrepareOutputParameterTableHeader()
{
  m_OutputHeader.push_back("mapped class") ;
  for ( unsigned int i = 0 ; i < VMeasurementVectorSize ; i++ )
    {
      itk::OStringStream field ;
      field << "mean." << i + 1 ;
      m_OutputHeader.push_back(field.str()) ;
    }
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
KdTreeBasedKmeansValidationApp< TPixel, VMeasurementVectorSize >
::StartClustering(ParametersType& params)
{
  m_ClusteringMethod.SetMaximumIteration(m_MaximumIteration) ;
  m_ClusteringMethod.SetKdTree(m_SampleGenerator.GetKdTree()) ;
  m_ClusteringMethod.SetInitialParameters(params) ;
  m_ClusteringMethod.Run() ;
  m_EstimatedParameters =  m_ClusteringMethod.GetEstimatedParameters() ;
  m_ClusterLabels = m_ClusteringMethod.GetClusterLabels() ;
}

template< class TPixel, unsigned int VMeasurementVectorSize >
void
KdTreeBasedKmeansValidationApp< TPixel, VMeasurementVectorSize >
::MapClusterToClass()
{
  VectorType x ;
  double temp, minDistance ;
  unsigned int paramIndex = 0 ;
  unsigned int numberOfParametersPerClass = VMeasurementVectorSize ;
  ParametersType tempParameters(m_EstimatedParameters) ;
  m_ClusterMap.resize(m_ClassLabels.size()) ;
  for ( unsigned int i = 0 ; i < m_NumberOfClasses ; i++ )
    {
      minDistance = itk::NumericTraits< double >::max() ;
      for ( unsigned int k = 0 ; k < VMeasurementVectorSize ; k++ )
        {
          x[k] = m_EstimatedParameters[paramIndex] ;
          ++paramIndex ;
        }

      for ( unsigned int j = 0 ; j < m_NumberOfClasses ; j++ )
        {
          temp = m_DistanceMetric->Evaluate(m_SampleGenerator.GetClassMean(m_ClassLabels[j]),
                                  x) ;
          if ( temp < minDistance )
            {
              minDistance = temp ;
              m_ClusterMap[i] = m_ClassLabels[j] ;
            }
        }
    }

  m_EstimatedParameters = tempParameters ;
}

template< class TPixel, unsigned int VMeasurementVectorSize >
void
KdTreeBasedKmeansValidationApp< TPixel, VMeasurementVectorSize >
::PutResult(unsigned int caseNo)
{
  unsigned int numberOfParametersPerClass = VMeasurementVectorSize ;
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
