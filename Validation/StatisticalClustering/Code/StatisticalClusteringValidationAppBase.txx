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
#ifndef __StatisticalClusteringValidationAppBase_txx
#define __StatisticalClusteringValidationAppBase_txx

#include "StatisticalClusteringValidationAppBase.h"

template< class TPixel, unsigned int VMeasurementVectorSize >
StatisticalClusteringValidationAppBase< TPixel, VMeasurementVectorSize >
::StatisticalClusteringValidationAppBase()
{
  m_DistanceMetric = DistanceMetricType::New() ;
}

template< class TPixel, unsigned int VMeasurementVectorSize >
StatisticalClusteringValidationAppBase< TPixel, VMeasurementVectorSize >
::~StatisticalClusteringValidationAppBase()
{
}

template< class TPixel, unsigned int VMeasurementVectorSize >
void
StatisticalClusteringValidationAppBase< TPixel, VMeasurementVectorSize >
::SetCommandLineOptions(int argc, char* argv[])
{
  namespace stat = itk::Statistics ;

  if (argc <= 1)
    {
      print_usage() ;
      exit(0) ;
    }

  OptionList options(argc, argv) ;

  std::vector< std::string > imageFiles ;

  try
    {
      options.GetMultiStringOption("images", &m_ImageFileNames, true) ;
      options.GetStringOption("mask", &m_MaskFileName, true) ;
      options.GetStringOption("parameters", &m_ParameterFileName, true) ;
      options.GetStringOption("result", &m_ResultFileName, true) ;
      m_MaskImageSliceOffset = options.GetIntOption("slice-offset", 0, false);
      m_KdTreeBucketSize = options.GetIntOption("bucket-size", -1, false) ;
      m_MaximumIteration = options.GetIntOption("iteration", 4000, false) ;
    }
  catch(OptionList::RequiredOptionMissing e)
    {
      std::cout << "ERROR: The '" << e.OptionTag
                << "' option is required but missing."
                << std::endl ;
      exit(1) ;
    }
  if ( VMeasurementVectorSize > 1 )
    {
      m_NormalizedSample = true ;
    }
  else
    {
      m_NormalizedSample = false ;
    }
}

template< class TPixel, unsigned int VMeasurementVectorSize >
void
StatisticalClusteringValidationAppBase< TPixel, VMeasurementVectorSize >
::SetSelectedClasses(const std::vector< unsigned int >& classLabels)
{
  m_SelectedClasses = classLabels ;
}

template< class TPixel, unsigned int VMeasurementVectorSize >
void
StatisticalClusteringValidationAppBase< TPixel, VMeasurementVectorSize >
::Run()
{
  std::cout << "DEBUG: Generating sample..." << std::endl ;
  // each subclass should specify the type of the output sample
  this->GenerateSample() ;

  // each subclass should specify which field of the parameter is
  // needed for the class
  std::cout << "DEBUG: Preparing the input parameter filter..." << std::endl ;
  this->PrepareInputParameterFilter() ;

  std::cout << "DEBUG: Loading Input parameter table..." << std::endl ;
  // load the input parameter table
  m_InputParameterTable.Load(m_ParameterFileName.c_str()) ;
  m_NumberOfCases = m_InputParameterTable.GetNumberOfCases() ;
  m_NumberOfClasses = m_InputParameterTable.GetNumberOfClasses() ;
  m_ClassLabels = m_InputParameterTable.GetClassLabels() ;
  m_InputParameterTable.SetFilter(m_InputFilter) ;

  m_InitialProportions = ParametersType(m_NumberOfClasses) ;
  for ( unsigned int i = 0 ; i < m_NumberOfClasses ; i++ )
    {
      m_InitialProportions[i] =
        (double)m_SampleGenerator.GetClassSize(m_ClassLabels[i]) /
        (double)m_SampleGenerator.GetListSample()->Size() ;
    }


  std::cout << "DEBUG: Preparing the Output header..." << std::endl ;
  this->PrepareOutputParameterTableHeader() ;

  std::cout << "DEBUG: Creating output parameter table..." << std::endl ;
  // creats a ParameterTable object of the result
  m_OutputParameterTable.Create(m_OutputHeader, m_NumberOfCases, m_ClassLabels) ;

  unsigned int caseNo, classIndex, paramIndex ;
  unsigned int numberOfParametersPerClass = m_InputFilter.size() ;
  itk::Array< double > params(numberOfParametersPerClass * m_NumberOfClasses) ;

  m_Evaluator.SetUniqueClassLabels(m_ClassLabels) ;
  m_Evaluator.SetTruth(m_SampleGenerator.GetClassLabels()) ;

  for ( caseNo = 0 ; caseNo < m_NumberOfCases ; caseNo++ )
    {
      std::cout << "DEBUG: =================================== " << std::endl ;
      std::cout << "DEBUG: run = " << caseNo + 1 << std::endl ;
      paramIndex = 0 ;
      for ( classIndex = 0 ; classIndex < m_NumberOfClasses ; classIndex++ )
        {
          for ( unsigned int i = 0 ; i < numberOfParametersPerClass ; i++ )
            {
              params[paramIndex] =
                m_InputParameterTable.GetParameters(caseNo, m_ClassLabels[classIndex])[i] ;
              ++paramIndex ;
            }
        }

      // each subclass does the estimation & mapping clusters to classes
      std::cout << "DEBUG: Clustering..." << std::endl ;
      std::cout << "DEBUG: Input parameters = " << params << std::endl ;
      this->StartClustering(params) ;

      std::cout << "DEBUG: estimated parameters = "
                << m_EstimatedParameters << std::endl ;

      // subclass
      std::cout << "DEBUG: Mapping clusters to classes..." << std::endl ;
      this->MapClusterToClass() ;
      std::cout << "DEBUG: Evaluating..." << std::endl ;
      m_Evaluator.SetClusteringResult(m_ClusterLabels) ;
      m_Evaluator.SetClusterMap(m_ClusterMap) ;
      m_Evaluator.GenerateData() ;

      std::cout << "DEBUG: Putting the result to the table" << std::endl ;
      this->PutResult(caseNo) ;
    }

  m_OutputParameterTable.Write(m_ResultFileName.c_str()) ;
}

#endif
