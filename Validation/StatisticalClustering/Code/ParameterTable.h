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
#ifndef __ParameterTable_h
#define __ParameterTable_h

#include <vector>
#include <map>
#include <string>

#include "itkArray.h"

class ParameterTable
{
public:
  typedef std::vector< unsigned int > FilterType ;
  typedef itk::Array< double > ParametersType ;
  typedef std::vector< ParametersType > GroupType ;
  typedef std::vector< GroupType > TableType ;
  typedef std::vector< std::string > HeaderType ;

  ParameterTable() {}
  virtual ~ParameterTable() {}

  void Load(const char* fileName) ;
  void Create(const HeaderType &header,
              const unsigned int numberOfCases,
              const std::vector< unsigned int >& classLabels) ;
  void Write(const char* fileName, bool useFilter = false) ;

  unsigned int GetFieldIndex(const char* fieldName) const ;
  unsigned int GetClassIndex(const unsigned int classLabel) const ;

  void SetFilter(const HeaderType &header) ;

  void ClearFilter() ;

  unsigned int GetHeaderSize()
  { return m_Fields.size() ; }

  ParametersType GetParameters(const unsigned int caseIndex,
                               const unsigned int classLabel) ;

  void SetParameters(const unsigned int caseIndex,
                     const unsigned int classLabel,
                     const ParametersType& params) ;

  void SetParameter(const unsigned int caseIndex,
                    const unsigned int classLabel,
                    const unsigned int fieldIndex,
                    const double value) ;

  double GetParameter(const unsigned int caseIndex,
                      const unsigned int classLabel,
                      const unsigned int fieldIndex) const ;

  double GetParameter(const unsigned int caseIndex,
                      const unsigned int classLabel,
                      const char* fieldName) const ;

  std::vector< unsigned int > GetClassLabels() ;

  unsigned int GetNumberOfClasses() const
  { return m_NumberOfClasses ; }

  unsigned int GetNumberOfCases() const
  { return m_NumberOfCases ; }

private:
  TableType m_Table ;
  std::vector< unsigned int > m_ClassLabels ;
  unsigned int m_NumberOfCases ;
  unsigned int m_NumberOfClasses ;
  HeaderType m_Fields ;
  FilterType m_Filter ;
  bool m_WriteMode ;
} ; // end of class

#endif
