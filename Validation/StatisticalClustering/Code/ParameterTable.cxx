#include "ParameterTable.h"

#include <fstream>
#include <string>

void
ParameterTable
::Load(const char* fileName)
{
  m_NumberOfClasses = 0 ;
  m_NumberOfCases = 0 ;

  std::ifstream paramFile(fileName) ;
  if ( !paramFile )
    {
      std::cout << "ERROR: cannot open the parameter file" << std::endl ;
      return ;
    } 
  
  // parse the header
  std::string header ;
  std::string field ;

  std::getline(paramFile, header) ;
  std::string::size_type fieldBegin = 0 ;
  std::string::size_type fieldEnd = 0 ;
  while ( fieldBegin < header.length() )
    {
      fieldBegin = header.find('"', fieldBegin) + 1 ;
      fieldEnd = header.find('"', fieldBegin) ;
      field = header.substr(fieldBegin, fieldEnd - fieldBegin) ;
      m_Fields.push_back(field) ; 
      fieldBegin = fieldEnd + 1 ;
    }

  // erase the "case" and "class" field names
  m_Fields.erase(m_Fields.begin()) ;
  m_Fields.erase(m_Fields.begin()) ;
//   std::copy(m_Fields.begin(), m_Fields.end(), 
//             std::ostream_iterator<std::string>(std::cout, " ") ) ;
//   std::cout << std::endl ;

  unsigned int i ;
  double tempValue ;
  unsigned int caseIndex ;
  unsigned int classLabel ;
  ParametersType tempRecord(m_Fields.size()) ;
  GroupType tempGroup ;
  bool first = true ;

  unsigned int firstClassLabel ;
  // parsing the first group of classes
  while ( true )
    {
      paramFile >> caseIndex ;
      paramFile >> classLabel ;

      if ( (classLabel == firstClassLabel && !first) || paramFile.eof() )
        {
          m_Table.push_back(tempGroup) ;
          break ;
        }

      if ( first )
        {
          firstClassLabel = classLabel ;
        }

      m_ClassLabels.push_back(classLabel) ;

      for ( i = 0 ; i < m_Fields.size() ; i++ )
        {  
          paramFile >> tempValue ;
          tempRecord[i] = tempValue ;
        }

      tempGroup.push_back(tempRecord) ;
      first = false ;
    }

  m_NumberOfClasses = m_ClassLabels.size() ;
  first = true ;
  // parse and store the rest 
  unsigned int index = 0 ;
  while ( true && !paramFile.eof() ) 
    {
      
      if ( !first )
        {
          paramFile >> caseIndex ;
          paramFile >> classLabel ;

          if ( paramFile.eof() )
            {
              m_Table.push_back(tempGroup) ;
              break ;
            }

          if ( classLabel == m_ClassLabels[0] )
            {
              m_Table.push_back(tempGroup) ;
              index = 0 ;
            }
        }
      else
        {
          first = false ;
        }
      
      for ( i = 0 ; i < m_Fields.size() ; i++ )
        {  
          paramFile >> tempValue ;
          tempRecord[i] = tempValue ;
        }
      
      tempGroup[index] = tempRecord ;
      ++index ;
    } // end of while

  m_NumberOfCases = m_Table.size() ;
//   std::cout << " table size = " << m_Table.size() << std::endl ;

//   int fieldIndex ;
//   for ( caseIndex = 0 ; caseIndex < m_Table.size() ; caseIndex++ )
//     {
//       for ( classLabel = 0 ; classLabel < m_NumberOfClasses ; classLabel++ )
//         {
//           std::cout << caseIndex << " " ;
//           std::cout << classLabel << " " ;
//           for ( fieldIndex = 0 ; fieldIndex < (m_Fields.size() - 2) ; fieldIndex++ )
//             {
//               std::cout << m_Table[caseIndex][classLabel][fieldIndex] << " " ;
//             }
//           std::cout << std::endl ;
//         }
//     }
  m_WriteMode = false ;
  this->ClearFilter() ;
}

void
ParameterTable
::Create(const HeaderType &header, 
         const unsigned int numberOfCases, 
         const std::vector< unsigned int >& classLabels)
{
  m_Fields = header ;
  m_NumberOfCases = numberOfCases ;
  m_ClassLabels = classLabels ;
  m_NumberOfClasses = m_ClassLabels.size() ;
  ParametersType tempRecord(m_Fields.size()) ;
  GroupType tempGroup(m_NumberOfClasses) ;
  m_Table.resize(m_NumberOfCases) ;
  for ( unsigned int caseIndex = 0 ; caseIndex < m_NumberOfCases ; caseIndex++ )
    {
      for ( unsigned int classIndex = 0 ; classIndex < m_NumberOfClasses ; classIndex++ )
        {
          tempGroup[classIndex] = tempRecord ;
        }
      m_Table[caseIndex] = tempGroup ;
    }
  
  m_WriteMode = true ;
  this->ClearFilter() ;
}

void 
ParameterTable
::Write(const char* fileName, bool useFilter)
{
  if ( !m_WriteMode )
    {
      return ;
    }

  std::ofstream paramFile(fileName) ;
  if ( !paramFile )
    {
      std::cout << "ERROR: cannot open the parameter file" << std::endl ;
      return ;
    } 
  
  // print the header
  paramFile << "\"case\" \"class\" " ;

  unsigned int caseIndex ;
  unsigned int classIndex ;
  unsigned int fieldIndex ;

  FilterType::iterator f_iter ;
  if ( !useFilter )
    {
      HeaderType::iterator h_iter = m_Fields.begin() ;
      while ( h_iter != m_Fields.end() )
        {
          paramFile << "\"" << *h_iter << "\" " ;
          ++h_iter ;
        }
      paramFile << std::endl ;

      for ( caseIndex = 0 ; caseIndex < m_Table.size() ; caseIndex++ )
        {
          for ( classIndex = 0 ; classIndex < m_NumberOfClasses ; classIndex++ )
            {
              paramFile << caseIndex + 1 << " " ;
              paramFile << m_ClassLabels[classIndex] << " " ;
              for ( fieldIndex = 0 ; fieldIndex < m_Fields.size() ; fieldIndex++ )
                {
                  paramFile << m_Table[caseIndex][classIndex][fieldIndex] << " " ;
                }
              paramFile << std::endl ;
            }
        }
    }
  else
    {
      f_iter = m_Filter.begin() ;
      while ( f_iter != m_Filter.end() )
        {
          paramFile << "\"" << m_Fields[*f_iter] << "\" " ;
          ++f_iter ;
        }
      paramFile << std::endl ;

      for ( caseIndex = 0 ; caseIndex < m_Table.size() ; caseIndex++ )
        {
          for ( classIndex = 0 ; classIndex < m_NumberOfClasses ; classIndex++ )
            {
              paramFile << caseIndex + 1 << " " ;
              paramFile << m_ClassLabels[classIndex] << " " ;
              f_iter = m_Filter.begin() ;
              while ( f_iter != m_Filter.end() )
                {
                  paramFile << m_Table[caseIndex][classIndex][*f_iter] << " " ;
                  ++f_iter ;
                }
              paramFile << std::endl ;
            }
        }
    }
}

void
ParameterTable
::SetFilter(const HeaderType &fields)
{
  unsigned int fieldIndex ;
  m_Filter.clear() ;
  HeaderType::const_iterator iter = fields.begin() ;
  while ( iter != fields.end() )
    {
      fieldIndex = this->GetFieldIndex((*iter).c_str()) ;
      m_Filter.push_back(fieldIndex) ;
      ++iter ;
    }
}

void 
ParameterTable
::ClearFilter()
{
  m_Filter.clear() ;
  for (unsigned int fieldIndex = 0 ; fieldIndex < m_Fields.size() ; fieldIndex++ )
    {
      m_Filter.push_back(fieldIndex) ;
    }
}

ParameterTable::ParametersType 
ParameterTable
::GetParameters(const unsigned int caseIndex, 
                const unsigned int classLabel)
{
  ParametersType params(m_Filter.size()) ;
  FilterType::iterator iter = m_Filter.begin() ;
  unsigned int index = 0 ;
  while ( iter != m_Filter.end() )
    {
      params[index] = m_Table[caseIndex][this->GetClassIndex(classLabel)][*iter] ;
      ++index ;
      ++iter ;
    }

  return params ;
}

void
ParameterTable
::SetParameters(const unsigned int caseIndex, 
                const unsigned int classLabel, 
                const ParametersType& params)
{
  if ( !m_WriteMode )
    {
      return ;
    }

  FilterType::iterator iter = m_Filter.begin() ;
  unsigned int index = 0 ;
  while ( iter != m_Filter.end() )
    {
      m_Table[caseIndex][this->GetClassIndex(classLabel)][*iter] = params[index] ;
      ++index ;
      ++iter ;
    }
}

unsigned int 
ParameterTable
::GetFieldIndex(const char* fieldName) const
{
  unsigned int index ;
  std::string sfield = fieldName ;
  for ( index = 0 ; index < m_Fields.size() ; index++ )
    {
      if ( m_Fields[index] == sfield )
        {
          break ;
        }
    }
  return index ;
}

unsigned int
ParameterTable
::GetClassIndex(const unsigned int classLabel) const
{
  for ( unsigned int i = 0 ; i < m_ClassLabels.size() ; i++ )
    {
      if ( classLabel == m_ClassLabels[i] )
        {
          return i ;
        }
    }
  return 0 ;
}


void
ParameterTable
::SetParameter(const unsigned int caseIndex, 
               const unsigned int classLabel,
               const unsigned int fieldIndex,
               const double value)
{
  if ( !m_WriteMode )
    {
      return ;
    }

  m_Table[caseIndex][this->GetClassIndex(classLabel)][fieldIndex] = value ;
}

double
ParameterTable
::GetParameter(const unsigned int caseIndex, 
               const unsigned int classLabel,
               const unsigned int fieldIndex) const
{
  return m_Table[caseIndex][this->GetClassIndex(classLabel)][fieldIndex] ;
}

double
ParameterTable
::GetParameter(const unsigned int caseIndex, 
               const unsigned int classLabel,
               const char* fieldName) const
{
  return m_Table[caseIndex][this->GetClassIndex(classLabel)][this->GetFieldIndex(fieldName)] ;
}

std::vector< unsigned int >
ParameterTable
::GetClassLabels()
{
  return m_ClassLabels ;
}
