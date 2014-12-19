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
#ifndef itkDataObjectIterator_h
#define itkDataObjectIterator_h

#include "itkProcessObject.h"

namespace itk
{
/** \class DataObjectIterator
 * \brief A forward iterator over the DataObject of a ProcessObject
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 * \ingroup ITKCommon
 */
class DataObjectIterator
{
public:

  typedef DataObject::DataObjectIdentifierType DataObjectIdentifierType;

  DataObjectIterator() {}

  DataObjectIterator(const DataObjectIterator & iter)
  {
    m_Iterator = iter.m_Iterator;
    m_Begin = iter.m_Begin;
    m_End = iter.m_End;
  }

  DataObjectIterator & operator=(const DataObjectIterator & iter)
  {
    if(this != &iter)
      {
      m_Iterator = iter.m_Iterator;
      m_Begin = iter.m_Begin;
      m_End = iter.m_End;
      }
    return *this;
  }

  DataObject * GetDataObject()
  {
    return m_Iterator->second;
  }

  const DataObjectIdentifierType & GetName() const
  {
    return m_Iterator->first;
  }

  DataObjectIterator operator++(int)
  {
    DataObjectIterator tmp = *this;
    ++(*this);
    return tmp;
  }

  DataObjectIterator & operator++()
  {
    ++m_Iterator;
    return *this;
  }

  bool operator==(const DataObjectIterator & iter) const
    {
    return m_Iterator == iter.m_Iterator && m_Begin == iter.m_Begin && m_End == iter.m_End;
    }

  bool operator!=(const DataObjectIterator & iter) const
    {
    return !( *this == iter );
    }

  void GoToBegin()
    {
      m_Iterator = m_Begin;
    }

    bool IsAtEnd() const
    {
      return m_Iterator == m_End;
    }

protected:
  typedef ProcessObject::DataObjectPointerMap::iterator InternalIteratorType;
  InternalIteratorType m_Iterator;
  InternalIteratorType m_Begin;
  InternalIteratorType m_End;
};
}
#endif
