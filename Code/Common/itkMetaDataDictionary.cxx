/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaDataDictionary.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMetaDataDictionary.h"

namespace itk
{

MetaDataDictionary
::MetaDataDictionary()
{
  m_Dictionary = new MetaDataDictionaryMapType;
}

MetaDataDictionary
::~MetaDataDictionary()
{
  if (m_Dictionary)
    {
    delete m_Dictionary;
    m_Dictionary = 0;
    }
}

MetaDataDictionary
::MetaDataDictionary(const MetaDataDictionary &old)
{
  m_Dictionary = new MetaDataDictionaryMapType;
  *m_Dictionary = *(old.m_Dictionary);
}

void MetaDataDictionary
::operator=(const MetaDataDictionary &old)
{
  *m_Dictionary = *(old.m_Dictionary);  
}

void
MetaDataDictionary
::Print(std::ostream& os) const
{
  for(MetaDataDictionaryMapType::const_iterator it=m_Dictionary->begin();
      it != m_Dictionary->end();
      it++)
    {
    os << it->first <<  "  " ;
    it->second->Print(os);
    }
}

MetaDataObjectBase::Pointer &
MetaDataDictionary
::operator [](const std::string &key)
{
  return (*m_Dictionary)[key];
}

const MetaDataObjectBase *
MetaDataDictionary
::operator [](const std::string &key) const
{
  MetaDataObjectBase::Pointer entry = (*m_Dictionary)[key];
  const  MetaDataObjectBase * constentry = entry.GetPointer();
  return constentry;
}


bool
MetaDataDictionary
::HasKey(const std::string &key)
{
  return m_Dictionary->find(key) != m_Dictionary->end();
}

std::vector<std::string>
MetaDataDictionary
::GetKeys() const
{
  typedef std::vector<std::string> VectorType;
  VectorType ans;

  for (MetaDataDictionaryMapType::const_iterator it = m_Dictionary->begin();
       it != m_Dictionary->end(); ++it)
    {
    ans.push_back( (*it).first );
    }

  return ans;
}

MetaDataDictionary::Iterator
MetaDataDictionary
::Begin()
{
  return m_Dictionary->begin();
}


MetaDataDictionary::ConstIterator
MetaDataDictionary
::Begin() const
{
  return m_Dictionary->begin();
}


MetaDataDictionary::Iterator
MetaDataDictionary
::End()
{
  return m_Dictionary->end();
}


MetaDataDictionary::ConstIterator
MetaDataDictionary
::End() const
{
  return m_Dictionary->end();
}



MetaDataDictionary::Iterator
MetaDataDictionary
::Find( const std::string & key)
{
  return m_Dictionary->find(key);
}


MetaDataDictionary::ConstIterator
MetaDataDictionary
::Find(const std::string & key) const
{
  return m_Dictionary->find(key);
}





}; // namespace

