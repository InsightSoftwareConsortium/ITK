/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkMetaDataDictionary.h"

namespace itk
{
MetaDataDictionary ::MetaDataDictionary()
  : m_Dictionary(std::make_shared<MetaDataDictionaryMapType>())
{}

MetaDataDictionary ::~MetaDataDictionary() = default;

// NOTE: Desired behavior is to perform shallow copy, so m_Dictionary is shared
//       as is thee default behavior for copy constructors.
MetaDataDictionary ::MetaDataDictionary(const MetaDataDictionary &) = default;

MetaDataDictionary &
MetaDataDictionary ::operator=(const MetaDataDictionary & old)
{
  if (this != &old)
  {
    // perform shallow copy, so m_Dictionary is shared
    m_Dictionary = old.m_Dictionary;
  }
  return *this;
}

void
MetaDataDictionary ::Print(std::ostream & os) const
{
  os << "Dictionary use_count: " << m_Dictionary.use_count() << std::endl;
  for (MetaDataDictionaryMapType::const_iterator it = m_Dictionary->begin(); it != m_Dictionary->end(); ++it)
  {
    os << (*it).first << "  ";
    (*it).second->Print(os);
  }
}

MetaDataObjectBase::Pointer & MetaDataDictionary ::operator[](const std::string & key)
{
  MakeUnique();
  return (*m_Dictionary)[key];
}

const MetaDataObjectBase * MetaDataDictionary ::operator[](const std::string & key) const
{
  auto iter = m_Dictionary->find(key);
  if (iter == m_Dictionary->end())
  {
    return nullptr;
  }

  const MetaDataObjectBase * constentry = iter->second.GetPointer();
  return constentry;
}

const MetaDataObjectBase *
MetaDataDictionary ::Get(const std::string & key) const
{
  if (!this->HasKey(key))
  {
    itkGenericExceptionMacro(<< "Key '" << key << "' does not exist ");
  }
  MetaDataObjectBase::Pointer entry = (*m_Dictionary)[key];
  const MetaDataObjectBase *  constentry = entry.GetPointer();
  return constentry;
}

void
MetaDataDictionary ::Set(const std::string & key, MetaDataObjectBase * object)
{
  MakeUnique();
  (*m_Dictionary)[key] = object;
}

bool
MetaDataDictionary ::HasKey(const std::string & key) const
{
  return m_Dictionary->find(key) != m_Dictionary->end();
}

std::vector<std::string>
MetaDataDictionary ::GetKeys() const
{
  using VectorType = std::vector<std::string>;
  VectorType ans;

  for (MetaDataDictionaryMapType::const_iterator it = m_Dictionary->begin(); it != m_Dictionary->end(); ++it)
  {
    ans.push_back((*it).first);
  }

  return ans;
}

MetaDataDictionary::Iterator
MetaDataDictionary ::Begin()
{
  MakeUnique();
  return m_Dictionary->begin();
}

MetaDataDictionary::ConstIterator
MetaDataDictionary ::Begin() const
{
  return m_Dictionary->begin();
}

MetaDataDictionary::Iterator
MetaDataDictionary ::End()
{
  MakeUnique();
  return m_Dictionary->end();
}

MetaDataDictionary::ConstIterator
MetaDataDictionary ::End() const
{
  return m_Dictionary->end();
}

MetaDataDictionary::Iterator
MetaDataDictionary ::Find(const std::string & key)
{
  MakeUnique();
  return m_Dictionary->find(key);
}

MetaDataDictionary::ConstIterator
MetaDataDictionary ::Find(const std::string & key) const
{
  return m_Dictionary->find(key);
}

void
MetaDataDictionary ::Clear()
{
  // Construct a new one instead of enforcing uniqueness then clearing
  this->m_Dictionary = std::make_shared<MetaDataDictionaryMapType>();
}

void
MetaDataDictionary ::Swap(MetaDataDictionary & other)
{
  using std::swap;
  swap(m_Dictionary, other.m_Dictionary);
}


bool
MetaDataDictionary ::MakeUnique()
{
  if (m_Dictionary.use_count() > 1)
  {
    // copy the shared dictionary.
    m_Dictionary = std::make_shared<MetaDataDictionaryMapType>(*m_Dictionary);
    return true;
  }
  return false;
}

bool
MetaDataDictionary ::Erase(const std::string & key)
{
  auto                                      it = m_Dictionary->find(key);
  const MetaDataDictionaryMapType::iterator end = m_Dictionary->end();

  if (it != end)
  {
    if (MakeUnique())
    {
      // Need to find the correct iterator, in the new copy
      it = m_Dictionary->find(key);
    }
    m_Dictionary->erase(it);
    return true;
  }
  return false;
}

} // namespace itk
