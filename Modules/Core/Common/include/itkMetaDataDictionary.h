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
#ifndef itkMetaDataDictionary_h
#define itkMetaDataDictionary_h

#include "itkMetaDataObjectBase.h"
#include <vector>
#include <map>
#include <string>

namespace itk
{
/** \class MetaDataDictionary
 *  \brief Provides a mechanism for storing a collection of arbitrary
 *         data types
 *
 * \author Hans J. Johnson
 *
 * The MetaDataDictionary, along with the MetaDataObject derived template
 * classes, is designed to provide a mechanism for storing a collection of
 * arbitrary data types. The main motivation for such a collection is to
 * associate arbitrary data elements with itk DataObjects.
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT MetaDataDictionary
{
public:
  typedef MetaDataDictionary Self;
  /**
   * Defines the default behavior for printing out this element
   * \param os An output stream
   */
  virtual void Print(std::ostream & os) const;

  // Declare the datastructure that will be used to hold the
  // dictionary.
  typedef std::map< std::string, MetaDataObjectBase::Pointer >  MetaDataDictionaryMapType;
  typedef MetaDataDictionaryMapType::iterator                   Iterator;
  typedef MetaDataDictionaryMapType::const_iterator             ConstIterator;

  // Constructor
  MetaDataDictionary();
  // Copy Constructor
  MetaDataDictionary(const MetaDataDictionary &);
  // operator =
  MetaDataDictionary & operator=(const MetaDataDictionary &);

  // Destructor
  virtual ~MetaDataDictionary();

  /** Returns a vector of keys to the key/value entries in the
   * dictionary.  Iterate through the dictionary using these keys.
   */
  std::vector< std::string > GetKeys() const;

  // Implement map's api. On some Micorsoft compilers, stl containers
  // cannot be exported. This causes problems when building DLL's.
  // Here we inherit privately from std::map and provide a simple
  // API. The implementation will be in the DLL.
  MetaDataObjectBase::Pointer & operator[](const std::string &);

  const MetaDataObjectBase * operator[](const std::string &) const;

  const MetaDataObjectBase * Get(const std::string &) const;
  void Set(const std::string &, MetaDataObjectBase * );
  bool HasKey(const std::string &) const;

  bool Erase(const std::string&);

  /** \warning the following functions SHOULD NOT be used with
   * the visual studio 6 compiler since iterator outside of the dll
   * context cannot be dereferenced safely */

  /** Returns an iterator to the beginning of the map */
#if !defined( ITK_WRAPPING_PARSER )
  Iterator  Begin();

  ConstIterator  Begin() const;

#endif

  /** Returns an iterator to the end of the map */
#if !defined( ITK_WRAPPING_PARSER )
  Iterator  End();

  ConstIterator  End() const;

#endif

  /** Returns an iterator matching the string key */
#if !defined( ITK_WRAPPING_PARSER )
  Iterator  Find(const std::string & key);

  ConstIterator  Find(const std::string & key) const;

#endif
  /** remove all MetaObjects from dictionary */
  void Clear();

private:
  MetaDataDictionaryMapType *m_Dictionary;
};
}
#endif // itkMetaDataDictionary_h
