/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaDataDictionary.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMetaDataDictionary_h
#define __itkMetaDataDictionary_h

#include "itkMetaDataObjectBase.h"
#include <vector>
#include <map>
#include <string>

namespace itk
{

/**
 * \author Hans J. Johnson
 * The MetaDataDictionary, along with the MetaDataObject derived template
 * classes, is designed to provide a mechanism for storing a collection of
 * arbitrary data types. The main motivation for such a collection is to
 * associate arbitrary data elements with itk DataObjects.
 */
class ITKCommon_EXPORT MetaDataDictionary
{
public:
  typedef MetaDataDictionary Self;
  /**
   * Defines the default behavior for printing out this element
   * \param os An output stream
   */
  virtual void Print(std::ostream& os) const;

  // Declare the datastructure that will be used to hold the
  // dictionary. 
  class MetaDataDictionaryMapType 
    : public std::map<std::string, MetaDataObjectBase::Pointer>
      {
      };

  typedef MetaDataDictionaryMapType::iterator       Iterator;
  typedef MetaDataDictionaryMapType::const_iterator ConstIterator;

  // Constructor
  MetaDataDictionary();
  // Copy Constructor
  MetaDataDictionary(const MetaDataDictionary&);
  // operator =
  void operator=(const MetaDataDictionary&);

  // Destructor
  virtual ~MetaDataDictionary();

  /** Returns a vector of keys to the key/value entries in the
    dictionary.  Iterate through the dictionary using these keys.
    */
  std::vector<std::string> GetKeys() const;

  // Implement map's api. On some Micorsoft compilers, stl containers
  // cannot be exported. This causes problems when building DLL's.
  // Here we inherit privately from std::map and provide a simple
  // API. The implementation will be in the DLL.
  MetaDataObjectBase::Pointer &operator [](const std::string &);
  const MetaDataObjectBase   * operator [](const std::string &) const;
  bool HasKey (const std::string &);


  /** \warning the following functions SHOULD NOT be used with 
   * the visual studio 6 compiler since iterator outside of the dll
   * context cannot be dereferenced safely */

  /** Returns an iterator to the beginning of the map */
  Iterator  Begin();
  ConstIterator  Begin() const;

  /** Returns an iterator to the end of the map */
  Iterator  End();
  ConstIterator  End() const;

  /** Returns an iterator matching the string key */
  Iterator  Find(const std::string & key);
  ConstIterator  Find(const std::string & key) const;

private:
  MetaDataDictionaryMapType *m_Dictionary;
};

}
#endif // __itkMetaDataDictionary_h

