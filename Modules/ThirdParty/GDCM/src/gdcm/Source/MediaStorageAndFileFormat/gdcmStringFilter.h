/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library
  Module:  $URL$

  Copyright (c) 2006-2010 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMSTRINGFILTER_H
#define GDCMSTRINGFILTER_H

#include "gdcmDataElement.h"
#include "gdcmDicts.h"
#include "gdcmFile.h"

namespace gdcm
{

/**
 * \brief StringFilter
 * StringFilter is the class that make gdcm2.x looks more like gdcm1 and transform the binary blob
 * contained in a DataElement into a string, typically this is a nice feature to have for wrapped language
 */
class GDCM_EXPORT StringFilter
{
public:
  StringFilter();
  ~StringFilter();

  ///
  void UseDictAlways(bool ) {}

  /// Allow user to pass in there own dicts
  void SetDicts(const Dicts &dicts);

  /// Convert to string the ByteValue contained in a DataElement
  std::string ToString(const Tag& t) const;

  //std::string ToMime64(const Tag& t) const;

  /// Convert to string the ByteValue contained in a DataElement
  /// the returned elements are:
  /// pair.first : the name as found in the dictionary of DataElement
  /// pari.second : the value encoded into a string (US,UL...) are properly converted
  std::pair<std::string, std::string> ToStringPair(const Tag& t) const;

  /// DEPRECATED: NEVER USE IT
  std::string FromString(const Tag&t, const char * value, VL const & vl);

  // Use this one
  std::string FromString(const Tag&t, const char * value, size_t len);

  //typedef std::map<Tag, gdcm::ConstCharWrapper> StringSet;

  /// Set/Get File
  void SetFile(const File& f) { F = f; }
  File &GetFile() { return *F; }
  const File &GetFile() const { return *F; }

protected:
  std::pair<std::string, std::string> ToStringPair(const Tag& t, DataSet const &ds) const;

private:
  SmartPointer<File> F;
};

} // end namespace gdcm

#endif //GDCMSTRINGFILTER_H
