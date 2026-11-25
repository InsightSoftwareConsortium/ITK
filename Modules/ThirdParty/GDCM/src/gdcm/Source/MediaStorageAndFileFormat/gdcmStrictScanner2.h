/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMSTRICTSCANNER2_H
#define GDCMSTRICTSCANNER2_H

#include "gdcmDirectory.h"
#include "gdcmPrivateTag.h"
#include "gdcmSmartPointer.h"
#include "gdcmSubject.h"
#include "gdcmTag.h"

#include <map>
#include <set>
#include <string>

#include <string.h>  // strcmp

namespace gdcm {
class StringFilter;

/**
 * \brief StrictScanner2
 * \details This filter is meant for quickly browsing a FileSet (a set of files
 * on disk). Special consideration are taken so as to read the minimum amount of
 * information in each file in order to retrieve the user specified set of
 * DICOM Attribute.
 *
 * This filter is dealing with both VRASCII and VRBINARY element, thanks to the
 * help of StringFilter
 *
 * \warning IMPORTANT In case of file where tags are not ordered (illegal as
 * per DICOM specification), the output will be missing information
 *
 * \note implementation details. All values are stored in a std::set of
 * std::string. Then the address of the cstring underlying the std::string is
 * used in the std::map.
 *
 * This class implement the Subject/Observer pattern trigger the following
 * events: \li ProgressEvent \li StartEvent \li EndEvent
 */
class GDCM_EXPORT StrictScanner2 : public Subject {
  friend std::ostream &operator<<(std::ostream &_os, const StrictScanner2 &s);

 public:
  StrictScanner2() : Values(), Filenames(), PublicMappings(), PrivateMappings(), Progress(0.0) {}
  ~StrictScanner2() override;

  /// struct to map a filename to a value
  /// Implementation note:
  /// all std::map in this class will be using const char * and not std::string
  /// since we are pointing to existing std::string (held in a std::vector)
  /// this avoid an extra copy of the byte array.
  /// Tag are used as Tag class since sizeof(tag) <= sizeof(pointer)
  typedef std::map<Tag, const char *> PublicTagToValue;
  typedef PublicTagToValue::value_type PublicTagToValueValueType;

  typedef std::map<PrivateTag, const char *> PrivateTagToValue;
  typedef PrivateTagToValue::value_type PrivateTagToValueValueType;

  /// Add a tag that will need to be read. Those are root level tags
  bool AddPublicTag(Tag const &t);
  void ClearPublicTags();

  // Work in progress do not use:
  bool AddPrivateTag(PrivateTag const &pt);
  void ClearPrivateTags();

  /// Add a tag that will need to be skipped. Those are root level skip tags
  bool AddSkipTag(Tag const &t);
  void ClearSkipTags();

  /// Start the scan !
  bool Scan(Directory::FilenamesType const &filenames);

  /// Return the list of filenames
  Directory::FilenamesType const &GetFilenames() const { return Filenames; }

  /// Print result
  void Print(std::ostream &os) const override;

  /// Print result as CSV table
  void PrintTable(std::ostream &os, bool header = false) const;

  /// Check if filename is a key in the Mapping table.
  /// returns true only of file can be found, which means
  /// the file was indeed a DICOM file that could be processed
  bool IsKey(const char *filename) const;

  /// Return the list of filename that are key in the internal map,
  /// which means those filename were properly parsed
  Directory::FilenamesType GetKeys() const;

  // struct to store all the values found:
  typedef std::set<std::string> ValuesType;

  /// Get all the values found (in lexicographic order)
  ValuesType const &GetValues() const { return Values; }

  /// Get all the values found (in lexicographic order) associated with Tag 't'
  ValuesType GetPublicValues(Tag const &t) const;

  /// Get all the values found (in lexicographic order) associated with
  /// PrivateTag 'pt'
  ValuesType GetPrivateValues(PrivateTag const &pt) const;

  /// Get all the values found (in a vector) associated with Tag 't'
  /// This function is identical to GetValues, but is accessible from the
  /// wrapped layer (python, C#, java)
  Directory::FilenamesType GetPublicOrderedValues(Tag const &t) const;

  Directory::FilenamesType GetPrivateOrderedValues(PrivateTag const &pt) const;

  /* ltstr is CRITICAL, otherwise pointers value are used to do the key
   * comparison */
  struct ltstr {
    bool operator()(const char *s1, const char *s2) const {
      gdcm_assert(s1 && s2);
      return strcmp(s1, s2) < 0;
    }
  };
  typedef std::map<const char *, PublicTagToValue, ltstr> PublicMappingType;
  typedef PublicMappingType::const_iterator PublicConstIterator;
  PublicConstIterator Begin() const { return PublicMappings.begin(); }
  PublicConstIterator End() const { return PublicMappings.end(); }

  typedef std::map<const char *, PrivateTagToValue, ltstr> PrivateMappingType;
  typedef PrivateMappingType::const_iterator PrivateConstIterator;
  PrivateConstIterator PrivateBegin() const { return PrivateMappings.begin(); }
  PrivateConstIterator PrivateEnd() const { return PrivateMappings.end(); }

  /// Mappings are the mapping from a particular tag to the map, mapping
  /// filename to value:
  PublicMappingType const &GetPublicMappings() const { return PublicMappings; }
  PrivateMappingType const &GetPrivateMappings() const {
    return PrivateMappings;
  }

  /// Get the std::map mapping filenames to value for file 'filename'
  PublicTagToValue const &GetPublicMapping(const char *filename) const;
  PrivateTagToValue const &GetPrivateMapping(const char *filename) const;

  /// Will loop over all files and return the first file where value match the
  /// reference value 'valueref'
  const char *GetFilenameFromPublicTagToValue(Tag const &t,
                                              const char *valueref) const;
  const char *GetFilenameFromPrivateTagToValue(PrivateTag const &pt,
                                               const char *valueref) const;

  /// Will loop over all files and return a vector of std::strings of filenames
  /// where value match the reference value 'valueref'
  Directory::FilenamesType GetAllFilenamesFromPublicTagToValue(
      Tag const &t, const char *valueref) const;
  Directory::FilenamesType GetAllFilenamesFromPrivateTagToValue(
      PrivateTag const &pt, const char *valueref) const;

  /// See GetFilenameFromTagToValue(). This is simply GetFilenameFromTagToValue
  /// followed
  // by a call to GetMapping()
  PublicTagToValue const &GetMappingFromPublicTagToValue(
      Tag const &t, const char *value) const;
  PrivateTagToValue const &GetMappingFromPrivateTagToValue(
      PrivateTag const &pt, const char *value) const;

  /// Retrieve the value found for tag: t associated with file: filename
  /// This is meant for a single short call. If multiple calls (multiple tags)
  /// should be done, prefer the GetMapping function, and then reuse the
  /// TagToValue hash table. \warning Tag 't' should have been added via
  /// AddTag() prior to the Scan() call !
  const char *GetPublicValue(const char *filename, Tag const &t) const;
  const char *GetPrivateValue(const char *filename, PrivateTag const &t) const;

  /// for wrapped language: instantiate a reference counted object
  static SmartPointer<StrictScanner2> New() { return new StrictScanner2; }

 protected:
  void ProcessPublicTag(StringFilter &sf, const char *filename);
  void ProcessPrivateTag(StringFilter &sf, const char *filename);

 private:
  // struct to store all uniq tags in ascending order:
  typedef std::set<Tag> PublicTagsType;
  typedef std::set<PrivateTag> PrivateTagsType;
  std::set<Tag> PublicTags;          // Public and Private Creator
  std::set<PrivateTag> PrivateTags;  // Only Private (no Private Creator)
  std::set<Tag> SkipTags;
  ValuesType Values;
  Directory::FilenamesType Filenames;

  // Main struct that will hold all public mapping:
  PublicMappingType PublicMappings;
  // Main struct that will hold all private mapping:
  PrivateMappingType PrivateMappings;

  double Progress;
};
//-----------------------------------------------------------------------------
inline std::ostream &operator<<(std::ostream &os, const StrictScanner2 &s) {
  s.Print(os);
  return os;
}

}  // end namespace gdcm

#endif  // GDCMSTRICTSCANNER2_H
