/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMCLEANER_H
#define GDCMCLEANER_H

#include "gdcmDPath.h"
#include "gdcmFile.h"
#include "gdcmSmartPointer.h"
#include "gdcmSubject.h"

namespace gdcm {
/**
 * \brief Cleaner
 *
 * This class implement the Subject/Observer pattern trigger the following
 * event: \li AnonymizeEvent \li IterationEvent \li StartEvent \li EndEvent
 *
 */
class GDCM_EXPORT Cleaner : public Subject {
 public:
  Cleaner();
  ~Cleaner() override;

  ///
  bool Empty(Tag const &t);
  bool Empty(PrivateTag const &pt);
  bool Empty(DPath const &dpath);
  bool Empty(VR const &vr);

  bool Remove(Tag const &t);
  bool Remove(PrivateTag const &pt);
  bool Remove(DPath const &dpath);
  bool Remove(VR const &vr);

  /// Clean digital tash (typically SIEMENS CSA header):
  bool Scrub(Tag const &t);
  bool Scrub(PrivateTag const &pt);
  bool Scrub(DPath const &dpath);
  bool Scrub(VR const &vr);

  // 8 Encoding of Coded Entry Data
  // https://dicom.nema.org/medical/dicom/current/output/chtml/part03/chapter_8.html
  typedef std::tuple<std::string, std::string, std::string> CodedEntryData;

  ///  Coded Entry Data
  bool ReplaceCodeMeaning(CodedEntryData const &ced);

  ///  Preserve
  bool Preserve(DPath const &dpath);

  /// Should I remove all private tag for which no private creator is found.
  /// Default: true
  void RemoveAllMissingPrivateCreator(bool remove);

  /// Specify a private tag (odd number) without a private creator (root level
  /// only for now):
  bool RemoveMissingPrivateCreator(Tag const &t);

  /// Should I remove all group length (deprecated). Default: true
  void RemoveAllGroupLength(bool remove);

  /// Should I remove all illegal attribute. Default: true
  void RemoveAllIllegal(bool remove);

  /// Should I empty instead of scrub upon failure
  void EmptyWhenScrubFails(bool empty);

  /// main loop
  bool Clean();

  /// Set/Get File
  void SetFile(const File &f) { F = f; }
  // const File &GetFile() const { return *F; }
  File &GetFile() { return *F; }

  /// for wrapped language: instantiate a reference counted object
  static SmartPointer<Cleaner> New() { return new Cleaner; }

 private:
  // I would prefer to have a smart pointer to DataSet but DataSet does not
  // derive from Object...
  SmartPointer<File> F;
  struct impl;
  // PIMPL idiom
  impl *pimpl;
};

}  // end namespace gdcm

#endif  // GDCMCLEANER_H
