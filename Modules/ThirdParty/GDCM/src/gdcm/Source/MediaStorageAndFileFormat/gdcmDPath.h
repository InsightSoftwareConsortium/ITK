/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMDPATH_H
#define GDCMDPATH_H

#include "gdcmTag.h"
#include <string>

namespace gdcm {

/**
 * \brief class to handle a DICOM path
 * While supp 118 did introduced a notion of XPath for XML Native model this
 * convention is too XML-centric. Instead prefer DCMTK style notation
 * https://groups.google.com/g/comp.protocols.dicom/c/IyIH0IOBMPA
 */
class GDCM_EXPORT DPath {
  friend std::ostream &operator<<(std::ostream &_os, const DPath &_val);

 public:
  DPath();
  ~DPath();
  void Print(std::ostream &) const;
  bool operator<(const DPath &rhs) const;

  bool ConstructFromString(const char *path);

  /// Return whether or not 'other' match the template DPath
  bool Match(DPath const &other) const;

  /// Return if path is valid or not
  static bool IsValid(const char *path);

 private:
  std::string Path;
};

inline std::ostream &operator<<(std::ostream &os, const DPath &val) {
  os << val.Path;
  return os;
}

}  // end namespace gdcm

#endif  // GDCMDPATH_H
