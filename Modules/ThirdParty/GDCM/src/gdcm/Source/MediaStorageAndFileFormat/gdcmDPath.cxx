/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmDPath.h"
#include "gdcmPrivateTag.h"
#include "gdcmTag.h"

namespace gdcm {

DPath::DPath() = default;

DPath::~DPath() = default;

// LO mandates that '\\' is never used:
static const char SEPARATOR = '\\';
void DPath::Print(std::ostream &os) const { os << Path; }

bool DPath::IsValid(const char *path) {
  DPath tp;
  return tp.ConstructFromString(path);
}

template <typename T>
static void print_contents(std::ostream &oss, const std::vector<T> &v,
                           const char *const separator = ",") {
  if (!v.empty()) {
    std::copy(v.begin(), --v.end(), std::ostream_iterator<T>(oss, separator));
    oss << v.back();
  }
}

static inline std::string tostring(uint16_t const val, int const width = 4) {
  std::ostringstream oss;
  oss.setf(std::ios::right);
  oss << std::hex << std::setw(width) << std::setfill('0') << val;
  return oss.str();
}

static std::vector<std::string> tag2strings(gdcm::Tag const &tag) {
  std::vector<std::string> ret;
  gdcm_assert(tag.IsPublic() || tag.IsPrivateCreator() || tag.IsGroupLength());
  ret.push_back(tostring(tag.GetGroup()));
  ret.push_back(tostring(tag.GetElement()));
  return ret;
}

static std::vector<std::string> tag2strings(gdcm::PrivateTag const &pt) {
  std::vector<std::string> ret;
  ret.push_back(tostring(pt.GetGroup()));
  ret.push_back(tostring(pt.GetElement(), 2));
  ret.push_back(pt.GetOwner());
  return ret;
}

static inline bool is_digits(const std::string &str) {
  return str.find_first_not_of("0123456789") == std::string::npos;
}

static std::vector<std::string> split_from_slash_separated(
    std::string const &path, const char separator) {
  std::vector<std::string> comps;
  std::istringstream is(path);
  std::string sub;
  while (std::getline(is, sub, separator)) {
    const bool isEmpty = sub.empty();
    const bool isDigits = is_digits(sub);
    const bool isWildCard = sub == "*";
    const bool hasComma = sub.find(',') != std::string::npos;
    if (isEmpty && comps.empty()) {
      comps.push_back(sub);
    } else if (isDigits) {
      comps.push_back(sub);
    } else if (isWildCard) {
      comps.push_back(sub);
    } else if (hasComma) {
      comps.push_back(sub);
    } else {
#if 0
      gdcm_assert(!comps.empty());
      std::string &last = comps.back();
      last.push_back(separator);
      last.append(sub);
#else
      gdcmErrorMacro("Failed to parse: " << path << " using : " << separator);
      comps.clear();
      return comps;
#endif
    }
  }
  return comps;
}

bool DPath::ConstructFromString(const char *spath) {
  Path.clear();
  if (!spath) return false;
  std::vector<std::string> comps;
  if (*spath == '/') {
    comps = split_from_slash_separated(spath, '/');
  } else if (*spath == '\\') {
    comps = split_from_slash_separated(spath, '\\');
  } else {
#if 0
    // 'name' and '//name' is equivalent:
    comps.push_back("");  //
    comps.push_back(spath);
#endif
    return false;
  }
  if (comps.empty()) return false;
  gdcm::PrivateTag pt;
  gdcm::Tag t;
  std::ostringstream os;
  std::vector<std::string>::const_iterator it = comps.begin();
  unsigned int index = 0;
  gdcm_assert(comps.size() >= 2);
  // check root
  if (!it->empty()) return false;
  ++it;
  for (; it != comps.end(); ++it) {
    os << SEPARATOR;
    const char *str = it->c_str();
    if (pt.ReadFromCommaSeparatedString(str)) {
      const std::vector<std::string> tag_strings = tag2strings(pt);
      print_contents(os, tag_strings, ",");
    } else if (t.ReadFromCommaSeparatedString(str)) {
      const std::vector<std::string> tag_strings = tag2strings(t);
      print_contents(os, tag_strings, ",");
    } else if (is_digits(str) && sscanf(str, "%u", &index) == 1 && index > 0) {
      os << index;
    } else if (*str == '*') {
      os << '*';
    } else {
      gdcmErrorMacro("Not implemented: " << str);
      return false;
    }
  }
  Path = os.str();
  return true;
}

bool DPath::Match(DPath const &other) const {
  if (this->Path == other.Path) return true;
  return false;
}

bool DPath::operator<(const DPath &rhs) const { return this->Path < rhs.Path; }

}  // end namespace gdcm
