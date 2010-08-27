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

#ifndef GDCMGROUPDICT_H
#define GDCMGROUPDICT_H

#include "gdcmTypes.h"

#include <assert.h>
#include <vector>
#include <string>
#include <iostream>
#include <iomanip>

namespace gdcm
{
/**
 * \brief Class to represent the mapping from group number to its abbreviation and name
 * \note Should I rewrite this class to use a std::map instead of std::vector for problem of
 * memory consumption ?
 */
class GDCM_EXPORT GroupDict
{
public:
  typedef std::vector<std::string> GroupStringVector;
  GroupDict() { FillDefaultGroupName(); }
  ~GroupDict() {}

  friend std::ostream& operator<<(std::ostream& _os, const GroupDict &_val);

  unsigned long Size() const
    {
    assert( Names.size() == Abbreviations.size() );
    return Names.size(); }

  std::string const &GetAbbreviation(uint16_t num) const;

  std::string const &GetName(uint16_t num) const;

protected:
  void Add(std::string const &abbreviation, std::string const &name);
  void Insert(uint16_t num, std::string const &abbreviation, std::string const &name);

private:
  // Generated implementation, see gdcmDefaultGroupNames
  void FillDefaultGroupName();

  GroupDict &operator=(const GroupDict &_val); // purposely not implemented
  GroupDict(const GroupDict &_val); // purposely not implemented

  GroupStringVector Abbreviations;
  GroupStringVector Names;
};
//-----------------------------------------------------------------------------
inline std::ostream& operator<<(std::ostream& _os, const GroupDict &_val)
{
  unsigned int size = _val.Size();
  for(unsigned int i=0; i<size; ++i)
    {
    _os << std::hex << std::setw(4) << std::setfill( '0' ) << i << ","
      << _val.GetAbbreviation(i) << "," << _val.GetName(i) << "\n";
    }
  return _os;
}

} // end namespace gdcm

#endif //GDCMGROUPDICT_H
