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
#ifndef GDCMNESTEDMODULEENTRIES_H
#define GDCMNESTEDMODULEENTRIES_H

#include "gdcmModuleEntry.h"
#include <vector>

namespace gdcm
{
/**
 * \brief Class for representing a NestedModuleEntries
 * \note bla
 * \sa ModuleEntry
 */
class GDCM_EXPORT NestedModuleEntries : public ModuleEntry
{
public:
  NestedModuleEntries(const char *name = "", const char *type = "3", const char *description = ""):ModuleEntry(name,type,description) { }
  friend std::ostream& operator<<(std::ostream& _os, const NestedModuleEntries &_val);

  unsigned int GetNumberOfModuleEntries() { return ModuleEntriesList.size(); }

  const ModuleEntry &GetModuleEntry(unsigned int idx) const { return ModuleEntriesList[idx]; }
  ModuleEntry &GetModuleEntry(unsigned int idx) { return ModuleEntriesList[idx]; }

  void AddModuleEntry(const ModuleEntry &me) { ModuleEntriesList.push_back( me ); }

private:
  std::vector<ModuleEntry> ModuleEntriesList;
};
//-----------------------------------------------------------------------------
inline std::ostream& operator<<(std::ostream& _os, const NestedModuleEntries &_val)
{
  _os << "Nested:" << _val.Name << "\t" << _val.DataElementType << "\t" << _val.DescriptionField;
  return _os;
}

typedef NestedModuleEntries NestedMacroEntries;


} // end namespace gdcm

#endif //GDCMNESTEDMODULEENTRIES_H
