/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmContentEntry.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
                                                                                
  Copyright (c) CREATIS (Centre de Recherche et d'Applications en Traitement de
  l'Image). All rights reserved. See Doc/License.txt or
  http://www.creatis.insa-lyon.fr/Public/Gdcm/License.html for details.
                                                                                
     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.
                                                                                
=========================================================================*/

#ifndef GDCMCONTENTENTRY_H
#define GDCMCONTENTENTRY_H

#include "gdcmDocEntry.h"

#include <iostream>

namespace gdcm 
{
//-----------------------------------------------------------------------------
/**
 * \brief   Any Dicom Document (File or DicomDir) contains 
 *           a set of DocEntry  - Dicom entries -
 *           (when successfuly parsed against a given Dicom dictionary)
 *          ContentEntry is an elementary DocEntry (as opposed to SeqEntry).
 *          Depending on the type of its content,
 *          ContentEntry is specialized as a ValEntry or a BinEntry
 */
class GDCM_EXPORT ContentEntry  : public DocEntry
{
public:
   virtual void WriteContent(std::ofstream *fp, FileType filetype) = 0;

   /// Sets the value (string) of the current Dicom entry
   virtual void SetValue(std::string const &val) { Value = val; };
   /// \brief Returns the 'Value' (e.g. "Dupond Marcel") converted 
   /// into a 'string', event if it's physically stored on disk as an integer
   std::string const &GetValue() const { return Value; };

   void Copy(DocEntry *doc);

protected:
   // Contructors are protected, not to be invoked by end user.
   ContentEntry(DictEntry *e);
   ContentEntry(DocEntry *d); 
   // Destructor is protected, not to be invoked by end user.
   ~ContentEntry();

private:
// Members :
   /// \brief Dicom entry value, internaly represented as a std::string.
   ///        The Value Representation (\ref VR) is independently used
   ///        in order to interpret (decode) this field.
   std::string Value;
};

} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif

