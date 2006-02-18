/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmValEntry.h
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

#ifndef GDCMVALENTRY_H
#define GDCMVALENTRY_H

#include "gdcmDocEntry.h"
#include "gdcmContentEntry.h"

#include <iostream>

namespace gdcm 
{
//-----------------------------------------------------------------------------
/**
 * \brief   Any Dicom Document (File or DicomDir) contains 
 *           a set of DocEntry  - Dicom entries -
 *          ValEntry is an elementary DocEntry (i.e. a ContentEntry, 
 *           as opposed to SeqEntry)
 *          whose content is 'std::string representable' : characters,
 *          or integers (loaded in memory as a std::string)
 *          ValEntry is a specialisation of ContentEntry
 */
class GDCM_EXPORT ValEntry  : public ContentEntry
{
public:

   // Contructors and Destructor are public.
   ValEntry(DictEntry *e);
   ValEntry(DocEntry *d); 

   ~ValEntry();

   // Other accessors are inherited from gdcm::ContentEntry

   void Print(std::ostream &os = std::cout, std::string const &indent = ""); 

   void WriteContent(std::ofstream *fp, FileType filetype); 
   
   /// Sets the value (string) of the current Dicom entry.
   /// The size is updated
   void SetValue(std::string const &val);

   uint32_t ComputeFullLength();


   /// \brief returns the size threshold above which an element value 
   ///        will NOT be *printed* in order no to polute the screen output
   static long GetMaxSizePrintEntry() { return ValEntry::MaxSizePrintEntry; }

   static void SetMaxSizePrintEntry(long);

protected:
   
private:

   /// \brief Size threshold above which an element val
   ///        By default, this upper bound is fixed to 64 bytes.

   static uint32_t MaxSizePrintEntry;   

};

} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif

