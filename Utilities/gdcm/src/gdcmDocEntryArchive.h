/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDocEntryArchive.h
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

#ifndef GDCMDOCENTRYARCHIVE_H
#define GDCMDOCENTRYARCHIVE_H

#include "gdcmFile.h"

namespace gdcm 
{

//-----------------------------------------------------------------------------
/**
 * \brief Container 
 * It's goal is to change the File header correctly. At this time, the change is 
 * only made for the first level of the Document. In the future, it might 
 * consider Dicom Sequences (SeqEntry, within any SQItem).
 * The change is made by replacing a DocEntry by an other that is created
 * outside the class. The old value is kept. When we restore the File
 * status, the added DocEntry is deleted and replaced by the old value.
 */
class GDCM_EXPORT DocEntryArchive 
{

friend class FileHelper;

private:
   DocEntryArchive(File *file);
   ~DocEntryArchive();

   void Print(std::ostream &os = std::cout);

   bool Push(DocEntry *newEntry);
   bool Push(uint16_t group,uint16_t elem);
   bool Restore(uint16_t group,uint16_t elem);

   void ClearArchive(void);

   /// pointer to the gdcm::File pointer we want to save values from
   File *ArchFile;
   /// H table to save values.
   TagDocEntryHT Archive;
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
