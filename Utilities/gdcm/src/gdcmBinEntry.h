/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmBinEntry.h
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

#ifndef GDCMBINENTRY_H
#define GDCMBINENTRY_H

#include "gdcmContentEntry.h"
#include <iostream>

namespace gdcm 
{

//-----------------------------------------------------------------------------
/**
 * \brief   Any Dicom Document (File or DicomDir) contains 
 *           a set of DocEntry - Dicom entries - 
 *          BinEntry is an elementary DocEntry (i.e. a ContentEntry, 
 *           as opposite to SeqEntry) whose content is non std::string
 *          representable
 *          BinEntry is a specialisation of ContentEntry
 */
 
class GDCM_EXPORT BinEntry  : public ContentEntry
{
public:
   BinEntry( DictEntry *e );
   BinEntry( DocEntry *d ); 

   ~BinEntry();
   
   void Print( std::ostream &os = std::cout, std::string const & indent = "" );

   void WriteContent( std::ofstream *fp, FileType ft);

   /// \brief Returns the area value of the current Dicom Entry
   ///  when it's not string-translatable (e.g : LUT table, overlay, icon)         
   uint8_t *GetBinArea()  { return BinArea; }
   void  SetBinArea( uint8_t *area, bool self = true );

   /// \brief Sets SelfArea
   void SetSelfArea(bool area) { SelfArea = area; };
   /// \brief Returns SelfArea
   bool IsSelfArea() { return SelfArea; };

private:
   /// \brief memory area to hold 'non std::string' representable values 
   ///       (ie : Lookup Tables, overlays, icons)   
   uint8_t *BinArea;
   /// \brief Whether BinEntry has its own BinArea or not
   bool SelfArea;
};

} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif

