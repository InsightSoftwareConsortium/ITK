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
#include <iostream>

namespace gdcm 
{
//-----------------------------------------------------------------------------
/**
 * \ingroup ValEntry
 * \brief   The dicom header of a Dicom file contains a set of such entries
 *          (when successfuly parsed against a given Dicom dictionary)
 */
class GDCM_EXPORT ValEntry  : public DocEntry
{
public:
   ValEntry(DictEntry* e);
   ValEntry(DocEntry* d); 
   ~ValEntry();

   /// \brief Returns the 'Value' (e.g. "Dupond Marcel") converted into a
   /// 'string', event if it's stored as an integer in the header of the
   /// current Dicom Document Entry
   std::string const & GetValue() const { return Value; };
    
   /// Sets the value (string) of the current Dicom Document Entry
   void SetValue(std::string const & val) { Value = val; }; 

   virtual void Print(std::ostream &os = std::cout); 
   virtual void Write(std::ofstream *fp, FileType filetype);

protected:
   
private:
// Members :
  
   /// \brief Document Entry value, internaly represented as a std::string
   ///        The Value Representation (\ref VR) is independently used
   ///        in order to interpret (decode) this field.
   std::string Value;
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif

