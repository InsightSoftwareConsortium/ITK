/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDicomDirElement.cxx
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

#include "gdcmDicomDirElement.h"
#include "gdcmUtil.h"
#include "gdcmDebug.h"
#include "gdcmDictSet.h"

#include <fstream>
#include <iostream>
#include <itksys/ios/sstream>

namespace gdcm 
{
void FillDefaultDIRDict(DicomDirElement *dde);
//-----------------------------------------------------------------------------
// Constructor / Destructor

/**
 * \brief   constructor : populates the chained lists 
 *          from the file 'Dicts/DicomDir.dic'
 */
DicomDirElement::DicomDirElement()
{
   std::string filename = DictSet::BuildDictPath() + DICT_ELEM;
   std::ifstream from(filename.c_str());
   if(!from)
   {
      dbg.Verbose(2, 
         "DicomDirElement::DicomDirElement: can't open dictionary", 
            filename.c_str());
      FillDefaultDIRDict( this );
   }
   else
   {
      char buff[1024];
      std::string type;
      Element elem;

      while (!from.eof())
      {
         from >> std::ws;
         from.getline(buff, 1024, ' ');
         type = buff;

         if( type == "metaElem"  || type == "patientElem" || 
             type == "studyElem" || type == "serieElem"   || 
             type == "imageElem" )
         {
            from >> std::hex >> elem.Group >> elem.Elem;

            from >> std::ws;
            from.getline(buff, 1024, '"');
            from >> std::ws;
            from.getline(buff, 1024, '"');
            elem.Value = buff;

            AddNewEntry(type, elem);
         }
         from.getline(buff, 1024, '\n');
      }
      from.close();
   }
}

/**
 * \ingroup DicomDirElement
 * \brief   canonical destructor 
 */
DicomDirElement::~DicomDirElement()
{
   DicomDirMetaList.clear();
   DicomDirPatientList.clear();
   DicomDirStudyList.clear();
   DicomDirSerieList.clear();
   DicomDirImageList.clear();
}

//-----------------------------------------------------------------------------
// Print
/**
 * \ingroup DicomDirElement
 * \brief   Print all
 * \todo add a 'Print Level' check 
 * @param   os The output stream to be written to.
 */
void DicomDirElement::Print(std::ostream &os)
{
   itksys_ios::ostringstream s;
   std::list<Element>::iterator it;
   //char greltag[10];  //group element tag
   std::string greltag;

   s << "Meta Elements :"<<std::endl;
   for (it = DicomDirMetaList.begin(); it != DicomDirMetaList.end(); ++it)
   {
      greltag = Util::Format("%04x|%04x ",it->Group,it->Elem);
      s << "   (" << greltag << ") = " << it->Value << std::endl;
   }

   s << "Patient Elements :"<<std::endl;
   for (it = DicomDirPatientList.begin(); it != DicomDirPatientList.end(); ++it)
   {
      greltag = Util::Format("%04x|%04x ",it->Group,it->Elem);
      s << "   (" << greltag << ") = " << it->Value << std::endl;
   }

   s << "Study Elements :"<<std::endl;
   for (it = DicomDirStudyList.begin(); it != DicomDirStudyList.end(); ++it)
   {
      greltag = Util::Format("%04x|%04x ", it->Group, it->Elem);
      s << "   (" << greltag << ") = " << it->Value << std::endl;
   }

   s << "Serie Elements :"<<std::endl;
   for (it = DicomDirSerieList.begin(); it != DicomDirSerieList.end(); ++it)
   {
      greltag = Util::Format("%04x|%04x ", it->Group, it->Elem);
      s << "   (" << greltag << ") = " << it->Value << std::endl;
   }

   s << "Image Elements :"<<std::endl;
   for (it = DicomDirImageList.begin(); it != DicomDirImageList.end(); ++it)
   {
      greltag = Util::Format("%04x|%04x ", it->Group, it->Elem);
      s << "   (" << greltag << ") = " << it->Value << std::endl;
   }

   os << s.str();
}

//-----------------------------------------------------------------------------
// Public

bool DicomDirElement::AddNewEntry(std::string const & type, 
                                  Element const & elem)
{
   if( type == "metaElem" )
   {
      DicomDirMetaList.push_back(elem);
   }
   else if( type == "patientElem" )
   {
      DicomDirPatientList.push_back(elem);
   }
   else if( type == "studyElem" )
   {
      DicomDirStudyList.push_back(elem);
   }
   else if( type == "serieElem" )
   {
      DicomDirSerieList.push_back(elem);
   }
   else if( type == "imageElem" )
   {
      DicomDirImageList.push_back(elem);
   }
   else
   {
     return false;
   }
   return true;
}
//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------

} // end namespace gdcm
