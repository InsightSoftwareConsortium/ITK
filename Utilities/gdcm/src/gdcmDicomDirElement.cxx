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

namespace gdcm 
{
//-----------------------------------------------------------------------------
/// \brief auto generate function, to fill up the default elements for 
///        a DICOMDIR, if relevant file is not found on user's disk
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
   if ( !from )
   {
      gdcmWarningMacro( "Can't open DicomDirElement dictionary" 
                        << filename.c_str());
      FillDefaultDIRDict( this );
   }
   else
   {
      char buff[1024];
      std::string strType;
      Element elem;
      DicomDirType type;

      while (!from.eof())
      {
         from >> std::ws;
         from.getline(buff, 1024, ' ');
         strType = buff;

         if ( strType == "imageElem" )
            type = DD_IMAGE;
         else if ( strType == "serieElem" )
            type = DD_SERIE;
         else if ( strType == "studyElem" )
            type = DD_STUDY;
         else if ( strType == "patientElem" )
            type = DD_PATIENT;
         else if ( strType == "metaElem" )
            type = DD_META;
         else
         {
            gdcmWarningMacro("Unknown type (" << strType 
                             << ") found in the file : "
                             << filename.c_str());
            type = DD_UNKNOWN;
         }

         if ( type!=DD_UNKNOWN )
         {
            from >> std::hex >> elem.Group >> elem.Elem;

            from >> std::ws;
            from.getline(buff, 1024, '"');
            from >> std::ws;
            from.getline(buff, 1024, '"');
            elem.Value = buff;

            AddEntry(type, elem);
         }
         from.getline(buff, 1024, '\n');
      }
      from.close();
   }
}

/**
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
// Public
/**
 * \brief Add an entry to one of the DicomDir Elements 
 *        (Patient, Study, Serie, Image)
 * @param type Element type (DD_PATIENT, DD_STUDY, DD_SERIE, DD_IMAGE) 
 * @param elem elem
 */
bool DicomDirElement::AddEntry(DicomDirType type, Element const &elem)
{
   switch( type )
   {
      case DD_IMAGE :
         DicomDirImageList.push_back(elem);
         break;
      case DD_SERIE :
         DicomDirSerieList.push_back(elem);
         break;
      case DD_STUDY :
         DicomDirStudyList.push_back(elem);
         break;
      case DD_PATIENT :
         DicomDirPatientList.push_back(elem);
         break;
      case DD_META :
         DicomDirMetaList.push_back(elem);
         break;
      default :
         return false;
   }
   return true;
}

/**
 * \brief Add an entry to one of the DicomDir Elements 
 *        (Patient, Study, Serie, Image)
 * @param type Element type (DD_PATIENT, DD_STUDY, DD_SERIE, DD_IMAGE) 
 * @param group  Group number of the entry to be added
 * @param elem Element number of the entry to be added
 */
void DicomDirElement::AddDicomDirElement(DicomDirType type,
                                         uint16_t group, uint16_t elem)
{
   Element el;
   el.Group = group;
   el.Elem  = elem;
   el.Value = "";
   AddEntry(type, el);
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print
/**
 * \brief   Print all
 * @param   os The output stream to be written to.
 */
void DicomDirElement::Print(std::ostream &os,std::string const &)
{
   std::ostringstream s;
   std::list<Element>::iterator it;
   //char greltag[10];  //group element tag
   TagKey greltag;

   s << "Meta Elements :"<<std::endl;
   for (it = DicomDirMetaList.begin(); it != DicomDirMetaList.end(); ++it)
   {
      greltag = DictEntry::TranslateToKey(it->Group,it->Elem);
      s << "   (" << greltag << ") = " << it->Value << std::endl;
   }

   s << "Patient Elements :"<<std::endl;
   for (it = DicomDirPatientList.begin(); it != DicomDirPatientList.end(); ++it)
   {
      greltag = DictEntry::TranslateToKey(it->Group,it->Elem);
      s << "   (" << greltag << ") = " << it->Value << std::endl;
   }

   s << "Study Elements :"<<std::endl;
   for (it = DicomDirStudyList.begin(); it != DicomDirStudyList.end(); ++it)
   {
      greltag = DictEntry::TranslateToKey(it->Group, it->Elem);
      s << "   (" << greltag << ") = " << it->Value << std::endl;
   }

   s << "Serie Elements :"<<std::endl;
   for (it = DicomDirSerieList.begin(); it != DicomDirSerieList.end(); ++it)
   {
      greltag = DictEntry::TranslateToKey( it->Group, it->Elem);
      s << "   (" << greltag << ") = " << it->Value << std::endl;
   }

   s << "Image Elements :"<<std::endl;
   for (it = DicomDirImageList.begin(); it != DicomDirImageList.end(); ++it)
   {
      greltag = DictEntry::TranslateToKey(it->Group, it->Elem);
      s << "   (" << greltag << ") = " << it->Value << std::endl;
   }

   os << s.str();
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
