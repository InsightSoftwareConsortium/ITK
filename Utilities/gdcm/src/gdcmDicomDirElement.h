/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDicomDirElement.h
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

#ifndef GDCMDICOMDIRELEMENT_H
#define GDCMDICOMDIRELEMENT_H

#include "gdcmCommon.h"

#include <list>

namespace gdcm 
{

//-----------------------------------------------------------------------------
typedef struct
{
   /// DicomGroup number
   unsigned short int Group;
   /// DicomElement number
   unsigned short int Elem;
   /// value (coded as a std::string) of the Element
   std::string Value;
} Element;

typedef std::list<Element> ListDicomDirElem;
typedef std::list<Element> ListDicomDirMetaElem;
typedef std::list<Element> ListDicomDirPatientElem;
typedef std::list<Element> ListDicomDirStudyElem;
typedef std::list<Element> ListDicomDirSerieElem;
typedef std::list<Element> ListDicomDirImageElem;

//-----------------------------------------------------------------------------
/**
 * \ingroup DicomDirElement
 * \brief    DicomDirElement represents elements contained in a dicom dir
 *           Class for the chained lists from the file 'Dicts/DicomDir.dic'
 */
class GDCM_EXPORT DicomDirElement
{
public:
   DicomDirElement();
   ~DicomDirElement();

  /**
    * \brief   canonical Printer 
    * \sa    SetPrintLevel
    */ 
   void Print(std::ostream &os);

   /**
    * \ingroup DicomDirElement
    * \brief   returns a reference to the chained List 
    *          related to the META Elements of a DICOMDIR.
    */
   ListDicomDirMetaElem const & GetDicomDirMetaElements() const
      { return DicomDirMetaList; };

   /**
    * \ingroup DicomDirElement
    * \brief   returns a reference to the chained List 
    *          related to the PATIENT Elements of a DICOMDIR.
    */      
   ListDicomDirPatientElem const & GetDicomDirPatientElements() const
      { return DicomDirPatientList; };

   /**
    * \ingroup DicomDirElement
    * \brief   returns a reference to the chained List 
    *          related to the STUDY Elements of a DICOMDIR.
    */      
   ListDicomDirStudyElem const & GetDicomDirStudyElements() const
      { return DicomDirStudyList; };

   /**
    * \ingroup DicomDirElement
    * \brief   returns a reference to the chained List 
    *          related to the SERIE Elements of a DICOMDIR.
    */
   ListDicomDirSerieElem const & GetDicomDirSerieElements() const
      { return DicomDirSerieList; };

   /**
    * \ingroup DicomDirElement
    * \brief   returns a reference to the chained List 
    *          related to the IMAGE Elements of a DICOMDIR.
    */
   ListDicomDirImageElem const & GetDicomDirImageElements() const
      { return DicomDirImageList; };

   /**
    * Public method to add an element
    */
   bool AddNewEntry(std::string const & type, Element const & elem);

private:
   /// Elements chained list, related to the MetaElements of DICOMDIR
   ListDicomDirMetaElem    DicomDirMetaList;
   /// Elements chained list, related to the PatientElements of DICOMDIR
   ListDicomDirPatientElem DicomDirPatientList;
   /// Elements chained list, related to the StudyElements of DICOMDIR
   ListDicomDirStudyElem   DicomDirStudyList;
   /// Elements chained list, related to the SerieElements of DICOMDIR
   ListDicomDirSerieElem   DicomDirSerieList;
   /// Elements chained list, related to the ImageElements of DICOMDIR
   
   ListDicomDirImageElem   DicomDirImageList;
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
