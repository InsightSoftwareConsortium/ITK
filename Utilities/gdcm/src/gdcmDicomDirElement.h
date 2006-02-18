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
#include <iostream>

namespace gdcm 
{

//-----------------------------------------------------------------------------

typedef std::list<Element> ListDicomDirElem;
typedef std::list<Element> ListDicomDirMetaElem;
typedef std::list<Element> ListDicomDirPatientElem;
typedef std::list<Element> ListDicomDirStudyElem;
typedef std::list<Element> ListDicomDirVisitElem;
typedef std::list<Element> ListDicomDirSerieElem;
typedef std::list<Element> ListDicomDirImageElem;

// For future use (Full DICOMDIR)

/*
typedef std::list<Element> ListDicomDirResultElem;
typedef std::list<Element> ListDicomDirStudyComponentElem;

typedef std::list<Element> ListDicomDirOverlayElem;
typedef std::list<Element> ListDicomDirModalityLutElem;
typedef std::list<Element> ListDicomDirModalityLutElem;
typedef std::list<Element> ListDicomDirCurveElem;
typedef std::list<Element> ListDicomDirStoredPrintElem;
typedef std::list<Element> ListDicomDirRtDoseElem;
typedef std::list<Element> ListDicomDirRtStructureSetElem;
typedef std::list<Element> ListDicomDirRtPlanElem;
typedef std::list<Element> ListDicomDirRtTreatRecordElem;
typedef std::list<Element> ListDicomDirPresentationElem;
typedef std::list<Element> ListDicomDirSrDocumentElem;
typedef std::list<Element> ListDicomDirKeyObjectDocElem;
typedef std::list<Element> ListDicomDirSpectroscopyElem;
typedef std::list<Element> ListDicomDirRawDataElem;
typedef std::list<Element> ListDicomDirRegistrationElem;
typedef std::list<Element> ListDicomDirFiducialElem;
*/

//-----------------------------------------------------------------------------
/**
 * \brief   Represents elements contained in a DicomDir class
 *          for the chained lists from the file 'Dicts/DicomDir.dic'
 */
class GDCM_EXPORT DicomDirElement
{
public:
   DicomDirElement();
   ~DicomDirElement();

   /**
    * \brief   canonical Printer 
    */ 
   void Print(std::ostream &os = std::cout, 
              std::string const &indent = "" );

   /**
    * \brief   returns a reference to the chained List 
    *          related to the META Elements of a DICOMDIR.
    */
   ListDicomDirMetaElem const &GetDicomDirMetaElements() const
      { return DicomDirMetaList; }

   /**
    * \brief   returns a reference to the chained List 
    *          related to the PATIENT Elements of a DICOMDIR.
    */      
   ListDicomDirPatientElem const &GetDicomDirPatientElements() const
      { return DicomDirPatientList; }

   /**
    * \brief   returns a reference to the chained List 
    *          related to the STUDY Elements of a DICOMDIR.
    */      
   ListDicomDirStudyElem const &GetDicomDirStudyElements() const
      { return DicomDirStudyList; }

   /**
    * \brief   returns a reference to the chained List 
    *          related to the VISIT Elements of a DICOMDIR.
    */      
   ListDicomDirVisitElem const &GetDicomDirVisitElements() const
      { return DicomDirVisitList; }
   /**
    * \brief   returns a reference to the chained List 
    *          related to the SERIE Elements of a DICOMDIR.
    */
   ListDicomDirSerieElem const &GetDicomDirSerieElements() const
      { return DicomDirSerieList; }

   /**
    * \brief   returns a reference to the chained List 
    *          related to the IMAGE Elements of a DICOMDIR.
    */
   ListDicomDirImageElem const &GetDicomDirImageElements() const
      { return DicomDirImageList; }

   // Public method to add an element
   bool AddEntry(DicomDirType type, Element const &elem);

   // Only one instance of ddElem 
   void AddDicomDirElement(DicomDirType type,
                           uint16_t group, uint16_t elem);

private:
   /// Elements chained list, related to the MetaElements of DICOMDIR
   ListDicomDirMetaElem    DicomDirMetaList;
   /// Elements chained list, related to the PatientElements of DICOMDIR
   ListDicomDirPatientElem DicomDirPatientList;
   /// Elements chained list, related to the StudyElements of DICOMDIR
   ListDicomDirStudyElem   DicomDirStudyList;
   /// Elements chained list, related to the VisitElements of DICOMDIR
   ListDicomDirVisitElem   DicomDirVisitList;
   /// Elements chained list, related to the SerieElements of DICOMDIR
   ListDicomDirSerieElem   DicomDirSerieList;
   /// Elements chained list, related to the ImageElements of DICOMDIR
   ListDicomDirImageElem   DicomDirImageList;
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
