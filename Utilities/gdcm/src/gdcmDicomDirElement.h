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

typedef std::list<Element> ListDicomDirElem;
typedef std::list<Element> ListDicomDirMetaElem;
typedef std::list<Element> ListDicomDirPatientElem;
typedef std::list<Element> ListDicomDirStudyElem;
typedef std::list<Element> ListDicomDirSerieElem;
typedef std::list<Element> ListDicomDirImageElem;

// For future use (Full DICOMDIR)
/*
typedef std::list<Element> ListDicomDirVisit;
typedef std::list<Element> ListDicomDirResult;
typedef std::list<Element> ListDicomDirStudyComponent;

typedef std::list<Element> ListDicomDirOverlay;
typedef std::list<Element> ListDicomDirModalityLut;
typedef std::list<Element> ListDicomDirModalityLut;
typedef std::list<Element> ListDicomDirCurve;
typedef std::list<Element> ListDicomDirStoredPrint;
typedef std::list<Element> ListDicomDirRtDose;
typedef std::list<Element> ListDicomDirRtStructureSet;
typedef std::list<Element> ListDicomDirRtPlan;
typedef std::list<Element> ListDicomDirRtTreatRecord;
typedef std::list<Element> ListDicomDirPresentation;
typedef std::list<Element> ListDicomDirSrDocument;
typedef std::list<Element> ListDicomDirKeyObjectDoc;
typedef std::list<Element> ListDicomDirSpectroscopy;
typedef std::list<Element> ListDicomDirRawData;
typedef std::list<Element> ListDicomDirRegistration;
typedef std::list<Element> ListDicomDirFiducial;
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
   void Print(std::ostream &os);

   /**
    * \brief   returns a reference to the chained List 
    *          related to the META Elements of a DICOMDIR.
    */
   ListDicomDirMetaElem const &GetDicomDirMetaElements() const
      { return DicomDirMetaList; };

   /**
    * \brief   returns a reference to the chained List 
    *          related to the PATIENT Elements of a DICOMDIR.
    */      
   ListDicomDirPatientElem const &GetDicomDirPatientElements() const
      { return DicomDirPatientList; };

   /**
    * \brief   returns a reference to the chained List 
    *          related to the STUDY Elements of a DICOMDIR.
    */      
   ListDicomDirStudyElem const &GetDicomDirStudyElements() const
      { return DicomDirStudyList; };

   /**
    * \brief   returns a reference to the chained List 
    *          related to the SERIE Elements of a DICOMDIR.
    */
   ListDicomDirSerieElem const &GetDicomDirSerieElements() const
      { return DicomDirSerieList; };

   /**
    * \brief   returns a reference to the chained List 
    *          related to the IMAGE Elements of a DICOMDIR.
    */
   ListDicomDirImageElem const &GetDicomDirImageElements() const
      { return DicomDirImageList; };

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
   /// Elements chained list, related to the SerieElements of DICOMDIR
   ListDicomDirSerieElem   DicomDirSerieList;
   /// Elements chained list, related to the ImageElements of DICOMDIR
   ListDicomDirImageElem   DicomDirImageList;
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
