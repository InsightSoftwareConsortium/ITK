/*=========================================================================
  
  Program:   gdcm
  Module:    gdcmDicomDir.h
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

#ifndef GDCMDICOMDIR_H
#define GDCMDICOMDIR_H

#include "gdcmCommon.h"
#include "gdcmDocument.h"
#include "gdcmDicomDirPatient.h"
#include "gdcmDicomDirMeta.h"
#include "gdcmDicomDirElement.h"

#include <list>
#include <vector>

namespace gdcm 
{
//-----------------------------------------------------------------------------
typedef std::list<DicomDirPatient*>   ListDicomDirPatient;
typedef std::vector<Document*>  VectDocument;

typedef GDCM_EXPORT void(Method)(void*);
//-----------------------------------------------------------------------------

/**
 * \ingroup DicomDir
 * \brief    DicomDir defines an object representing a DICOMDIR in memory.
 *
 */
class GDCM_EXPORT DicomDir: public Document
{
public:
   DicomDir( std::string const & filename, bool parseDir = false );
   DicomDir(); 
                   
   ~DicomDir();

   /// \brief   canonical Printer 
   /// \sa    SetPrintLevel
   void Print(std::ostream &os = std::cout);

   /// Informations contained in the parser
   virtual bool IsReadable();

   /// Returns a pointer to the DicomDirMeta for this DICOMDIR. 
   DicomDirMeta* GetDicomDirMeta() { return MetaElems; };

   /// Returns the PATIENT chained List for this DICOMDIR.    
   ListDicomDirPatient const & GetDicomDirPatients() const { return Patients; };

   /// Parsing
   void ParseDirectory();
   
   void SetStartMethod(Method*, void* = NULL, Method* = NULL);
   void SetStartMethodArgDelete(Method*);
   void SetProgressMethod(Method* ,void* = NULL, Method* = NULL);
   void SetProgressMethodArgDelete(Method*);
   void SetEndMethod(Method*, void* = NULL,Method* = NULL);
   void SetEndMethodArgDelete(Method*);

   /// GetProgress GetProgress
   float GetProgress()  { return Progress; };

   /// AbortProgress AbortProgress
   void  AbortProgress() { Abort = true; };

   /// IsAborted IsAborted
   bool  IsAborted() { return Abort; };
   
   /// Adding
   DicomDirMeta*    NewMeta();
   DicomDirPatient* NewPatient();

   /// Write  
   bool WriteDicomDir(std::string const & fileName);

   /// Types of the DicomDirObject within the DicomDir
   typedef enum
   {
      GDCM_DICOMDIR_NONE,
      GDCM_DICOMDIR_META,
      GDCM_DICOMDIR_PATIENT,
      GDCM_DICOMDIR_STUDY,
      GDCM_DICOMDIR_SERIE,
      GDCM_DICOMDIR_IMAGE
   } DicomDirType;
   
protected:
   void CreateDicomDirChainedList(std::string const & path);
   void CallStartMethod();
   void CallProgressMethod();
   void CallEndMethod();

private:
   void Initialize();
   void CreateDicomDir();
   void AddDicomDirMeta();
   void AddDicomDirPatientToEnd(SQItem* s);
   void AddDicomDirStudyToEnd  (SQItem* s);
   void AddDicomDirSerieToEnd  (SQItem* s);
   void AddDicomDirImageToEnd  (SQItem* s);

   void SetElements(std::string const & path, VectDocument const &list);
   void SetElement (std::string const & path, DicomDirType type,
                    Document* header);

   static bool HeaderLessThan(Document* header1, Document* header2);
   
// Variables

   /// Pointer on *the* DicomDirObject 'DicomDirMeta Elements'
   DicomDirMeta* MetaElems;

   /// Chained list of DicomDirPatient (to be exploited recursively) 
   ListDicomDirPatient Patients;

   /// pointer to the initialisation method for any progress bar   
   Method* StartMethod;
   /// pointer to the incrementation method for any progress bar
   Method* ProgressMethod;
   /// pointer to the termination method for any progress bar
   Method* EndMethod;
   /// pointer to the ??? method for any progress bar   
   Method* StartMethodArgDelete;
   /// pointer to the ??? method for any progress bar
   Method* ProgressMethodArgDelete;
   /// pointer to the ??? method for any progress bar
   Method* EndMethodArgDelete;
   /// pointer to the ??? for any progress bar   
   void* StartArg;
   /// pointer to the ??? for any progress bar
   void* ProgressArg;
   /// pointer to the ??? for any progress bar   
   void* EndArg;
   /// value of the ??? for any progress bar
   float Progress;
   /// value of the ??? for any progress bar   
   bool Abort;
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
