/*=========================================================================
  
  Program:   gdcm
  Module:    gdcmDicomDir.cxx
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

//-----------------------------------------------------------------------------
//  For full DICOMDIR description, see:
//  PS 3.3-2003, pages 731-750
//-----------------------------------------------------------------------------
#include "gdcmDicomDir.h"
#include "gdcmDicomDirStudy.h"
#include "gdcmDicomDirSerie.h"
#include "gdcmDicomDirImage.h"
#include "gdcmDicomDirPatient.h"
#include "gdcmDicomDirMeta.h"
#include "gdcmDicomDirElement.h"
#include "gdcmDirList.h"
#include "gdcmUtil.h"
#include "gdcmDebug.h"
#include "gdcmGlobal.h"
#include "gdcmFile.h"
#include "gdcmSeqEntry.h"
#include "gdcmSQItem.h"
#include "gdcmValEntry.h"

#include <fstream>
#include <string>
#include <algorithm>
#include <sys/types.h>

#ifdef _MSC_VER
#   define getcwd _getcwd
#endif

#if defined(_MSC_VER) || defined(__BORLANDC__)
#   include <direct.h>
#else
#   include <unistd.h>
#endif
// ----------------------------------------------------------------------------
//         Note for future developpers
// ----------------------------------------------------------------------------
//
//  Dicom PS 3.3 describes the relationship between Directory Records, as follow
//
//  Directory Record Type      Directory Record Types which may be included
//                                in the next lower-level directory Entity
//
// (Root directory Entity)     PATIENT
//
// PATIENT                     STUDY
//
// STUDY                       SERIES, VISIT, RESULTS, STUDY COMPONENT
//
// SERIES                      IMAGE, OVERLAYS, MODALITY LUT, VOI LUT,
//                             CURVE, STORED PRINT, RT DOSE, RT STRUCTURE SET
//                             RT PLAN, RT TREAT RECORD, PRESENTATION, WAVEFORM,
//                             SR DOCUMENT, KEY OBJECT DOC, SPECTROSCOPY,
//                             RAW DATA, REGISTRATION, FIDUCIAL
// IMAGE
// OVERLAY
// MODALITY LUT
// VOI LUT
// CURVE
// STORED PRINT
// RT DOSE
// RT STRUCTURE SET
// RT PLAN
// RT TREAT RECORD
// PRESENTATION
// WAVEFORM
// SR DOCUMENT
// KEY OBJECT DOC
// SPECTROSCOPY
// RAW DATA
// REGISTRATION
// FIDUCIAL
// 
// ----------------------
// The current gdcm version only deals with :
//
// (Root directory Entity)     PATIENT
// PATIENT                     STUDY
// STUDY                       SERIES 
// SERIES                      IMAGE
// IMAGE                       /
//
// DicomDir::CreateDicomDir will have to be completed
// Treelike structure management will have to be upgraded
// ----------------------------------------------------------------------------
    
namespace gdcm 
{
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief   Constructor : creates an empty DicomDir
 */
DicomDir::DicomDir()
         :Document( )
{
   Initialize();  // sets all private fields to NULL
   NewMeta();
}

/**
 * \brief Constructor Parses recursively the directory and creates the DicomDir
 *        or uses an already built DICOMDIR, depending on 'parseDir' value.
 * @param fileName  name 
 *                      - of the root directory (parseDir = true)
 *                      - of the DICOMDIR       (parseDir = false)
 * @param parseDir boolean
 *                      - true if user passed an entry point 
 *                        and wants to explore recursively the directories
 *                      - false if user passed an already built DICOMDIR file
 *                        and wants to use it 
 */
DicomDir::DicomDir(std::string const &fileName, bool parseDir ):
   Document( fileName )
{
   // At this step, Document constructor is already executed,
   // whatever user passed (a root directory or a DICOMDIR)
   // and whatever the value of parseDir was.
   // (nothing is cheked in Document constructor, to avoid overhead)

   Initialize();  // sets all private fields to NULL

   // if user passed a root directory, sure we didn't get anything

   if ( GetFirstEntry() == 0 ) // when user passed a Directory to parse
   {
      if (!parseDir)
         gdcmWarningMacro( "Entry HT empty for file: "<<fileName);

   // Only if user passed a root directory
   // ------------------------------------
      if ( fileName == "." )
      {
         // user passed '.' as Name
         // we get current directory name
         char dummy[1000];
         getcwd(dummy, (size_t)1000);
         SetFileName( dummy ); // will be converted into a string
      }

      if ( parseDir ) // user asked for a recursive parsing of a root directory
      {
         NewMeta();

         gdcmWarningMacro( "Parse directory and create the DicomDir");
         ParseDirectory();
      }
      else
      {
         //  user may just call ParseDirectory() *after* constructor
      }
   }
   // Only if user passed a DICOMDIR
   // ------------------------------
   else 
   {
      // Directory record sequence
      DocEntry *e = GetDocEntry(0x0004, 0x1220);
      if ( !e )
      {
         gdcmWarningMacro( "NO 'Directory record sequence' (0x0004,0x1220)"
                          << " in file " << fileName);
      }
      else
         CreateDicomDir();
   }
}

/**
 * \brief  Canonical destructor 
 */
DicomDir::~DicomDir() 
{
   SetStartMethod(NULL);
   SetProgressMethod(NULL);
   SetEndMethod(NULL);

   ClearPatient();
   if ( MetaElems )
   {
      delete MetaElems;
   }
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief  This predicate, based on hopefully reasonable heuristics,
 *         decides whether or not the current document was properly parsed
 *         and contains the mandatory information for being considered as
 *         a well formed and usable DicomDir.
 * @return true when Document is the one of a reasonable DicomDir,
 *         false otherwise. 
 */
bool DicomDir::IsReadable()
{
   if( Filetype == Unknown)
   {
      gdcmWarningMacro( "Wrong filetype");
      return false;
   }
   if( !MetaElems )
   {
      gdcmWarningMacro( "Meta Elements missing in DicomDir");
      return false;
   }
   if( Patients.size() <= 0 )
   {
      gdcmWarningMacro( "NO Patient in DicomDir");
      return false;
   }

   return true;
}

/**
 * \brief   adds *the* Meta to a partially created DICOMDIR
 */  
DicomDirMeta *DicomDir::NewMeta()
{
   if( MetaElems )
      delete MetaElems;

   DocEntry *entry = GetFirstEntry();
   if( entry )
   { 
      MetaElems = new DicomDirMeta(true);

      entry = GetFirstEntry();
      while( entry )
      {
         if( dynamic_cast<SeqEntry *>(entry) )
            break;

         RemoveEntryNoDestroy(entry);
         MetaElems->AddEntry(entry);

         entry = GetFirstEntry();
      }
   }
   else  // after root directory parsing
   {
      MetaElems = new DicomDirMeta(false);
   }
   MetaElems->SetSQItemNumber(0); // To avoid further missprinting
   return MetaElems;  
}

/**
 * \brief   adds a new Patient (with the basic elements) to a partially created
 *          DICOMDIR
 */
DicomDirPatient *DicomDir::NewPatient()
{
   DicomDirPatient *p = new DicomDirPatient();
   AddPatientToEnd( p );
   return p;
}

/**
 * \brief   Remove all Patients
 */
void DicomDir::ClearPatient()
{
   for(ListDicomDirPatient::iterator cc = Patients.begin();
                                     cc!= Patients.end();
                                   ++cc)
   {
      delete *cc;
   }
   Patients.clear();
}

/**
 * \brief   Get the first entry while visiting the DicomDirPatients
 * \return  The first DicomDirPatient if found, otherwhise NULL
 */ 
DicomDirPatient *DicomDir::GetFirstPatient()
{
   ItPatient = Patients.begin();
   if ( ItPatient != Patients.end() )
      return *ItPatient;
   return NULL;
}

/**
 * \brief   Get the next entry while visiting the DicomDirPatients
 * \note : meaningfull only if GetFirstEntry already called
 * \return  The next DicomDirPatient if found, otherwhise NULL
 */
DicomDirPatient *DicomDir::GetNextPatient()
{
   gdcmAssertMacro (ItPatient != Patients.end());

   ++ItPatient;
   if ( ItPatient != Patients.end() )
      return *ItPatient;
   return NULL;
}

/**
 * \brief  fills the whole structure, starting from a root Directory
 */
void DicomDir::ParseDirectory()
{
   CreateDicomDirChainedList( GetFileName() );
   CreateDicomDir();
}

/**
 * \brief   Set the start method to call when the parsing of the
 *          directory starts.
 * @param   method Method to call
 * @param   arg    Argument to pass to the method
 * @param   argDelete    Argument 
 * \warning In python : the arg parameter isn't considered
 */
void DicomDir::SetStartMethod( DicomDir::Method *method, void *arg, 
                               DicomDir::Method *argDelete )
{
   if( StartArg && StartMethodArgDelete )
   {
      StartMethodArgDelete( StartArg );
   }

   StartMethod          = method;
   StartArg             = arg;
   StartMethodArgDelete = argDelete;
}

/**
 * \brief   Set the progress method to call when the parsing of the
 *          directory progress
 * @param   method Method to call
 * @param   arg    Argument to pass to the method
 * @param   argDelete    Argument  
 * \warning In python : the arg parameter isn't considered
 */
void DicomDir::SetProgressMethod( DicomDir::Method *method, void *arg, 
                                  DicomDir::Method *argDelete )
{
   if( ProgressArg && ProgressMethodArgDelete )
   {
      ProgressMethodArgDelete( ProgressArg );
   }

   ProgressMethod          = method;
   ProgressArg             = arg;
   ProgressMethodArgDelete = argDelete;
}

/**
 * \brief   Set the end method to call when the parsing of the directory ends
 * @param   method Method to call
 * @param   arg    Argument to pass to the method
 * @param   argDelete    Argument 
 * \warning In python : the arg parameter isn't considered
 */
void DicomDir::SetEndMethod( DicomDir::Method *method, void *arg, 
                             DicomDir::Method *argDelete )
{
   if( EndArg && EndMethodArgDelete )
   {
      EndMethodArgDelete( EndArg );
   }

   EndMethod          = method;
   EndArg             = arg;
   EndMethodArgDelete = argDelete;
}

/**
 * \brief   Set the method to delete the argument
 *          The argument is destroyed when the method is changed or when the
 *          class is destroyed
 * @param   method Method to call to delete the argument
 */
void DicomDir::SetStartMethodArgDelete( DicomDir::Method *method ) 
{
   StartMethodArgDelete = method;
}

/**
 * \brief   Set the method to delete the argument
 *          The argument is destroyed when the method is changed or when the 
 *          class is destroyed          
 * @param   method Method to call to delete the argument
 */
void DicomDir::SetProgressMethodArgDelete( DicomDir::Method *method )
{
   ProgressMethodArgDelete = method;
}

/**
 * \brief   Set the method to delete the argument
 *          The argument is destroyed when the method is changed or when
 *          the class is destroyed
 * @param   method Method to call to delete the argument
 */
void DicomDir::SetEndMethodArgDelete( DicomDir::Method *method )
{
   EndMethodArgDelete = method;
}

/**
 * \brief    writes on disc a DICOMDIR
 * \ warning does NOT add the missing elements in the header :
 *           it's up to the user doing it !
 * @param  fileName file to be written to 
 * @return false only when fail to open
 */
 
bool DicomDir::WriteDicomDir(std::string const &fileName) 
{  
   int i;
   uint16_t sq[4] = { 0x0004, 0x1220, 0xffff, 0xffff };
   uint16_t sqt[4]= { 0xfffe, 0xe0dd, 0xffff, 0xffff };

   std::ofstream *fp = new std::ofstream(fileName.c_str(),  
                                         std::ios::out | std::ios::binary);
   if( !fp ) 
   {
      gdcmWarningMacro("Failed to open(write) File: " << fileName.c_str());
      return false;
   }

   char filePreamble[128];
   memset(filePreamble, 0, 128);
   fp->write(filePreamble, 128);
   binary_write( *fp, "DICM");
 
   DicomDirMeta *ptrMeta = GetMeta();
   ptrMeta->WriteContent(fp, ExplicitVR);
   
   // force writing 0004|1220 [SQ ], that CANNOT exist within DicomDirMeta
   for(i=0;i<4;++i)
   {
      binary_write(*fp, sq[i]);
   }
        
   for(ListDicomDirPatient::iterator cc  = Patients.begin();
                                     cc != Patients.end();
                                   ++cc )
   {
      (*cc)->WriteContent( fp, ExplicitVR );
   }
   
   // force writing Sequence Delimitation Item
   for(i=0;i<4;++i)
   {
      binary_write(*fp, sqt[i]);  // fffe e0dd ffff ffff 
   }

   fp->close();
   delete fp;

   return true;
}

//-----------------------------------------------------------------------------
// Protected
/**
 * \brief create a Document-like chained list from a root Directory 
 * @param path entry point of the tree-like structure
 */
void DicomDir::CreateDicomDirChainedList(std::string const &path)
{
   CallStartMethod();
   DirList dirList(path,1); // gets recursively the file list
   unsigned int count = 0;
   VectDocument list;
   File *header;

   DirListType fileList = dirList.GetFilenames();

   for( DirListType::iterator it  = fileList.begin();
                              it != fileList.end();
                              ++it )
   {
      Progress = (float)(count+1)/(float)fileList.size();
      CallProgressMethod();
      if( Abort )
      {
         break;
      }

      header = new File( it->c_str() );
      if( !header )
      {
         gdcmWarningMacro( "Failure in new gdcm::File " << it->c_str() );
         continue;
      }
      
      if( header->IsReadable() )
      {
         // Add the file to the chained list:
         list.push_back(header);
         gdcmWarningMacro( "Readable " << it->c_str() );
       }
       else
       {
          delete header;
       }
       count++;
   }
   // sorts Patient/Study/Serie/
   std::sort(list.begin(), list.end(), DicomDir::HeaderLessThan );
   
   std::string tmp = dirList.GetDirName();      
   //for each File of the chained list, add/update the Patient/Study/Serie/Image info
   SetElements(tmp, list);
   CallEndMethod();

   for(VectDocument::iterator itDoc=list.begin();
       itDoc!=list.end();
       ++itDoc)
   {
      delete dynamic_cast<File *>(*itDoc);
   }
}

/**
 * \brief   CallStartMethod
 */
void DicomDir::CallStartMethod()
{
   Progress = 0.0f;
   Abort    = false;
   if( StartMethod )
   {
      StartMethod( StartArg );
   }
}

/**
 * \brief   CallProgressMethod
 */
void DicomDir::CallProgressMethod()
{
   if( ProgressMethod )
   {
      ProgressMethod( ProgressArg );
   }
}

/**
 * \brief   CallEndMethod
 */
void DicomDir::CallEndMethod()
{
   Progress = 1.0f;
   if( EndMethod )
   {
      EndMethod( EndArg );
   }
}

//-----------------------------------------------------------------------------
// Private
/**
 * \brief Sets all fields to NULL
 */
void DicomDir::Initialize()
{
   StartMethod             = NULL;
   ProgressMethod          = NULL;
   EndMethod               = NULL;
   StartMethodArgDelete    = NULL;
   ProgressMethodArgDelete = NULL;
   EndMethodArgDelete      = NULL;
   StartArg                = NULL;
   ProgressArg             = NULL;
   EndArg                  = NULL;

   Progress = 0.0;
   Abort = false;

   MetaElems = NULL;   
}

/**
 * \brief create a 'DicomDir' from a DICOMDIR Header 
 */
void DicomDir::CreateDicomDir()
{
   // The SeqEntries of "Directory Record Sequence" are parsed. 
   //  When a DicomDir tag ("PATIENT", "STUDY", "SERIE", "IMAGE") is found :
   //  1 - we save the beginning iterator
   //  2 - we continue to parse
   //  3 - we find an other tag
   //       + we create the object for the precedent tag
   //       + loop to 1 -

   // Directory record sequence
   DocEntry *e = GetDocEntry(0x0004, 0x1220);
   if ( !e )
   {
      gdcmWarningMacro( "No Directory Record Sequence (0004,1220) found");
      return;         
   }
   
   SeqEntry *s = dynamic_cast<SeqEntry *>(e);
   if ( !s )
   {
      gdcmWarningMacro( "Element (0004,1220) is not a Sequence ?!?");
      return;
   }

   NewMeta();
   
   DocEntry *d;
   std::string v;
   SQItem *si;

   SQItem *tmpSI=s->GetFirstSQItem();
   while(tmpSI)
   {
      d = tmpSI->GetDocEntry(0x0004, 0x1430); // Directory Record Type
      if ( ValEntry* valEntry = dynamic_cast<ValEntry *>(d) )
      {
         v = valEntry->GetValue();
      }
      else
      {
         gdcmWarningMacro( "Not a ValEntry.");
         continue;
      }

      if( v == "PATIENT " )
      {
         si = new DicomDirPatient(true);
         if( !AddPatientToEnd( static_cast<DicomDirPatient *>(si)) )
         {
            delete si;
            si = NULL;
            gdcmErrorMacro( "Add PatientToEnd failed");
         }
      }
      else if( v == "STUDY " )
      {
         si = new DicomDirStudy(true);
         if( !AddStudyToEnd( static_cast<DicomDirStudy *>(si)) )
         {
            delete si;
            si = NULL;
            gdcmErrorMacro( "Add AddStudyToEnd failed");
         }
      }
      else if( v == "SERIES" )
      {
         si = new DicomDirSerie(true);
         if( !AddSerieToEnd( static_cast<DicomDirSerie *>(si)) )
         {
            delete si;
            si = NULL;
            gdcmErrorMacro( "Add AddSerieToEnd failed");
         }
      }
      else if( v == "IMAGE " ) 
      {
         si = new DicomDirImage(true);
         if( !AddImageToEnd( static_cast<DicomDirImage *>(si)) )
         {
            delete si;
            si = NULL;
            gdcmErrorMacro( "Add AddImageToEnd failed");
         }
      }
      else
      {
         // It was not a 'PATIENT', nor a 'STUDY', nor a 'SERIE',
         // neither an 'IMAGE' SQItem. Skip to next item.
         continue;
      }
      if( si )
         MoveSQItem(si,tmpSI);

      tmpSI=s->GetNextSQItem();
   }
   ClearEntry();
}

/**
 * \brief  AddPatientToEnd 
 * @param   dd SQ Item to enqueue to the DicomPatient chained List
 */
bool DicomDir::AddPatientToEnd(DicomDirPatient *dd)
{
   Patients.push_back(dd);
   return true;
}

/**
 * \brief  AddStudyToEnd 
 * @param   dd SQ Item to enqueue to the DicomDirStudy chained List
 */
bool DicomDir::AddStudyToEnd(DicomDirStudy *dd)
{
   if( Patients.size() > 0 )
   {
      ListDicomDirPatient::iterator itp = Patients.end();
      itp--;
      (*itp)->AddStudy(dd);
      return true;
   }
   return false;
}

/**
 * \brief  AddSerieToEnd 
 * @param   dd SQ Item to enqueue to the DicomDirSerie chained List
 */
bool DicomDir::AddSerieToEnd(DicomDirSerie *dd)
{
   if( Patients.size() > 0 )
   {
      ListDicomDirPatient::iterator itp = Patients.end();
      itp--;

      DicomDirStudy *study = (*itp)->GetLastStudy();
      if( study )
      {
         study->AddSerie(dd);
         return true;
      }
   }
   return false;
}

/**
 * \brief   AddImageToEnd
 * @param   dd SQ Item to enqueue to the DicomDirImage chained List
 */
bool DicomDir::AddImageToEnd(DicomDirImage *dd)
{
   if( Patients.size() > 0 )
   {
      ListDicomDirPatient::iterator itp = Patients.end();
      itp--;

      DicomDirStudy *study = (*itp)->GetLastStudy();
      if( study )
      {
         DicomDirSerie *serie = study->GetLastSerie();
         if( serie )
         {
            serie->AddImage(dd);
            return true;
         }
      }
   }
   return false;
}

/**
 * \brief  for each Header of the chained list, 
 *         add/update the Patient/Study/Serie/Image info 
 * @param   path path of the root directory
 * @param   list chained list of Headers
 */
void DicomDir::SetElements(std::string const &path, VectDocument const &list)
{
   ClearEntry();
   ClearPatient();

   std::string patPrevName         = "", patPrevID  = "";
   std::string studPrevInstanceUID = "", studPrevID = "";
   std::string serPrevInstanceUID  = "", serPrevID  = "";

   std::string patCurName,         patCurID;
   std::string studCurInstanceUID, studCurID;
   std::string serCurInstanceUID,  serCurID;

   bool first = true;
   for( VectDocument::const_iterator it = list.begin();
                                     it != list.end(); 
                                   ++it )
   {
      // get the current file characteristics
      patCurName         = (*it)->GetEntryValue(0x0010,0x0010);
      patCurID           = (*it)->GetEntryValue(0x0010,0x0011);
      studCurInstanceUID = (*it)->GetEntryValue(0x0020,0x000d);
      studCurID          = (*it)->GetEntryValue(0x0020,0x0010);
      serCurInstanceUID  = (*it)->GetEntryValue(0x0020,0x000e);
      serCurID           = (*it)->GetEntryValue(0x0020,0x0011);

      if( patCurName != patPrevName || patCurID != patPrevID || first )
      {
         SetElement(path, GDCM_DICOMDIR_PATIENT, *it);
         first = true;
      }

      // if new Study Deal with 'STUDY' Elements   
      if( studCurInstanceUID != studPrevInstanceUID || studCurID != studPrevID 
         || first )
      {
         SetElement(path, GDCM_DICOMDIR_STUDY, *it);
         first = true;
      }

      // if new Serie Deal with 'SERIE' Elements   
      if( serCurInstanceUID != serPrevInstanceUID || serCurID != serPrevID
         || first )
      {
         SetElement(path, GDCM_DICOMDIR_SERIE, *it);
         first = true;
      }
      
      // Always Deal with 'IMAGE' Elements  
      SetElement(path, GDCM_DICOMDIR_IMAGE, *it);

      patPrevName         = patCurName;
      patPrevID           = patCurID;
      studPrevInstanceUID = studCurInstanceUID;
      studPrevID          = studCurID;
      serPrevInstanceUID  = serCurInstanceUID;
      serPrevID           = serCurID;
      first = false;
   }
}

/**
 * \brief   adds to the HTable 
 *          the Entries (Dicom Elements) corresponding to the given type
 * @param   path full path file name (only used when type = GDCM_DICOMDIR_IMAGE
 * @param   type DicomDirObject type to create (GDCM_DICOMDIR_PATIENT,
 *          GDCM_DICOMDIR_STUDY, GDCM_DICOMDIR_SERIE ...)
 * @param   header Header of the current file
 */
void DicomDir::SetElement(std::string const &path, DicomDirType type,
                          Document *header)
{
   ListDicomDirElem elemList;
   ListDicomDirElem::const_iterator it;
   uint16_t tmpGr, tmpEl;
   DictEntry *dictEntry;
   ValEntry *entry;
   std::string val;
   SQItem *si;

   switch( type )
   {
      case GDCM_DICOMDIR_IMAGE:
         elemList = Global::GetDicomDirElements()->GetDicomDirImageElements();
         si = new DicomDirImage(true);
         if( !AddImageToEnd(static_cast<DicomDirImage *>(si)) )
         {
            delete si;
            gdcmErrorMacro( "Add ImageToEnd failed");
         }
         break;
      case GDCM_DICOMDIR_SERIE:
         elemList = Global::GetDicomDirElements()->GetDicomDirSerieElements();
         si = new DicomDirSerie(true);
         if( !AddSerieToEnd(static_cast<DicomDirSerie *>(si)) )
         {
            delete si;
            gdcmErrorMacro( "Add SerieToEnd failed");
         }
         break;
      case GDCM_DICOMDIR_STUDY:
         elemList = Global::GetDicomDirElements()->GetDicomDirStudyElements();
         si = new DicomDirStudy(true);
         if( !AddStudyToEnd(static_cast<DicomDirStudy *>(si)) )
         {
            delete si;
            gdcmErrorMacro( "Add StudyToEnd failed");
         }
         break;
      case GDCM_DICOMDIR_PATIENT:
         elemList = Global::GetDicomDirElements()->GetDicomDirPatientElements();
         si = new DicomDirPatient(true);
         if( !AddPatientToEnd(static_cast<DicomDirPatient *>(si)) )
         {
            delete si;
            gdcmErrorMacro( "Add PatientToEnd failed");
         }
         break;
      case GDCM_DICOMDIR_META:
         elemList = Global::GetDicomDirElements()->GetDicomDirMetaElements();
         si = new DicomDirMeta(true);
         if( MetaElems )
         {
            delete MetaElems;
            gdcmErrorMacro( "MetaElements already exist, they will be destroyed");
         }
         MetaElems = static_cast<DicomDirMeta *>(si);
         break;
      default:
         return;
   }
   // removed all the seems-to-be-useless stuff about Referenced Image Sequence
   // to avoid further troubles
   // imageElem 0008 1140 "" // Referenced Image Sequence
   // imageElem fffe e000 "" // 'no length' item : length to be set to 0xffffffff later
   // imageElem 0008 1150 "" // Referenced SOP Class UID    : to be set/forged later
   // imageElem 0008 1155 "" // Referenced SOP Instance UID : to be set/forged later
   // imageElem fffe e00d "" // Item delimitation : length to be set to ZERO later
 
   // FIXME : troubles found when it's a SeqEntry

   // for all the relevant elements found in their own spot of the DicomDir.dic
   for( it = elemList.begin(); it != elemList.end(); ++it)
   {
      tmpGr     = it->Group;
      tmpEl     = it->Elem;
      dictEntry = GetPubDict()->GetEntry(tmpGr, tmpEl);

      entry     = new ValEntry( dictEntry ); // Be sure it's never a BinEntry !

      entry->SetOffset(0); // just to avoid further missprinting

      if( header )
      {
         // NULL when we Build Up (ex nihilo) a DICOMDIR
         //   or when we add the META elems
         val = header->GetEntryValue(tmpGr, tmpEl);
      }
      else
      {
         val = GDCM_UNFOUND;
      }

      if( val == GDCM_UNFOUND) 
      {
         if( tmpGr == 0x0004 && tmpEl == 0x1130 ) // File-set ID
         {
           // force to the *end* File Name
           val = Util::GetName( path );
         }
         else if( tmpGr == 0x0004 && tmpEl == 0x1500 ) // Only used for image
         {
            if( header->GetFileName().substr(0, path.length()) != path )
            {
               gdcmWarningMacro( "The base path of file name is incorrect");
               val = header->GetFileName();
            }
            else
            {
               val = &(header->GetFileName().c_str()[path.length()]);
            }
         }
         else
         {
            val = it->Value;
         }
      }
      else
      {
         if ( header->GetEntryLength(tmpGr,tmpEl) == 0 )
            val = it->Value;
      }

      entry->SetValue( val ); // troubles expected when vr=SQ ...

      if ( type == GDCM_DICOMDIR_META ) // fusible : should never print !
      {
         gdcmWarningMacro("GDCM_DICOMDIR_META ?!? should never print that");
      }
      si->AddEntry(entry);
   }
}

/**
 * \brief   Move the content of the source SQItem to the destination SQItem
 *          Only DocEntry's are moved
 * @param dst destination SQItem
 * @param src source SQItem
 */
void DicomDir::MoveSQItem(DocEntrySet *dst,DocEntrySet *src)
{
   DocEntry *entry;

   entry = src->GetFirstEntry();
   while(entry)
   {
      src->RemoveEntryNoDestroy(entry);
      dst->AddEntry(entry);
      // we destroyed -> the current iterator is not longer valid
      entry = src->GetFirstEntry();
   }
}

/**
 * \brief   compares two files
 */
bool DicomDir::HeaderLessThan(Document *header1, Document *header2)
{
   return *header1 < *header2;
}

//-----------------------------------------------------------------------------
// Print
/**
 * \brief  Canonical Printer 
 * @param   os ostream we want to print in
 * @param indent Indentation string to be prepended during printing
 */
void DicomDir::Print(std::ostream &os, std::string const & )
{
   if( MetaElems )
   {
      MetaElems->SetPrintLevel(PrintLevel);
      MetaElems->Print(os);   
   }   
   for(ListDicomDirPatient::iterator cc  = Patients.begin();
                                     cc != Patients.end();
                                   ++cc)
   {
     (*cc)->SetPrintLevel(PrintLevel);
     (*cc)->Print(os);
   }
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
