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

#include "gdcmDicomDir.h"
#include "gdcmDicomDirStudy.h"
#include "gdcmDicomDirSerie.h"
#include "gdcmDicomDirImage.h"
#include "gdcmDirList.h"
#include "gdcmUtil.h"
#include "gdcmDebug.h"
#include "gdcmGlobal.h"
#include "gdcmHeader.h"
#include "gdcmSeqEntry.h"
#include "gdcmSQItem.h"
#include "gdcmValEntry.h"

#include <fstream>
#include <string>
#include <algorithm>
#include <sys/types.h>

#if defined( _MSC_VER) || defined(__BORLANDC__)
   #include <direct.h>
#else
   #include <unistd.h>
#endif
namespace gdcm 
{

//-----------------------------------------------------------------------------
//  For full DICOMDIR description, see:
//  PS 3.3-2003, pages 731-750
//-----------------------------------------------------------------------------
// Constructor / Destructor

/**
 * \ingroup DicomDir
 * \brief   Constructor : creates an empty DicomDir
 */
DicomDir::DicomDir()
   :Document( )
{ 
   Initialize();  // sets all private fields to NULL
   std::string pathBidon = "Bidon"; // Sorry, NULL not allowed ...   
   MetaElems = NewMeta();
}

/**
 * \brief Constructor Parses recursively the directory and creates the DicomDir
 *        or uses an already built DICOMDIR, depending on 'parseDir' value.
 * @param fileName        name 
 *                      - of the root directory (parseDir = true)
 *                      - of the DICOMDIR       (parseDir = false)
 * @param parseDir boolean
 *                      - true if user passed an entry point 
 *                        and wants to explore recursively the directories
 *                      - false if user passed an already built DICOMDIR file
 *                        and wants to use it 
 */
DicomDir::DicomDir(std::string const & fileName, bool parseDir ):
   Document( fileName )
{
   // Whatever user passed (a root directory or a DICOMDIR)
   // and whatever the value of parseDir was,
   // Document is already executed
   Initialize();  // sets all private fields to NULL

   // if user passed a root directory, sure we didn't get anything

   if ( TagHT.begin() == TagHT.end() ) // when user passed a Directory to parse
   {
      dbg.Verbose(0, "DicomDir::DicomDir : entry HT empty");

      if ( fileName.size() == 1 && fileName[0] == '.' )
      {
         // user passed '.' as Name
         // we get current directory name
         char dummy[1000];
         getcwd(dummy, (size_t)1000);
         SetFileName( dummy ); // will be converted into a string
      }

      if ( parseDir ) // user asked for a recursive parsing of a root directory
      {
         MetaElems = NewMeta();

         dbg.Verbose(0, "DicomDir::DicomDir : Parse directory"
                        " and create the DicomDir");
         ParseDirectory();
      }
      else
      {
         /// \todo if parseDir == false, it should be tagged as an error
         // NON ! il suffit d'appeler ParseDirectory() 
         // apres le constructeur
      }
   }
   else // Only if user passed a DICOMDIR
   {
      // Directory record sequence
      DocEntry *e = GetDocEntryByNumber(0x0004, 0x1220);
      if ( !e )
      {
         dbg.Verbose(0, "DicomDir::DicomDir : NO Directory record"
                        " sequence (0x0004,0x1220)");
         /// \todo FIXME : what do we do when the parsed file IS NOT a
         ///       DICOMDIR file ?         
      }
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
   for(ListDicomDirPatient::iterator cc = Patients.begin();
                                     cc!= Patients.end();
                                   ++cc)
   {
      delete *cc;
   }
   if ( MetaElems )
   {
      delete MetaElems;
   }
}

//-----------------------------------------------------------------------------
// Print
/**
 * \brief  Canonical Printer 
 */
void DicomDir::Print(std::ostream &os)
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
     (*cc)->SetPrintLevel( PrintLevel );
     (*cc)->Print( os );
   }
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief  This predicate, based on hopefully reasonable heuristics,
 *         decides whether or not the current header was properly parsed
 *         and contains the mandatory information for being considered as
 *         a well formed and usable DicomDir.
 * @return true when Document is the one of a reasonable DicomDir,
 *         false otherwise. 
 */
bool DicomDir::IsReadable()
{
   if( !Document::IsReadable() )
   {
      return false;
   }
   if( !MetaElems )
   {
      return false;
   }
   if( Patients.size() <= 0 )
   {
      return false;
   }

   return true;
}

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

   MetaElems = 0;   
}


/**
 * \ingroup DicomDir
 * \brief  fills the whole structure, starting from a root Directory
 */
void DicomDir::ParseDirectory()
{
   CreateDicomDirChainedList( GetFileName() );
   CreateDicomDir();
}

/**
 * \ingroup DicomDir
 * \brief   Set the start method to call when the parsing of the directory starts
 * @param   method Method to call
 * @param   arg    Argument to pass to the method
 * @param   argDelete    Argument 
 * \warning In python : the arg parameter isn't considered
 */
void DicomDir::SetStartMethod(Method* method, void* arg, 
                              Method* argDelete )
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
 * \ingroup DicomDir
 * \brief   Set the method to delete the argument
 *          The argument is destroyed when the method is changed or when the
 *          class is destroyed
 * @param   method Method to call to delete the argument
 */
void DicomDir::SetStartMethodArgDelete(Method* method) 
{
   StartMethodArgDelete = method;
}

/**
 * \ingroup DicomDir
 * \brief   Set the progress method to call when the parsing of the directory progress
 * @param   method Method to call
 * @param   arg    Argument to pass to the method
 * @param   argDelete    Argument  
 * \warning In python : the arg parameter isn't considered
 */
void DicomDir::SetProgressMethod(Method* method, void* arg, 
                                 Method* argDelete )
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
 * \ingroup DicomDir
 * \brief   Set the method to delete the argument
 *          The argument is destroyed when the method is changed or when the 
 *          class is destroyed          
 * @param   method Method to call to delete the argument
 */
void DicomDir::SetProgressMethodArgDelete(Method* method)
{
   ProgressMethodArgDelete = method;
}

/**
 * \ingroup DicomDir
 * \brief   Set the end method to call when the parsing of the directory ends
 * @param   method Method to call
 * @param   arg    Argument to pass to the method
 * @param   argDelete    Argument 
 * \warning In python : the arg parameter isn't considered
 */
void DicomDir::SetEndMethod(Method* method, void* arg, 
                            Method* argDelete )
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
 * \ingroup DicomDir
 * \brief   Set the method to delete the argument
 *          The argument is destroyed when the method is changed or when the class
 *          is destroyed
 * @param   method Method to call to delete the argument
 */
void DicomDir::SetEndMethodArgDelete(Method* method)
{
   EndMethodArgDelete = method;
}

/**
 * \ingroup DicomDir
 * \brief   writes on disc a DICOMDIR
 * \ warning does NOT add the missing elements in the header :
 *           it's up to the user doing it !
 * \todo : to be re-written using the DICOMDIR tree-like structure
 *         *not* the chained list
 *         (does NOT exist if the DICOMDIR is user-forged !)
 * @param  fileName file to be written to 
 * @return false only when fail to open
 */
 
bool DicomDir::WriteDicomDir(std::string const& fileName) 
{  
   int i;
   uint16_t sq[4] = { 0x0004, 0x1220, 0xffff, 0xffff };
   uint16_t sqt[4]= { 0xfffe, 0xe0dd, 0xffff, 0xffff };

   std::ofstream* fp = new std::ofstream(fileName.c_str(),  
                                         std::ios::out | std::ios::binary);
   if( !fp ) 
   {
      dbg.Verbose(2, "Failed to open(write) File: ", fileName.c_str());
      return false;
   }

   char filePreamble[128];
   memset(filePreamble, 0, 128);
   fp->write(filePreamble, 128); //FIXME
   binary_write( *fp, "DICM");
 
   DicomDirMeta *ptrMeta = GetDicomDirMeta();
   ptrMeta->Write(fp, ExplicitVR);
   
   // force writing 0004|1220 [SQ ], that CANNOT exist within DicomDirMeta
   for(i=0;i<4;++i)
   {
      binary_write(*fp, sq[i]);
   }
        
   for(ListDicomDirPatient::iterator cc  = Patients.begin();
                                     cc != Patients.end();
                                   ++cc )
   {
      (*cc)->Write( fp, ExplicitVR );
   }
   
   // force writing Sequence Delimitation Item
   for(i=0;i<4;++i)
   {
      binary_write(*fp, sqt[i]);  // fffe e0dd ffff ffff 
   }

   fp->close();
   return true;
}

//-----------------------------------------------------------------------------
// Protected

/**
 * \ingroup DicomDir
 * \brief create a Document-like chained list from a root Directory 
 * @param path entry point of the tree-like structure
 */
void DicomDir::CreateDicomDirChainedList(std::string const & path)
{
   CallStartMethod();
   DirList fileList(path,1); // gets recursively the file list
   unsigned int count = 0;
   VectDocument list;
   Header *header;

   TagHT.clear();
   Patients.clear();

   for( DirList::iterator it  = fileList.begin();
                              it != fileList.end();
                              ++it )
   {
      Progress = (float)(count+1)/(float)fileList.size();
      CallProgressMethod();
      if( Abort )
      {
         break;
      }

      header = new Header( it->c_str() );
      if( !header )
      {
         dbg.Verbose( 1,
                      "DicomDir::CreateDicomDirChainedList: "
                      "failure in new Header ",
                      it->c_str() );
      }
      
      if( header->IsReadable() )
      {
         // Add the file header to the chained list:
         list.push_back(header);
         dbg.Verbose( 1,
                      "DicomDir::CreateDicomDirChainedList: readable ",
                      it->c_str() );

       }
       else
       {
          delete header;
       }
       count++;
   }
   // sorts Patient/Study/Serie/
   std::sort(list.begin(), list.end(), DicomDir::HeaderLessThan );
   
   std::string tmp = fileList.GetDirName();      
   //for each Header of the chained list, add/update the Patient/Study/Serie/Image info
   SetElements(tmp, list);
   CallEndMethod();
}

/**
 * \ingroup DicomDir
 * \brief   adds *the* Meta to a partially created DICOMDIR
 */
  
DicomDirMeta * DicomDir::NewMeta()
{
   DicomDirMeta *m = new DicomDirMeta( &TagHT );
  
   if ( TagHT.begin() != TagHT.end() ) // after Document Parsing
   { 
      TagDocEntryHT::iterator lastOneButSequence = TagHT.end();
      lastOneButSequence --;
      // ALL the 'out of Sequence' Tags belong to Meta Elems
      // (we skip 0004|1220 [Directory record sequence] )
      for ( TagDocEntryHT::iterator cc  = TagHT.begin(); 
                                    cc != lastOneButSequence;
                                   ++cc)
      {
         m->AddDocEntry( cc->second );
      }
   }
   else  // after root directory parsing
   {
      ListDicomDirMetaElem const & elemList = 
         Global::GetDicomDirElements()->GetDicomDirMetaElements();
      m->FillObject(elemList);
   }
   m->SetSQItemNumber(0); // To avoid further missprinting
   return m;  
}

/**
 * \brief   adds a new Patient (with the basic elements) to a partially created DICOMDIR
 */
DicomDirPatient * DicomDir::NewPatient()
{
   ListDicomDirPatientElem::const_iterator it;
   uint16_t tmpGr,tmpEl;
   DictEntry *dictEntry;
   ValEntry *entry;

   ListDicomDirPatientElem const & elemList =
      Global::GetDicomDirElements()->GetDicomDirPatientElements(); 
   SQItem *s = new SQItem(0);

   // for all the DicomDirPatient Elements      
   for( it = elemList.begin(); it != elemList.end(); ++it ) 
   {
      tmpGr     = it->Group;
      tmpEl     = it->Elem;
      dictEntry = GetPubDict()->GetDictEntryByNumber(tmpGr, tmpEl);
      entry     = new ValEntry( dictEntry );
      entry->SetOffset(0); // just to avoid further missprinting
      entry->SetValue( it->Value );

      // dealing with value length ...
      
      if( dictEntry->GetGroup() == 0xfffe)
      {
         entry->SetLength(entry->GetValue().length());
      }
      else if( dictEntry->GetVR() == "UL" || dictEntry->GetVR() == "SL" )
      {
         entry->SetLength( 4 );
      } 
      else if( dictEntry->GetVR() == "US" || dictEntry->GetVR() == "SS" )
      {
         entry->SetLength(2); 
      } 
      else if( dictEntry->GetVR() == "SQ" )
      {
         entry->SetLength( 0xffffffff );
      }
      else
      {
         entry->SetLength( entry->GetValue().length() );
      }
      s->AddDocEntry( entry );
   }

   DicomDirPatient *p = new DicomDirPatient(s, &TagHT);
   Patients.push_front( p );

   return p;   
}

/**
 * \brief   adds to the HTable 
 *          the Entries (Dicom Elements) corresponding to the given type
 * @param   path full path file name (only used when type = GDCM_DICOMDIR_IMAGE
 * @param   type DicomDirObject type to create (GDCM_DICOMDIR_PATIENT,
 *          GDCM_DICOMDIR_STUDY, GDCM_DICOMDIR_SERIE ...)
 * @param   header Header of the current file
 */
void DicomDir::SetElement(std::string const & path, DicomDirType type,
                          Document *header)
{
   ListDicomDirElem elemList; //FIXME this is going to be a by copy operation
   ListDicomDirElem::const_iterator it;
   uint16_t tmpGr, tmpEl;
   DictEntry *dictEntry;
   ValEntry *entry;
   std::string val;
   SQItem *si = new SQItem(0); // all the items will be at level 1
   switch( type )
   {
      case GDCM_DICOMDIR_IMAGE:
         elemList = Global::GetDicomDirElements()->GetDicomDirImageElements();
         break;

      case GDCM_DICOMDIR_SERIE:
         elemList = Global::GetDicomDirElements()->GetDicomDirSerieElements();
         break;

      case GDCM_DICOMDIR_STUDY:
         elemList = Global::GetDicomDirElements()->GetDicomDirStudyElements();
         break;

      case GDCM_DICOMDIR_PATIENT:
         elemList = Global::GetDicomDirElements()->GetDicomDirPatientElements();
         break;
  
      case GDCM_DICOMDIR_META:
         elemList = Global::GetDicomDirElements()->GetDicomDirMetaElements();
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
   // for all the relevant elements found in their own spot of the DicomDir.dic
   // FIXME : troubles found when it's a SeqEntry

   for( it = elemList.begin(); it != elemList.end(); ++it)
   {
      tmpGr     = it->Group;
      tmpEl     = it->Elem;
      dictEntry = GetPubDict()->GetDictEntryByNumber(tmpGr, tmpEl);

      entry     = new ValEntry( dictEntry ); // Be sure it's never a BinEntry !

      entry->SetOffset(0); // just to avoid further missprinting
      entry->SetLength(0); // just to avoid further missprinting

      if( header )
      {
         // NULL when we Build Up (ex nihilo) a DICOMDIR
         //   or when we add the META elems
         val = header->GetEntryByNumber(tmpGr, tmpEl);
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
               dbg.Verbose(0, "DicomDir::SetElement : the base path"
                              " of file name is incorrect");
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
         if ( header->GetEntryLengthByNumber(tmpGr,tmpEl) == 0 )
            val = it->Value;
      }

     // GDCM_UNFOUND or not !

      entry->SetValue( val ); // troubles expected when vr=SQ ...

      if( dictEntry )
      {
         if( dictEntry->GetGroup() == 0xfffe )
         {
            entry->SetLength( entry->GetValue().length() ); // FIXME 
         }
         else if( dictEntry->GetVR() == "UL" || dictEntry->GetVR() == "SL" )
         {
            entry->SetLength(4);
         }
         else if( dictEntry->GetVR() == "US" || dictEntry->GetVR() == "SS" )
         {
            entry->SetLength(2); 
         }
         else if( dictEntry->GetVR() == "SQ" )
         {
            entry->SetLength( 0xffffffff );
         }
         else
         {
            entry->SetLength( entry->GetValue().length() );
         }
      }

      if ( type == GDCM_DICOMDIR_META ) // fusible : should never print !
      {
         std::cout << "GDCM_DICOMDIR_META ?!? should never print that" 
                   << std::endl;
      }
      si->AddEntry(entry);
   }
   switch( type )
   {
      case GDCM_DICOMDIR_IMAGE:
         AddDicomDirImageToEnd(si);
         break;

      case GDCM_DICOMDIR_SERIE:
         AddDicomDirSerieToEnd(si);
         break;

      case GDCM_DICOMDIR_STUDY:
         AddDicomDirStudyToEnd(si);
         break;

      case GDCM_DICOMDIR_PATIENT:
         AddDicomDirPatientToEnd(si);
         break;

      default:
         return;
   }
   //int count=1;            // find a trick to increment
   //s->AddEntry(si, count); // Seg Faults 

}

//-----------------------------------------------------------------------------
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

//-----------------------------------------------------------------------------
/**
 * \ingroup DicomDir
 * \brief   CallProgressMethod
 */
void DicomDir::CallProgressMethod()
{
   if( ProgressMethod )
   {
      ProgressMethod( ProgressArg );
   }
}

//-----------------------------------------------------------------------------
/**
 * \ingroup DicomDir
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
 * \ingroup DicomDir
 * \brief create a 'DicomDir' from a DICOMDIR Header 
 */
void DicomDir::CreateDicomDir()
{
   // The list is parsed. 
   //  When a DicomDir tag ("PATIENT", "STUDY", "SERIE", "IMAGE") is found :
   //  1 - we save the beginning iterator
   //  2 - we continue to parse
   //  3 - we find an other tag
   //       + we create the object for the precedent tag
   //       + loop to 1 -

   // Directory record sequence
   DocEntry *e = GetDocEntryByNumber(0x0004, 0x1220);
   if ( !e )
   {
      dbg.Verbose(0, "DicomDir::DicomDir : NO Directory record"
                     " sequence (0x0004,0x1220)");
      /// \todo FIXME: what to do when the parsed file IS NOT a DICOMDIR file ? 
      return;         
   }
   
   SeqEntry* s = dynamic_cast<SeqEntry*>(e);
   if ( !s )
   {
      dbg.Verbose(0, "DicomDir::CreateDicomDir: no SeqEntry present");
      // useless : (0x0004,0x1220) IS a Sequence !
      return;
   }

   DicomDirType type = DicomDir::GDCM_DICOMDIR_META;
   MetaElems = NewMeta();

   ListSQItem listItems = s->GetSQItems();
   
   DocEntry * d;
   std::string v;
   for( ListSQItem::iterator i = listItems.begin(); 
                             i !=listItems.end(); ++i ) 
   {
      d = (*i)->GetDocEntryByNumber(0x0004, 0x1430); // Directory Record Type
      if ( ValEntry* valEntry = dynamic_cast< ValEntry* >(d) )
      {
         v = valEntry->GetValue();
      }
      else
      {
         dbg.Verbose(0, "DicomDir::CreateDicomDir: not a ValEntry.");
         continue;
      }

      if( v == "PATIENT " )
      {
         AddDicomDirPatientToEnd( *i );
         type = DicomDir::GDCM_DICOMDIR_PATIENT;
      }
      else if( v == "STUDY " )
      {
         AddDicomDirStudyToEnd( *i );
         type = DicomDir::GDCM_DICOMDIR_STUDY;
      }
      else if( v == "SERIES" )
      {
         AddDicomDirSerieToEnd( *i );
         type = DicomDir::GDCM_DICOMDIR_SERIE;
      }
      else if( v == "IMAGE " ) 
      {
         AddDicomDirImageToEnd( *i );
         type = DicomDir::GDCM_DICOMDIR_IMAGE;
      }
      else
      {
         // It was not a 'PATIENT', nor a 'STUDY', nor a 'SERIE',
         // neither an 'IMAGE' SQItem. Skip to next item.
         continue;
      }
   }
}

/**
 * \ingroup DicomDir
 * \brief Well ... there is only one occurence  
 */
void DicomDir::AddDicomDirMeta()
{
   if( MetaElems )
   {
      delete MetaElems;
   }
   MetaElems = new DicomDirMeta( &TagHT );
}

/**
 * \ingroup DicomDir
 * \brief  AddDicomDirPatientToEnd 
 * @param   s SQ Item to enqueue to the DicomPatient chained List
 */
void DicomDir::AddDicomDirPatientToEnd(SQItem *s)
{
   Patients.push_back(new DicomDirPatient(s, &TagHT));
}

/**
 * \ingroup DicomDir
 * \brief  AddDicomDirStudyToEnd 
 * @param   s SQ Item to enqueue to the DicomDirStudy chained List
 */
 void DicomDir::AddDicomDirStudyToEnd(SQItem *s)
{
   if( Patients.size() > 0 )
   {
      ListDicomDirPatient::iterator itp = Patients.end();
      itp--;
      (*itp)->AddDicomDirStudy(new DicomDirStudy(s, &TagHT));
   }
}

/**
 * \ingroup DicomDir
 * \brief  AddDicomDirSerieToEnd 
 * @param   s SQ Item to enqueue to the DicomDirSerie chained List
 */
void DicomDir::AddDicomDirSerieToEnd(SQItem *s)
{
   if( Patients.size() > 0 )
   {
      ListDicomDirPatient::iterator itp = Patients.end();
      itp--;

      if( (*itp)->GetDicomDirStudies().size() > 0 )
      {
         ListDicomDirStudy::const_iterator itst = 
            (*itp)->GetDicomDirStudies().end();
         itst--;
         (*itst)->AddDicomDirSerie(new DicomDirSerie(s, &TagHT));
      }
   }
}

/**
 * \ingroup DicomDir
 * \brief   AddDicomDirImageToEnd
 * @param   s SQ Item to enqueue to the DicomDirImage chained List
 */
 void DicomDir::AddDicomDirImageToEnd(SQItem *s)
{
   if( Patients.size() > 0 )
   {
      ListDicomDirPatient::iterator itp = Patients.end();
      itp--;

      if( (*itp)->GetDicomDirStudies().size() > 0 )
      {
         ListDicomDirStudy::const_iterator itst = 
            (*itp)->GetDicomDirStudies().end();
         itst--;

         if( (*itst)->GetDicomDirSeries().size() > 0 )
         {
            ListDicomDirSerie::const_iterator its = (*itst)->GetDicomDirSeries().end();
            its--;
            (*its)->AddDicomDirImage(new DicomDirImage(s, &TagHT));
         }
      }
   }
}

/**
 * \ingroup DicomDir
 * \brief  for each Header of the chained list, add/update the Patient/Study/Serie/Image info 
 * @param   path path of the root directory
 * @param   list chained list of Headers
 */
void DicomDir::SetElements(std::string const & path, VectDocument const &list)
{
   std::string patPrevName         = "", patPrevID  = "";
   std::string studPrevInstanceUID = "", studPrevID = "";
   std::string serPrevInstanceUID  = "", serPrevID  = "";

   std::string patCurName,         patCurID;
   std::string studCurInstanceUID, studCurID;
   std::string serCurInstanceUID,  serCurID;

   for( VectDocument::const_iterator it = list.begin();
                                     it != list.end(); ++it )
   {
      // get the current file characteristics
      patCurName         = (*it)->GetEntryByNumber(0x0010,0x0010); 
      patCurID           = (*it)->GetEntryByNumber(0x0010,0x0011); 
      studCurInstanceUID = (*it)->GetEntryByNumber(0x0020,0x000d);            
      studCurID          = (*it)->GetEntryByNumber(0x0020,0x0010);            
      serCurInstanceUID  = (*it)->GetEntryByNumber(0x0020,0x000e);            
      serCurID           = (*it)->GetEntryByNumber(0x0020,0x0011);

      if( patCurName != patPrevName || patCurID != patPrevID)
      {
         SetElement(path, GDCM_DICOMDIR_PATIENT, *it);
      }

      // if new Study Deal with 'STUDY' Elements   
      if( studCurInstanceUID != studPrevInstanceUID || studCurID != studPrevID )
      {
         SetElement(path, GDCM_DICOMDIR_STUDY, *it);
      }

      // if new Serie Deal with 'SERIE' Elements   
      if( serCurInstanceUID != serPrevInstanceUID || serCurID != serPrevID )
      {
         SetElement(path, GDCM_DICOMDIR_SERIE, *it);
      }
      
      // Always Deal with 'IMAGE' Elements  
      SetElement(path, GDCM_DICOMDIR_IMAGE, *it);

      patPrevName         = patCurName;
      patPrevID           = patCurID;
      studPrevInstanceUID = studCurInstanceUID;
      studPrevID          = studCurID;
      serPrevInstanceUID  = serCurInstanceUID;
      serPrevID           = serCurID;
   }
}

/**
 * \ingroup DicomDir
 * \brief   compares two dgcmHeaders
 */
bool DicomDir::HeaderLessThan(Document *header1, Document *header2)
{
   return *header1 < *header2;
}
} // end namespace gdcm

//-----------------------------------------------------------------------------
