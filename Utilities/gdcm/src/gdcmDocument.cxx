/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDocument.cxx
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

#include "gdcmDocument.h"
#include "gdcmValEntry.h"
#include "gdcmBinEntry.h"
#include "gdcmSeqEntry.h"
#include "gdcmGlobal.h"
#include "gdcmUtil.h"
#include "gdcmDebug.h"
#include "gdcmTS.h"
#include "gdcmDictSet.h"
#include "gdcmDocEntrySet.h"
#include "gdcmSQItem.h"

#include <vector>
#include <iomanip>
#include <fstream>
#include <ctype.h>  // for isdigit
#include <stdlib.h> // for atoi

#if defined(__BORLANDC__)
   #include <mem.h> // for memset
#endif 

namespace gdcm 
{
//-----------------------------------------------------------------------------

// Refer to Document::SetMaxSizeLoadEntry()
const unsigned int Document::MAX_SIZE_LOAD_ELEMENT_VALUE = 0xfff; // 4096

//-----------------------------------------------------------------------------
// Constructor / Destructor
// Constructors and destructors are protected to avoid user to invoke directly

/**
 * \brief This default constructor neither loads nor parses the file. 
 *        You should then invoke \ref Document::Load.
 *         
 */
Document::Document() 
         :ElementSet(-1)
{
   Fp = 0;

   SetMaxSizeLoadEntry(MAX_SIZE_LOAD_ELEMENT_VALUE);
   Initialize();
   SwapCode = 1234;
   Filetype = ExplicitVR;
   // Load will set it to true if sucessfull
   Group0002Parsed = false;
   IsDocumentAlreadyLoaded = false;
   IsDocumentModified = true;
   LoadMode = LD_ALL; // default : load everything, later
   SetFileName("");
}

#ifndef GDCM_LEGACY_REMOVE
/**
 * \brief   Constructor (DEPRECATED : not to break the API) 
 * @param   fileName 'Document' (File or DicomDir) to be open for parsing
 */
Document::Document( std::string const &fileName )
         :ElementSet(-1) 
{
   Fp = 0;

   SetMaxSizeLoadEntry(MAX_SIZE_LOAD_ELEMENT_VALUE);
   Initialize();
   SwapCode = 1234;
   Filetype = ExplicitVR;
   Group0002Parsed = false;
   LoadMode = LD_ALL; // Load everything, later

   // Load will set it to true if sucessfull
   IsDocumentAlreadyLoaded = false;
   IsDocumentModified = true;

   SetFileName(fileName);
   Load( );
}
#endif
/**
 * \brief   Canonical destructor.
 */
Document::~Document ()
{
   CloseFile();
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   Loader. use SetLoadMode(), SetFileName() before ! 
 * @return false if file cannot be open or no swap info was found,
 *         or no tag was found.
 */
bool Document::Load(  ) 
{
   if ( GetFileName() == "" )
   {
      gdcmWarningMacro( "Use SetFileName, before !" );
      return false;
   }
   return DoTheLoadingDocumentJob( );
} 

#ifndef GDCM_LEGACY_REMOVE
/**
 * \brief   Loader. (DEPRECATED : not to break the API)   
 * @param   fileName 'Document' (File or DicomDir) to be open for parsing
 * @return false if file cannot be open or no swap info was found,
 *         or no tag was found.
 */
bool Document::Load( std::string const &fileName ) 
{
   Filename = fileName;
   return DoTheLoadingDocumentJob( );
}
#endif

/**
 * \brief   Performs the Loading Job (internal use only)  
 * @return false if file cannot be open or no swap info was found,
 *         or no tag was found.
 */
bool Document::DoTheLoadingDocumentJob(  ) 
{
   if ( ! IsDocumentModified ) // Nothing to do !
      return true;

   ClearEntry();

   Fp = 0;
   if ( !OpenFile() )
   {
      // warning already performed in OpenFile()
      Filetype = Unknown;
      return false;
   }

   Group0002Parsed = false;

   gdcmDebugMacro( "Starting parsing of file: " << Filename.c_str());

   // Computes the total length of the file
   Fp->seekg(0, std::ios::end);  // Once per Document !
   long lgt = Fp->tellg();       // Once per Document !   
   Fp->seekg(0, std::ios::beg);  // Once per Document !

   // CheckSwap returns a boolean 
   // (false if no swap info of any kind was found)
   if (! CheckSwap() )
   {
      gdcmWarningMacro( "Neither a DICOM V3 nor an ACR-NEMA file: " 
                   << Filename.c_str());
      CloseFile(); 
      return false;      
    }

   long beg = Fp->tellg();      // just after DICOM preamble (if any)

   lgt -= beg;                  // remaining length to parse    

   // Recursive call.
   // Loading is done during parsing
   ParseDES( this, beg, lgt, false); // delim_mode is first defaulted to false

   if ( IsEmpty() )
   { 
      gdcmErrorMacro( "No tag in internal hash table for: "
                        << Filename.c_str());
      CloseFile(); 
      return false;
   }
   IsDocumentAlreadyLoaded = true;

   //Fp->seekg(0, std::ios::beg);  // Once per Document!
   
   // Load 'non string' values
      
   std::string PhotometricInterpretation = GetEntryValue(0x0028,0x0004);   
   if ( PhotometricInterpretation == "PALETTE COLOR " )
   {
   // FIXME
   // Probabely this line should be outside the 'if'
   // Try to find an image sample holding a 'gray LUT'
      LoadEntryBinArea(0x0028,0x1200);  // gray LUT
   
      /// FIXME
      /// The tags refered by the three following lines used to be CORRECTLY
      /// defined as having an US Value Representation in the public
      /// dictionary. BUT the semantics implied by the three following
      /// lines state that the corresponding tag contents are in fact
      /// the ones of a BinEntry.
      /// In order to fix things "Quick and Dirty" the dictionary was
      /// altered on PURPOSE but now contains a WRONG value.
      /// In order to fix things and restore the dictionary to its
      /// correct value, one needs to decide of the semantics by deciding
      /// whether the following tags are either :
      /// - multivaluated US, and hence loaded as ValEntry, but afterwards
      ///   also used as BinEntry, which requires the proper conversion,
      /// - OW, and hence loaded as BinEntry, but afterwards also used
      ///   as ValEntry, which requires the proper conversion.
      // --> OB (byte aray) or OW (short int aray)
      // The actual VR has to be deduced from other entries.
      // Our way of loading them may fail in some cases :
      // We must or not SwapByte depending on other field values.
             
      LoadEntryBinArea(0x0028,0x1201);  // R    LUT
      LoadEntryBinArea(0x0028,0x1202);  // G    LUT
      LoadEntryBinArea(0x0028,0x1203);  // B    LUT
      
      // Segmented Red   Palette Color LUT Data
      LoadEntryBinArea(0x0028,0x1221);
      // Segmented Green Palette Color LUT Data
      LoadEntryBinArea(0x0028,0x1222);
      // Segmented Blue  Palette Color LUT Data
      LoadEntryBinArea(0x0028,0x1223);
   }
 
   //FIXME later : how to use it?
   SeqEntry *modLutSeq = GetSeqEntry(0x0028,0x3000); // Modality LUT Sequence
   if ( modLutSeq !=0 )
   {
      SQItem *sqi= modLutSeq->GetFirstSQItem();
      if ( sqi != 0 )
      {
         BinEntry *b = sqi->GetBinEntry(0x0028,0x3006);
         if ( b != 0 )
         {
            if ( b->GetLength() != 0 )
            {
               // FIXME : CTX dependent means : contexted dependant.
               //         see upper comment.
               LoadEntryBinArea(b);    //LUT Data (CTX dependent)
            }   
        }
     }      
   }

   // Force Loading some more elements if user asked to.

   gdcm::DocEntry *d;
   for (ListElements::iterator it = UserForceLoadList.begin();  
                               it != UserForceLoadList.end();
                             ++it)
   {
      gdcmDebugMacro( "Force Load " << std::hex 
                       << (*it).Group << "|" <<(*it).Elem );
  
      d = GetDocEntry( (*it).Group, (*it).Elem);
  
      if ( d == NULL)
      {
         gdcmWarningMacro( "You asked to ForceLoad "  << std::hex
                          << (*it).Group <<"|"<< (*it).Elem
                          << " that doesn't exist" );
         continue;
      }

      if ( dynamic_cast<ValEntry *>(d) )
      {
         LoadDocEntry(d, true);
         continue;
      }

      BinEntry *b = dynamic_cast<BinEntry *>(d);
      if ( b )
      {
         LoadEntryBinArea(b);
         b->SetValue(GDCM_BINLOADED);
         continue;
      }
 
      if ( dynamic_cast<SeqEntry *>(d) )
      {
         gdcmWarningMacro( "You cannot 'ForceLoad' a SeqEntry :" << std::hex
                           << (*it).Group <<"|"<< (*it).Elem );
         continue;
      }
   }

   CloseFile(); 
  
   // ----------------------------
   // Specific code to allow gdcm to read ACR-LibIDO formated images
   // Note: ACR-LibIDO is an extension of the ACR standard that was
   //       used at CREATIS. For the time being (say a couple of years)
   //       we keep this kludge to allow CREATIS users 
   //       reading their old images.
   //
   // if recognition code tells us we deal with a LibIDO image
   // we switch lineNumber and columnNumber
   //
   std::string RecCode;
   RecCode = GetEntryValue(0x0008, 0x0010); // recognition code (RET)
   if (RecCode == "ACRNEMA_LIBIDO_1.1" ||
       RecCode == "CANRME_AILIBOD1_1." )  // for brain-damaged softwares
                                          // with "little-endian strings"
   {
         Filetype = ACR_LIBIDO; 
         std::string rows    = GetEntryValue(0x0028, 0x0010);
         std::string columns = GetEntryValue(0x0028, 0x0011);
         SetValEntry(columns, 0x0028, 0x0010);
         SetValEntry(rows   , 0x0028, 0x0011);
   }
   // --- End of ACR-LibIDO kludge --- 
   return true;
}


/**
 * \brief Adds a new element we want to load anyway
 * @param   group  Group number of the target tag.
 * @param   elem Element number of the target tag.
 */
void Document::AddForceLoadElement (uint16_t group, uint16_t elem) 
{ 
   Element el;
   el.Group = group;
   el.Elem  = elem;
   UserForceLoadList.push_back(el); 
}
/**
 * \brief   Get the public dictionary used
 */
Dict *Document::GetPubDict()
{
   return RefPubDict;
}

/**
 * \brief   Get the shadow dictionary used
 */
Dict *Document::GetShaDict()
{
   return RefShaDict;
}

/**
 * \brief   Set the shadow dictionary used
 * @param   dict dictionary to use in shadow
 */
bool Document::SetShaDict(Dict *dict)
{
   RefShaDict = dict;
   return !RefShaDict;
}

/**
 * \brief   Set the shadow dictionary used
 * @param   dictName name of the dictionary to use in shadow
 */
bool Document::SetShaDict(DictKey const &dictName)
{
   RefShaDict = Global::GetDicts()->GetDict(dictName);
   return !RefShaDict;
}

/**
 * \brief  This predicate tells us whether or not the current Document 
 *         was properly parsed and contains at least *one* Dicom Element
 *         (and nothing more, sorry).
 * @return false when we're 150 % sure it's NOT a Dicom/Acr file,
 *         true otherwise. 
 */
bool Document::IsParsable()
{
   if ( Filetype == Unknown )
   {
      gdcmWarningMacro( "Wrong filetype for " << GetFileName());
      return false;
   }

   if ( IsEmpty() )
   { 
      gdcmWarningMacro( "No tag in internal hash table.");
      return false;
   }

   return true;
}
/**
 * \brief  This predicate tells us whether or not the current Document 
 *         was properly parsed and contains at least *one* Dicom Element
 *         (and nothing more, sorry).
 * @return false when we're 150 % sure it's NOT a Dicom/Acr file,
 *         true otherwise. 
 */
bool Document::IsReadable()
{
   return IsParsable();
}


/**
 * \brief   Predicate for dicom version 3 file.
 * @return  True when the file is a dicom version 3.
 */
bool Document::IsDicomV3()
{
   // Checking if Transfer Syntax exists is enough
   // Anyway, it's too late check if the 'Preamble' was found ...
   // And ... would it be a rich idea to check ?
   // (some 'no Preamble' DICOM images exist !)
   return GetDocEntry(0x0002, 0x0010) != NULL;
}

/**
 * \brief   Predicate for Papyrus file
 *          Dedicated to whomsoever it may concern
 * @return  True when the file is a Papyrus file.
 */
bool Document::IsPapyrus()
{
   // check for Papyrus private Sequence
   DocEntry *e = GetDocEntry(0x0041, 0x1050);
   if ( !e )
      return false;
   // check if it's actually a Sequence
   if ( !dynamic_cast<SeqEntry*>(e) )
      return  false;
   return true;
}

/**
 * \brief  returns the File Type 
 *         (ACR, ACR_LIBIDO, ExplicitVR, ImplicitVR, Unknown)
 * @return the FileType code
 */
FileType Document::GetFileType()
{
   return Filetype;
}

/**
 * \brief   Accessor to the Transfer Syntax (when present) of the
 *          current document (it internally handles reading the
 *          value from disk when only parsing occured).
 * @return  The encountered Transfer Syntax of the current document, if DICOM.
 *          GDCM_UNKNOWN for ACR-NEMA files (or broken headers ...)
 */
std::string Document::GetTransferSyntax()
{
   DocEntry *entry = GetDocEntry(0x0002, 0x0010);
   if ( !entry )
   {
      return GDCM_UNKNOWN;
   }

   // The entry might be present but not loaded (parsing and loading
   // happen at different stages): try loading and proceed with check...
   LoadDocEntrySafe(entry);
   if (ValEntry *valEntry = dynamic_cast< ValEntry* >(entry) )
   {
      std::string transfer = valEntry->GetValue();
      // The actual transfer (as read from disk) might be padded. We
      // first need to remove the potential padding. We can make the
      // weak assumption that padding was not executed with digits...
      if  ( transfer.length() == 0 )
      {
         // for brain damaged headers
         return GDCM_UNKNOWN;
      }
      while ( !isdigit((unsigned char)transfer[transfer.length()-1]) )
      {
         transfer.erase(transfer.length()-1, 1);
         if  ( transfer.length() == 0 )
         {
            // for brain damaged headers
            gdcmWarningMacro( "Transfer Syntax contains no valid character.");
            return GDCM_UNKNOWN;
         }
      }
      return transfer;
   }
   return GDCM_UNKNOWN;
}

/**
 * \brief Accesses the info from 0002,0010 : Transfer Syntax and TS
 * @return The full Transfer Syntax Name (as opposed to Transfer Syntax UID)
 */
std::string Document::GetTransferSyntaxName()
{
   // use the TS (TS : Transfer Syntax)
   std::string transferSyntax = GetEntryValue(0x0002,0x0010);

   if ( (transferSyntax.find(GDCM_NOTLOADED) < transferSyntax.length()) )
   {
      gdcmErrorMacro( "Transfer Syntax not loaded. " << std::endl
               << "Better you increase MAX_SIZE_LOAD_ELEMENT_VALUE" );
      return "Uncompressed ACR-NEMA";
   }
   if ( transferSyntax == GDCM_UNFOUND )
   {
      gdcmDebugMacro( "Unfound Transfer Syntax (0002,0010)");
      return "Uncompressed ACR-NEMA";
   }

   // we do it only when we need it
   const TSKey &tsName = Global::GetTS()->GetValue( transferSyntax );

   // Global::GetTS() is a global static you shall never try to delete it!
   return tsName;
}
//
// --------------- Swap Code ------------------
/**
 * \brief   Swaps the bytes so they agree with the processor order
 * @return  The properly swaped 16 bits integer.
 */
uint16_t Document::SwapShort(uint16_t a)
{
   if ( SwapCode == 4321 || SwapCode == 2143 )
   {
      //a = ((( a << 8 ) & 0xff00 ) | (( a >> 8 ) & 0x00ff ) );
      // Save CPU time
      a = ( a << 8 ) | ( a >> 8 );
   }
   return a;
}

/**
 * \brief   Swaps back the bytes of 4-byte long integer accordingly to
 *          processor order.
 * @return  The properly swaped 32 bits integer.
 */
uint32_t Document::SwapLong(uint32_t a)
{
   switch (SwapCode)
   {
      case 1234 :
         break;
      case 4321 :
//         a=( ((a<<24) & 0xff000000) | ((a<<8)  & 0x00ff0000) | 
//             ((a>>8)  & 0x0000ff00) | ((a>>24) & 0x000000ff) );
// save CPU time
         a=( ( a<<24)               | ((a<<8)  & 0x00ff0000) | 
             ((a>>8)  & 0x0000ff00) |  (a>>24)                );
         break;   
      case 3412 :
//       a=( ((a<<16) & 0xffff0000) | ((a>>16) & 0x0000ffff) );
         a=( (a<<16)                | (a>>16)  );
         break;  
      case 2143 :
         a=( ((a<< 8) & 0xff00ff00) | ((a>>8) & 0x00ff00ff)  );
      break;
      default :
         gdcmErrorMacro( "Unexpected swap code:" << SwapCode );
         a = 0;
   }
   return a;
} 

//
// -----------------File I/O ---------------
/**
 * \brief  Tries to open the file \ref Document::Filename and
 *         checks the preamble when existing.
 * @return The FILE pointer on success. 
 */
std::ifstream *Document::OpenFile()
{
   HasDCMPreamble = false;
   if (Filename.length() == 0) 
   {
      return 0;
   }

   if ( Fp )
   {
      gdcmDebugMacro( "File already open: " << Filename.c_str());
      CloseFile();
   }

   Fp = new std::ifstream(Filename.c_str(), std::ios::in | std::ios::binary);
   if ( ! *Fp )
   {
   // Don't user gdcmErrorMacro :
   // a spurious message will appear when you use, for instance 
   // gdcm::FileHelper *fh = new gdcm::FileHelper( outputFileName );
   // to create outputFileName.
   
   // FIXME : if the upper comment is still usefull 
   //         --> the constructor is not so good ...
   
      gdcmWarningMacro( "Cannot open file: " << Filename.c_str());
      delete Fp;
      Fp = 0;
      return 0;
      //exit(1); // No function is allowed to leave the application instead
                 // of warning the caller
   }
 
   uint16_t zero = 0;
   Fp->read((char*)&zero, (size_t)2);
   if ( Fp->eof() )
   {
      CloseFile();
      return 0;
   }
 
   //-- Broken ACR or DICOM with no Preamble; may start with a Shadow Group --
   // FIXME : We cannot be sure the preable is only zeroes..
   //         (see ACUSON-24-YBR_FULL-RLE.dcm )
   if ( 
       zero == 0x0001 || zero == 0x0100 || zero == 0x0002 || zero == 0x0200 ||
       zero == 0x0003 || zero == 0x0300 || zero == 0x0004 || zero == 0x0400 ||
       zero == 0x0005 || zero == 0x0500 || zero == 0x0006 || zero == 0x0600 ||
       zero == 0x0007 || zero == 0x0700 || zero == 0x0008 || zero == 0x0800 )
   {
      std::string msg = Util::Format(
        "ACR/DICOM starting by 0x(%04x) at the beginning of the file\n", zero);
      // FIXME : is it a Warning message, or a Debug message?
      gdcmWarningMacro( msg.c_str() );
      return Fp;
   }
 
   //-- DICOM --
   Fp->seekg(126L, std::ios::cur);  // Once per Document
   char dicm[4]; // = {' ',' ',' ',' '};
   Fp->read(dicm,  (size_t)4);
   if ( Fp->eof() )
   {
      CloseFile();
      return 0;
   }
   if ( memcmp(dicm, "DICM", 4) == 0 )
   {
      HasDCMPreamble = true;
      return Fp;
   }

   // -- Neither ACR/No Preamble Dicom nor DICOMV3 file
   CloseFile();
   // Don't user Warning nor Error, not to pollute the output
   // while directory recursive parsing ...
   gdcmDebugMacro( "Neither ACR/No Preamble Dicom nor DICOMV3 file: "
                      << Filename.c_str()); 
   return 0;
}

/**
 * \brief closes the file  
 * @return  TRUE if the close was successfull 
 */
bool Document::CloseFile()
{
   if ( Fp )
   {
      Fp->close();
      delete Fp;
      Fp = 0;
   }
   return true;
}

/**
 * \brief Writes in a file all the Entries (Dicom Elements) 
 * @param fp file pointer on an already open file (actually: Output File Stream)
 * @param filetype Type of the File to be written 
 *          (ACR-NEMA, ExplicitVR, ImplicitVR)
 */
void Document::WriteContent(std::ofstream *fp, FileType filetype)
{
   // Skip if user wants to write an ACR-NEMA file

   if ( filetype == ImplicitVR || filetype == ExplicitVR ||
        filetype == JPEG )
   {
      // writing Dicom File Preamble
      char filePreamble[128];
      memset(filePreamble, 0, 128);
      fp->write(filePreamble, 128);
      fp->write("DICM", 4);
   }

   /*
    * \todo rewrite later, if really usefull
    *       - 'Group Length' element is optional in DICOM
    *       - but un-updated odd groups lengthes can causes pb
    *         (xmedcon breaker)
    *
    * if ( (filetype == ImplicitVR) || (filetype == ExplicitVR) )
    *    UpdateGroupLength(false,filetype);
    * if ( filetype == ACR)
    *    UpdateGroupLength(true,ACR);
    *
    * --> Computing group length for groups with embeded Sequences
    * --> was too much tricky / we were [in a hurry / too lazy]
    * --> We don't write the element 0x0000 (group length)
    */

   ElementSet::WriteContent(fp, filetype); // This one is recursive
}

// -----------------------------------------
// Content entries 
/**
 * \brief Loads (from disk) the element content 
 *        when a string is not suitable
 * @param group   group number of the Entry 
 * @param elem  element number of the Entry
 */
void Document::LoadEntryBinArea(uint16_t group, uint16_t elem)
{
   // Search the corresponding DocEntry
   DocEntry *docElement = GetDocEntry(group, elem);
   if ( !docElement )
   {
      gdcmDebugMacro(std::hex << group << "|" << elem 
                       <<  " doesn't exist" );
      return;
   }
   BinEntry *binElement = dynamic_cast<BinEntry *>(docElement);
   if ( !binElement )
   {
      gdcmWarningMacro(std::hex << group << "|" << elem 
                       <<  "is NOT a BinEntry");
      return;
   }
   LoadEntryBinArea(binElement);
}

/**
 * \brief Loads (from disk) the element content 
 *        when a string is not suitable
 * @param elem  Entry whose binArea is going to be loaded
 */
void Document::LoadEntryBinArea(BinEntry *elem) 
{
   if (elem->GetBinArea() )
      return;

   bool openFile = !Fp;
   if ( openFile )
      OpenFile();

   size_t o =(size_t)elem->GetOffset();
   Fp->seekg(o, std::ios::beg);

   size_t l = elem->GetLength();
   uint8_t *a = new uint8_t[l];
   if ( !a )
   {
      gdcmWarningMacro(  "Cannot allocate BinEntry content for : "
                       << std::hex << elem->GetGroup() 
                       << "|" << elem->GetElement() );
      return;
   }

   // Read the data
   Fp->read((char*)a, l);
   if ( Fp->fail() || Fp->eof() )
   {
      delete[] a;
      return;
   }

   elem->SetBinArea(a);

   if ( openFile )
      CloseFile();
}

/**
 * \brief  Loads the element while preserving the current
 *         underlying file position indicator as opposed to
 *        LoadDocEntry that modifies it.
 * @param entry   DocEntry whose value will be loaded. 
 */
void Document::LoadDocEntrySafe(DocEntry *entry)
{
   if ( Fp )
   {
      long PositionOnEntry = Fp->tellg();
      LoadDocEntry(entry);
      Fp->seekg(PositionOnEntry, std::ios::beg);
   }
}

/**
 * \brief   Compares two documents, according to \ref DicomDir rules
 * \warning Does NOT work with ACR-NEMA files
 * \todo    Find a trick to solve the pb (use RET fields ?)
 * @param   document to compare with current one
 * @return  true if 'smaller'
 */
bool Document::operator<(Document &document)
{
   // Patient Name
   std::string s1 = GetEntryValue(0x0010,0x0010);
   std::string s2 = document.GetEntryValue(0x0010,0x0010);
   if (s1 < s2)
   {
      return true;
   }
   else if ( s1 > s2 )
   {
      return false;
   }
   else
   {
      // Patient ID
      s1 = GetEntryValue(0x0010,0x0020);
      s2 = document.GetEntryValue(0x0010,0x0020);
      if ( s1 < s2 )
      {
         return true;
      }
      else if ( s1 > s2 )
      {
         return false;
      }
      else
      {
         // Study Instance UID
         s1 = GetEntryValue(0x0020,0x000d);
         s2 = document.GetEntryValue(0x0020,0x000d);
         if ( s1 < s2 )
         {
            return true;
         }
         else if ( s1 > s2 )
         {
            return false;
         }
         else
         {
            // Serie Instance UID
            s1 = GetEntryValue(0x0020,0x000e);
            s2 = document.GetEntryValue(0x0020,0x000e);    
            if ( s1 < s2 )
            {
               return true;
            }
            else if ( s1 > s2 )
            {
               return false;
            }
         }
      }
   }
   return false;
}

//-----------------------------------------------------------------------------
// Protected
/**
 * \brief Reads a supposed to be 16 Bits integer
 *       (swaps it depending on processor endianness) 
 * @return read value
 */
uint16_t Document::ReadInt16()
   throw( FormatError )
{
   uint16_t g;
   Fp->read ((char*)&g, (size_t)2);
   if ( Fp->fail() )
   {
      throw FormatError( "Document::ReadInt16()", " file error." );
   }
   if ( Fp->eof() )
   {
      throw FormatError( "Document::ReadInt16()", "EOF." );
   }
   g = SwapShort(g); 
   return g;
}

/**
 * \brief  Reads a supposed to be 32 Bits integer
 *        (swaps it depending on processor endianness)  
 * @return read value
 */
uint32_t Document::ReadInt32()
   throw( FormatError )
{
   uint32_t g;
   Fp->read ((char*)&g, (size_t)4);
   if ( Fp->fail() )
   {
      throw FormatError( "Document::ReadInt32()", " file error." );
   }
   if ( Fp->eof() )
   {
      throw FormatError( "Document::ReadInt32()", "EOF." );
   }
   g = SwapLong(g);
   return g;
}

/**
 * \brief skips bytes inside the source file 
 * \warning NOT end user intended method !
 * @return 
 */
void Document::SkipBytes(uint32_t nBytes)
{
   //FIXME don't dump the returned value
   Fp->seekg((long)nBytes, std::ios::cur);
}

/**
 * \brief   Re-computes the length of a ACR-NEMA/Dicom group from a DcmHeader
 */
int Document::ComputeGroup0002Length( /*FileType filetype*/ ) 
{
   uint16_t gr;
   std::string vr;
   
   int groupLength = 0;
   bool found0002 = false;   
  
   // for each zero-level Tag in the DCM Header
   DocEntry *entry = GetFirstEntry();
   while( entry )
   {
      gr = entry->GetGroup();

      if ( gr == 0x0002 )
      {
         found0002 = true;

         if ( entry->GetElement() != 0x0000 )
         {
            vr = entry->GetVR();

            //if ( (vr == "OB")||(vr == "OW")||(vr == "UT")||(vr == "SQ"))
            // (no SQ, OW, UT in group 0x0002;)
               if ( vr == "OB" ) 
               {
                  // explicit VR AND (OB, OW, SQ, UT) : 4 more bytes
                  groupLength +=  4;
               }
            groupLength += 2 + 2 + 4 + entry->GetLength();   
         }
      }
      else if (found0002 )
         break;

      entry = GetNextEntry();
   }
   return groupLength; 
}

//-----------------------------------------------------------------------------
// Private
/**
 * \brief Loads all the needed Dictionaries
 * \warning NOT end user intended method !
 */
void Document::Initialize() 
{
   RefPubDict = Global::GetDicts()->GetDefaultPubDict();
   RefShaDict = NULL;
   Filetype   = Unknown;
}

/**
 * \brief   Parses a DocEntrySet (Zero-level DocEntries or SQ Item DocEntries)
 * @param set DocEntrySet we are going to parse ('zero level' or a SQItem)
 * @param offset start of parsing
 * @param l_max  length to parse (meaningless when we are in 'delimitor mode')
 * @param delim_mode : whether we are in 'delimitor mode' (l=0xffffff) or not
 */ 
void Document::ParseDES(DocEntrySet *set, long offset, 
                        long l_max, bool delim_mode)
{
   DocEntry *newDocEntry;
   ValEntry *newValEntry;
   BinEntry *newBinEntry;
   SeqEntry *newSeqEntry;
   VRKey vr;
   bool used; // will be set to false when something wrong happens to an Entry.
              // (Entry will then be deleted)
   bool delim_mode_intern = delim_mode;
   bool first = true;
   gdcmDebugMacro( "Enter in ParseDES, delim-mode " <<  delim_mode
                     << " at offset " << std::hex << "0x(" << offset << ")" ); 
   while (true)
   {
      if ( !delim_mode && ((long)(Fp->tellg())-offset) >= l_max)
      {
         break;
      }

      newDocEntry = ReadNextDocEntry( );

      // FIXME :
      // Private tag, in IMplicit VR are defaulted as a BinEntry,
      // Very often they are only composed of Printable characters, 
      // and could be defaulted as a ValEntry.
      // It's too late to do the Job
      // (we should check the value, but we know it after LoadDocEntry ...)
      // --> in next gdcm major release let's unify ValEntry and BinEntry !

      // Uncoment this printf line to be able to 'follow' the DocEntries
      // when something *very* strange happens

      if ( !newDocEntry )
      {
         break;
      }

       // an Item Starter found elsewhere but the first position
       // of a SeqEntry means previous entry was a Sequence
       // but we didn't get it (private Sequence + Implicit VR)
       // we have to backtrack.
      if ( !first && newDocEntry->IsItemStarter() )
      {
         // Debug message within the method !
         newDocEntry = Backtrack(newDocEntry);
      }
      else
      { 
         PreviousDocEntry = newDocEntry; 
      }
 
      used = true;
      newValEntry = dynamic_cast<ValEntry*>(newDocEntry);
      newBinEntry = dynamic_cast<BinEntry*>(newDocEntry);

      if ( newValEntry || newBinEntry )  
      {
       //////////////////////////// ContentEntry
         if ( newBinEntry )
         {
            vr = newDocEntry->GetVR();
            if ( Filetype == ExplicitVR && 
                 !Global::GetVR()->IsVROfBinaryRepresentable(vr) )
            { 
                ////// Neither ValEntry NOR BinEntry: should mean UNKOWN VR
                gdcmWarningMacro( std::hex << newDocEntry->GetGroup() 
                                  << "|" << newDocEntry->GetElement()
                                  << " : Neither Valentry, nor BinEntry." 
                                  "Probably unknown VR.");
            }

         //////////////////// BinEntry or UNKOWN VR:

            // When "this" is a Document the Key is simply of the
            // form ( group, elem )...
            //if ( set == this ) // ( dynamic_cast< Document* > ( set ) )
            //{
            //   newBinEntry->SetKey( newBinEntry->GetKey() );
            //}
            // but when "this" is a SQItem, we are inserting this new
            // valEntry in a sequence item, and the key has the
            // generalized form (refer to \ref BaseTagKey):

            // time waste hunting
            //if (SQItem *parentSQItem = dynamic_cast< SQItem* > ( set ) )
            //{
            //   newBinEntry->SetKey(  parentSQItem->GetBaseTagKey()
            //                       + newBinEntry->GetKey() );
            //}
           
            if ( !set->AddEntry( newBinEntry ) )
            {
               gdcmWarningMacro( "in ParseDES : cannot add a BinEntry "
                                   << newBinEntry->GetKey()  
                                   << " (at offset : " 
                                   << newBinEntry->GetOffset() << " )" );
               used=false;
            }
            else
            {
               // Load only if we can add (not a duplicate key)
               LoadDocEntry( newBinEntry );
            }
         }  // end BinEntry
         else
         {
         /////////////////////// ValEntry

            // When "set" is a Document, then we are at the top of the
            // hierarchy and the Key is simply of the form ( group, elem )...
            //if ( set == this ) // ( dynamic_cast< Document* > ( set ) )
            //{
            //   newValEntry->SetKey( newValEntry->GetKey() );
            //}
            // ...but when "set" is a SQItem, we are inserting this new
            // valEntry in a sequence item. Hence the key has the
            // generalized form (refer to \ref BaseTagKey):

            // time waste hunting
            //if (SQItem *parentSQItem = dynamic_cast< SQItem* > ( set ) )
            //{
            //   newValEntry->SetKey(  parentSQItem->GetBaseTagKey()
            //                      + newValEntry->GetKey() );
            //}

            if ( !set->AddEntry( newValEntry ) )
            {
              gdcmWarningMacro( "in ParseDES : cannot add a ValEntry "
                                  << newValEntry->GetKey()
                                  << " (at offset : " 
                                  << newValEntry->GetOffset() << " )" );   
              used=false;
            }
            else
            {
               // Load only if we can add (not a duplicate key)
               LoadDocEntry( newValEntry );
            }

            if ( newValEntry->GetElement() == 0x0000 ) // if on group length
            {
               if ( newValEntry->GetGroup()%2 != 0 )   // if Shadow Group
               {
                  if ( LoadMode & LD_NOSHADOW ) // if user asked to skip shad.gr
                  {
                     std::string strLgrGroup = newValEntry->GetValue();
                     int lgrGroup;
                     if ( strLgrGroup != GDCM_UNFOUND)
                     {
                        lgrGroup = atoi(strLgrGroup.c_str());
                        Fp->seekg(lgrGroup, std::ios::cur);
                        //used = false;  // never used
                        RemoveEntry( newDocEntry );  // Remove and delete
                        // bcc 5.5 is right "assigned a value that's never used"
                        // newDocEntry = 0;
                        continue;
                     }
                  }
               }
            }

            bool delimitor = newValEntry->IsItemDelimitor();

            if ( (delimitor) || 
                (!delim_mode && ((long)(Fp->tellg())-offset) >= l_max) )
            {
               if ( !used )
                  delete newDocEntry;
               break;
            }
         }

         // Just to make sure we are at the beginning of next entry.
         SkipToNextDocEntry(newDocEntry);
      }
      else
      {
         /////////////////////// SeqEntry :  VR = "SQ"

         unsigned long l = newDocEntry->GetReadLength();          
         if ( l != 0 ) // don't mess the delim_mode for 'zero-length sequence'
         {
            if ( l == 0xffffffff )
            {
              delim_mode_intern = true;
            }
            else
            {
              delim_mode_intern = false;
            }
         }

         if ( (LoadMode & LD_NOSHADOWSEQ) && ! delim_mode_intern )
         { 
           // User asked to skip SeQuences *only* if they belong to Shadow Group
            if ( newDocEntry->GetGroup()%2 != 0 )
            {
                Fp->seekg( l, std::ios::cur);
                RemoveEntry( newDocEntry );  // Remove and delete
                continue;  
            } 
         } 
         if ( (LoadMode & LD_NOSEQ) && ! delim_mode_intern ) 
         {
           // User asked to skip *any* SeQuence
            Fp->seekg( l, std::ios::cur);
            RemoveEntry( newDocEntry );  // Remove and delete
            continue;
         }
         // delay the dynamic cast as late as possible
         newSeqEntry = dynamic_cast<SeqEntry*>(newDocEntry);
         
         // no other way to create the Delimitor ...
         newSeqEntry->SetDelimitorMode( delim_mode_intern );

         // At the top of the hierarchy, stands a Document. When "set"
         // is a Document, then we are building the first depth level.
         // Hence the SeqEntry we are building simply has a depth
         // level of one:
        if ( set == this ) // ( dynamic_cast< Document* > ( set ) )
         {
            newSeqEntry->SetDepthLevel( 1 );
         }
         // But when "set" is already a SQItem, we are building a nested
         // sequence, and hence the depth level of the new SeqEntry
         // we are building, is one level deeper:

         // time waste hunting
         else if (SQItem *parentSQItem = dynamic_cast< SQItem* > ( set ) )
         {
            newSeqEntry->SetDepthLevel( parentSQItem->GetDepthLevel() + 1 );
         }

         if ( l != 0 )
         {  // Don't try to parse zero-length sequences

            gdcmDebugMacro( "Entry in ParseSQ, delim " << delim_mode_intern
                               << " at offset 0x(" << std::hex
                               << newDocEntry->GetOffset() << ")");

            ParseSQ( newSeqEntry, 
                     newDocEntry->GetOffset(),
                     l, delim_mode_intern);

            gdcmDebugMacro( "Exit from ParseSQ, delim " << delim_mode_intern);
 
         }
         if ( !set->AddEntry( newSeqEntry ) )
         {
            gdcmWarningMacro( "in ParseDES : cannot add a SeqEntry "
                                << newSeqEntry->GetKey()
                                << " (at offset : 0x(" 
                                << newSeqEntry->GetOffset() << ") )" ); 
            used = false;
         }
 
         if ( !delim_mode && ((long)(Fp->tellg())-offset) >= l_max)
         {
            if ( !used )
               delete newDocEntry;  
            break;
         }
      }  // end SeqEntry : VR = "SQ"

      if ( !used )
      {
         delete newDocEntry;
      }
      first = false;
   }                               // end While
   gdcmDebugMacro( "Exit from ParseDES, delim-mode " << delim_mode );
}

/**
 * \brief   Parses a Sequence ( SeqEntry after SeqEntry)
 * @return  parsed length for this level
 */ 
void Document::ParseSQ( SeqEntry *seqEntry,
                        long offset, long l_max, bool delim_mode)
{
   int SQItemNumber = 0;
   bool dlm_mod;
   long offsetStartCurrentSQItem = offset;

   while (true)
   {
      // the first time, we read the fff0,e000 of the first SQItem
      DocEntry *newDocEntry = ReadNextDocEntry();

      if ( !newDocEntry )
      {
         gdcmWarningMacro("in ParseSQ : should never get here!");
         break;
      }
      if ( delim_mode )
      {
         if ( newDocEntry->IsSequenceDelimitor() )
         {
            seqEntry->SetDelimitationItem( newDocEntry ); 
            break;
         }
      }
      if ( !delim_mode && ((long)(Fp->tellg())-offset) >= l_max)
      {
         delete newDocEntry;
         break;
      }
      // create the current SQItem
      SQItem *itemSQ = new SQItem( seqEntry->GetDepthLevel() );
      unsigned int l = newDocEntry->GetReadLength();
      
      if ( l == 0xffffffff )
      {
         dlm_mod = true;
      }
      else
      {
         dlm_mod = false;
      }

      // remove fff0,e000, created out of the SQItem
      delete newDocEntry;
      // fill up the current SQItem, starting at the beginning of fff0,e000

      ParseDES(itemSQ, offsetStartCurrentSQItem, l+8, dlm_mod);

      offsetStartCurrentSQItem = Fp->tellg();
 
      seqEntry->AddSQItem( itemSQ, SQItemNumber ); 
      SQItemNumber++;
      if ( !delim_mode && ((long)(Fp->tellg())-offset ) >= l_max )
      {
         break;
      }
   }
}

/**
 * \brief   When a private Sequence + Implicit VR is encountered
 *           we cannot guess it's a Sequence till we find the first
 *           Item Starter. We then backtrack to do the job.
 * @param   docEntry Item Starter that warned us 
 */
DocEntry *Document::Backtrack(DocEntry *docEntry)
{
   // delete the Item Starter, built erroneously out of any Sequence
   // it's not yet in the HTable/chained list
   delete docEntry;

   // Get all info we can from PreviousDocEntry
   uint16_t group = PreviousDocEntry->GetGroup();
   uint16_t elem  = PreviousDocEntry->GetElement();
   uint32_t lgt   = PreviousDocEntry->GetLength();
   long offset    = PreviousDocEntry->GetOffset();

   gdcmDebugMacro( "Backtrack :" << std::hex << group 
                                 << "|" << elem
                                 << " at offset 0x(" <<offset << ")" );
   RemoveEntry( PreviousDocEntry );

   // forge the Seq Entry
   DocEntry *newEntry = NewSeqEntry(group, elem);
   newEntry->SetLength(lgt);
   newEntry->SetOffset(offset);

   // Move back to the beginning of the Sequence

   Fp->seekg(offset, std::ios::beg); // Only for Shadow Implicit VR SQ
   return newEntry;
}

/**
 * \brief   Loads (or not) the element content depending if its length exceeds
 *          or not the value specified with Document::SetMaxSizeLoadEntry()
 * @param   entry Header Entry (Dicom Element) to be dealt with
 * @param forceLoad whether you want to force loading of 'long' elements
 */
void Document::LoadDocEntry(DocEntry *entry, bool forceLoad)
{
   uint16_t group  = entry->GetGroup();
   uint16_t elem   = entry->GetElement();
   std::string  vr = entry->GetVR();
   uint32_t length = entry->GetLength();

   Fp->seekg((long)entry->GetOffset(), std::ios::beg);

   // A SeQuence "contains" a set of Elements.  
   //          (fffe e000) tells us an Element is beginning
   //          (fffe e00d) tells us an Element just ended
   //          (fffe e0dd) tells us the current SeQuence just ended
   //          (fffe 0000) is an 'impossible' tag value, 
   //                                    found in MR-PHILIPS-16-Multi-Seq.dcm
   if ( (group == 0xfffe && elem != 0x0000 ) || vr == "SQ" )
   {
      // NO more value field for SQ !
      return;
   }

   // When the length is zero things are easy:
   if ( length == 0 )
   {
      ((ValEntry *)entry)->SetValue("");
      return;
   }

   // The elements whose length is bigger than the specified upper bound
   // are not loaded. Instead we leave a short notice on the offset of
   // the element content and it's length.

   itksys_ios::ostringstream s;

   if (!forceLoad)
   {
      if (length > MaxSizeLoadEntry)
      {
         if (BinEntry *binEntryPtr = dynamic_cast< BinEntry* >(entry) )
         {  
            s << GDCM_NOTLOADED;
            s << " Ad.:" << (long)entry->GetOffset();
            s << " x(" << std::hex << entry->GetOffset() << ")";
            s << std::dec;
            s << " Lgt:"  << entry->GetLength();
            s << " x(" << std::hex << entry->GetLength() << ")";
            binEntryPtr->SetValue(s.str());
         }
         else if (ValEntry *valEntryPtr = dynamic_cast< ValEntry* >(entry) )
         {
            s << GDCM_NOTLOADED;  
            s << " Address:" << (long)entry->GetOffset();
            s << " Length:"  << entry->GetLength();
            s << " x(" << std::hex << entry->GetLength() << ")";
            valEntryPtr->SetValue(s.str());
         }
         else
         {
            // fusible
            gdcmErrorMacro( "MaxSizeLoadEntry exceeded, neither a BinEntry "
                         << "nor a ValEntry ?! Should never print that !" );
         }

       // to be sure we are at the end of the value ...
       //  Fp->seekg((long)entry->GetOffset()+(long)entry->GetLength(),
       //            std::ios::beg);
       return;
      }
   }

   // When we find a BinEntry not very much can be done :
   if (BinEntry *binEntryPtr = dynamic_cast< BinEntry* >(entry) )
   {
      s << GDCM_BINLOADED;
      binEntryPtr->SetValue(s.str());
      LoadEntryBinArea(binEntryPtr); // last one, not to erase length !
      return;
   }

   if ( IsDocEntryAnInteger(entry) )
   {   
      uint32_t NewInt;
      int nbInt;
      // When short integer(s) are expected, read and convert the following 
      // (n * 2) characters properly i.e. consider them as short integers as
      // opposed to strings.
      // Elements with Value Multiplicity > 1
      // contain a set of integers (not a single one)       
      if (vr == "US" || vr == "SS")
      {
         nbInt = length / 2;
         NewInt = ReadInt16();
         s << NewInt;
         if (nbInt > 1)
         {
            for (int i=1; i < nbInt; i++)
            {
               s << '\\';
               NewInt = ReadInt16();
               s << NewInt;
            }
         }
      }
      // See above comment on multiple integers (mutatis mutandis).
      else if (vr == "UL" || vr == "SL")
      {
         nbInt = length / 4;
         NewInt = ReadInt32();
         s << NewInt;
         if (nbInt > 1)
         {
            for (int i=1; i < nbInt; i++)
            {
               s << '\\';
               NewInt = ReadInt32();
               s << NewInt;
            }
         }
      }
#ifdef GDCM_NO_ANSI_STRING_STREAM
      s << std::ends; // to avoid oddities on Solaris
#endif //GDCM_NO_ANSI_STRING_STREAM

      ((ValEntry *)entry)->SetValue(s.str());
      return;
   }
   
  // FIXME: We need an additional byte for storing \0 that is not on disk
   char *str = new char[length+1];
   Fp->read(str, (size_t)length);
   str[length] = '\0'; //this is only useful when length is odd
   // Special DicomString call to properly handle \0 and even length
   std::string newValue;
   if ( length % 2 )
   {
      newValue = Util::DicomString(str, length+1);
      gdcmWarningMacro("Warning: bad length: " << length <<
                       " For string :" <<  newValue.c_str()); 
      // Since we change the length of string update it length
      //entry->SetReadLength(length+1);
   }
   else
   {
      newValue = Util::DicomString(str, length);
   }
   delete[] str;

   if ( ValEntry *valEntry = dynamic_cast<ValEntry* >(entry) )
   {
      if ( Fp->fail() || Fp->eof())
      {
         if ( Fp->fail() )
            gdcmWarningMacro("--> fail");

         gdcmWarningMacro("Unread element value " << valEntry->GetKey() 
                          << " lgt : " << valEntry->GetReadLength() 
                          << " at " << std::hex << valEntry->GetOffset());
         valEntry->SetValue(GDCM_UNREAD);
         return;
      }

//      if ( vr == "UI" )
//      {
//         // Because of correspondance with the VR dic
//         valEntry->SetValue(newValue);
//      }
//      else
//      {
//         valEntry->SetValue(newValue);
//      }

// Anybody remembers the genesis of strange previous (commented out) code?
      valEntry->SetValue(newValue);

   }
   else
   {
      gdcmWarningMacro("Should have a ValEntry, here ! " << valEntry->GetKey() 
                          << " lgt : " << valEntry->GetReadLength() 
                          << " at " << std::hex << valEntry->GetOffset());
   }
}

/**
 * \brief  Find the value Length of the passed Doc Entry
 * @param  entry Header Entry whose length of the value shall be loaded. 
 */
void Document::FindDocEntryLength( DocEntry *entry )
   throw ( FormatError )
{
   std::string vr  = entry->GetVR();
   uint16_t length16;       
   
   if ( Filetype == ExplicitVR && !entry->IsImplicitVR() ) 
   {
      if ( vr == "OB" || vr == "OW" || vr == "SQ" || vr == "UT" 
                                                           || vr == "UN" )
      {
         // The following reserved two bytes (see PS 3.5-2003, section
         // "7.1.2 Data element structure with explicit vr", p 27) must be
         // skipped before proceeding on reading the length on 4 bytes.

         Fp->seekg( 2L, std::ios::cur); // Once per OW,OB,SQ DocEntry
         uint32_t length32 = ReadInt32();

         if ( (vr == "OB" || vr == "OW") && length32 == 0xffffffff ) 
         {
            uint32_t lengthOB;
            try 
            {
               lengthOB = FindDocEntryLengthOBOrOW();// for encapsulation of encoded pixel 
            }
            catch ( FormatUnexpected )
            {
               // Computing the length failed (this happens with broken
               // files like gdcm-JPEG-LossLess3a.dcm). We still have a
               // chance to get the pixels by deciding the element goes
               // until the end of the file. Hence we artificially fix the
               // the length and proceed.
               gdcmWarningMacro( " Computing the length failed for " << 
                                   entry->GetKey() <<" in " <<GetFileName());

               long currentPosition = Fp->tellg(); // Only for gdcm-JPEG-LossLess3a.dcm-like
               Fp->seekg(0L,std::ios::end);        // Only for gdcm-JPEG-LossLess3a.dcm-like

               long lengthUntilEOF = (long)(Fp->tellg())-currentPosition; // Only for gdcm-JPEG-LossLess3a.dcm-like
               Fp->seekg(currentPosition, std::ios::beg);                 // Only for gdcm-JPEG-LossLess3a.dcm-like

               entry->SetReadLength(lengthUntilEOF);
               entry->SetLength(lengthUntilEOF);
               return;
            }
            entry->SetReadLength(lengthOB);
            entry->SetLength(lengthOB);
            return;
         }
         FixDocEntryFoundLength(entry, length32); 
         return;
      }

      // Length is encoded on 2 bytes.
      length16 = ReadInt16();
  
      // 0xffff means that we deal with 'No Length' Sequence 
      //        or 'No Length' SQItem
      if ( length16 == 0xffff) 
      {           
         length16 = 0;
      }
      FixDocEntryFoundLength( entry, (uint32_t)length16 );
      return;
   }
   else
   {
      // Either implicit VR or a non DICOM conformal (see note below) explicit
      // VR that ommited the VR of (at least) this element. Farts happen.
      // [Note: according to the part 5, PS 3.5-2001, section 7.1 p25
      // on Data elements "Implicit and Explicit VR Data Elements shall
      // not coexist in a Data Set and Data Sets nested within it".]
      // Length is on 4 bytes.

     // Well ... group 0002 is always coded in 'Explicit VR Litle Endian'
     // even if Transfer Syntax is 'Implicit VR ...'
     // --> Except for 'Implicit VR Big Endian Transfer Syntax GE Private' 

      FixDocEntryFoundLength( entry, ReadInt32() );
      return;
   }
}

/**
 * \brief  Find the Length till the next sequence delimiter
 * \warning NOT end user intended method !
 * @return 
 */
uint32_t Document::FindDocEntryLengthOBOrOW()
   throw( FormatUnexpected )
{
   // See PS 3.5-2001, section A.4 p. 49 on encapsulation of encoded pixel data.
   long positionOnEntry = Fp->tellg(); // Only for OB,OW DataElements

   bool foundSequenceDelimiter = false;
   uint32_t totalLength = 0;

   while ( !foundSequenceDelimiter )
   {
      uint16_t group;
      uint16_t elem;
      try
      {
         group = ReadInt16();
         elem  = ReadInt16();   
      }
      catch ( FormatError )
      {
         throw FormatError("Unexpected end of file encountered during ",
                           "Document::FindDocEntryLengthOBOrOW()");
      }
      // We have to decount the group and element we just read
      totalLength += 4;     
      if ( group != 0xfffe || ( ( elem != 0xe0dd ) && ( elem != 0xe000 ) ) )
      {
         gdcmWarningMacro( 
              "Neither an Item tag nor a Sequence delimiter tag on :" 
           << std::hex << group << " , " << elem 
           << ")" );
  
         Fp->seekg(positionOnEntry, std::ios::beg); // Once per fragment (if any) of OB,OW DataElements
         throw FormatUnexpected( 
               "Neither an Item tag nor a Sequence delimiter tag.");
      }
      if ( elem == 0xe0dd )
      {
         foundSequenceDelimiter = true;
      }
      uint32_t itemLength = ReadInt32();
      // We add 4 bytes since we just read the ItemLength with ReadInt32
      totalLength += itemLength + 4;
      SkipBytes(itemLength);
      
      if ( foundSequenceDelimiter )
      {
         break;
      }
   }
   Fp->seekg( positionOnEntry, std::ios::beg); // Only for OB,OW DataElements
   return totalLength;
}

/**
 * \brief     Find the Value Representation of the current Dicom Element.
 * @return    Value Representation of the current Entry
 */
std::string Document::FindDocEntryVR()
{
   if ( Filetype != ExplicitVR )
      return GDCM_UNKNOWN;

   long positionOnEntry = Fp->tellg();
   // Warning: we believe this is explicit VR (Value Representation) because
   // we used a heuristic that found "UL" in the first tag. Alas this
   // doesn't guarantee that all the tags will be in explicit VR. In some
   // cases (see e-film filtered files) one finds implicit VR tags mixed
   // within an explicit VR file. Hence we make sure the present tag
   // is in explicit VR and try to fix things if it happens not to be
   // the case.

   char vr[3];
   Fp->read (vr, (size_t)2);
   vr[2] = 0;

   if ( !CheckDocEntryVR(vr) )
   {
      Fp->seekg(positionOnEntry, std::ios::beg);
      return GDCM_UNKNOWN;
   }
   return vr;
}

/**
 * \brief     Check the correspondance between the VR of the header entry
 *            and the taken VR. If they are different, the header entry is 
 *            updated with the new VR.
 * @param     vr    Dicom Value Representation
 * @return    false if the VR is incorrect or if the VR isn't referenced
 *            otherwise, it returns true
*/
bool Document::CheckDocEntryVR(VRKey vr)
{
   if ( !Global::GetVR()->IsValidVR(vr) )
      return false;

   return true; 
}

/**
 * \brief   Get the transformed value of the header entry. The VR value 
 *          is used to define the transformation to operate on the value
 * \warning NOT end user intended method !
 * @param   entry entry to tranform
 * @return  Transformed entry value
 */
std::string Document::GetDocEntryValue(DocEntry *entry)
{
   if ( IsDocEntryAnInteger(entry) && entry->IsImplicitVR() )
   {
      std::string val = ((ValEntry *)entry)->GetValue();
      std::string vr  = entry->GetVR();
      uint32_t length = entry->GetLength();
      itksys_ios::ostringstream s;
      int nbInt;

      // When short integer(s) are expected, read and convert the following 
      // n * 2 bytes properly i.e. as a multivaluated strings
      // (each single value is separated fromthe next one by '\'
      // as usual for standard multivaluated filels
      // Elements with Value Multiplicity > 1
      // contain a set of short integers (not a single one) 
   
      if ( vr == "US" || vr == "SS" )
      {
         uint16_t newInt16;

         nbInt = length / 2;
         for (int i=0; i < nbInt; i++) 
         {
            if ( i != 0 )
            {
               s << '\\';
            }
            newInt16 = ( val[2*i+0] & 0xFF ) + ( ( val[2*i+1] & 0xFF ) << 8);
            newInt16 = SwapShort( newInt16 );
            s << newInt16;
         }
      }

      // When integer(s) are expected, read and convert the following 
      // n * 4 bytes properly i.e. as a multivaluated strings
      // (each single value is separated fromthe next one by '\'
      // as usual for standard multivaluated filels
      // Elements with Value Multiplicity > 1
      // contain a set of integers (not a single one) 
      else if ( vr == "UL" || vr == "SL" )
      {
         uint32_t newInt32;

         nbInt = length / 4;
         for (int i=0; i < nbInt; i++) 
         {
            if ( i != 0)
            {
               s << '\\';
            }
            newInt32 = ( val[4*i+0] & 0xFF )
                    + (( val[4*i+1] & 0xFF ) <<  8 )
                    + (( val[4*i+2] & 0xFF ) << 16 )
                    + (( val[4*i+3] & 0xFF ) << 24 );
            newInt32 = SwapLong( newInt32 );
            s << newInt32;
         }
      }
#ifdef GDCM_NO_ANSI_STRING_STREAM
      s << std::ends; // to avoid oddities on Solaris
#endif //GDCM_NO_ANSI_STRING_STREAM
      return s.str();
   }
   return ((ValEntry *)entry)->GetValue();
}

/**
 * \brief   Get the reverse transformed value of the header entry. The VR 
 *          value is used to define the reverse transformation to operate on
 *          the value
 * \warning NOT end user intended method !
 * @param   entry Entry to reverse transform
 * @return  Reverse transformed entry value
 */
std::string Document::GetDocEntryUnvalue(DocEntry *entry)
{
   if ( IsDocEntryAnInteger(entry) && entry->IsImplicitVR() )
   {
      std::string vr = entry->GetVR();
      std::vector<std::string> tokens;
      itksys_ios::ostringstream s;

      if ( vr == "US" || vr == "SS" ) 
      {
         uint16_t newInt16;

         tokens.erase( tokens.begin(), tokens.end()); // clean any previous value
         Util::Tokenize (((ValEntry *)entry)->GetValue(), tokens, "\\");
         for (unsigned int i=0; i<tokens.size(); i++) 
         {
            newInt16 = atoi(tokens[i].c_str());
            s << (  newInt16        & 0xFF ) 
              << (( newInt16 >> 8 ) & 0xFF );
         }
         tokens.clear();
      }
      if ( vr == "UL" || vr == "SL")
      {
         uint32_t newInt32;

         tokens.erase(tokens.begin(),tokens.end()); // clean any previous value
         Util::Tokenize (((ValEntry *)entry)->GetValue(), tokens, "\\");
         for (unsigned int i=0; i<tokens.size();i++) 
         {
            newInt32 = atoi(tokens[i].c_str());
            s << (char)(  newInt32         & 0xFF ) 
              << (char)(( newInt32 >>  8 ) & 0xFF )
              << (char)(( newInt32 >> 16 ) & 0xFF )
              << (char)(( newInt32 >> 24 ) & 0xFF );
         }
         tokens.clear();
      }

#ifdef GDCM_NO_ANSI_STRING_STREAM
      s << std::ends; // to avoid oddities on Solaris
#endif //GDCM_NO_ANSI_STRING_STREAM
      return s.str();
   }

   return ((ValEntry *)entry)->GetValue();
}

/**
 * \brief   Skip a given Header Entry 
 * \warning NOT end user intended method !
 * @param   entry entry to skip
 */
void Document::SkipDocEntry(DocEntry *entry) 
{
   SkipBytes(entry->GetLength());
}

/**
 * \brief   Skips to the beginning of the next Header Entry 
 * \warning NOT end user intended method !
 * @param   currentDocEntry entry to skip
 */
void Document::SkipToNextDocEntry(DocEntry *currentDocEntry) 
{
   long l = currentDocEntry->GetReadLength();
   if ( l == -1 ) // length = 0xffff shouldn't appear here ...
                  // ... but PMS imagers happen !
      return;
   Fp->seekg((size_t)(currentDocEntry->GetOffset()), std::ios::beg); //FIXME :each DocEntry
   if (currentDocEntry->GetGroup() != 0xfffe)  // for fffe pb
   {
      Fp->seekg( l,std::ios::cur);                                 //FIXME :each DocEntry
   }
}

/**
 * \brief   When the length of an element value is obviously wrong (because
 *          the parser went Jabberwocky) one can hope improving things by
 *          applying some heuristics.
 * @param   entry entry to check
 * @param   foundLength first assumption about length    
 */
void Document::FixDocEntryFoundLength(DocEntry *entry,
                                      uint32_t foundLength)
{
   entry->SetReadLength( foundLength );// will be updated only if a bug is found
   if ( foundLength == 0xffffffff)
   {
      foundLength = 0;
   }
   
   uint16_t gr   = entry->GetGroup();
   uint16_t elem = entry->GetElement(); 
     
   if ( foundLength % 2)
   {
      gdcmWarningMacro( "Warning : Tag with uneven length " << foundLength
        <<  " in x(" << std::hex << gr << "," << elem <<")");
   }
      
   //////// Fix for some naughty General Electric images.
   // Allthough not recent many such GE corrupted images are still present
   // on Creatis hard disks. Hence this fix shall remain when such images
   // are no longer in use (we are talking a few years, here)...
   // Note: XMedCon probably uses such a trick since it is able to read
   //       those pesky GE images ...
   if ( foundLength == 13)
   {
      // Only happens for this length !
      if ( gr != 0x0008 || ( elem != 0x0070 && elem != 0x0080 ) )
      {
         foundLength = 10;
         entry->SetReadLength(10); // a bug is to be fixed !?
      }
   }

   //////// Fix for some brain-dead 'Leonardo' Siemens images.
   // Occurence of such images is quite low (unless one leaves close to a
   // 'Leonardo' source. Hence, one might consider commenting out the
   // following fix on efficiency reasons.
   else if ( gr   == 0x0009 && ( elem == 0x1113 || elem == 0x1114 ) )
   {
      foundLength = 4;
      entry->SetReadLength(4); // a bug is to be fixed !
   } 
 
   else if ( entry->GetVR() == "SQ" )
   {
      foundLength = 0;      // ReadLength is unchanged 
   } 
    
   //////// We encountered a 'delimiter' element i.e. a tag of the form 
   // "fffe|xxxx" which is just a marker. Delimiters length should not be
   // taken into account.
   else if ( gr == 0xfffe )
   {    
     // According to the norm, fffe|0000 shouldn't exist. BUT the Philips
     // image gdcmData/gdcm-MR-PHILIPS-16-Multi-Seq.dcm happens to
     // causes extra troubles...
     if ( entry->GetElement() != 0x0000 )
     {
        foundLength = 0;
     }
   }
   entry->SetLength(foundLength);
}

/**
 * \brief   Apply some heuristics to predict whether the considered 
 *          element value contains/represents an integer or not.
 * @param   entry The element value on which to apply the predicate.
 * @return  The result of the heuristical predicate.
 */
bool Document::IsDocEntryAnInteger(DocEntry *entry)
{
   uint16_t elem         = entry->GetElement();
   uint16_t group        = entry->GetGroup();
   const std::string &vr = entry->GetVR();
   uint32_t length       = entry->GetLength();

   // When we have some semantics on the element we just read, and if we
   // a priori know we are dealing with an integer, then we shall be
   // able to swap it's element value properly.
   if ( elem == 0 )  // This is the group length of the group
   {  
      if ( length == 4 )
      {
         return true;
      }
      else 
      {
         // Although this should never happen, still some images have a
         // corrupted group length [e.g. have a glance at offset x(8336) of
         // gdcmData/gdcm-MR-PHILIPS-16-Multi-Seq.dcm.
         // Since for dicom compliant and well behaved headers, the present
         // test is useless (and might even look a bit paranoid), when we
         // encounter such an ill-formed image, we simply display a warning
         // message and proceed on parsing (while crossing fingers).
         long filePosition = Fp->tellg(); // Only when elem 0x0000 length is not 4 (?!?)
         gdcmWarningMacro( "Erroneous Group Length element length  on : (" 
           << std::hex << group << " , " << elem
           << ") -before- position x(" << filePosition << ")"
           << "lgt : " << length );
      }
   }

   if ( vr == "UL" || vr == "US" || vr == "SL" || vr == "SS" )
   {
      return true;
   }   
   return false;
}

/**
 * \brief   Discover what the swap code is (among little endian, big endian,
 *          bad little endian, bad big endian).
 *          sw is set
 * @return false when we are absolutely sure 
 *               it's neither ACR-NEMA nor DICOM
 *         true  when we hope ours assuptions are OK
 */
bool Document::CheckSwap()
{   
   uint32_t  s32;
   uint16_t  s16;
       
   char deb[256];
    
   // First, compare HostByteOrder and NetworkByteOrder in order to
   // determine if we shall need to swap bytes (i.e. the Endian type).
   bool net2host = Util::IsCurrentProcessorBigEndian();
         
   // The easiest case is the one of a 'true' DICOM header, we just have
   // to look for the string "DICM" inside the file preamble.
   Fp->read(deb, 256);
   
   char *entCur = deb + 128;
   if ( memcmp(entCur, "DICM", (size_t)4) == 0 )
   {
      gdcmDebugMacro( "Looks like DICOM Version3 (preamble + DCM)" );
      
      // Group 0002 should always be VR, and the first element 0000
      // Let's be carefull (so many wrong headers ...)
      // and determine the value representation (VR) : 
      // Let's skip to the first element (0002,0000) and check there if we find
      // "UL"  - or "OB" if the 1st one is (0002,0001) -,
      // in which case we (almost) know it is explicit VR.
      // WARNING: if it happens to be implicit VR then what we will read
      // is the length of the group. If this ascii representation of this
      // length happens to be "UL" then we shall believe it is explicit VR.
      // We need to skip :
      // * the 128 bytes of File Preamble (often padded with zeroes),
      // * the 4 bytes of "DICM" string,
      // * the 4 bytes of the first tag (0002, 0000),or (0002, 0001)
      // i.e. a total of  136 bytes.
      entCur = deb + 136;
     
      // group 0x0002 *is always* Explicit VR Sometimes ,
      // even if elem 0002,0010 (Transfer Syntax) tells us the file is
      // *Implicit* VR  (see former 'gdcmData/icone.dcm')
      
      if ( memcmp(entCur, "UL", (size_t)2) == 0 ||
           memcmp(entCur, "OB", (size_t)2) == 0 ||
           memcmp(entCur, "UI", (size_t)2) == 0 ||
           memcmp(entCur, "CS", (size_t)2) == 0 )  // CS, to remove later
                                                   // when Write DCM *adds*
      // FIXME
      // Use Document::dicom_vr to test all the possibilities
      // instead of just checking for UL, OB and UI !? group 0000 
      {
         Filetype = ExplicitVR;
         gdcmDebugMacro( "Group 0002 : Explicit Value Representation");
      } 
      else 
      {
         Filetype = ImplicitVR;
         gdcmWarningMacro( "Group 0002 :Not an explicit Value Representation;"
                        << "Looks like a bugged Header!");
      }
      
      if ( net2host )
      {
         SwapCode = 4321;
         gdcmDebugMacro( "HostByteOrder != NetworkByteOrder, SwapCode = 4321");
      }
      else 
      {
         SwapCode = 1234;
         gdcmDebugMacro( "HostByteOrder = NetworkByteOrder, SwapCode = 1234");
      }
      
      // Position the file position indicator at first tag 
      // (i.e. after the file preamble and the "DICM" string).

      Fp->seekg ( 132L, std::ios::beg); // Once per Document
      return true;
   } // ------------------------------- End of DicomV3 ----------------

   // Alas, this is not a DicomV3 file and whatever happens there is no file
   // preamble. We can reset the file position indicator to where the data
   // is (i.e. the beginning of the file).

   gdcmWarningMacro( "Not a Kosher DICOM Version3 file (no preamble)");

   Fp->seekg(0, std::ios::beg); // Once per ACR-NEMA Document

   // Let's check 'No Preamble Dicom File' :
   // Should start with group 0x0002
   // and be Explicit Value Representation

   s16 = *((uint16_t *)(deb));
   SwapCode = 0;     
   switch ( s16 )
   {
      case 0x0002 :
         SwapCode = 1234;
         entCur = deb + 4;
         break;
      case 0x0200 :
         SwapCode = 4321;
         entCur = deb + 6;
    } 

   if ( SwapCode != 0 )
   {
      if ( memcmp(entCur, "UL", (size_t)2) == 0 ||
           memcmp(entCur, "OB", (size_t)2) == 0 ||
           memcmp(entCur, "UI", (size_t)2) == 0 ||
           memcmp(entCur, "SH", (size_t)2) == 0 ||
           memcmp(entCur, "AE", (size_t)2) == 0 ||
           memcmp(entCur, "OB", (size_t)2) == 0 )
         {
            Filetype = ExplicitVR;  // FIXME : not enough to say it's Explicit
                                    // Wait untill reading Transfer Syntax
            gdcmDebugMacro( "Group 0002 : Explicit Value Representation");
            return true;
          }
    }
// ------------------------------- End of 'No Preamble' DicomV3 -------------

   // Our next best chance would be to be considering a 'clean' ACR/NEMA file.
   // By clean we mean that the length of the first group is written down.
   // If this is the case and since the length of the first group HAS to be
   // four (bytes), then determining the proper swap code is straightforward.

   entCur = deb + 4;
   // We assume the array of char we are considering contains the binary
   // representation of a 32 bits integer. Hence the following dirty
   // trick :
   s32 = *((uint32_t *)(entCur));
   switch( s32 )
   {
      case 0x00040000 :
         SwapCode = 3412;
         Filetype = ACR;
         return true;
      case 0x04000000 :
         SwapCode = 4321;
         Filetype = ACR;
         return true;
      case 0x00000400 :
         SwapCode = 2143;
         Filetype = ACR;
         return true;
      case 0x00000004 :
         SwapCode = 1234;
         Filetype = ACR;
         return true;
      default :
         // We are out of luck. It is not a DicomV3 nor a 'clean' ACR/NEMA file.
         // It is time for despaired wild guesses. 
         // So, let's check if this file wouldn't happen to be 'dirty' ACR/NEMA,
         //  i.e. the 'group length' element is not present :     
         
         //  check the supposed-to-be 'group number'
         //  in ( 0x0001 .. 0x0008 )
         //  to determine ' SwapCode' value .
         //  Only 0 or 4321 will be possible 
         //  (no oportunity to check for the formerly well known
         //  ACR-NEMA 'Bad Big Endian' or 'Bad Little Endian' 
         //  if unsuccessfull (i.e. neither 0x0002 nor 0x0200 etc-3, 4, ..., 8-)
         //  the file IS NOT ACR-NEMA nor DICOM V3
         //  Find a trick to tell it the caller...
      
         s16 = *((uint16_t *)(deb));
      
         switch ( s16 )
         {
            case 0x0001 :
            case 0x0002 :
            case 0x0003 :
            case 0x0004 :
            case 0x0005 :
            case 0x0006 :
            case 0x0007 :
            case 0x0008 :
               SwapCode = 1234;
               Filetype = ACR;
               return true;
            case 0x0100 :
            case 0x0200 :
            case 0x0300 :
            case 0x0400 :
            case 0x0500 :
            case 0x0600 :
            case 0x0700 :
            case 0x0800 :
               SwapCode = 4321;
               Filetype = ACR;
               return true;
            default :
               gdcmWarningMacro("ACR/NEMA unfound swap info (Hopeless !)");
               Filetype = Unknown;
               return false;
         }
   }
}

/**
 * \brief Change the Byte Swap code. 
 */
void Document::SwitchByteSwapCode() 
{
   gdcmDebugMacro( "Switching Byte Swap code from "<< SwapCode
                     << " at: 0x" << std::hex << Fp->tellg() );  // Only when DEBUG
   if ( SwapCode == 1234 ) 
   {
      SwapCode = 4321;
   }
   else if ( SwapCode == 4321 ) 
   {
      SwapCode = 1234;
   }
   else if ( SwapCode == 3412 ) 
   {
      SwapCode = 2143;
   }
   else if ( SwapCode == 2143 )
   {
      SwapCode = 3412;
   }
   gdcmDebugMacro( " Into: "<< SwapCode );
}

/**
 * \brief  during parsing, Header Elements too long are not loaded in memory
 * @param newSize new size
 */
void Document::SetMaxSizeLoadEntry(long newSize) 
{
   if ( newSize < 0 )
   {
      return;
   }
   if ((uint32_t)newSize >= (uint32_t)0xffffffff )
   {
      MaxSizeLoadEntry = 0xffffffff;
      return;
   }
   MaxSizeLoadEntry = newSize;
   IsDocumentModified = true;
}

/**
 * \brief   Read the next tag WITHOUT loading it's value
 *          (read the 'Group Number', the 'Element Number',
 *          gets the Dict Entry
 *          gets the VR, gets the length, gets the offset value)
 * @return  On succes : the newly created DocEntry, NULL on failure.      
 */
DocEntry *Document::ReadNextDocEntry()
{
   uint16_t group;
   uint16_t elem;

   try
   {
      group = ReadInt16();
      elem  = ReadInt16();
   }
   catch ( FormatError )
   {
      // We reached the EOF (or an error occured) therefore 
      // header parsing has to be considered as finished.
      return 0;
   }

   // Sometimes file contains groups of tags with reversed endianess.
   HandleBrokenEndian(group, elem);

   // In 'true DICOM' files Group 0002 is always little endian
   if ( HasDCMPreamble )
      HandleOutOfGroup0002(group, elem);
 
   std::string vr = FindDocEntryVR();
   std::string realVR = vr;

   if ( vr == GDCM_UNKNOWN )
   {
      if ( elem == 0x0000 ) // Group Length
      {
         realVR = "UL";     // must be UL
      }
      else if (group%2 == 1 &&  (elem >= 0x0010 && elem <=0x00ff ))
      {  
      // DICOM PS 3-5 7.8.1 a) states that those 
      // (gggg-0010->00FF where gggg is odd) attributes have to be LO
         realVR = "LO";
      }
      else
      {
         DictEntry *dictEntry = GetDictEntry(group,elem);
         if ( dictEntry )
         {
            realVR = dictEntry->GetVR();
         }
      }
   }

   DocEntry *newEntry;
   if ( Global::GetVR()->IsVROfSequence(realVR) )
      newEntry = NewSeqEntry(group, elem);
   else if ( Global::GetVR()->IsVROfStringRepresentable(realVR) )
      newEntry = NewValEntry(group, elem, realVR);
   else
      newEntry = NewBinEntry(group, elem, realVR);

   if ( vr == GDCM_UNKNOWN )
   {
      if ( Filetype == ExplicitVR )
      {
         // We thought this was explicit VR, but we end up with an
         // implicit VR tag. Let's backtrack.
         if ( newEntry->GetGroup() != 0xfffe )
         { 
            std::string msg;
            int offset = Fp->tellg();
            msg = Util::Format("Entry (%04x,%04x) at 0x(%x) should be Explicit VR\n", 
                          newEntry->GetGroup(), newEntry->GetElement(), offset );
            gdcmWarningMacro( msg.c_str() );
          }
      }
      newEntry->SetImplicitVR();
   }

   try
   {
      FindDocEntryLength(newEntry);
   }
   catch ( FormatError )
   {
      // Call it quits
      delete newEntry;
      return 0;
   }

   newEntry->SetOffset(Fp->tellg());  // for each DocEntry

   return newEntry;
}

/**
 * \brief   Handle broken private tag from Philips NTSCAN
 *          where the endianess is being switched to BigEndian 
 *          for no apparent reason
 * @return  no return
 */
void Document::HandleBrokenEndian(uint16_t &group, uint16_t &elem)
{
   // Endian reversion. 
   // Some files contain groups of tags with reversed endianess.
   static int reversedEndian = 0;
   // try to fix endian switching in the middle of headers
   if ((group == 0xfeff) && (elem == 0x00e0))
   {
     // start endian swap mark for group found
     gdcmDebugMacro( "Start endian swap mark found." );
     reversedEndian++;
     SwitchByteSwapCode();
     // fix the tag
     group = 0xfffe;
     elem  = 0xe000;
   } 
   else if (group == 0xfffe && elem == 0xe00d && reversedEndian) 
   {
     // end of reversed endian group
     gdcmDebugMacro( "End of reversed endian." );
     reversedEndian--;
     SwitchByteSwapCode();
   }
   else if (group == 0xfeff && elem == 0xdde0) 
   {
     // reversed Sequence Terminator found
     // probabely a bug in the header !
     // Do what you want, it breaks !
     //reversedEndian--;
     //SwitchByteSwapCode();
     gdcmWarningMacro( "Should never get here! reversed Sequence Terminator!" );
     // fix the tag
      group = 0xfffe;
      elem  = 0xe0dd;  
   }
   else if (group == 0xfffe && elem == 0xe0dd) 
   {
      gdcmDebugMacro( "Straight Sequence Terminator." );  
   }
}

/**
 * \brief   Group 0002 is always coded Little Endian
 *          whatever Transfer Syntax is
 * @return  no return
 */
void Document::HandleOutOfGroup0002(uint16_t &group, uint16_t &elem)
{
   // Endian reversion. 
   // Some files contain groups of tags with reversed endianess.
   if ( !Group0002Parsed && group != 0x0002)
   {
      Group0002Parsed = true;
      // we just came out of group 0002
      // if Transfer Syntax is Big Endian we have to change CheckSwap

      std::string ts = GetTransferSyntax();
      if ( !Global::GetTS()->IsTransferSyntax(ts) )
      {
         gdcmWarningMacro("True DICOM File, with NO Tansfer Syntax: " << ts );
         return;
      }

      // Group 0002 is always 'Explicit ...' 
      // even when Transfer Syntax says 'Implicit ..." 

      if ( Global::GetTS()->GetSpecialTransferSyntax(ts) == TS::ImplicitVRLittleEndian )
         {
            Filetype = ImplicitVR;
         }
       
      // FIXME Strangely, this works with 
      //'Implicit VR BigEndian Transfer Syntax' (GE Private)
      //
      // --> Probabely normal, since we considered we never have 
      // to trust manufacturers.
      // (we find very often 'Implicit VR' tag, 
      // even when Transfer Syntax tells us it's Explicit ...
      if ( Global::GetTS()->GetSpecialTransferSyntax(ts) == TS::ExplicitVRBigEndian )
      {
         gdcmDebugMacro("Transfer Syntax Name = [" 
                        << GetTransferSyntaxName() << "]" );
         SwitchByteSwapCode();
         group = SwapShort(group);
         elem  = SwapShort(elem);
      }
   }
}

//-----------------------------------------------------------------------------
// Print

//-----------------------------------------------------------------------------
} // end namespace gdcm
