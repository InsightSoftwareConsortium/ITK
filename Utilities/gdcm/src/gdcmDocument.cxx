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

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Refer to Document::CheckSwap()
//const unsigned int Document::HEADER_LENGTH_TO_READ = 256;

// Refer to Document::SetMaxSizeLoadEntry()
const unsigned int Document::MAX_SIZE_LOAD_ELEMENT_VALUE = 0xfff; // 4096
const unsigned int Document::MAX_SIZE_PRINT_ELEMENT_VALUE = 0x7fffffff;

//-----------------------------------------------------------------------------
// Constructor / Destructor
// Constructors and destructors are protected to avoid user to invoke directly
/**
 * \brief   constructor  
 * @param   filename 'Document' (File or DicomDir) to be opened for parsing
 */
Document::Document( std::string const &filename ) 
         :ElementSet(-1)
{
   SetMaxSizeLoadEntry(MAX_SIZE_LOAD_ELEMENT_VALUE); 
   Filename = filename;
   Initialize();

   Fp = 0;
   if ( !OpenFile() )
   {
      return;
   }

   Group0002Parsed = false;

   gdcmWarningMacro( "Starting parsing of file: " << Filename.c_str());

   Fp->seekg(0, std::ios::end);
   long lgt = Fp->tellg();
           
   Fp->seekg(0, std::ios::beg);

   CheckSwap();
   long beg = Fp->tellg();
   lgt -= beg;
   
   ParseDES( this, beg, lgt, false); // Loading is done during parsing

   Fp->seekg( 0,  std::ios::beg);
   
   // Load 'non string' values
      
   std::string PhotometricInterpretation = GetEntryValue(0x0028,0x0004);   
   if( PhotometricInterpretation == "PALETTE COLOR " )
   {
      LoadEntryBinArea(0x0028,0x1200);  // gray LUT   
      /// FIXME FIXME FIXME
      /// The tags refered by the three following lines used to be CORRECTLY
      /// defined as having an US Value Representation in the public
      /// dictionary. BUT the semantics implied by the three following
      /// lines state that the corresponding tag contents are in fact
      /// the ones of a BinEntry.
      /// In order to fix things "Quick and Dirty" the dictionary was
      /// altered on PURPOSE but now contains a WRONG value.
      /// In order to fix things and restore the dictionary to its
      /// correct value, one needs to decided of the semantics by deciding
      /// whether the following tags are either:
      /// - multivaluated US, and hence loaded as ValEntry, but afterwards
      ///   also used as BinEntry, which requires the proper conversion,
      /// - OW, and hence loaded as BinEntry, but afterwards also used
      ///   as ValEntry, which requires the proper conversion.
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
   LoadEntryBinArea(0x0028,0x3006);  //LUT Data (CTX dependent) 

   CloseFile(); 
  
   // ----------------------------
   // Specific code to allow gdcm to read ACR-LibIDO formated images
   // Note: ACR-LibIDO is an extension of the ACR standard that was
   //       used at CREATIS. For the time being (say a couple years)
   //       we keep this kludge to allow a smooth move to gdcm for
   //       CREATIS developpers (sorry folks).
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
}

/**
 * \brief This default constructor doesn't parse the file. You should
 *        then invoke \ref Document::SetFileName and then the parsing.
 */
Document::Document() 
         :ElementSet(-1)
{
   Fp = 0;

   SetMaxSizeLoadEntry(MAX_SIZE_LOAD_ELEMENT_VALUE);
   Initialize();
   SwapCode = 1234;
   Filetype = ExplicitVR;
   Group0002Parsed = false;
}

/**
 * \brief   Canonical destructor.
 */
Document::~Document ()
{
   RefPubDict = NULL;
   RefShaDict = NULL;
}

//-----------------------------------------------------------------------------
// Public
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
 * \brief  This predicate, based on hopefully reasonable heuristics,
 *         decides whether or not the current Document was properly parsed
 *         and contains the mandatory information for being considered as
 *         a well formed and usable Dicom/Acr File.
 * @return true when Document is the one of a reasonable Dicom/Acr file,
 *         false otherwise. 
 */
bool Document::IsReadable()
{
   if( Filetype == Unknown)
   {
      gdcmWarningMacro( "Wrong filetype");
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
 * \brief   Predicate for dicom version 3 file.
 * @return  True when the file is a dicom version 3.
 */
bool Document::IsDicomV3()
{
   // Checking if Transfer Syntax exists is enough
   // Anyway, it's to late check if the 'Preamble' was found ...
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
 * @return  The encountered Transfer Syntax of the current document.
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
      gdcmWarningMacro( "Unfound Transfer Syntax (0002,0010)");
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
         gdcmErrorMacro( "Unset swap code:" << SwapCode );
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

   if(Fp)
   {
      gdcmWarningMacro( "File already open: " << Filename.c_str());
      CloseFile();
   }

   Fp = new std::ifstream(Filename.c_str(), std::ios::in | std::ios::binary);
   if( ! *Fp )
   {
      gdcmDebugMacro( "Cannot open file: " << Filename.c_str());
      delete Fp;
      Fp = 0;
      return 0;
   }
 
   uint16_t zero = 0;
   Fp->read((char*)&zero, (size_t)2);
   if( Fp->eof() )
   {
      CloseFile();
      return 0;
   }
 
   //ACR -- or DICOM with no Preamble; may start with a Shadow Group --
   if( 
       zero == 0x0001 || zero == 0x0100 || zero == 0x0002 || zero == 0x0200 ||
       zero == 0x0003 || zero == 0x0300 || zero == 0x0004 || zero == 0x0400 ||
       zero == 0x0005 || zero == 0x0500 || zero == 0x0006 || zero == 0x0600 ||
       zero == 0x0007 || zero == 0x0700 || zero == 0x0008 || zero == 0x0800 )
   {
      std::string msg 
         = Util::Format("ACR/DICOM with no preamble: (%04x)\n", zero);
      gdcmWarningMacro( msg.c_str() );
      return Fp;
   }
 
   //DICOM
   Fp->seekg(126L, std::ios::cur);
   char dicm[4] = {' ',' ',' ',' '};
   Fp->read(dicm,  (size_t)4);
   if( Fp->eof() )
   {
      CloseFile();
      return 0;
   }
   if( memcmp(dicm, "DICM", 4) == 0 )
   {
      HasDCMPreamble = true;
      return Fp;
   }
 
   CloseFile();
   gdcmWarningMacro( "Not DICOM/ACR (missing preamble)" << Filename.c_str());
 
   return 0;
}

/**
 * \brief closes the file  
 * @return  TRUE if the close was successfull 
 */
bool Document::CloseFile()
{
   if( Fp )
   {
      Fp->close();
      delete Fp;
      Fp = 0;
   }
   return true;
}

/**
 * \brief Writes in a file all the Header Entries (Dicom Elements) 
 * @param fp file pointer on an already open file (actually: Output File Stream)
 * @param filetype Type of the File to be written 
 *          (ACR-NEMA, ExplicitVR, ImplicitVR)
 * @return Always true.
 */
void Document::WriteContent(std::ofstream *fp, FileType filetype)
{
   // \TODO move the following lines (and a lot of others, to be written)
   // to a future function CheckAndCorrectHeader  

   // (necessary if user wants to write a DICOM V3 file
   // starting from an ACR-NEMA (V2) Header

   if ( filetype == ImplicitVR || filetype == ExplicitVR )
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
      return;

   BinEntry *binElement = dynamic_cast<BinEntry *>(docElement);
   if( !binElement )
      return;

   LoadEntryBinArea(binElement);
}

/**
 * \brief Loads (from disk) the element content 
 *        when a string is not suitable
 * @param elem  Entry whose binArea is going to be loaded
 */
void Document::LoadEntryBinArea(BinEntry *elem) 
{
   if(elem->GetBinArea())
      return;

   bool openFile = !Fp;
   if(openFile)
      OpenFile();

   size_t o =(size_t)elem->GetOffset();
   Fp->seekg(o, std::ios::beg);

   size_t l = elem->GetLength();
   uint8_t *a = new uint8_t[l];
   if( !a )
   {
      gdcmWarningMacro( "Cannot allocate BinEntry content");
      return;
   }

   Fp->read((char*)a, l);
   if( Fp->fail() || Fp->eof())
   {
      delete[] a;
      return;
   }

   elem->SetBinArea(a);

   if(openFile)
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
   if(Fp)
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
   if(s1 < s2)
   {
      return true;
   }
   else if( s1 > s2 )
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
         else if( s1 > s2 )
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
            else if( s1 > s2 )
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
   if( Fp->eof() )
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
   if( Fp->eof() )
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
 * @param filetype Type of the File to be written 
 */
int Document::ComputeGroup0002Length( FileType filetype ) 
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

      if( gr == 0x0002 )
      {
         found0002 = true;

         if( entry->GetElement() != 0x0000 )
         {
            vr = entry->GetVR();
 
            if( filetype == ExplicitVR )
            {
               if ( (vr == "OB") || (vr == "OW") || (vr == "SQ") ) 
               {
                  // explicit VR AND OB, OW, SQ : 4 more bytes
                  groupLength +=  4;
               }
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
 * @return  length of the parsed set. 
 */ 
void Document::ParseDES(DocEntrySet *set, long offset, 
                        long l_max, bool delim_mode)
{
   DocEntry *newDocEntry;
   ValEntry *newValEntry;
   BinEntry *newBinEntry;
   SeqEntry *newSeqEntry;
   VRKey vr;
   bool used;

   while (true)
   {
      if ( !delim_mode && ((long)(Fp->tellg())-offset) >= l_max)
      {
         break;
      }

      used = true;
      newDocEntry = ReadNextDocEntry( );

      if ( !newDocEntry )
      {
         break;
      }

      vr = newDocEntry->GetVR();
      newValEntry = dynamic_cast<ValEntry*>(newDocEntry);
      newBinEntry = dynamic_cast<BinEntry*>(newDocEntry);
      newSeqEntry = dynamic_cast<SeqEntry*>(newDocEntry);

      if ( newValEntry || newBinEntry )
      {
         if ( newBinEntry )
         {
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
            if ( dynamic_cast< Document* > ( set ) )
            {
               newBinEntry->SetKey( newBinEntry->GetKey() );
            }
            // but when "this" is a SQItem, we are inserting this new
            // valEntry in a sequence item, and the key has the
            // generalized form (refer to \ref BaseTagKey):
            if (SQItem *parentSQItem = dynamic_cast< SQItem* > ( set ) )
            {
               newBinEntry->SetKey(  parentSQItem->GetBaseTagKey()
                                   + newBinEntry->GetKey() );
            }

            LoadDocEntry( newBinEntry );
            if( !set->AddEntry( newBinEntry ) )
            {
              //Expect big troubles if here
              //delete newBinEntry;
              used=false;
            }
         }
         else
         {
         /////////////////////// ValEntry
            // When "set" is a Document, then we are at the top of the
            // hierarchy and the Key is simply of the form ( group, elem )...
            if ( dynamic_cast< Document* > ( set ) )
            {
               newValEntry->SetKey( newValEntry->GetKey() );
            }
            // ...but when "set" is a SQItem, we are inserting this new
            // valEntry in a sequence item. Hence the key has the
            // generalized form (refer to \ref BaseTagKey):
            if (SQItem *parentSQItem = dynamic_cast< SQItem* > ( set ) )
            {
               newValEntry->SetKey(  parentSQItem->GetBaseTagKey()
                                   + newValEntry->GetKey() );
            }
             
            LoadDocEntry( newValEntry );
            bool delimitor=newValEntry->IsItemDelimitor();
            if( !set->AddEntry( newValEntry ) )
            {
              // If here expect big troubles
              //delete newValEntry; //otherwise mem leak
              used=false;
            }

            if (delimitor)
            {
               if(!used)
                  delete newDocEntry;
               break;
            }
            if ( !delim_mode && ((long)(Fp->tellg())-offset) >= l_max)
            {
               if(!used)
                  delete newDocEntry;
               break;
            }
         }

         // Just to make sure we are at the beginning of next entry.
         SkipToNextDocEntry(newDocEntry);
      }
      else
      {
         // VR = "SQ"
         unsigned long l = newDocEntry->GetReadLength();            
         if ( l != 0 ) // don't mess the delim_mode for zero-length sequence
         {
            if ( l == 0xffffffff )
            {
              delim_mode = true;
            }
            else
            {
              delim_mode = false;
            }
         }
         // no other way to create it ...
         newSeqEntry->SetDelimitorMode( delim_mode );

         // At the top of the hierarchy, stands a Document. When "set"
         // is a Document, then we are building the first depth level.
         // Hence the SeqEntry we are building simply has a depth
         // level of one:
         if (/*Document *dummy =*/ dynamic_cast< Document* > ( set ) )
         {
            //(void)dummy;
            newSeqEntry->SetDepthLevel( 1 );
            newSeqEntry->SetKey( newSeqEntry->GetKey() );
         }
         // But when "set" is already a SQItem, we are building a nested
         // sequence, and hence the depth level of the new SeqEntry
         // we are building, is one level deeper:
         if (SQItem *parentSQItem = dynamic_cast< SQItem* > ( set ) )
         {
            newSeqEntry->SetDepthLevel( parentSQItem->GetDepthLevel() + 1 );
            newSeqEntry->SetKey(  parentSQItem->GetBaseTagKey()
                                + newSeqEntry->GetKey() );
         }

         if ( l != 0 )
         {  // Don't try to parse zero-length sequences
            ParseSQ( newSeqEntry, 
                     newDocEntry->GetOffset(),
                     l, delim_mode);
         }
         if( !set->AddEntry( newSeqEntry ) )
         {
            used = false;
         }
         if ( !delim_mode && ((long)(Fp->tellg())-offset) >= l_max)
         {
            if( !used )
               delete newDocEntry;
            break;
         }
      }

      if( !used )
         delete newDocEntry;
   }
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
         // FIXME Should warn user
         break;
      }
      if( delim_mode )
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
      itksys_ios::ostringstream newBase;
      newBase << seqEntry->GetKey()
              << "/"
              << SQItemNumber
              << "#";
      itemSQ->SetBaseTagKey( newBase.str() );
      unsigned int l = newDocEntry->GetReadLength();
      
      if ( l == 0xffffffff )
      {
         dlm_mod = true;
      }
      else
      {
         dlm_mod = false;
      }

      // Let's try :------------
      // remove fff0,e000, created out of the SQItem
      delete newDocEntry;
      Fp->seekg(offsetStartCurrentSQItem, std::ios::beg);
      // fill up the current SQItem, starting at the beginning of fff0,e000
      ParseDES(itemSQ, offsetStartCurrentSQItem, l+8, dlm_mod);
      offsetStartCurrentSQItem = Fp->tellg();
      // end try -----------------
 
      seqEntry->AddSQItem( itemSQ, SQItemNumber ); 
      SQItemNumber++;
      if ( !delim_mode && ((long)(Fp->tellg())-offset ) >= l_max )
      {
         break;
      }
   }
}

/**
 * \brief   Loads the element content if its length doesn't exceed
 *          the value specified with Document::SetMaxSizeLoadEntry()
 * @param   entry Header Entry (Dicom Element) to be dealt with
 */
void Document::LoadDocEntry(DocEntry *entry)
{
   uint16_t group  = entry->GetGroup();
   std::string  vr = entry->GetVR();
   uint32_t length = entry->GetLength();

   Fp->seekg((long)entry->GetOffset(), std::ios::beg);

   // A SeQuence "contains" a set of Elements.  
   //          (fffe e000) tells us an Element is beginning
   //          (fffe e00d) tells us an Element just ended
   //          (fffe e0dd) tells us the current SeQuence just ended
   if( group == 0xfffe )
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
   // are not loaded. Instead we leave a short notice of the offset of
   // the element content and it's length.

   itksys_ios::ostringstream s;
   if (length > MaxSizeLoadEntry)
   {
      if (BinEntry *binEntryPtr = dynamic_cast< BinEntry* >(entry) )
      {  
         //s << "gdcm::NotLoaded (BinEntry)";
         s << GDCM_NOTLOADED;
         s << " Address:" << (long)entry->GetOffset();
         s << " Length:"  << entry->GetLength();
         s << " x(" << std::hex << entry->GetLength() << ")";
         binEntryPtr->SetValue(s.str());
      }
      // Be carefull : a BinEntry IS_A ValEntry ... 
      else if (ValEntry *valEntryPtr = dynamic_cast< ValEntry* >(entry) )
      {
        // s << "gdcm::NotLoaded. (ValEntry)";
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
      Fp->seekg((long)entry->GetOffset()+(long)entry->GetLength(),
                std::ios::beg);
      return;
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
      // n *two characters properly i.e. consider them as short integers as
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
   if( length % 2 )
   {
      newValue = Util::DicomString(str, length+1);
      gdcmWarningMacro("Warning: bad length: " << length <<
                       ",For string :" <<  newValue.c_str()); 
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
         gdcmWarningMacro("Unread element value");
         valEntry->SetValue(GDCM_UNREAD);
         return;
      }

      if( vr == "UI" )
      {
         // Because of correspondance with the VR dic
         valEntry->SetValue(newValue);
      }
      else
      {
         valEntry->SetValue(newValue);
      }
   }
   else
   {
      gdcmErrorMacro( "Should have a ValEntry, here !");
   }
}

/**
 * \brief  Find the value Length of the passed Header Entry
 * @param  entry Header Entry whose length of the value shall be loaded. 
 */
void Document::FindDocEntryLength( DocEntry *entry )
   throw ( FormatError )
{
   std::string  vr  = entry->GetVR();
   uint16_t length16;       
   
   if ( Filetype == ExplicitVR && !entry->IsImplicitVR() ) 
   {
      if ( vr == "OB" || vr == "OW" || vr == "SQ" || vr == "UN" ) 
      {
         // The following reserved two bytes (see PS 3.5-2003, section
         // "7.1.2 Data element structure with explicit vr", p 27) must be
         // skipped before proceeding on reading the length on 4 bytes.
         Fp->seekg( 2L, std::ios::cur);
         uint32_t length32 = ReadInt32();

         if ( (vr == "OB" || vr == "OW") && length32 == 0xffffffff ) 
         {
            uint32_t lengthOB;
            try 
            {
               lengthOB = FindDocEntryLengthOBOrOW();
            }
            catch ( FormatUnexpected )
            {
               // Computing the length failed (this happens with broken
               // files like gdcm-JPEG-LossLess3a.dcm). We still have a
               // chance to get the pixels by deciding the element goes
               // until the end of the file. Hence we artificially fix the
               // the length and proceed.
               long currentPosition = Fp->tellg();
               Fp->seekg(0L,std::ios::end);

               long lengthUntilEOF = (long)(Fp->tellg())-currentPosition;
               Fp->seekg(currentPosition, std::ios::beg);

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

      // FIXME : This heuristic supposes that the first group following
      //         group 0002 *has* and element 0000.
      // BUT ... Element 0000 is optionnal :-(


   // Fixed using : HandleOutOfGroup0002()
   //              (first hereafter strategy ...)
      
      // We can tell the current file is encoded in big endian (like
      // Data/US-RGB-8-epicard) when we find the "Transfer Syntax" tag
      // and it's value is the one of the encoding of a big endian file.
      // In order to deal with such big endian encoded files, we have
      // (at least) two strategies:
      // * when we load the "Transfer Syntax" tag with value of big endian
      //   encoding, we raise the proper flags. Then we wait for the end
      //   of the META group (0x0002) among which is "Transfer Syntax",
      //   before switching the swap code to big endian. We have to postpone
      //   the switching of the swap code since the META group is fully encoded
      //   in little endian, and big endian coding only starts at the next
      //   group. The corresponding code can be hard to analyse and adds
      //   many additional unnecessary tests for regular tags.
      // * the second strategy consists in waiting for trouble, that shall
      //   appear when we find the first group with big endian encoding. This
      //   is easy to detect since the length of a "Group Length" tag (the
      //   ones with zero as element number) has to be of 4 (0x0004). When we
      //   encounter 1024 (0x0400) chances are the encoding changed and we
      //   found a group with big endian encoding.
      //---> Unfortunately, element 0000 is optional.
      //---> This will not work when missing!
      // We shall use this second strategy. In order to make sure that we
      // can interpret the presence of an apparently big endian encoded
      // length of a "Group Length" without committing a big mistake, we
      // add an additional check: we look in the already parsed elements
      // for the presence of a "Transfer Syntax" whose value has to be "big
      // endian encoding". When this is the case, chances are we have got our
      // hands on a big endian encoded file: we switch the swap code to
      // big endian and proceed...

//      if ( element  == 0x0000 && length16 == 0x0400 ) 
//      {
//         std::string ts = GetTransferSyntax();
//         if ( Global::GetTS()->GetSpecialTransferSyntax(ts) 
//                != TS::ExplicitVRBigEndian ) 
//         {
//            throw FormatError( "Document::FindDocEntryLength()",
//                               " not explicit VR." );
//           return;
//        }
//        length16 = 4;
//        SwitchByteSwapCode();
//
//         // Restore the unproperly loaded values i.e. the group, the element
//         // and the dictionary entry depending on them.
//         uint16_t correctGroup = SwapShort( entry->GetGroup() );
//         uint16_t correctElem  = SwapShort( entry->GetElement() );
//         DictEntry *newTag = GetDictEntry( correctGroup, correctElem );
//         if ( !newTag )
//         {
//            // This correct tag is not in the dictionary. Create a new one.
//            newTag = NewVirtualDictEntry(correctGroup, correctElem);
//         }
//         // FIXME this can create a memory leaks on the old entry that be
//         // left unreferenced.
//         entry->SetDictEntry( newTag );
//      }
  
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
   long positionOnEntry = Fp->tellg();
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
         long filePosition = Fp->tellg();
         gdcmWarningMacro( "Neither an Item tag nor a Sequence delimiter tag on :" 
           << std::hex << group << " , " << elem 
           << ") -before- position x(" << filePosition << ")" );
  
         Fp->seekg(positionOnEntry, std::ios::beg);
         throw FormatUnexpected( "Neither an Item tag nor a Sequence delimiter tag.");
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
   Fp->seekg( positionOnEntry, std::ios::beg);
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

   if( !CheckDocEntryVR(vr) )
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
 * @return    false if the VR is incorrect of if the VR isn't referenced
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
   
      if( vr == "US" || vr == "SS" )
      {
         uint16_t newInt16;

         nbInt = length / 2;
         for (int i=0; i < nbInt; i++) 
         {
            if( i != 0 )
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
      else if( vr == "UL" || vr == "SL" )
      {
         uint32_t newInt32;

         nbInt = length / 4;
         for (int i=0; i < nbInt; i++) 
         {
            if( i != 0)
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
   Fp->seekg((long)(currentDocEntry->GetOffset()),     std::ios::beg);
   if (currentDocEntry->GetGroup() != 0xfffe)  // for fffe pb
      Fp->seekg( (long)(currentDocEntry->GetReadLength()),std::ios::cur);
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
   entry->SetReadLength( foundLength ); // will be updated only if a bug is found        
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
   // Note: XMedCom probably uses such a trick since it is able to read
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
      entry->SetReadLength(4); // a bug is to be fixed !?
   } 
 
   else if ( entry->GetVR() == "SQ" )
   {
      foundLength = 0;      // ReadLength is unchanged 
   } 
    
   //////// We encountered a 'delimiter' element i.e. a tag of the form 
   // "fffe|xxxx" which is just a marker. Delimiters length should not be
   // taken into account.
   else if( gr == 0xfffe )
   {    
     // According to the norm, fffe|0000 shouldn't exist. BUT the Philips
     // image gdcmData/gdcm-MR-PHILIPS-16-Multi-Seq.dcm happens to
     // causes extra troubles...
     if( entry->GetElement() != 0x0000 )
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
   uint16_t elem          = entry->GetElement();
   uint16_t group         = entry->GetGroup();
   const std::string &vr  = entry->GetVR();
   uint32_t length        = entry->GetLength();

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
         // Allthough this should never happen, still some images have a
         // corrupted group length [e.g. have a glance at offset x(8336) of
         // gdcmData/gdcm-MR-PHILIPS-16-Multi-Seq.dcm].
         // Since for dicom compliant and well behaved headers, the present
         // test is useless (and might even look a bit paranoid), when we
         // encounter such an ill-formed image, we simply display a warning
         // message and proceed on parsing (while crossing fingers).
         long filePosition = Fp->tellg();
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
   // The only guaranted way of finding the swap code is to find a
   // group tag since we know it's length has to be of four bytes i.e.
   // 0x00000004. Finding the swap code in then straigthforward. Trouble
   // occurs when we can't find such group...
   
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
   if( memcmp(entCur, "DICM", (size_t)4) == 0 )
   {
      gdcmWarningMacro( "Looks like DICOM Version3 (preamble + DCM)" );
      
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
      
      if( memcmp(entCur, "UL", (size_t)2) == 0 ||
          memcmp(entCur, "OB", (size_t)2) == 0 ||
          memcmp(entCur, "UI", (size_t)2) == 0 ||
          memcmp(entCur, "CS", (size_t)2) == 0 )  // CS, to remove later
                                                  // when Write DCM *adds*
      // FIXME
      // Use Document::dicom_vr to test all the possibilities
      // instead of just checking for UL, OB and UI !? group 0000 
      {
         Filetype = ExplicitVR;
         gdcmWarningMacro( "Group 0002 : Explicit Value Representation");
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
         gdcmWarningMacro( "HostByteOrder != NetworkByteOrder");
      }
      else 
      {
         SwapCode = 1234;
         gdcmWarningMacro( "HostByteOrder = NetworkByteOrder");
      }
      
      // Position the file position indicator at first tag 
      // (i.e. after the file preamble and the "DICM" string).
      Fp->seekg(0, std::ios::beg);
      Fp->seekg ( 132L, std::ios::beg);
      return true;
   } // End of DicomV3

   // Alas, this is not a DicomV3 file and whatever happens there is no file
   // preamble. We can reset the file position indicator to where the data
   // is (i.e. the beginning of the file).
   gdcmWarningMacro( "Not a DICOM Version3 file");
   Fp->seekg(0, std::ios::beg);

   // Our next best chance would be to be considering a 'clean' ACR/NEMA file.
   // By clean we mean that the length of the first tag is written down.
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
         //  if unsuccessfull (i.e. neither 0x0002 nor 0x0200 etc -3, 4, ..., 8-) 
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
               gdcmWarningMacro( "ACR/NEMA unfound swap info (Really hopeless !)");
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
   gdcmWarningMacro( "Switching Byte Swap code from "<< SwapCode);
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
}

/**
 * \brief Header Elements too long will not be printed
 *   See comments of \ref Document::MAX_SIZE_PRINT_ELEMENT_VALUE 
 * @param newSize new size
 */
void Document::SetMaxSizePrintEntry(long newSize) 
{
   if ( newSize < 0 )
   {
      return;
   }
   if ((uint32_t)newSize >= (uint32_t)0xffffffff )
   {
      MaxSizePrintEntry = 0xffffffff;
      return;
   }
   MaxSizePrintEntry = newSize;
}


/**
 * \brief   Read the next tag but WITHOUT loading it's value
 *          (read the 'Group Number', the 'Element Number',
 *          gets the Dict Entry
 *          gets the VR, gets the length, gets the offset value)
 * @return  On succes the newly created DocEntry, NULL on failure.      
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
   catch ( FormatError e )
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

   if( vr == GDCM_UNKNOWN)
   {
      DictEntry *dictEntry = GetDictEntry(group,elem);
      if( dictEntry )
         realVR = dictEntry->GetVR();
   }

   DocEntry *newEntry;
   if( Global::GetVR()->IsVROfSequence(realVR) )
      newEntry = NewSeqEntry(group, elem);
   else if( Global::GetVR()->IsVROfStringRepresentable(realVR) )
      newEntry = NewValEntry(group, elem,vr);
   else
      newEntry = NewBinEntry(group, elem,vr);

   if( vr == GDCM_UNKNOWN )
   {
      if( Filetype == ExplicitVR )
      {
         // We thought this was explicit VR, but we end up with an
         // implicit VR tag. Let's backtrack.
         if ( newEntry->GetGroup() != 0xfffe )
         { 
            std::string msg;
            msg = Util::Format("Entry (%04x,%04x) should be Explicit VR\n", 
                          newEntry->GetGroup(), newEntry->GetElement());
            gdcmWarningMacro( msg.c_str() );
          }
      }
      newEntry->SetImplicitVR();
   }

   try
   {
      FindDocEntryLength(newEntry);
   }
   catch ( FormatError e )
   {
      // Call it quits
      //std::cout << e;
      delete newEntry;
      return 0;
   }

   newEntry->SetOffset(Fp->tellg());  

   return newEntry;
}

/**
 * \brief   Handle broken private tag from Philips NTSCAN
 *          where the endianess is being switch to BigEndian for no
 *          apparent reason
 * @return  no return
 */
void Document::HandleBrokenEndian(uint16_t &group, uint16_t &elem)
{
   // Endian reversion. Some files contain groups of tags with reversed endianess.
   static int reversedEndian = 0;
   // try to fix endian switching in the middle of headers
   if ((group == 0xfeff) && (elem == 0x00e0))
   {
     // start endian swap mark for group found
     reversedEndian++;
     SwitchByteSwapCode();
     // fix the tag
     group = 0xfffe;
     elem  = 0xe000;
   } 
   else if (group == 0xfffe && elem == 0xe00d && reversedEndian) 
   {
     // end of reversed endian group
     reversedEndian--;
     SwitchByteSwapCode();
   }
}

/**
 * \brief   Group 0002 is always coded Little Endian
 *          whatever Transfer Syntax is
 * @return  no return
 */
void Document::HandleOutOfGroup0002(uint16_t &group, uint16_t &elem)
{
   // Endian reversion. Some files contain groups of tags with reversed endianess.
   if ( !Group0002Parsed && group != 0x0002)
   {
      Group0002Parsed = true;
      // we just came out of group 0002
      // if Transfer syntax is Big Endian we have to change CheckSwap

      std::string ts = GetTransferSyntax();
      if ( !Global::GetTS()->IsTransferSyntax(ts) )
      {
         gdcmWarningMacro("True DICOM File, with NO Tansfer Syntax: " << ts );
         return;
      }

      // Group 0002 is always 'Explicit ...' enven when Transfer Syntax says 'Implicit ..." 

      if ( Global::GetTS()->GetSpecialTransferSyntax(ts) == TS::ImplicitVRLittleEndian )
         {
            Filetype = ImplicitVR;
         }
       
      // FIXME Strangely, this works with 
      //'Implicit VR Transfer Syntax (GE Private)
      if ( Global::GetTS()->GetSpecialTransferSyntax(ts) == TS::ExplicitVRBigEndian )
      {
         gdcmWarningMacro("Transfer Syntax Name = [" 
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
