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

#include <vector>
#include <iomanip>

// For nthos:
#if defined(_MSC_VER) || defined(__BORLANDC__)
   #include <winsock.h>
#else
   #include <netinet/in.h>
#endif

namespace gdcm 
{
static const char *TransferSyntaxStrings[] =  {
  // Implicit VR Little Endian
  "1.2.840.10008.1.2",
  // Explicit VR Little Endian
  "1.2.840.10008.1.2.1",
  // Deflated Explicit VR Little Endian
  "1.2.840.10008.1.2.1.99",
  // Explicit VR Big Endian
  "1.2.840.10008.1.2.2",
  // JPEG Baseline (Process 1)
  "1.2.840.10008.1.2.4.50",
  // JPEG Extended (Process 2 & 4)
  "1.2.840.10008.1.2.4.51",
  // JPEG Extended (Process 3 & 5)
  "1.2.840.10008.1.2.4.52",
  // JPEG Spectral Selection, Non-Hierarchical (Process 6 & 8)
  "1.2.840.10008.1.2.4.53",
  // JPEG Full Progression, Non-Hierarchical (Process 10 & 12)
  "1.2.840.10008.1.2.4.55",
  // JPEG Lossless, Non-Hierarchical (Process 14)
  "1.2.840.10008.1.2.4.57",
  // JPEG Lossless, Hierarchical, First-Order Prediction (Process 14, [Selection Value 1])
  "1.2.840.10008.1.2.4.70",
  // JPEG 2000 Lossless
  "1.2.840.10008.1.2.4.90",
  // JPEG 2000
  "1.2.840.10008.1.2.4.91",
  // RLE Lossless
  "1.2.840.10008.1.2.5",
  // Unknown
  "Unknown Transfer Syntax"
};

//-----------------------------------------------------------------------------
// Refer to Document::CheckSwap()
//const unsigned int Document::HEADER_LENGTH_TO_READ = 256;

// Refer to Document::SetMaxSizeLoadEntry()
const unsigned int Document::MAX_SIZE_LOAD_ELEMENT_VALUE = 0xfff; // 4096
const unsigned int Document::MAX_SIZE_PRINT_ELEMENT_VALUE = 0x7fffffff;

//-----------------------------------------------------------------------------
// Constructor / Destructor

/**
 * \brief   constructor  
 * @param   filename file to be opened for parsing
 */
Document::Document( std::string const & filename ) : ElementSet(-1)
{
   SetMaxSizeLoadEntry(MAX_SIZE_LOAD_ELEMENT_VALUE); 
   Filename = filename;
   Initialise();

   if ( !OpenFile() )
   {
      return;
   }

   dbg.Verbose(0, "Document::Document: starting parsing of file: ",
                  Filename.c_str());
   Fp->seekg( 0,  std::ios::beg);
   
   Fp->seekg(0,  std::ios::end);
   long lgt = Fp->tellg();
           
   Fp->seekg( 0,  std::ios::beg);
   CheckSwap();
   long beg = Fp->tellg();
   lgt -= beg;
   
   ParseDES( this, beg, lgt, false); // le Load sera fait a la volee

   Fp->seekg( 0,  std::ios::beg);
   
   // Load 'non string' values
      
   std::string PhotometricInterpretation = GetEntryByNumber(0x0028,0x0004);   
   if( PhotometricInterpretation == "PALETTE COLOR " )
   {
      LoadEntryBinArea(0x0028,0x1200);  // gray LUT   
      /// FIXME FIXME FIXME
      /// The tags refered by the three following lines used to be CORRECTLY
      /// defined as having an US Value Representation in the public
      /// dictionnary. BUT the semantics implied by the three following
      /// lines state that the corresponding tag contents are in fact
      /// the ones of a BinEntry.
      /// In order to fix things "Quick and Dirty" the dictionnary was
      /// altered on PURPOUS but now contains a WRONG value.
      /// In order to fix things and restore the dictionary to its
      /// correct value, one needs to decided of the semantics by deciding
      /// wether the following tags are either:
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
  
   // --------------------------------------------------------------
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
   RecCode = GetEntryByNumber(0x0008, 0x0010); // recognition code
   if (RecCode == "ACRNEMA_LIBIDO_1.1" ||
       RecCode == "CANRME_AILIBOD1_1." )  // for brain-damaged softwares
                                          // with "little-endian strings"
   {
         Filetype = ACR_LIBIDO; 
         std::string rows    = GetEntryByNumber(0x0028, 0x0010);
         std::string columns = GetEntryByNumber(0x0028, 0x0011);
         SetEntryByNumber(columns, 0x0028, 0x0010);
         SetEntryByNumber(rows   , 0x0028, 0x0011);
   }
   // ----------------- End of ACR-LibIDO kludge ------------------ 

   PrintLevel = 1;  // 'Medium' print level by default
}

/**
 * \brief This default constructor doesn't parse the file. You should
 *        then invoke \ref Document::SetFileName and then the parsing.
 */
Document::Document() : ElementSet(-1)
{
   SetMaxSizeLoadEntry(MAX_SIZE_LOAD_ELEMENT_VALUE);
   Initialise();
   PrintLevel = 1;  // 'Medium' print level by default
}

/**
 * \brief   Canonical destructor.
 */
Document::~Document ()
{
   RefPubDict = NULL;
   RefShaDict = NULL;

   // Recursive clean up of sequences
   for (TagDocEntryHT::const_iterator it = TagHT.begin(); 
                                      it != TagHT.end(); ++it )
   { 
      //delete it->second; //temp remove
   }
   TagHT.clear();
   delete RLEInfo;
   delete JPEGInfo;
}

//-----------------------------------------------------------------------------
// Print

/**
  * \brief   Prints The Dict Entries of THE public Dicom Dictionary
  * @return
  */  
void Document::PrintPubDict(std::ostream & os)
{
   RefPubDict->Print(os);
}

/**
  * \brief   Prints The Dict Entries of THE shadow Dicom Dictionary
  * @return
  */
void Document::PrintShaDict(std::ostream & os)
{
   RefShaDict->Print(os);
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   Get the public dictionary used
 */
Dict* Document::GetPubDict()
{
   return RefPubDict;
}

/**
 * \brief   Get the shadow dictionary used
 */
Dict* Document::GetShaDict()
{
   return RefShaDict;
}

/**
 * \brief   Set the shadow dictionary used
 * \param   dict dictionary to use in shadow
 */
bool Document::SetShaDict(Dict *dict)
{
   RefShaDict = dict;
   return !RefShaDict;
}

/**
 * \brief   Set the shadow dictionary used
 * \param   dictName name of the dictionary to use in shadow
 */
bool Document::SetShaDict(DictKey const & dictName)
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
      dbg.Verbose(0, "Document::IsReadable: wrong filetype");
      return false;
   }

   if( TagHT.empty() )
   {
      dbg.Verbose(0, "Document::IsReadable: no tags in internal"
                     " hash table.");
      return false;
   }

   return true;
}


/**
 * \brief   Internal function that checks whether the Transfer Syntax given
 *          as argument is the one present in the current document.
 * @param   syntaxToCheck The transfert syntax we need to check against.
 * @return  True when SyntaxToCheck corresponds to the Transfer Syntax of
 *          the current document. False either when the document contains
 *          no Transfer Syntax, or when the Tranfer Syntaxes doesn't match.
 */
TransferSyntaxType Document::GetTransferSyntax()
{
   DocEntry *entry = GetDocEntryByNumber(0x0002, 0x0010);
   if ( !entry )
   {
      return UnknownTS;
   }

   // The entry might be present but not loaded (parsing and loading
   // happen at different stages): try loading and proceed with check...
   LoadDocEntrySafe(entry);
   if (ValEntry* valEntry = dynamic_cast< ValEntry* >(entry) )
   {
      std::string transfer = valEntry->GetValue();
      // The actual transfer (as read from disk) might be padded. We
      // first need to remove the potential padding. We can make the
      // weak assumption that padding was not executed with digits...
      if  ( transfer.length() == 0 )
      {
         // for brain damaged headers
         return UnknownTS;
      }
      while ( !isdigit(transfer[transfer.length()-1]) )
      {
         transfer.erase(transfer.length()-1, 1);
      }
      for (int i = 0; TransferSyntaxStrings[i] != NULL; i++)
      {
         if ( TransferSyntaxStrings[i] == transfer )
         {
            return TransferSyntaxType(i);
         }
      }
   }
   return UnknownTS;
}

bool Document::IsJPEGLossless()
{
   TransferSyntaxType r = GetTransferSyntax();
   return    r ==  JPEGFullProgressionProcess10_12
          || r == JPEGLosslessProcess14
          || r == JPEGLosslessProcess14_1;
}
                                                                                
/**
 * \brief   Determines if the Transfer Syntax was already encountered
 *          and if it corresponds to a JPEG2000 one
 * @return  True when JPEG2000 (Lossly or LossLess) found. False in all
 *          other cases.
 */
bool Document::IsJPEG2000()
{
   TransferSyntaxType r = GetTransferSyntax();
   return r == JPEG2000Lossless || r == JPEG2000;
}

/**
 * \brief   Determines if the Transfer Syntax corresponds to any form
 *          of Jpeg encoded Pixel data.
 * @return  True when any form of JPEG found. False otherwise.
 */
bool Document::IsJPEG()
{
   TransferSyntaxType r = GetTransferSyntax();
   return r == JPEGBaselineProcess1 
     || r == JPEGExtendedProcess2_4
     || r == JPEGExtendedProcess3_5
     || r == JPEGSpectralSelectionProcess6_8
     ||      IsJPEGLossless()
     ||      IsJPEG2000();
}

/**
 * \brief   Determines if the Transfer Syntax corresponds to encapsulated
 *          of encoded Pixel Data (as opposed to native).
 * @return  True when encapsulated. False when native.
 */
bool Document::IsEncapsulate()
{
   TransferSyntaxType r = GetTransferSyntax();
   return IsJPEG() || r == RLELossless;
}

/**
 * \brief   Predicate for dicom version 3 file.
 * @return  True when the file is a dicom version 3.
 */
bool Document::IsDicomV3()
{
   // Checking if Transfert Syntax exists is enough
   // Anyway, it's to late check if the 'Preamble' was found ...
   // And ... would it be a rich idea to check ?
   // (some 'no Preamble' DICOM images exist !)
   return GetDocEntryByNumber(0x0002, 0x0010) != NULL;
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
 * \brief  Tries to open the file \ref Document::Filename and
 *         checks the preamble when existing.
 * @return The FILE pointer on success. 
 */
std::ifstream* Document::OpenFile()
{
   Fp = new std::ifstream(Filename.c_str(), std::ios::in | std::ios::binary);

   if(!Fp)
   {
      dbg.Verbose( 0,
                   "Document::OpenFile cannot open file: ",
                   Filename.c_str());
      return 0;
   }
 
   uint16_t zero;
   Fp->read((char*)&zero,  (size_t)2 );
 
   //ACR -- or DICOM with no Preamble --
   if( zero == 0x0008 || zero == 0x0800 || zero == 0x0002 || zero == 0x0200 )
   {
      return Fp;
   }
 
   //DICOM
   Fp->seekg(126L, std::ios::cur);
   char dicm[4];
   Fp->read(dicm,  (size_t)4);
   if( memcmp(dicm, "DICM", 4) == 0 )
   {
      return Fp;
   }
 
   Fp->close();
   dbg.Verbose( 0,
                "Document::OpenFile not DICOM/ACR (missing preamble)",
                Filename.c_str());
 
   return 0;
}

/**
 * \brief closes the file  
 * @return  TRUE if the close was successfull 
 */
bool Document::CloseFile()
{
  Fp->close();
  delete Fp;
  Fp = 0;

  return true; //FIXME how do we detect a non-close ifstream ?
}

/**
 * \brief Writes in a file all the Header Entries (Dicom Elements) 
 * @param fp file pointer on an already open file
 * @param filetype Type of the File to be written 
 *          (ACR-NEMA, ExplicitVR, ImplicitVR)
 * \return Always true.
 */
void Document::Write(std::ofstream* fp, FileType filetype)
{
   /// \todo move the following lines (and a lot of others, to be written)
   /// to a future function CheckAndCorrectHeader  
   /// (necessary if user wants to write a DICOM V3 file
   /// starting from an  ACR-NEMA (V2)  Header

   if (filetype == ImplicitVR) 
   {
      std::string ts = 
         Util::DicomString( TransferSyntaxStrings[ImplicitVRLittleEndian] );
      ReplaceOrCreateByNumber(ts, 0x0002, 0x0010);
      
      /// \todo Refer to standards on page 21, chapter 6.2
      ///       "Value representation": values with a VR of UI shall be
      ///       padded with a single trailing null
      ///       in the following case we have to padd manually with a 0
      
      SetEntryLengthByNumber(18, 0x0002, 0x0010);
   } 

   if (filetype == ExplicitVR)
   {
      std::string ts = 
         Util::DicomString( TransferSyntaxStrings[ExplicitVRLittleEndian] );
      ReplaceOrCreateByNumber(ts, 0x0002, 0x0010); //LEAK
      
      /// \todo Refer to standards on page 21, chapter 6.2
      ///       "Value representation": values with a VR of UI shall be
      ///       padded with a single trailing null
      ///       Dans le cas suivant on doit pader manuellement avec un 0
      
      SetEntryLengthByNumber(20, 0x0002, 0x0010);
   }
  
/**
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
 
   ElementSet::Write(fp, filetype); // This one is recursive

}

/**
 * \brief   Modifies the value of a given Header Entry (Dicom Element)
 *          when it exists. Create it with the given value when unexistant.
 * @param   value (string) Value to be set
 * @param   group   Group number of the Entry 
 * @param   elem  Element number of the Entry
 * @param   VR  V(alue) R(epresentation) of the Entry -if private Entry-
 * \return  pointer to the modified/created Header Entry (NULL when creation
 *          failed).
 */ 
ValEntry* Document::ReplaceOrCreateByNumber(
                                         std::string const & value, 
                                         uint16_t group, 
                                         uint16_t elem,
                                         TagName const & vr )
{
   ValEntry* valEntry = 0;
   DocEntry* currentEntry = GetDocEntryByNumber( group, elem);
   
   if (!currentEntry)
   {
      // check if (group,element) DictEntry exists
      // if it doesn't, create an entry in DictSet::VirtualEntry
      // and use it

   // Find out if the tag we received is in the dictionaries:
      Dict *pubDict = Global::GetDicts()->GetDefaultPubDict();
      DictEntry* dictEntry = pubDict->GetDictEntryByNumber(group, elem);
      if (!dictEntry)
      {
         currentEntry = NewDocEntryByNumber(group, elem, vr);
      }
      else
      {
         currentEntry = NewDocEntryByNumber(group, elem);
      }

      if (!currentEntry)
      {
         dbg.Verbose(0, "Document::ReplaceOrCreateByNumber: call to"
                        " NewDocEntryByNumber failed.");
         return NULL;
      }
      valEntry = new ValEntry(currentEntry);
      if ( !AddEntry(valEntry))
      {
         delete valEntry;
         dbg.Verbose(0, "Document::ReplaceOrCreateByNumber: AddEntry"
                        " failed allthough this is a creation.");
      }
      // This is the reason why NewDocEntryByNumber are a real
      // bad habit !!! FIXME
      delete currentEntry;
   }
   else
   {
      valEntry = dynamic_cast< ValEntry* >(currentEntry);
      if ( !valEntry ) // Euuuuh? It wasn't a ValEntry
                       // then we change it to a ValEntry ?
                       // Shouldn't it be considered as an error ?
      {
         // We need to promote the DocEntry to a ValEntry:
         valEntry = new ValEntry(currentEntry);
         if (!RemoveEntry(currentEntry))
         {
            delete valEntry;
            dbg.Verbose(0, "Document::ReplaceOrCreateByNumber: removal"
                           " of previous DocEntry failed.");
            return NULL;
         }
         if ( !AddEntry(valEntry))
         {
            delete valEntry;
            dbg.Verbose(0, "Document::ReplaceOrCreateByNumber: adding"
                           " promoted ValEntry failed.");
            return NULL;
         }
      }
   }

   SetEntryByNumber(value, group, elem);

   return valEntry;
}   

/*
 * \brief   Modifies the value of a given Header Entry (Dicom Element)
 *          when it exists. Create it with the given value when unexistant.
 * @param   binArea (binary) value to be set
 * @param   Group   Group number of the Entry 
 * @param   Elem  Element number of the Entry
 * \return  pointer to the modified/created Header Entry (NULL when creation
 *          failed).
 */
BinEntry* Document::ReplaceOrCreateByNumber(
                                         uint8_t* binArea,
                                         int lgth, 
                                         uint16_t group, 
                                         uint16_t elem,
                                         TagName const & vr )
{
   BinEntry* binEntry = 0;
   DocEntry* currentEntry = GetDocEntryByNumber( group, elem);
   if (!currentEntry)
   {

      // check if (group,element) DictEntry exists
      // if it doesn't, create an entry in DictSet::VirtualEntry
      // and use it

   // Find out if the tag we received is in the dictionaries:
      Dict *pubDict = Global::GetDicts()->GetDefaultPubDict();
      DictEntry *dictEntry = pubDict->GetDictEntryByNumber(group, elem);

      if (!dictEntry)
      {
         currentEntry = NewDocEntryByNumber(group, elem, vr);
      }
      else
      {
         currentEntry = NewDocEntryByNumber(group, elem);
      }
      if (!currentEntry)
      {
         dbg.Verbose(0, "Document::ReplaceOrCreateByNumber: call to"
                        " NewDocEntryByNumber failed.");
         return NULL;
      }
      binEntry = new BinEntry(currentEntry);
      if ( !AddEntry(binEntry))
      {
         dbg.Verbose(0, "Document::ReplaceOrCreateByNumber: AddEntry"
                        " failed allthough this is a creation.");
      }
   }
   else
   {
      binEntry = dynamic_cast< BinEntry* >(currentEntry);
      if ( !binEntry ) // Euuuuh? It wasn't a BinEntry
                       // then we change it to a BinEntry ?
                       // Shouldn't it be considered as an error ?
      {
         // We need to promote the DocEntry to a BinEntry:
         binEntry = new BinEntry(currentEntry);
         if (!RemoveEntry(currentEntry))
         {
            dbg.Verbose(0, "Document::ReplaceOrCreateByNumber: removal"
                           " of previous DocEntry failed.");
            return NULL;
         }
         if ( !AddEntry(binEntry))
         {
            dbg.Verbose(0, "Document::ReplaceOrCreateByNumber: adding"
                           " promoted BinEntry failed.");
            return NULL;
         }
      }
   }

   SetEntryByNumber(binArea, lgth, group, elem);

   return binEntry;
}  


/*
 * \brief   Modifies the value of a given Header Entry (Dicom Element)
 *          when it exists. Create it when unexistant.
 * @param   Group   Group number of the Entry 
 * @param   Elem  Element number of the Entry
 * \return  pointer to the modified/created SeqEntry (NULL when creation
 *          failed).
 */
SeqEntry* Document::ReplaceOrCreateByNumber( uint16_t group, uint16_t elem)
{
   SeqEntry* b = 0;
   DocEntry* a = GetDocEntryByNumber( group, elem);
   if (!a)
   {
      a = NewSeqEntryByNumber(group, elem);
      if (!a)
      {
         return 0;
      }

      b = new SeqEntry(a, 1); // FIXME : 1 (Depth)
      AddEntry(b);
   }   
   return b;
} 
 
/**
 * \brief Set a new value if the invoked element exists
 *        Seems to be useless !!!
 * @param value new element value
 * @param group  group number of the Entry 
 * @param elem element number of the Entry
 * \return  boolean 
 */
bool Document::ReplaceIfExistByNumber(std::string const & value, 
                                      uint16_t group, uint16_t elem ) 
{
   SetEntryByNumber(value, group, elem);

   return true;
} 

//-----------------------------------------------------------------------------
// Protected

/**
 * \brief   Checks if a given Dicom Element exists within the H table
 * @param   group      Group number of the searched Dicom Element 
 * @param   element  Element number of the searched Dicom Element 
 * @return true is found
 */
bool Document::CheckIfEntryExistByNumber(uint16_t group, uint16_t element )
{
   const std::string &key = DictEntry::TranslateToKey(group, element );
   return TagHT.count(key) != 0;
}

/**
 * \brief   Searches within Header Entries (Dicom Elements) parsed with 
 *          the public and private dictionaries 
 *          for the element value of a given tag.
 * \warning Don't use any longer : use GetPubEntryByName
 * @param   tagName name of the searched element.
 * @return  Corresponding element value when it exists,
 *          and the string GDCM_UNFOUND ("gdcm::Unfound") otherwise.
 */
std::string Document::GetEntryByName(TagName const & tagName)
{
   DictEntry* dictEntry = RefPubDict->GetDictEntryByName(tagName); 
   if( !dictEntry )
   {
      return GDCM_UNFOUND;
   }

   return GetEntryByNumber(dictEntry->GetGroup(),dictEntry->GetElement());
}

/**
 * \brief   Searches within Header Entries (Dicom Elements) parsed with 
 *          the public and private dictionaries 
 *          for the element value representation of a given tag.
 *
 *          Obtaining the VR (Value Representation) might be needed by caller
 *          to convert the string typed content to caller's native type 
 *          (think of C++ vs Python). The VR is actually of a higher level
 *          of semantics than just the native C++ type.
 * @param   tagName name of the searched element.
 * @return  Corresponding element value representation when it exists,
 *          and the string GDCM_UNFOUND ("gdcm::Unfound") otherwise.
 */
std::string Document::GetEntryVRByName(TagName const& tagName)
{
   DictEntry *dictEntry = RefPubDict->GetDictEntryByName(tagName); 
   if( dictEntry == NULL)
   {
      return GDCM_UNFOUND;
   }

   DocEntry* elem = GetDocEntryByNumber(dictEntry->GetGroup(),
                                        dictEntry->GetElement());
   return elem->GetVR();
}

/**
 * \brief   Searches within Header Entries (Dicom Elements) parsed with 
 *          the public and private dictionaries 
 *          for the element value representation of a given tag.
 * @param   group Group number of the searched tag.
 * @param   element Element number of the searched tag.
 * @return  Corresponding element value representation when it exists,
 *          and the string GDCM_UNFOUND ("gdcm::Unfound") otherwise.
 */
std::string Document::GetEntryByNumber(uint16_t group, uint16_t element)
{
   TagKey key = DictEntry::TranslateToKey(group, element);
   /// \todo use map methods, instead of multimap JPR
   if ( !TagHT.count(key))
   {
      return GDCM_UNFOUND;
   }

   return ((ValEntry *)TagHT.find(key)->second)->GetValue();
}

/**
 * \brief   Searches within Header Entries (Dicom Elements) parsed with 
 *          the public and private dictionaries 
 *          for the element value representation of a given tag..
 *
 *          Obtaining the VR (Value Representation) might be needed by caller
 *          to convert the string typed content to caller's native type 
 *          (think of C++ vs Python). The VR is actually of a higher level
 *          of semantics than just the native C++ type.
 * @param   group     Group number of the searched tag.
 * @param   element Element number of the searched tag.
 * @return  Corresponding element value representation when it exists,
 *          and the string GDCM_UNFOUND ("gdcm::Unfound") otherwise.
 */
std::string Document::GetEntryVRByNumber(uint16_t group, uint16_t element)
{
   DocEntry* elem = GetDocEntryByNumber(group, element);
   if ( !elem )
   {
      return GDCM_UNFOUND;
   }
   return elem->GetVR();
}

/**
 * \brief   Searches within Header Entries (Dicom Elements) parsed with 
 *          the public and private dictionaries 
 *          for the value length of a given tag..
 * @param   group     Group number of the searched tag.
 * @param   element Element number of the searched tag.
 * @return  Corresponding element length; -2 if not found
 */
int Document::GetEntryLengthByNumber(uint16_t group, uint16_t element)
{
   DocEntry* elem =  GetDocEntryByNumber(group, element);
   if ( !elem )
   {
      return -2;  //magic number
   }
   return elem->GetLength();
}
/**
 * \brief   Sets the value (string) of the Header Entry (Dicom Element)
 * @param   content string value of the Dicom Element
 * @param   tagName name of the searched Dicom Element.
 * @return  true when found
 */
bool Document::SetEntryByName(std::string const & content,
                              TagName const & tagName)
{
   DictEntry *dictEntry = RefPubDict->GetDictEntryByName(tagName); 
   if( !dictEntry )
   {
      return false;
   }

   return SetEntryByNumber(content,dictEntry->GetGroup(),
                                   dictEntry->GetElement());
}

/**
 * \brief   Accesses an existing DocEntry (i.e. a Dicom Element)
 *          through it's (group, element) and modifies it's content with
 *          the given value.
 * @param   content new value (string) to substitute with
 * @param   group     group number of the Dicom Element to modify
 * @param   element element number of the Dicom Element to modify
 */
bool Document::SetEntryByNumber(std::string const& content, 
                                uint16_t group, uint16_t element) 
{
   int c;
   int l;

   ValEntry* valEntry = GetValEntryByNumber(group, element);
   if (!valEntry )
   {
      dbg.Verbose(0, "Document::SetEntryByNumber: no corresponding",
                     " ValEntry (try promotion first).");
      return false;
   }
   // Non even content must be padded with a space (020H)...
   std::string finalContent = Util::DicomString( content.c_str() );
   assert( !(finalContent.size() % 2) );
   valEntry->SetValue(finalContent);

   // Integers have a special treatement for their length:

   l = finalContent.length();
   if ( l != 0) // To avoid to be cheated by 'zero length' integers
   {   
      VRKey vr = valEntry->GetVR();
      if( vr == "US" || vr == "SS" )
      {
         // for multivaluated items
         c = Util::CountSubstring(content, "\\") + 1;
         l = c*2;
      }
      else if( vr == "UL" || vr == "SL" )
      {
         // for multivaluated items
         c = Util::CountSubstring(content, "\\") + 1;
         l = c*4;;
      }
   }
   valEntry->SetLength(l);
   return true;
} 

/**
 * \brief   Accesses an existing DocEntry (i.e. a Dicom Element)
 *          through it's (group, element) and modifies it's content with
 *          the given value.
 * @param   content new value (void*  -> uint8_t*) to substitute with
 * @param   lgth new value length
 * @param   group     group number of the Dicom Element to modify
 * @param   element element number of the Dicom Element to modify
 */
bool Document::SetEntryByNumber(uint8_t*content, int lgth, 
                                uint16_t group, uint16_t element) 
{
   (void)lgth;  //not used
   TagKey key = DictEntry::TranslateToKey(group, element);
   if ( !TagHT.count(key))
   {
      return false;
   }

/* Hope Binary field length is *never* wrong    
   if(lgth%2) // Non even length are padded with a space (020H).
   {  
      lgth++;
      //content = content + '\0'; // fing a trick to enlarge a binary field?
   }
*/      
   BinEntry* a = (BinEntry *)TagHT[key];           
   a->SetBinArea(content);  
   a->SetLength(lgth);
   a->SetValue(GDCM_BINLOADED);

   return true;
} 

/**
 * \brief   Accesses an existing DocEntry (i.e. a Dicom Element)
 *          in the PubDocEntrySet of this instance
 *          through it's (group, element) and modifies it's length with
 *          the given value.
 * \warning Use with extreme caution.
 * @param l new length to substitute with
 * @param group     group number of the Entry to modify
 * @param element element number of the Entry to modify
 * @return  true on success, false otherwise.
 */
bool Document::SetEntryLengthByNumber(uint32_t l, 
                                      uint16_t group, uint16_t element) 
{
   /// \todo use map methods, instead of multimap JPR
   TagKey key = DictEntry::TranslateToKey(group, element);
   if ( !TagHT.count(key) )
   {
      return false;
   }
   if ( l % 2 )
   {
      l++; // length must be even
   }
   ( ((TagHT.equal_range(key)).first)->second )->SetLength(l); 

   return true ;
}

/**
 * \brief   Gets (from Header) the offset  of a 'non string' element value 
 *          (LoadElementValues has already be executed)
 * @param group   group number of the Entry 
 * @param elem  element number of the Entry
 * @return File Offset of the Element Value 
 */
size_t Document::GetEntryOffsetByNumber(uint16_t group, uint16_t elem) 
{
   DocEntry* entry = GetDocEntryByNumber(group, elem);
   if (!entry) 
   {
      dbg.Verbose(1, "Document::GetDocEntryByNumber: no entry present.");
      return 0;
   }
   return entry->GetOffset();
}

/**
 * \brief   Gets (from Header) a 'non string' element value 
 *          (LoadElementValues has already be executed)  
 * @param group   group number of the Entry 
 * @param elem  element number of the Entry
 * @return Pointer to the 'non string' area
 */
void*  Document::GetEntryBinAreaByNumber(uint16_t group, uint16_t elem) 
{
   DocEntry* entry = GetDocEntryByNumber(group, elem);
   if (!entry) 
   {
      dbg.Verbose(1, "Document::GetDocEntryByNumber: no entry");
      return 0;
   }
   if ( BinEntry* binEntry = dynamic_cast<BinEntry*>(entry) )
   {
      return binEntry->GetBinArea();
   }

   return 0;
}

/**
 * \brief         Loads (from disk) the element content 
 *                when a string is not suitable
 * @param group   group number of the Entry 
 * @param elem  element number of the Entry
 */
void* Document::LoadEntryBinArea(uint16_t group, uint16_t elem)
{
   DocEntry *docElement = GetDocEntryByNumber(group, elem);
   if ( !docElement )
   {
      return NULL;
   }
   size_t o =(size_t)docElement->GetOffset();
   Fp->seekg( o, std::ios::beg);
   size_t l = docElement->GetLength();
   uint8_t* a = new uint8_t[l];
   if(!a)
   {
      dbg.Verbose(0, "Document::LoadEntryBinArea cannot allocate a");
      return NULL;
   }
   Fp->read((char*)a, l);
   if( Fp->fail() || Fp->eof() )//Fp->gcount() == 1
   {
      delete[] a;
      return NULL;
   }
  /// \todo Drop any already existing void area! JPR
   if( !SetEntryBinAreaByNumber( a, group, elem ) )
   {
      dbg.Verbose(0, "Document::LoadEntryBinArea setting failed.");
   }
   return a;
}
/**
 * \brief         Loads (from disk) the element content 
 *                when a string is not suitable
 * @param element  Entry whose binArea is going to be loaded
 */
void* Document::LoadEntryBinArea(BinEntry* element) 
{
   size_t o =(size_t)element->GetOffset();
   Fp->seekg(o, std::ios::beg);
   size_t l = element->GetLength();
   uint8_t* a = new uint8_t[l];
   if( !a )
   {
      dbg.Verbose(0, "Document::LoadEntryBinArea cannot allocate a");
      return NULL;
   }
   element->SetBinArea((uint8_t*)a);
   /// \todo check the result 
   Fp->read((char*)a, l);
   if( Fp->fail() || Fp->eof()) //Fp->gcount() == 1
   {
      delete[] a;
      return NULL;
   }

   return a;
}

/**
 * \brief   Sets a 'non string' value to a given Dicom Element
 * @param   area area containing the 'non string' value
 * @param   group     Group number of the searched Dicom Element 
 * @param   element Element number of the searched Dicom Element 
 * @return  
 */
bool Document::SetEntryBinAreaByNumber(uint8_t* area,
                                       uint16_t group, uint16_t element) 
{
   DocEntry* currentEntry = GetDocEntryByNumber(group, element);
   if ( !currentEntry )
   {
      return false;
   }
   if ( BinEntry* binEntry = dynamic_cast<BinEntry*>(currentEntry) )
   {
      binEntry->SetBinArea( area );
      return true;
   }
   return true;
}

/**
 * \brief   Update the entries with the shadow dictionary. 
 *          Only non even entries are analyzed       
 */
void Document::UpdateShaEntries()
{
   //DictEntry *entry;
   std::string vr;
   
   /// \todo TODO : still any use to explore recursively the whole structure?
/*
   for(ListTag::iterator it=listEntries.begin();
       it!=listEntries.end();
       ++it)
   {
      // Odd group => from public dictionary
      if((*it)->GetGroup()%2==0)
         continue;

      // Peer group => search the corresponding dict entry
      if(RefShaDict)
         entry=RefShaDict->GetDictEntryByNumber((*it)->GetGroup(),(*it)->GetElement());
      else
         entry=NULL;

      if((*it)->IsImplicitVR())
         vr="Implicit";
      else
         vr=(*it)->GetVR();

      (*it)->SetValue(GetDocEntryUnvalue(*it));  // to go on compiling
      if(entry){
         // Set the new entry and the new value
         (*it)->SetDictEntry(entry);
         CheckDocEntryVR(*it,vr);

         (*it)->SetValue(GetDocEntryValue(*it));    // to go on compiling
 
      }
      else
      {
         // Remove precedent value transformation
         (*it)->SetDictEntry(NewVirtualDictEntry((*it)->GetGroup(),(*it)->GetElement(),vr));
      }
   }
*/   
}

/**
 * \brief   Searches within the Header Entries for a Dicom Element of
 *          a given tag.
 * @param   tagName name of the searched Dicom Element.
 * @return  Corresponding Dicom Element when it exists, and NULL
 *          otherwise.
 */
DocEntry* Document::GetDocEntryByName(TagName const & tagName)
{
   DictEntry *dictEntry = RefPubDict->GetDictEntryByName(tagName); 
   if( !dictEntry )
   {
      return NULL;
   }

  return GetDocEntryByNumber(dictEntry->GetGroup(),dictEntry->GetElement());
}

/**
 * \brief  retrieves a Dicom Element (the first one) using (group, element)
 * \warning (group, element) IS NOT an identifier inside the Dicom Header
 *           if you think it's NOT UNIQUE, check the count number
 *           and use iterators to retrieve ALL the Dicoms Elements within
 *           a given couple (group, element)
 * @param   group Group number of the searched Dicom Element 
 * @param   element Element number of the searched Dicom Element 
 * @return  
 */
DocEntry* Document::GetDocEntryByNumber(uint16_t group, uint16_t element) 
{
   TagKey key = DictEntry::TranslateToKey(group, element);
   if ( !TagHT.count(key))
   {
      return NULL;
   }
   return TagHT.find(key)->second;
}

/**
 * \brief  Same as \ref Document::GetDocEntryByNumber except it only
 *         returns a result when the corresponding entry is of type
 *         ValEntry.
 * @return When present, the corresponding ValEntry. 
 */
ValEntry* Document::GetValEntryByNumber(uint16_t group, uint16_t element)
{
   DocEntry* currentEntry = GetDocEntryByNumber(group, element);
   if ( !currentEntry )
   {
      return 0;
   }
   if ( ValEntry* valEntry = dynamic_cast<ValEntry*>(currentEntry) )
   {
      return valEntry;
   }
   dbg.Verbose(0, "Document::GetValEntryByNumber: unfound ValEntry.");

   return 0;
}

/**
 * \brief         Loads the element while preserving the current
 *                underlying file position indicator as opposed to
 *                to LoadDocEntry that modifies it.
 * @param entry   Header Entry whose value shall be loaded. 
 * @return  
 */
void Document::LoadDocEntrySafe(DocEntry * entry)
{
   long PositionOnEntry = Fp->tellg();
   LoadDocEntry(entry);
   Fp->seekg(PositionOnEntry, std::ios::beg);
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
      case    0 :
         break;
      case 4321 :
         a=( ((a<<24) & 0xff000000) | ((a<<8)  & 0x00ff0000) | 
             ((a>>8)  & 0x0000ff00) | ((a>>24) & 0x000000ff) );
         break;
   
      case 3412 :
         a=( ((a<<16) & 0xffff0000) | ((a>>16) & 0x0000ffff) );
         break;
   
      case 2143 :
         a=( ((a<< 8) & 0xff00ff00) | ((a>>8) & 0x00ff00ff)  );
         break;
      default :
         //std::cout << "swapCode= " << SwapCode << std::endl;
         dbg.Error(" Document::SwapLong : unset swap code");
         a = 0;
   }
   return a;
} 

/**
 * \brief   Unswaps back the bytes of 4-byte long integer accordingly to
 *          processor order.
 * @return  The properly unswaped 32 bits integer.
 */
uint32_t Document::UnswapLong(uint32_t a)
{
   return SwapLong(a);
}

/**
 * \brief   Swaps the bytes so they agree with the processor order
 * @return  The properly swaped 16 bits integer.
 */
uint16_t Document::SwapShort(uint16_t a)
{
   if ( SwapCode == 4321 || SwapCode == 2143 )
   {
      a = ((( a << 8 ) & 0x0ff00 ) | (( a >> 8 ) & 0x00ff ) );
   }
   return a;
}

/**
 * \brief   Unswaps the bytes so they agree with the processor order
 * @return  The properly unswaped 16 bits integer.
 */
uint16_t Document::UnswapShort(uint16_t a)
{
   return SwapShort(a);
}

//-----------------------------------------------------------------------------
// Private

/**
 * \brief   Parses a DocEntrySet (Zero-level DocEntries or SQ Item DocEntries)
 * @return  length of the parsed set. 
 */ 
void Document::ParseDES(DocEntrySet *set, long offset, 
                        long l_max, bool delim_mode)
{
   DocEntry *newDocEntry = 0;
   
   while (true)
   { 
      if ( !delim_mode && ((long)Fp->tellg()-offset) >= l_max)
      {
         break;
      }
      newDocEntry = ReadNextDocEntry( );
      if ( !newDocEntry )
      {
         break;
      }

      VRKey vr = newDocEntry->GetVR();
      if ( vr != "SQ" )
      {
               
         if ( Global::GetVR()->IsVROfGdcmStringRepresentable(vr) )
         {
         /////////////////////// ValEntry
            ValEntry* newValEntry =
               new ValEntry( newDocEntry->GetDictEntry() );
            newValEntry->Copy( newDocEntry );
             
            // When "set" is a Document, then we are at the top of the
            // hierarchy and the Key is simply of the form ( group, elem )...
            if (Document* dummy = dynamic_cast< Document* > ( set ) )
            {
               (void)dummy;
               newValEntry->SetKey( newValEntry->GetKey() );
            }
            // ...but when "set" is a SQItem, we are inserting this new
            // valEntry in a sequence item. Hence the key has the
            // generalized form (refer to \ref BaseTagKey):
            if (SQItem* parentSQItem = dynamic_cast< SQItem* > ( set ) )
            {
               newValEntry->SetKey(  parentSQItem->GetBaseTagKey()
                                   + newValEntry->GetKey() );
            }
             
            set->AddEntry( newValEntry );
            LoadDocEntry( newValEntry );
            if (newValEntry->IsItemDelimitor())
            {
               break;
            }
            if ( !delim_mode && ((long)Fp->tellg()-offset) >= l_max)
            {
               break;
            }
         }
         else
         {
            if ( ! Global::GetVR()->IsVROfGdcmBinaryRepresentable(vr) )
            { 
                ////// Neither ValEntry NOR BinEntry: should mean UNKOWN VR
                dbg.Verbose(0, "Document::ParseDES: neither Valentry, "
                               "nor BinEntry. Probably unknown VR.");
            }

         //////////////////// BinEntry or UNKOWN VR:
            BinEntry* newBinEntry =
               new BinEntry( newDocEntry->GetDictEntry() );  //LEAK
            newBinEntry->Copy( newDocEntry );

            // When "this" is a Document the Key is simply of the
            // form ( group, elem )...
            if (Document* dummy = dynamic_cast< Document* > ( set ) )
            {
               (void)dummy;
               newBinEntry->SetKey( newBinEntry->GetKey() );
            }
            // but when "this" is a SQItem, we are inserting this new
            // valEntry in a sequence item, and the kay has the
            // generalized form (refer to \ref BaseTagKey):
            if (SQItem* parentSQItem = dynamic_cast< SQItem* > ( set ) )
            {
               newBinEntry->SetKey(  parentSQItem->GetBaseTagKey()
                                   + newBinEntry->GetKey() );
            }

            set->AddEntry( newBinEntry );
            LoadDocEntry( newBinEntry );
         }

         if (    ( newDocEntry->GetGroup()   == 0x7fe0 )
              && ( newDocEntry->GetElement() == 0x0010 ) )
         {
             TransferSyntaxType ts = GetTransferSyntax();
             if ( ts == RLELossless ) 
             {
                long PositionOnEntry = Fp->tellg();
                Fp->seekg( newDocEntry->GetOffset(), std::ios::beg );
                ComputeRLEInfo();
                Fp->seekg( PositionOnEntry, std::ios::beg );
             }
             else if ( IsJPEG() )
             {
                long PositionOnEntry = Fp->tellg();
                Fp->seekg( newDocEntry->GetOffset(), std::ios::beg );
                ComputeJPEGFragmentInfo();
                Fp->seekg( PositionOnEntry, std::ios::beg );
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
         SeqEntry* newSeqEntry =
            new SeqEntry( newDocEntry->GetDictEntry() );
         newSeqEntry->Copy( newDocEntry );
         newSeqEntry->SetDelimitorMode( delim_mode );

         // At the top of the hierarchy, stands a Document. When "set"
         // is a Document, then we are building the first depth level.
         // Hence the SeqEntry we are building simply has a depth
         // level of one:
         if (Document* dummy = dynamic_cast< Document* > ( set ) )
         {
            (void)dummy;
            newSeqEntry->SetDepthLevel( 1 );
            newSeqEntry->SetKey( newSeqEntry->GetKey() );
         }
         // But when "set" is allready a SQItem, we are building a nested
         // sequence, and hence the depth level of the new SeqEntry
         // we are building, is one level deeper:
         if (SQItem* parentSQItem = dynamic_cast< SQItem* > ( set ) )
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
         set->AddEntry( newSeqEntry );
         if ( !delim_mode && ((long)Fp->tellg()-offset) >= l_max)
         {
            break;
         }
      }
      delete newDocEntry;
   }
}

/**
 * \brief   Parses a Sequence ( SeqEntry after SeqEntry)
 * @return  parsed length for this level
 */ 
void Document::ParseSQ( SeqEntry* seqEntry,
                        long offset, long l_max, bool delim_mode)
{
   int SQItemNumber = 0;
   bool dlm_mod;

   while (true)
   {
      DocEntry* newDocEntry = ReadNextDocEntry();   
      if ( !newDocEntry )
      {
         // FIXME Should warn user
         break;
      }
      if( delim_mode )
      {
         if ( newDocEntry->IsSequenceDelimitor() )
         {
            seqEntry->SetSequenceDelimitationItem( newDocEntry );
            break;
         }
      }
      if ( !delim_mode && ((long)Fp->tellg()-offset) >= l_max)
      {
          break;
      }

      SQItem *itemSQ = new SQItem( seqEntry->GetDepthLevel() );
      std::ostringstream newBase;
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
   
      ParseDES(itemSQ, newDocEntry->GetOffset(), l, dlm_mod);
      
      seqEntry->AddEntry( itemSQ, SQItemNumber ); 
      SQItemNumber++;
      if ( !delim_mode && ( (long)Fp->tellg() - offset ) >= l_max )
      {
         break;
      }
   }
}

/**
 * \brief         Loads the element content if its length doesn't exceed
 *                the value specified with Document::SetMaxSizeLoadEntry()
 * @param         entry Header Entry (Dicom Element) to be dealt with
 */
void Document::LoadDocEntry(DocEntry* entry)
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

   std::ostringstream s;
   if (length > MaxSizeLoadEntry)
   {
      if (BinEntry* binEntryPtr = dynamic_cast< BinEntry* >(entry) )
      {  
         //s << "gdcm::NotLoaded (BinEntry)";
         s << GDCM_NOTLOADED;
         s << " Address:" << (long)entry->GetOffset();
         s << " Length:"  << entry->GetLength();
         s << " x(" << std::hex << entry->GetLength() << ")";
         binEntryPtr->SetValue(s.str());
      }
       // Be carefull : a BinEntry IS_A ValEntry ... 
      else if (ValEntry* valEntryPtr = dynamic_cast< ValEntry* >(entry) )
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
         std::cout<< "MaxSizeLoadEntry exceeded, neither a BinEntry "
                  << "nor a ValEntry ?! Should never print that !" << std::endl;
      }

      // to be sure we are at the end of the value ...
      Fp->seekg((long)entry->GetOffset()+(long)entry->GetLength(),
                std::ios::beg);
      return;
   }

   // When we find a BinEntry not very much can be done :
   if (BinEntry* binEntryPtr = dynamic_cast< BinEntry* >(entry) )
   {
      s << GDCM_BINLOADED;
      binEntryPtr->SetValue(s.str());
      LoadEntryBinArea(binEntryPtr); // last one, not to erase length !
      return;
   }
    
   /// \todo Any compacter code suggested (?)
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
      //dbg.Verbose(0, "Warning: bad length: ", length );
      dbg.Verbose(0, "For string :",  newValue.c_str()); 
      // Since we change the length of string update it length
      entry->SetReadLength(length+1);
   }
   else
   {
      newValue = Util::DicomString(str, length);
   }
   delete[] str;

   if ( ValEntry* valEntry = dynamic_cast<ValEntry* >(entry) )
   {
      if ( Fp->fail() || Fp->eof())//Fp->gcount() == 1
      {
         dbg.Verbose(1, "Document::LoadDocEntry",
                        "unread element value");
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
      dbg.Error(true, "Document::LoadDocEntry"
                      "Should have a ValEntry, here !");
   }
}


/**
 * \brief  Find the value Length of the passed Header Entry
 * @param  entry Header Entry whose length of the value shall be loaded. 
 */
void Document::FindDocEntryLength( DocEntry *entry )
   throw ( FormatError )
{
   uint16_t element = entry->GetElement();
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
               /// \todo rename that to FindDocEntryLengthOBOrOW since
               ///       the above test is on both OB and OW...
               lengthOB = FindDocEntryLengthOB();
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
               long lengthUntilEOF = (long)Fp->tellg() - currentPosition;
               Fp->seekg(currentPosition, std::ios::beg);
               entry->SetLength(lengthUntilEOF);
               return;
            }
            entry->SetLength(lengthOB);
            return;
         }
         FixDocEntryFoundLength(entry, length32); 
         return;
      }

      // Length is encoded on 2 bytes.
      length16 = ReadInt16();
      
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
      // We shall use this second strategy. In order to make sure that we
      // can interpret the presence of an apparently big endian encoded
      // length of a "Group Length" without committing a big mistake, we
      // add an additional check: we look in the already parsed elements
      // for the presence of a "Transfer Syntax" whose value has to be "big
      // endian encoding". When this is the case, chances are we have got our
      // hands on a big endian encoded file: we switch the swap code to
      // big endian and proceed...
      if ( element  == 0x0000 && length16 == 0x0400 ) 
      {
         TransferSyntaxType ts = GetTransferSyntax();
         if ( ts != ExplicitVRBigEndian ) 
         {
            throw FormatError( "Document::FindDocEntryLength()",
                               " not explicit VR." );
            return;
         }
         length16 = 4;
         SwitchSwapToBigEndian();
         // Restore the unproperly loaded values i.e. the group, the element
         // and the dictionary entry depending on them.
         uint16_t correctGroup = SwapShort( entry->GetGroup() );
         uint16_t correctElem  = SwapShort( entry->GetElement() );
         DictEntry* newTag = GetDictEntryByNumber( correctGroup,
                                                       correctElem );
         if ( !newTag )
         {
            // This correct tag is not in the dictionary. Create a new one.
            newTag = NewVirtualDictEntry(correctGroup, correctElem);
         }
         // FIXME this can create a memory leaks on the old entry that be
         // left unreferenced.
         entry->SetDictEntry( newTag );
      }
       
      // Heuristic: well, some files are really ill-formed.
      if ( length16 == 0xffff) 
      {
         // 0xffff means that we deal with 'Unknown Length' Sequence  
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
      
      FixDocEntryFoundLength( entry, ReadInt32() );
      return;
   }
}

/**
 * \brief     Find the Value Representation of the current Dicom Element.
 * @param     entry
 */
void Document::FindDocEntryVR( DocEntry *entry )
{
   if ( Filetype != ExplicitVR )
   {
      return;
   }

   char vr[3];

   long positionOnEntry = Fp->tellg();
   // Warning: we believe this is explicit VR (Value Representation) because
   // we used a heuristic that found "UL" in the first tag. Alas this
   // doesn't guarantee that all the tags will be in explicit VR. In some
   // cases (see e-film filtered files) one finds implicit VR tags mixed
   // within an explicit VR file. Hence we make sure the present tag
   // is in explicit VR and try to fix things if it happens not to be
   // the case.
   
   Fp->read (vr, (size_t)2);
   vr[2] = 0;

   if( !CheckDocEntryVR(entry, vr) )
   {
      Fp->seekg(positionOnEntry, std::ios::beg);
      // When this element is known in the dictionary we shall use, e.g. for
      // the semantics (see the usage of IsAnInteger), the VR proposed by the
      // dictionary entry. Still we have to flag the element as implicit since
      // we know now our assumption on expliciteness is not furfilled.
      // avoid  .
      if ( entry->IsVRUnknown() )
      {
         entry->SetVR("Implicit");
      }
      entry->SetImplicitVR();
   }
}

/**
 * \brief     Check the correspondance between the VR of the header entry
 *            and the taken VR. If they are different, the header entry is 
 *            updated with the new VR.
 * @param     entry Header Entry to check
 * @param     vr    Dicom Value Representation
 * @return    false if the VR is incorrect of if the VR isn't referenced
 *            otherwise, it returns true
*/
bool Document::CheckDocEntryVR(DocEntry *entry, VRKey vr)
{
   std::string msg;
   bool realExplicit = true;

   // Assume we are reading a falsely explicit VR file i.e. we reached
   // a tag where we expect reading a VR but are in fact we read the
   // first to bytes of the length. Then we will interogate (through find)
   // the dicom_vr dictionary with oddities like "\004\0" which crashes
   // both GCC and VC++ implementations of the STL map. Hence when the
   // expected VR read happens to be non-ascii characters we consider
   // we hit falsely explicit VR tag.

   if ( !isalpha(vr[0]) && !isalpha(vr[1]) )
   {
      realExplicit = false;
   }

   // CLEANME searching the dicom_vr at each occurence is expensive.
   // PostPone this test in an optional integrity check at the end
   // of parsing or only in debug mode.
   if ( realExplicit && !Global::GetVR()->Count(vr) )
   {
      realExplicit = false;
   }

   if ( !realExplicit ) 
   {
      // We thought this was explicit VR, but we end up with an
      // implicit VR tag. Let's backtrack.   
      msg = Util::Format("Falsely explicit vr file (%04x,%04x)\n", 
                    entry->GetGroup(), entry->GetElement());
      dbg.Verbose(1, "Document::FindVR: ", msg.c_str());

      if( entry->GetGroup() % 2 && entry->GetElement() == 0x0000)
      {
         // Group length is UL !
         DictEntry* newEntry = NewVirtualDictEntry(
                                   entry->GetGroup(), entry->GetElement(),
                                   "UL", "FIXME", "Group Length");
         entry->SetDictEntry( newEntry );
      }
      return false;
   }

   if ( entry->IsVRUnknown() )
   {
      // When not a dictionary entry, we can safely overwrite the VR.
      if( entry->GetElement() == 0x0000 )
      {
         // Group length is UL !
         entry->SetVR("UL");
      }
      else
      {
         entry->SetVR(vr);
      }
   }
   else if ( entry->GetVR() != vr ) 
   {
      // The VR present in the file and the dictionary disagree. We assume
      // the file writer knew best and use the VR of the file. Since it would
      // be unwise to overwrite the VR of a dictionary (since it would
      // compromise it's next user), we need to clone the actual DictEntry
      // and change the VR for the read one.
      DictEntry* newEntry = NewVirtualDictEntry(
                                entry->GetGroup(), entry->GetElement(),
                                vr, "FIXME", entry->GetName());
      entry->SetDictEntry(newEntry);
   }

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
      std::ostringstream s;
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
std::string Document::GetDocEntryUnvalue(DocEntry* entry)
{
   if ( IsDocEntryAnInteger(entry) && entry->IsImplicitVR() )
   {
      std::string vr = entry->GetVR();
      std::vector<std::string> tokens;
      std::ostringstream s;

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
 * \brief   Skips to the begining of the next Header Entry 
 * \warning NOT end user intended method !
 * @param   entry entry to skip
 */
void Document::SkipToNextDocEntry(DocEntry *entry) 
{
   Fp->seekg((long)(entry->GetOffset()),     std::ios::beg);
   Fp->seekg( (long)(entry->GetReadLength()), std::ios::cur);
}

/**
 * \brief   When the length of an element value is obviously wrong (because
 *          the parser went Jabberwocky) one can hope improving things by
 *          applying some heuristics.
 * @param   entry entry to check
 * @param   foundLength fist assumption about length    
 */
void Document::FixDocEntryFoundLength(DocEntry *entry,
                                      uint32_t foundLength)
{
   entry->SetReadLength( foundLength ); // will be updated only if a bug is found        
   if ( foundLength == 0xffffffff)
   {
      foundLength = 0;
   }
   
   uint16_t gr = entry->GetGroup();
   uint16_t el = entry->GetElement(); 
     
   if ( foundLength % 2)
   {
      std::ostringstream s;
      s << "Warning : Tag with uneven length "
        << foundLength 
        <<  " in x(" << std::hex << gr << "," << el <<")" << std::dec;
      dbg.Verbose(0, s.str().c_str());
   }
      
   //////// Fix for some naughty General Electric images.
   // Allthough not recent many such GE corrupted images are still present
   // on Creatis hard disks. Hence this fix shall remain when such images
   // are no longer in user (we are talking a few years, here)...
   // Note: XMedCom probably uses such a trick since it is able to read
   //       those pesky GE images ...
   if ( foundLength == 13)
   {
      // Only happens for this length !
      if ( entry->GetGroup()   != 0x0008
      || ( entry->GetElement() != 0x0070
        && entry->GetElement() != 0x0080 ) )
      {
         foundLength = 10;
         entry->SetReadLength(10); /// \todo a bug is to be fixed !?
      }
   }

   //////// Fix for some brain-dead 'Leonardo' Siemens images.
   // Occurence of such images is quite low (unless one leaves close to a
   // 'Leonardo' source. Hence, one might consider commenting out the
   // following fix on efficiency reasons.
   else if ( entry->GetGroup()   == 0x0009 
        && ( entry->GetElement() == 0x1113
          || entry->GetElement() == 0x1114 ) )
   {
      foundLength = 4;
      entry->SetReadLength(4); /// \todo a bug is to be fixed !?
   } 
 
   else if ( entry->GetVR() == "SQ" )
   {
      foundLength = 0;      // ReadLength is unchanged 
   } 
    
   //////// We encountered a 'delimiter' element i.e. a tag of the form 
   // "fffe|xxxx" which is just a marker. Delimiters length should not be
   // taken into account.
   else if( entry->GetGroup() == 0xfffe )
   {    
     // According to the norm, fffe|0000 shouldn't exist. BUT the Philips
     // image gdcmData/gdcm-MR-PHILIPS-16-Multi-Seq.dcm happens to
     // causes extra troubles...
     if( entry->GetElement() != 0x0000 )
     {
        foundLength = 0;
     }
   } 
           
   entry->SetUsableLength(foundLength);
}

/**
 * \brief   Apply some heuristics to predict whether the considered 
 *          element value contains/represents an integer or not.
 * @param   entry The element value on which to apply the predicate.
 * @return  The result of the heuristical predicate.
 */
bool Document::IsDocEntryAnInteger(DocEntry *entry)
{
   uint16_t element = entry->GetElement();
   uint16_t group   = entry->GetGroup();
   const std::string & vr  = entry->GetVR();
   uint32_t length  = entry->GetLength();

   // When we have some semantics on the element we just read, and if we
   // a priori know we are dealing with an integer, then we shall be
   // able to swap it's element value properly.
   if ( element == 0 )  // This is the group length of the group
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
         std::ostringstream s;
         long filePosition = Fp->tellg();
         s << "Erroneous Group Length element length  on : (" \
           << std::hex << group << " , " << element 
           << ") -before- position x(" << filePosition << ")"
           << "lgt : " << length;
         dbg.Verbose(0, "Document::IsDocEntryAnInteger", s.str().c_str() );
      }
   }

   if ( vr == "UL" || vr == "US" || vr == "SL" || vr == "SS" )
   {
      return true;
   }
   
   return false;
}

/**
 * \brief  Find the Length till the next sequence delimiter
 * \warning NOT end user intended method !
 * @return 
 */

uint32_t Document::FindDocEntryLengthOB()
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
         throw FormatError("Document::FindDocEntryLengthOB()",
                           " group or element not present.");
      }

      // We have to decount the group and element we just read
      totalLength += 4;
     
      if ( group != 0xfffe || ( ( elem != 0xe0dd ) && ( elem != 0xe000 ) ) )
      {
         dbg.Verbose(1, "Document::FindDocEntryLengthOB: neither an Item "
                        "tag nor a Sequence delimiter tag."); 
         Fp->seekg(positionOnEntry, std::ios::beg);
         throw FormatUnexpected("Document::FindDocEntryLengthOB()",
                                "Neither an Item tag nor a Sequence "
                                "delimiter tag.");
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
 * \brief Reads a supposed to be 16 Bits integer
 *       (swaps it depending on processor endianity) 
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
 *         (swaps it depending on processor endianity)  
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
 * \brief Loads all the needed Dictionaries
 * \warning NOT end user intended method !   
 */
void Document::Initialise() 
{
   RefPubDict = Global::GetDicts()->GetDefaultPubDict();
   RefShaDict = NULL;
   RLEInfo  = new RLEFramesInfo;
   JPEGInfo = new JPEGFragmentsInfo;
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
   
   uint32_t  x = 4;  // x : for ntohs
   bool net2host; // true when HostByteOrder is the same as NetworkByteOrder
   uint32_t  s32;
   uint16_t  s16;
       
   char deb[256];
    
   // First, compare HostByteOrder and NetworkByteOrder in order to
   // determine if we shall need to swap bytes (i.e. the Endian type).
   if ( x == ntohs(x) )
   {
      net2host = true;
   }
   else
   {
      net2host = false;
   }
         
   // The easiest case is the one of a DICOM header, since it possesses a
   // file preamble where it suffice to look for the string "DICM".
   Fp->read(deb, 256);
   
   char *entCur = deb + 128;
   if( memcmp(entCur, "DICM", (size_t)4) == 0 )
   {
      dbg.Verbose(1, "Document::CheckSwap:", "looks like DICOM Version3");
      
      // Next, determine the value representation (VR). Let's skip to the
      // first element (0002, 0000) and check there if we find "UL" 
      // - or "OB" if the 1st one is (0002,0001) -,
      // in which case we (almost) know it is explicit VR.
      // WARNING: if it happens to be implicit VR then what we will read
      // is the length of the group. If this ascii representation of this
      // length happens to be "UL" then we shall believe it is explicit VR.
      // FIXME: in order to fix the above warning, we could read the next
      // element value (or a couple of elements values) in order to make
      // sure we are not commiting a big mistake.
      // We need to skip :
      // * the 128 bytes of File Preamble (often padded with zeroes),
      // * the 4 bytes of "DICM" string,
      // * the 4 bytes of the first tag (0002, 0000),or (0002, 0001)
      // i.e. a total of  136 bytes.
      entCur = deb + 136;
     
      // FIXME : FIXME:
      // Sometimes (see : gdcmData/icone.dcm) group 0x0002 *is* Explicit VR,
      // but elem 0002,0010 (Transfert Syntax) tells us the file is
      // *Implicit* VR.  -and it is !- 
      
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
         dbg.Verbose(1, "Document::CheckSwap:",
                     "explicit Value Representation");
      } 
      else 
      {
         Filetype = ImplicitVR;
         dbg.Verbose(1, "Document::CheckSwap:",
                     "not an explicit Value Representation");
      }
      
      if ( net2host )
      {
         SwapCode = 4321;
         dbg.Verbose(1, "Document::CheckSwap:",
                        "HostByteOrder != NetworkByteOrder");
      }
      else 
      {
         SwapCode = 0;
         dbg.Verbose(1, "Document::CheckSwap:",
                        "HostByteOrder = NetworkByteOrder");
      }
      
      // Position the file position indicator at first tag (i.e.
      // after the file preamble and the "DICM" string).
      Fp->seekg(0, std::ios::beg);
      Fp->seekg ( 132L, std::ios::beg);
      return true;
   } // End of DicomV3

   // Alas, this is not a DicomV3 file and whatever happens there is no file
   // preamble. We can reset the file position indicator to where the data
   // is (i.e. the beginning of the file).
   dbg.Verbose(1, "Document::CheckSwap:", "not a DICOM Version3 file");
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
         SwapCode = 0;
         Filetype = ACR;
         return true;
      default :
         // We are out of luck. It is not a DicomV3 nor a 'clean' ACR/NEMA file.
         // It is time for despaired wild guesses. 
         // So, let's check if this file wouldn't happen to be 'dirty' ACR/NEMA,
         //  i.e. the 'group length' element is not present :     
         
         //  check the supposed to be 'group number'
         //  0x0002 or 0x0004 or 0x0008
         //  to determine ' SwapCode' value .
         //  Only 0 or 4321 will be possible 
         //  (no oportunity to check for the formerly well known
         //  ACR-NEMA 'Bad Big Endian' or 'Bad Little Endian' 
         //  if unsuccessfull (i.e. neither 0x0002 nor 0x0200 etc -4, 8-) 
         //  the file IS NOT ACR-NEMA nor DICOM V3
         //  Find a trick to tell it the caller...
      
         s16 = *((uint16_t *)(deb));
      
         switch ( s16 )
         {
            case 0x0002 :
            case 0x0004 :
            case 0x0008 :      
               SwapCode = 0;
               Filetype = ACR;
               return true;
            case 0x0200 :
            case 0x0400 :
            case 0x0800 : 
               SwapCode = 4321;
               Filetype = ACR;
               return true;
            default :
               dbg.Verbose(0, "Document::CheckSwap:",
                     "ACR/NEMA unfound swap info (Really hopeless !)"); 
               Filetype = Unknown;     
               return false;
         }
         // Then the only info we have is the net2host one.
         //if (! net2host )
         //   SwapCode = 0;
         //else
         //  SwapCode = 4321;
         //return;
   }
}

/**
 * \brief Restore the unproperly loaded values i.e. the group, the element
 *        and the dictionary entry depending on them. 
 */
void Document::SwitchSwapToBigEndian() 
{
   dbg.Verbose(1, "Document::SwitchSwapToBigEndian",
                  "Switching to BigEndian mode.");
   if ( SwapCode == 0    ) 
   {
      SwapCode = 4321;
   }
   else if ( SwapCode == 4321 ) 
   {
      SwapCode = 0;
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
 * @param newSize
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
 * \todo  See comments of \ref Document::MAX_SIZE_PRINT_ELEMENT_VALUE 
 * @param newSize
 */
void Document::SetMaxSizePrintEntry(long newSize) 
{
   //DOH !! This is exactly SetMaxSizeLoadEntry FIXME FIXME
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
 * \brief   Handle broken private tag from Philips NTSCAN
 *          where the endianess is being switch to BigEndian for no
 *          apparent reason
 * @return  no return
 */
void Document::HandleBrokenEndian(uint16_t group, uint16_t elem)
{
   // Endian reversion. Some files contain groups of tags with reversed endianess.
   static int reversedEndian = 0;
   // try to fix endian switching in the middle of headers
   if ((group == 0xfeff) && (elem == 0x00e0))
   {
     // start endian swap mark for group found
     reversedEndian++;
     SwitchSwapToBigEndian();
     // fix the tag
     group = 0xfffe;
     elem = 0xe000;
   } 
   else if ((group == 0xfffe) && (elem == 0xe00d) && reversedEndian) 
   {
     // end of reversed endian group
     reversedEndian--;
     SwitchSwapToBigEndian();
   }

}

/**
 * \brief   Read the next tag but WITHOUT loading it's value
 *          (read the 'Group Number', the 'Element Number',
 *           gets the Dict Entry
 *          gets the VR, gets the length, gets the offset value)
 * @return  On succes the newly created DocEntry, NULL on failure.      
 */
DocEntry* Document::ReadNextDocEntry()
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
      //std::cout << e;
      return 0;
   }

   HandleBrokenEndian(group, elem);
   DocEntry *newEntry = NewDocEntryByNumber(group, elem);
   FindDocEntryVR(newEntry);

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
 * \brief   Generate a free TagKey i.e. a TagKey that is not present
 *          in the TagHt dictionary.
 * @param   group The generated tag must belong to this group.  
 * @return  The element of tag with given group which is fee.
 */
uint32_t Document::GenerateFreeTagKeyInGroup(uint16_t group) 
{
   for (uint32_t elem = 0; elem < UINT32_MAX; elem++) 
   {
      TagKey key = DictEntry::TranslateToKey(group, elem);
      if (TagHT.count(key) == 0)
      {
         return elem;
      }
   }
   return UINT32_MAX;
}

/**
 * \brief   Assuming the internal file pointer \ref Document::Fp 
 *          is placed at the beginning of a tag check whether this
 *          tag is (TestGroup, TestElement).
 * \warning On success the internal file pointer \ref Document::Fp
 *          is modified to point after the tag.
 *          On failure (i.e. when the tag wasn't the expected tag
 *          (TestGroup, TestElement) the internal file pointer
 *          \ref Document::Fp is restored to it's original position.
 * @param   testGroup   The expected group of the tag.
 * @param   testElement The expected Element of the tag.
 * @return  True on success, false otherwise.
 */
bool Document::ReadTag(uint16_t testGroup, uint16_t testElement)
{
   long positionOnEntry = Fp->tellg();
   long currentPosition = Fp->tellg();          // On debugging purposes

   //// Read the Item Tag group and element, and make
   // sure they are what we expected:
   uint16_t itemTagGroup;
   uint16_t itemTagElement;
   try
   {
      itemTagGroup   = ReadInt16();
      itemTagElement = ReadInt16();
   }
   catch ( FormatError e )
   {
      //std::cerr << e << std::endl;
      return false;
   }
   if ( itemTagGroup != testGroup || itemTagElement != testElement )
   {
      std::ostringstream s;
      s << "   We should have found tag (";
      s << std::hex << testGroup << "," << testElement << ")" << std::endl;
      s << "   but instead we encountered tag (";
      s << std::hex << itemTagGroup << "," << itemTagElement << ")"
        << std::endl;
      s << "  at address: " << (unsigned)currentPosition << std::endl;
      dbg.Verbose(0, "Document::ReadItemTagLength: wrong Item Tag found:");
      dbg.Verbose(0, s.str().c_str());
      Fp->seekg(positionOnEntry, std::ios::beg);

      return false;
   }
   return true;
}

/**
 * \brief   Assuming the internal file pointer \ref Document::Fp 
 *          is placed at the beginning of a tag (TestGroup, TestElement),
 *          read the length associated to the Tag.
 * \warning On success the internal file pointer \ref Document::Fp
 *          is modified to point after the tag and it's length.
 *          On failure (i.e. when the tag wasn't the expected tag
 *          (TestGroup, TestElement) the internal file pointer
 *          \ref Document::Fp is restored to it's original position.
 * @param   testGroup   The expected group of the tag.
 * @param   testElement The expected Element of the tag.
 * @return  On success returns the length associated to the tag. On failure
 *          returns 0.
 */
uint32_t Document::ReadTagLength(uint16_t testGroup, uint16_t testElement)
{
   long positionOnEntry = Fp->tellg();
   (void)positionOnEntry;

   if ( !ReadTag(testGroup, testElement) )
   {
      return 0;
   }
                                                                                
   //// Then read the associated Item Length
   long currentPosition = Fp->tellg();
   uint32_t itemLength  = ReadInt32();
   {
      std::ostringstream s;
      s << "Basic Item Length is: "
        << itemLength << std::endl;
      s << "  at address: " << (unsigned)currentPosition << std::endl;
      dbg.Verbose(0, "Document::ReadItemTagLength: ", s.str().c_str());
   }
   return itemLength;
}

/**
 * \brief When parsing the Pixel Data of an encapsulated file, read
 *        the basic offset table (when present, and BTW dump it).
 */
void Document::ReadAndSkipEncapsulatedBasicOffsetTable()
{
   //// Read the Basic Offset Table Item Tag length...
   uint32_t itemLength = ReadTagLength(0xfffe, 0xe000);

   // When present, read the basic offset table itself.
   // Notes: - since the presence of this basic offset table is optional
   //          we can't rely on it for the implementation, and we will simply
   //          trash it's content (when present).
   //        - still, when present, we could add some further checks on the
   //          lengths, but we won't bother with such fuses for the time being.
   if ( itemLength != 0 )
   {
      char* basicOffsetTableItemValue = new char[itemLength + 1];
      Fp->read(basicOffsetTableItemValue, itemLength);

#ifdef GDCM_DEBUG
      for (unsigned int i=0; i < itemLength; i += 4 )
      {
         uint32_t individualLength = str2num( &basicOffsetTableItemValue[i],
                                              uint32_t);
         std::ostringstream s;
         s << "   Read one length: ";
         s << std::hex << individualLength << std::endl;
         dbg.Verbose(0,
                     "Document::ReadAndSkipEncapsulatedBasicOffsetTable: ",
                     s.str().c_str());
      }
#endif //GDCM_DEBUG

      delete[] basicOffsetTableItemValue;
   }
}

/**
 * \brief Parse pixel data from disk of [multi-]fragment RLE encoding.
 *        Compute the RLE extra information and store it in \ref RLEInfo
 *        for later pixel retrieval usage.
 */
void Document::ComputeRLEInfo()
{
   TransferSyntaxType ts = GetTransferSyntax();
   if ( ts != RLELossless )
   {
      return;
   }

   // Encoded pixel data: for the time being we are only concerned with
   // Jpeg or RLE Pixel data encodings.
   // As stated in PS 3.5-2003, section 8.2 p44:
   // "If sent in Encapsulated Format (i.e. other than the Native Format) the
   //  value representation OB is used".
   // Hence we expect an OB value representation. Concerning OB VR,
   // the section PS 3.5-2003, section A.4.c p 58-59, states:
   // "For the Value Representations OB and OW, the encoding shall meet the
   //   following specifications depending on the Data element tag:"
   //   [...snip...]
   //    - the first item in the sequence of items before the encoded pixel
   //      data stream shall be basic offset table item. The basic offset table
   //      item value, however, is not required to be present"

   ReadAndSkipEncapsulatedBasicOffsetTable();

   // Encapsulated RLE Compressed Images (see PS 3.5-2003, Annex G)
   // Loop on the individual frame[s] and store the information
   // on the RLE fragments in a RLEFramesInfo.
   // Note: - when only a single frame is present, this is a
   //         classical image.
   //       - when more than one frame are present, then we are in 
   //         the case of a multi-frame image.
   long frameLength;
   while ( (frameLength = ReadTagLength(0xfffe, 0xe000)) )
   { 
      // Parse the RLE Header and store the corresponding RLE Segment
      // Offset Table information on fragments of this current Frame.
      // Note that the fragment pixels themselves are not loaded
      // (but just skipped).
      long frameOffset = Fp->tellg();

      uint32_t nbRleSegments = ReadInt32();
      if ( nbRleSegments > 16 )
      {
         // There should be at most 15 segments (refer to RLEFrame class)
         dbg.Verbose(0, "Document::ComputeRLEInfo: too many segments.");
      }
 
      uint32_t rleSegmentOffsetTable[15];
      for( int k = 1; k <= 15; k++ )
      {
         rleSegmentOffsetTable[k] = ReadInt32();
      }

      // Deduce from both the RLE Header and the frameLength the
      // fragment length, and again store this info in a
      // RLEFramesInfo.
      long rleSegmentLength[15];
      // skipping (not reading) RLE Segments
      if ( nbRleSegments > 1)
      {
         for(unsigned int k = 1; k <= nbRleSegments-1; k++)
         {
             rleSegmentLength[k] =  rleSegmentOffsetTable[k+1]
                                  - rleSegmentOffsetTable[k];
             SkipBytes(rleSegmentLength[k]);
          }
       }

       rleSegmentLength[nbRleSegments] = frameLength 
                                      - rleSegmentOffsetTable[nbRleSegments];
       SkipBytes(rleSegmentLength[nbRleSegments]);

       // Store the collected info
       RLEFrame* newFrameInfo = new RLEFrame;
       newFrameInfo->NumberFragments = nbRleSegments;
       for( unsigned int uk = 1; uk <= nbRleSegments; uk++ )
       {
          newFrameInfo->Offset[uk] = frameOffset + rleSegmentOffsetTable[uk];
          newFrameInfo->Length[uk] = rleSegmentLength[uk];
       }
       RLEInfo->Frames.push_back( newFrameInfo );
   }

   // Make sure that at the end of the item we encounter a 'Sequence
   // Delimiter Item':
   if ( !ReadTag(0xfffe, 0xe0dd) )
   {
      dbg.Verbose(0, "Document::ComputeRLEInfo: no sequence delimiter ");
      dbg.Verbose(0, "    item at end of RLE item sequence");
   }
}

/**
 * \brief Parse pixel data from disk of [multi-]fragment Jpeg encoding.
 *        Compute the jpeg extra information (fragment[s] offset[s] and
 *        length) and store it[them] in \ref JPEGInfo for later pixel
 *        retrieval usage.
 */
void Document::ComputeJPEGFragmentInfo()
{
   // If you need to, look for comments of ComputeRLEInfo().
   if ( ! IsJPEG() )
   {
      return;
   }

   ReadAndSkipEncapsulatedBasicOffsetTable();

   // Loop on the fragments[s] and store the parsed information in a
   // JPEGInfo.
   long fragmentLength;
   while ( (fragmentLength = ReadTagLength(0xfffe, 0xe000)) )
   { 
      long fragmentOffset = Fp->tellg();

       // Store the collected info
       JPEGFragment* newFragment = new JPEGFragment;
       newFragment->Offset = fragmentOffset;
       newFragment->Length = fragmentLength;
       JPEGInfo->Fragments.push_back( newFragment );

       SkipBytes( fragmentLength );
   }

   // Make sure that at the end of the item we encounter a 'Sequence
   // Delimiter Item':
   if ( !ReadTag(0xfffe, 0xe0dd) )
   {
      dbg.Verbose(0, "Document::ComputeRLEInfo: no sequence delimiter ");
      dbg.Verbose(0, "    item at end of JPEG item sequence");
   }
}

/**
 * \brief Walk recursively the given \ref DocEntrySet, and feed
 *        the given hash table (\ref TagDocEntryHT) with all the
 *        \ref DocEntry (Dicom entries) encountered.
 *        This method does the job for \ref BuildFlatHashTable.
 * @param builtHT Where to collect all the \ref DocEntry encountered
 *        when recursively walking the given set.
 * @param set The structure to be traversed (recursively).
 */
void Document::BuildFlatHashTableRecurse( TagDocEntryHT& builtHT,
                                          DocEntrySet* set )
{ 
   if (ElementSet* elementSet = dynamic_cast< ElementSet* > ( set ) )
   {
      TagDocEntryHT const & currentHT = elementSet->GetTagHT();
      for( TagDocEntryHT::const_iterator i  = currentHT.begin();
                                         i != currentHT.end();
                                       ++i)
      {
         DocEntry* entry = i->second;
         if ( SeqEntry* seqEntry = dynamic_cast<SeqEntry*>(entry) )
         {
            const ListSQItem& items = seqEntry->GetSQItems();
            for( ListSQItem::const_iterator item  = items.begin();
                                            item != items.end();
                                          ++item)
            {
               BuildFlatHashTableRecurse( builtHT, *item );
            }
            continue;
         }
         builtHT[entry->GetKey()] = entry;
      }
      return;
    }

   if (SQItem* SQItemSet = dynamic_cast< SQItem* > ( set ) )
   {
      const ListDocEntry& currentList = SQItemSet->GetDocEntries();
      for (ListDocEntry::const_iterator i  = currentList.begin();
                                        i != currentList.end();
                                      ++i)
      {
         DocEntry* entry = *i;
         if ( SeqEntry* seqEntry = dynamic_cast<SeqEntry*>(entry) )
         {
            const ListSQItem& items = seqEntry->GetSQItems();
            for( ListSQItem::const_iterator item  = items.begin();
                                            item != items.end();
                                          ++item)
            {
               BuildFlatHashTableRecurse( builtHT, *item );
            }
            continue;
         }
         builtHT[entry->GetKey()] = entry;
      }

   }
}

/**
 * \brief Build a \ref TagDocEntryHT (i.e. a std::map<>) from the current
 *        Document.
 *
 *        The structure used by a Document (through \ref ElementSet),
 *        in order to old the parsed entries of a Dicom header, is a recursive
 *        one. This is due to the fact that the sequences (when present)
 *        can be nested. Additionaly, the sequence items (represented in
 *        gdcm as \ref SQItem) add an extra complexity to the data
 *        structure. Hence, a gdcm user whishing to visit all the entries of
 *        a Dicom header will need to dig in the gdcm internals (which
 *        implies exposing all the internal data structures to the API).
 *        In order to avoid this burden to the user, \ref BuildFlatHashTable
 *        recursively builds a temporary hash table, which holds all the
 *        Dicom entries in a flat structure (a \ref TagDocEntryHT i.e. a
 *        std::map<>).
 * \warning Of course there is NO integrity constrain between the 
 *        returned \ref TagDocEntryHT and the \ref ElementSet used
 *        to build it. Hence if the underlying \ref ElementSet is
 *        altered, then it is the caller responsability to invoke 
 *        \ref BuildFlatHashTable again...
 * @return The flat std::map<> we juste build.
 */
TagDocEntryHT* Document::BuildFlatHashTable()
{
   TagDocEntryHT* FlatHT = new TagDocEntryHT;
   BuildFlatHashTableRecurse( *FlatHT, this );
   return FlatHT;
}



/**
 * \brief   Compares two documents, according to \ref DicomDir rules
 * \warning Does NOT work with ACR-NEMA files
 * \todo    Find a trick to solve the pb (use RET fields ?)
 * @param   document
 * @return  true if 'smaller'
 */
bool Document::operator<(Document &document)
{
   // Patient Name
   std::string s1 = GetEntryByNumber(0x0010,0x0010);
   std::string s2 = document.GetEntryByNumber(0x0010,0x0010);
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
      s1 = GetEntryByNumber(0x0010,0x0020);
      s2 = document.GetEntryByNumber(0x0010,0x0020);
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
         s1 = GetEntryByNumber(0x0020,0x000d);
         s2 = document.GetEntryByNumber(0x0020,0x000d);
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
            s1 = GetEntryByNumber(0x0020,0x000e);
            s2 = document.GetEntryByNumber(0x0020,0x000e);    
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

} // end namespace gdcm

//-----------------------------------------------------------------------------
