/*=========================================================================
 
  Program:   gdcm
  Module:    gdcmDocument.h
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

#ifndef GDCMDOCUMENT_H
#define GDCMDOCUMENT_H

#include "gdcmCommon.h"
#include "gdcmVR.h"
#include "gdcmTS.h"
#include "gdcmException.h"
#include "gdcmDictSet.h"
#include "gdcmDocEntry.h"
#include "gdcmRLEFramesInfo.h"
#include "gdcmJPEGFragmentsInfo.h"
#include "gdcmDocEntrySet.h"
#include "gdcmElementSet.h"

class ValEntry;
class BinEntry;
class SeqEntry;

#include <map>
#include <list>
#include <fstream>

namespace gdcm 
{

enum TransferSyntaxType {
  ImplicitVRLittleEndian = 0,
  ExplicitVRLittleEndian,
  DeflatedExplicitVRLittleEndian,
  ExplicitVRBigEndian,
  JPEGBaselineProcess1,
  JPEGExtendedProcess2_4,
  JPEGExtendedProcess3_5,
  JPEGSpectralSelectionProcess6_8,
  JPEGFullProgressionProcess10_12,
  JPEGLosslessProcess14,
  JPEGLosslessProcess14_1,
  JPEG2000Lossless,
  JPEG2000,
  RLELossless,
  UnknownTS
};

//-----------------------------------------------------------------------------
/**
 * \brief Derived by both Header and DicomDir
 */
class GDCM_EXPORT Document : public ElementSet
{
friend class File;
private:
   /// Public dictionary used to parse this header
   Dict* RefPubDict;
   
   /// \brief Optional "shadow dictionary" (private elements) used to parse
   /// this header
   Dict* RefShaDict;

   /// \brief Size threshold above which an element value will NOT be loaded
   /// in memory (to avoid loading the image/volume itself). By default,
   /// this upper bound is fixed to 1024 bytes (which might look reasonable
   /// when one considers the definition of the various VR contents).
   uint32_t MaxSizeLoadEntry;
   
   /// \brief Size threshold above which an element value will NOT be *printed*
   /// in order no to polute the screen output. By default, this upper bound
   /// is fixed to 64 bytes.
   uint32_t MaxSizePrintEntry;   

protected:
   /// Refering underlying filename.
   std::string Filename;

   /// \brief SWap code (e.g. Big Endian, Little Endian, Bad Big Endian,
   /// Bad Little Endian) according to the processor Endianity and
   /// what is written on disc.
   int SwapCode;

   /// File Pointer, opened during Header parsing.
   std::ifstream* Fp;

   /// ACR, ACR_LIBIDO, ExplicitVR, ImplicitVR, Unknown
   FileType Filetype;  

   /// After opening the file, we read HEADER_LENGTH_TO_READ bytes.
   static const unsigned int HEADER_LENGTH_TO_READ; 

   /// \brief Elements whose value is longer than MAX_SIZE_LOAD_ELEMENT_VALUE
   /// are NOT loaded.
   static const unsigned int MAX_SIZE_LOAD_ELEMENT_VALUE;

   /// \brief Elements whose value is longer than  MAX_SIZE_PRINT_ELEMENT_VALUE
   /// are NOT printed.
   /// \todo Currently not used since collides with #define in
   ///       class DocEntry . See also
   ///       method ref Document::SetMaxSizePrintEntry()
   static const unsigned int MAX_SIZE_PRINT_ELEMENT_VALUE;

   /// Store the RLE frames info obtained during parsing of pixels.
   RLEFramesInfo* RLEInfo;

   /// Store the JPEG fragments info obtained during parsing of pixels.
   JPEGFragmentsInfo* JPEGInfo;

   /// \brief Amount of printed details for each Header Entry (Dicom Element):
   /// 0 : stands for the least detail level.
   int PrintLevel;
   
public:
// the 2 following will be merged
   virtual void PrintPubDict (std::ostream &os = std::cout);
   virtual void PrintShaDict (std::ostream &os = std::cout);

// Dictionnaries
   Dict* GetPubDict();
   Dict* GetShaDict();
   bool SetShaDict(Dict* dict);
   bool SetShaDict(DictKey const & dictName);

// Informations contained in the parser
   virtual bool IsReadable();
   TransferSyntaxType GetTransferSyntax();
   bool IsJPEGLossless();
   bool IsJPEG2000();
   bool IsJPEG();
   bool IsEncapsulate();
   bool IsDicomV3();

   FileType GetFileType();

   std::ifstream * OpenFile();
   bool CloseFile();

   void Write( std::ofstream* fp, FileType type );

   ValEntry* ReplaceOrCreateByNumber(std::string const & value,
                                     uint16_t group, uint16_t elem,
                                     std::string const & vr = "unkn");
   
   BinEntry* ReplaceOrCreateByNumber(uint8_t* binArea, int lgth,
                                     uint16_t group, uint16_t elem,
                                     std::string const & vr = "unkn");

   SeqEntry* ReplaceOrCreateByNumber(uint16_t group, uint16_t elem);

   bool ReplaceIfExistByNumber ( std::string const & value,
                                 uint16_t group, uint16_t elem );
   
   virtual void* LoadEntryBinArea(uint16_t group, uint16_t elem);
   virtual void* LoadEntryBinArea(BinEntry* entry);
      
   // System access (meaning endian related !?)
   uint16_t SwapShort(uint16_t);   // needed by File
   uint32_t SwapLong(uint32_t);    // needed by File
   uint16_t UnswapShort(uint16_t); // needed by File
   uint32_t UnswapLong(uint32_t);  // needed by File

protected:
   // Constructor and destructor are protected to forbid end user 
   // to instanciate from this class Document (only Header and
   // DicomDir are meaningfull).
   Document();
   Document( std::string const & filename );
   virtual ~Document();
   
   void ReadAndSkipEncapsulatedBasicOffsetTable();
   void ComputeRLEInfo();
   void ComputeJPEGFragmentInfo();
   // Entry
   bool CheckIfEntryExistByNumber(uint16_t group, uint16_t elem );
public:
   virtual std::string GetEntryByName    (TagName const & tagName);
   virtual std::string GetEntryVRByName  (TagName const & tagName);
   virtual std::string GetEntryByNumber  (uint16_t group, uint16_t elem);
   virtual std::string GetEntryVRByNumber(uint16_t group, uint16_t elem);
   virtual int     GetEntryLengthByNumber(uint16_t group, uint16_t elem);
//protected:
   virtual bool SetEntryByName  (std::string const & content, 
                                 std::string const & tagName);
   virtual bool SetEntryByNumber(std::string const & content,
                                 uint16_t group, uint16_t element);
   virtual bool SetEntryByNumber(uint8_t* content, int lgth,
                                 uint16_t group, uint16_t element);
   virtual bool SetEntryLengthByNumber(uint32_t length,
                                       uint16_t group, uint16_t element);

   virtual size_t GetEntryOffsetByNumber (uint16_t group, uint16_t elem);
   virtual void* GetEntryBinAreaByNumber(uint16_t group, uint16_t elem);   
   virtual bool  SetEntryBinAreaByNumber(uint8_t* a, uint16_t group,
                                                   uint16_t elem);

   virtual void UpdateShaEntries();

   // Header entry
   DocEntry* GetDocEntryByNumber(uint16_t group, uint16_t element); 
   DocEntry* GetDocEntryByName  (std::string const & tagName);

   ValEntry* GetValEntryByNumber(uint16_t group, uint16_t element); 
   //BinEntry* GetBinEntryByNumber(uint16_t group, uint16_t element); 
   RLEFramesInfo* GetRLEInfo() { return RLEInfo; }
   JPEGFragmentsInfo* GetJPEGInfo() { return JPEGInfo; }

   void LoadDocEntrySafe(DocEntry* entry);
   TagDocEntryHT* BuildFlatHashTable();

private:
   // Read
   void ParseDES(DocEntrySet *set,long offset, long l_max, bool delim_mode);
   void ParseSQ (SeqEntry *seq,   long offset, long l_max, bool delim_mode);

   void LoadDocEntry      (DocEntry *);
   void FindDocEntryLength(DocEntry *) throw ( FormatError );
   void FindDocEntryVR    (DocEntry *);
   bool CheckDocEntryVR   (DocEntry *, VRKey);

   std::string GetDocEntryValue  (DocEntry *);
   std::string GetDocEntryUnvalue(DocEntry *);

   void SkipDocEntry          (DocEntry *);
   void SkipToNextDocEntry    (DocEntry *);

   void FixDocEntryFoundLength(DocEntry *, uint32_t);
   bool IsDocEntryAnInteger   (DocEntry *);

   uint32_t FindDocEntryLengthOB() throw( FormatUnexpected );

   uint16_t ReadInt16() throw ( FormatError );
   uint32_t ReadInt32() throw ( FormatError );
   void     SkipBytes(uint32_t);
   bool     ReadTag(uint16_t, uint16_t);
   uint32_t ReadTagLength(uint16_t, uint16_t);

   void Initialise();
   bool CheckSwap();
   void SwitchSwapToBigEndian();
   void SetMaxSizeLoadEntry(long);
   void SetMaxSizePrintEntry(long);

   // DocEntry related utilities
   DocEntry* ReadNextDocEntry();

   uint32_t GenerateFreeTagKeyInGroup(uint16_t group);
   void BuildFlatHashTableRecurse( TagDocEntryHT& builtHT,
                                   DocEntrySet* set );

public:
// Accessors:
   /// Accessor to \ref PrintLevel
   void SetPrintLevel(int level) { PrintLevel = level; }

   /// Accessor to \ref Filename
   const std::string &GetFileName() const { return Filename; }

   /// Accessor to \ref Filename
   void SetFileName(std::string const & fileName) { Filename = fileName; }

   /// 'Swap code' accessor (see \ref SwapCode )
   int GetSwapCode() { return SwapCode; }
   
   /// File pointer
   std::ifstream * GetFP() { return Fp; }

   bool operator<(Document &document);

};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
