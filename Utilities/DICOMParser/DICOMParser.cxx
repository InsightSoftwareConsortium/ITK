#ifdef WIN32
#pragma warning(disable:4786)
#endif


#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <assert.h>
#include <sys/types.h>
#include <string>
#include <fstream>
#include <map>
#include <iomanip>


#include "DICOMParser.h"
#include "DICOMCallback.h"

/*
  #ifdef WIN32
  #define WORDS_LITTLEENDIAN
  #endif
*/

// Define DEBUG_DICOM to get debug messages sent to std::cerr
// #define DEBUG_DICOM
// #define DUMP_ALL_DICOM_TAGS

#ifdef DEBUG_DICOM
#define DICOM_DBG_MSG(x) {std::cout x}
#else 
#define DICOM_DBG_MSG(x)
#endif

static const char* DICOM_MAGIC = "DICM";
static const int   OPTIONAL_SKIP = 128;


// -- Open an image file so it is ready for reading, and calls to 
//    GetSection and PutSection.  
DICOMParser::DICOMParser() :  DebugFile() 
{
  /*
  // Image data on disk is stored in LITTLEENDIAN format
  #ifdef WORDS_LITTLEENDIAN
  ByteSwap = false;
  ByteSwapData = false;
  #else
  ByteSwap = true;
  ByteSwapData = true;
  #endif
  */
  this->InitTypeMap();
}

bool DICOMParser::OpenFile(char* filename)
{
  DataFile = new DICOMFile();
  bool val = DataFile->Open(filename);

#ifdef  DUMP_ALL_DICOM_TAGS
  char debug_filename[4096];
  sprintf(debug_filename, "%s.txt", filename);
  DebugFile.open(debug_filename);

  if (!DebugFile.is_open())
    {
    std::cerr << "Couldn't open debug file!" << endl;
    }
#endif
  return val;
}

DICOMParser::~DICOMParser() {
  DebugFile.close();
  if (DataFile)
    {
    delete DataFile;
    }
}

bool DICOMParser::ReadHeader() {

  DataFile->SkipToStart();
  DataFile->Read(Header.magic_num,4);

  if (!CheckMagic(Header.magic_num)) 
    {
    DataFile->SkipToPos(OPTIONAL_SKIP);
    DataFile->Read((void*)Header.magic_num,4);
    if (!CheckMagic(Header.magic_num)) 
      {
      std::cerr << "Error, DICOMParser magic number is incorrect: "
                << Header.magic_num << std::endl;
      return false;
      }
    }

  doublebyte group = 0;
  doublebyte element = 0;

  // unsigned int startHeaderPos = DataFile->Tell();
  // unsigned int endHeaderPos = 0;

  //
  // Now, really parse the header.
  //
  // DataFile->SkipToPos(startHeaderPos);
  do 
    {
    this->ReadNextRecord(group, element);

    } while (DataFile->Tell() < DataFile->GetSize());

  DebugFile.close();

  return true;
}

// NOTE: Only works on part 10 format
// read magic number from file
// return true if this is your image type, false if it is not
bool DICOMParser::IsDICOMFile(DICOMFile* file) {
  char magic_number[4];    
  file->SkipToStart();
  file->Read((void*)magic_number,4);
  if (CheckMagic(magic_number)) 
    {
    return(true);
    } 
  // try with optional skip
  else 
    {
    file->Skip(OPTIONAL_SKIP-4);
    file->Read((void*)magic_number,4);
    if (CheckMagic(magic_number)) 
      {
      return(true);
      }
    else 
      {
      return(false);
      }
    }
}


bool DICOMParser::IsValidRepresentation(doublebyte rep, quadbyte& len, VRTypes &mytype)
{
  switch (rep)
    {
    case DICOMParser::VR_AW: 
    case DICOMParser::VR_AE: 
    case DICOMParser::VR_AS: 
    case DICOMParser::VR_CS:
    case DICOMParser::VR_UI:
    case DICOMParser::VR_DA:
    case DICOMParser::VR_DS:
    case DICOMParser::VR_DT:
    case DICOMParser::VR_IS:
    case DICOMParser::VR_LO:
    case DICOMParser::VR_LT:
    case DICOMParser::VR_PN:
    case DICOMParser::VR_ST:
    case DICOMParser::VR_TM:
    case DICOMParser::VR_UT: // new
    case DICOMParser::VR_SH: 
    case DICOMParser::VR_FL:
    case DICOMParser::VR_SL:
    case DICOMParser::VR_AT: 
    case DICOMParser::VR_UL: 
    case DICOMParser::VR_US:
    case DICOMParser::VR_SS:
    case DICOMParser::VR_FD:
      len = DataFile->ReadDoubleByte();
      mytype = VRTypes(rep);
      return true;

    case DICOMParser::VR_OB: // OB - LE
    case DICOMParser::VR_OW:
    case DICOMParser::VR_UN:      
    case DICOMParser::VR_SQ:
      DataFile->ReadDoubleByte();
      len = DataFile->ReadQuadByte();
      mytype = VRTypes(rep);
      return true;

    default:
      //
      //
      // Need to comment out in new paradigm.
      //
      DataFile->Skip(-2);
      len = DataFile->ReadQuadByte();
      mytype = DICOMParser::VR_UNKNOWN;
      return false;  
    }
}

void DICOMParser::ReadNextRecord(doublebyte& group, doublebyte& element)
{
  //
  // WE SHOULD IMPLEMENT THIS ALGORITHM.
  //
  // FIND A WAY TO STOP IF THERE ARE NO MORE CALLBACKS.
  //
  // DO WE NEED TO ENSURE THAT WHEN A CALLBACK IS ADDED THAT 
  // THE IMPLICIT TYPE MAP IS UPDATED?  ONLY IF THERE ISN'T
  // A VALUE IN THE IMPLICIT TYPE MAP.
  //
  // New algorithm:
  //
  // 1. Read group & element
  // 2. ParseExplicitRecord
  //      a. Check to see if the next doublebyte is a valid datatype
  //      b. If the type is valid, lookup type to find the size of
  //              the length field.
  // 3.   If ParseExplicitRecord fails, ParseImplicitRecord
  //      a. Lookup implicit datatype
  // 4. Check to see if there is a registered callback for the group,element.
  // 5. If there are callbacks, read the data and call them, otherwise
  //      skip ahead to the next record.
  //
  //

  group = DataFile->ReadDoubleByte();
  element = DataFile->ReadDoubleByte();

  doublebyte representation = DataFile->ReadDoubleByte();
  quadbyte length = 0;
  VRTypes mytype = VR_UNKNOWN;
  this->IsValidRepresentation(representation, length, mytype);

  DICOMParserMap::iterator iter = 
    Map.find(DICOMMapKey(group,element));

  if (iter != Map.end())
    {
    //
    // Only read the data if there's a registered callback.
    //
    unsigned char* tempdata = (unsigned char*) DataFile->ReadAsciiCharArray(length);

    // this->DumpTag(group, element, VRTypes::VR_UNKNOWN, tempdata, length);
        
    DICOMMapKey ge = (*iter).first;
    VRTypes valType = VRTypes(((*iter).second.first));

    std::pair<const DICOMMapKey,DICOMMapValue> p = *iter;
    DICOMMapValue mv = p.second;
        
    std::vector<DICOMCallback*> * cbVector = mv.second;
    for (std::vector<DICOMCallback*>::iterator iter = cbVector->begin();
         iter != cbVector->end();
         iter++)
      {
      (*iter)->Execute(ge.first,  // group
                       ge.second,  // element
                       valType,  // type
                       tempdata, // data
                       length);  // length
      }

    }
    
  else
    {
    //
    // Some lengths are negative, but we don't 
    // want to back up the file pointer.
    //
    if (length > 0) 
      {
      DataFile->Skip(length);
      }
    //this->DumpTag(group, element, VRTypes::VR_UNKNOWN, (unsigned char*) "NULL", length);
    }


}

void DICOMParser::InitTypeMap()
{
  DicomRecord dicom_tags[] = {{0x0002, 0x0002, DICOMParser::VR_UI}, // Media storage SOP class uid
                              {0x0002, 0x0003, DICOMParser::VR_UI}, // Media storage SOP inst uid
                              {0x0002, 0x0010, DICOMParser::VR_UI}, // Transfer syntax uid
                              {0x0002, 0x0012, DICOMParser::VR_UI}, // Implementation class uid
                              {0x0008, 0x0018, DICOMParser::VR_UI}, // Image UID
                              {0x0008, 0x0020, DICOMParser::VR_DA}, // Series date
                              {0x0008, 0x0030, DICOMParser::VR_TM}, // Series time
                              {0x0008, 0x0060, DICOMParser::VR_SH}, // Modality
                              {0x0008, 0x0070, DICOMParser::VR_SH}, // Manufacturer
                              {0x0008, 0x1060, DICOMParser::VR_SH}, // Physician
                              {0x0018, 0x0050, DICOMParser::VR_FL}, // slice thickness
                              {0x0018, 0x0060, DICOMParser::VR_FL}, // kV
                              {0x0018, 0x0088, DICOMParser::VR_FL}, // slice spacing
                              {0x0018, 0x1100, DICOMParser::VR_SH}, // Recon diameter
                              {0x0018, 0x1151, DICOMParser::VR_FL}, // mA
                              {0x0018, 0x1210, DICOMParser::VR_SH}, // Recon kernel
                              {0x0020, 0x000d, DICOMParser::VR_UI}, // Study UID
                              {0x0020, 0x000e, DICOMParser::VR_UI}, // Series UID
                              {0x0020, 0x0013, DICOMParser::VR_IS}, // Image number
                              {0x0020, 0x0032, DICOMParser::VR_SH}, // Patient position
                              {0x0020, 0x0037, DICOMParser::VR_SH}, // Patient position cosines
                              {0x0028, 0x0010, DICOMParser::VR_US}, // Num rows
                              {0x0028, 0x0011, DICOMParser::VR_US}, // Num cols
                              {0x0028, 0x0030, DICOMParser::VR_FL}, // pixel spacing
                              {0x0028, 0x0100, DICOMParser::VR_US}, // Bits allocated
                              {0x0028, 0x0120, DICOMParser::VR_UL}, // pixel padding
                              {0x0028, 0x1052, DICOMParser::VR_FL}, // pixel offset
                              {0x7FE0, 0x0010, DICOMParser::VR_OW}   // pixel data
  };


  int num_tags = sizeof(dicom_tags)/sizeof(DicomRecord);

  doublebyte group;
  doublebyte element;
  VRTypes datatype;

  DICOMMemberCallback<DICOMParser>* printCb = new DICOMMemberCallback<DICOMParser>;
  printCb->SetCallbackFunction(this, &DICOMParser::DumpTag);


  std::vector<DICOMCallback*>* callbackVector;

  for (int i = 0; i < num_tags; i++)
    {
    callbackVector = new std::vector<DICOMCallback*>;
    callbackVector->push_back(printCb);

    group = dicom_tags[i].group;
    element = dicom_tags[i].element;
    datatype = (VRTypes) dicom_tags[i].datatype;
    // this->SetDICOMTagCallbacks(group, element, datatype, callbackVector);

    TypeMap.insert(std::pair<DICOMMapKey, DICOMTypeValue>(DICOMMapKey(group, element), datatype));
    }

  // DICOMMemberCallback<DICOMParser>* modalityCb = new DICOMMemberCallback<DICOMParser>;
  // modalityCb->SetCallbackFunction(this, this->ModalityTag);
    
  // std::vector<DICOMCallback*>* modalityCbVector = new std::vector<DICOMCallback*>;
  // modalityCbVector->push_back(modalityCb);

  // this->SetDICOMTagCallbacks(0x0008, 0x0060, VR_SH, modalityCbVector);

}

void DICOMParser::SetDICOMTagCallbacks(doublebyte group, doublebyte element, VRTypes datatype, std::vector<DICOMCallback*>* cbVector)
{
  Map.insert(std::pair<DICOMMapKey, DICOMMapValue>(DICOMMapKey(group, element), DICOMMapValue((int)datatype, cbVector)));
}


int DICOMParser::CheckMagic(char* magic_number) 
{
  return (
          (magic_number[0] == DICOM_MAGIC[0]) &&
          (magic_number[1] == DICOM_MAGIC[1]) &&
          (magic_number[2] == DICOM_MAGIC[2]) &&
          (magic_number[3] == DICOM_MAGIC[3])
          );
}

void DICOMParser::DumpTag(doublebyte group, doublebyte element, VRTypes, unsigned char* tempdata, quadbyte length)
{
  if (group == 0x7FE0 && element == 0x0010)
    {
    std::cout << std::cout.fill('0');
    std::cout << "(" << std::hex << std::cout.width(4) << group << ", " << std::hex << std::cout.width(4) << element << ")  : ";
    std::cout << "Image data not printed.";
    std::cout << std::dec;
    std::cout << " [" << length << " bytes] " << std::endl;
    }
  else
    {
    std::cout << std::cout.fill('0');
    std::cout << "(" << std::hex << std::cout.width(4) << group << ", " << std::hex << std::cout.width(4) << element << ")  : ";
    std::cout << (tempdata ? (char*) tempdata : "NULL");
    std::cout << std::dec;
    std::cout << " [" << length << " bytes] " << std::endl;
    }
}

void DICOMParser::ModalityTag(doublebyte, doublebyte, VRTypes, unsigned char* tempdata, quadbyte)
{
  if (!strcmp( (char*)tempdata, "MR"))
    {
    // this->AddMRTags();
    }
  else if (!strcmp((char*) tempdata, "CT"))
    {
    }
  else if (!strcmp((char*) tempdata, "US"))
    {
    }

}

void DICOMParser::AddMRTags()
{
  DICOMMemberCallback<DICOMParser>* modalityCb = new DICOMMemberCallback<DICOMParser>;
  modalityCb->SetCallbackFunction(this, &DICOMParser::DumpTag);
    
  std::vector<DICOMCallback*>* modalityCbVector = new std::vector<DICOMCallback*>;
  modalityCbVector->push_back(modalityCb);

  this->SetDICOMTagCallbacks(0x0018, 0x0081, VR_FL, modalityCbVector);
}

void DICOMParser::AddDICOMTagCallbacks(doublebyte group, doublebyte element, VRTypes datatype, std::vector<DICOMCallback*>* cbVector)
{
  DICOMParserMap::iterator miter = Map.find(DICOMMapKey(group,element));
  if (miter != Map.end())
    {
    for (std::vector<DICOMCallback*>::iterator iter = cbVector->begin();
         iter != cbVector->end();
         iter++)
      {
      std::vector<DICOMCallback*>* callbacks = (*miter).second.second;
      callbacks->push_back(*iter);
      }
    }
  else
    {
    this->SetDICOMTagCallbacks(group, element, datatype, cbVector);
    }
}

void DICOMParser::AddDICOMTagCallback(doublebyte group, doublebyte element, VRTypes datatype, DICOMCallback* cb)
{
  DICOMParserMap::iterator miter = Map.find(DICOMMapKey(group,element));
  if (miter != Map.end())
    {
    std::vector<DICOMCallback*>* callbacks = (*miter).second.second;
    callbacks->push_back(cb);
    }
  else
    {
    std::vector<DICOMCallback*>* callback = new std::vector<DICOMCallback*>;
    callback->push_back(cb);
    this->SetDICOMTagCallbacks(group, element, datatype, callback);
    }
}

void DICOMParser::AddDICOMTagCallbackToAllTags(DICOMCallback* cb)
{
  DICOMParserMap::iterator miter;
  for (miter = Map.begin();
       miter != Map.end();
       miter++);
  {
  std::vector<DICOMCallback*>* callbacks = (*miter).second.second;
  callbacks->push_back(cb);
  }
}

bool DICOMParser::ParseExplicitRecord(doublebyte, doublebyte, 
                                      quadbyte& length, 
                                      VRTypes& represent)
{
  doublebyte representation = DataFile->ReadDoubleByte();

  bool valid = this->IsValidRepresentation(representation, length, represent);

  if (valid)
    {
    return true;        
    }
  else
    {
    representation = VR_UNKNOWN;
    length = 0;
    return false;
    }
}

bool DICOMParser::ParseImplicitRecord(doublebyte group, doublebyte element,
                                      quadbyte& length,
                                      VRTypes& represent)
{
  DICOMImplicitTypeMap::iterator iter = 
    TypeMap.find(DICOMMapKey(group,element));
  represent = VRTypes((*iter).second);
  //
  // length?
  //
  length = DataFile->ReadQuadByte();
  return false;
}

