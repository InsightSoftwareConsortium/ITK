
#ifndef DICOMParser_h_
#define DICOMParser_h_

#ifdef WIN32
#pragma warning(disable:4786)
#endif

#include <map>
#include <utility> 
#include <fstream>
#include <vector>

#include "DICOMHeaderValues.h"
#include "DICOMFile.h"
#include "DICOMTypes.h"
#include "DICOMParserMap.h"

class DICOMCallback;

//
// We should keep a map with the implicit types for the groups and elements
// separate from the callbacks.  We can use this for the implicit types.
//
//


class DICOMParser
{
 public:

  //
  // Default constructor
  //
  DICOMParser();

  //
  // Default destructor.
  //
  ~DICOMParser();

  //
  // Opens a file and initializes the parser.
  //
  bool OpenFile(char* filename);

  //
  // This method kicks off the parser.
  // OpenFile needs to be called first.
  //
  bool ReadHeader();

  //
  // Return the header structure.
  //
  DICOMHeaderValues* GetHeader() {return &Header;}
  
  //
  // Static method that returns true if DICOMFile is opened 
  // to a file that contains a DICOM image.
  //
  static bool IsDICOMFile(DICOMFile* file);

  //
  // Static method that checks the DICOM magic number.
  //
  static int CheckMagic(char* magic_number);

  //
  // Defined DICOM types.
  //
  enum VRTypes 
    {
      VR_UNKNOWN = 0x0,
      VR_OB=0x424f,
      VR_AW=0x5741,
      VR_AE=0x4541,
      VR_AS=0x5341,
      VR_CS=0x5343,
      VR_UI=0x4955,
      VR_DA=0x4144,
      VR_DS=0x5344,
      VR_DT=0x5444,
      VR_IS=0x5349,
      VR_LO=0x4f4c,
      VR_LT=0x544c,
      VR_OW=0x574f,
      VR_PN=0x4e50,
      VR_ST=0x5453,
      VR_TM=0x4d54,
      VR_UN=0x4e55,
      VR_UT=0x5455,
      VR_SQ=0x5153,
      VR_SH=0x4853,
      VR_FL=0x4c46,
      VR_SL=0x4c53,
      VR_AT=0x5441,
      VR_UL=0x4c55,
      VR_US=0x5355,
      VR_SS=0x5353,
      VR_FD=0x4446
    };

  //
  // Callback for the modality tag.
  //
  void ModalityTag(doublebyte group, doublebyte element, VRTypes datatype, unsigned char* tempdata, quadbyte length);

  void SetDICOMTagCallbacks(doublebyte group, doublebyte element, VRTypes datatype, std::vector<DICOMCallback*>* cbVector);
  void AddDICOMTagCallbacks(doublebyte group, doublebyte element, VRTypes datatype, std::vector<DICOMCallback*>* cbVector);
  void AddDICOMTagCallback (doublebyte group, doublebyte element, VRTypes datatype, DICOMCallback* cb);
  void AddDICOMTagCallbackToAllTags(DICOMCallback* cb);

  DICOMFile* GetDICOMFile()
  {
    return this->DataFile;
  }

 protected:

  bool ParseExplicitRecord(doublebyte group, doublebyte element, 
                           quadbyte& length, 
                           VRTypes& represent);

  bool ParseImplicitRecord(doublebyte group, doublebyte element,
                           quadbyte& length,
                           VRTypes& represent);
  //
  // Print a tag.
  //
  void DumpTag(doublebyte group, doublebyte element, VRTypes datatype, unsigned char* data, quadbyte length);

  //
  // Hack to check out dynamic modality tags.
  //
  void AddMRTags();

  struct DicomRecord 
  {
    doublebyte group;
    doublebyte element;
    VRTypes datatype;
  };

  //
  // Check to see if the type is a valid DICOM type.  If not, figure
  // out the right thing to do next (i.e. compute the element length).
  //
  bool IsValidRepresentation(doublebyte rep, quadbyte& len, VRTypes &mytype);

  //
  // Reads a record.
  //
  void ReadNextRecord(doublebyte& group, doublebyte& element);

  //
  // Sets up the type map.
  //
  void InitTypeMap();
  
  //
  // Stores particular values parsed by the parser.
  // NEEDS TO MOVE TO READER
  //
  DICOMHeaderValues Header;

  //
  // Flags for byte swaping header values and 
  // image data.
  // 
  bool ByteSwap;
  bool ByteSwapData;

  //
  // Stores a map from pair<group, element> keys to
  // values of pair<vector<DICOMCallback*>, datatype>
  //
  DICOMParserMap Map;

  //
  // Stores a map from pair<group, element> keys to
  // values of datatype.  We use this to store the 
  // datatypes for implicit keys that we are 
  // interested in.
  //
  DICOMImplicitTypeMap TypeMap;

  //
  // Used for outputting debug information.
  //
  std::ofstream DebugFile;

  //
  // Pointer to the DICOMFile we're parsing.
  //
  DICOMFile* DataFile;

};

#endif   // DICOMParser_h_
