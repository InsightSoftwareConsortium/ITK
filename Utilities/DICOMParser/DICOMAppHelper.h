
#ifndef __DICOM_APP_HELPER_H_
#define __DICOM_APP_HELPER_H_

#include <fstream>
#include <string.h>
#include "DICOMTypes.h"
#include "DICOMParser.h"

struct ltstr
{
  bool operator()(const char* s1, const char* s2) const
  {
    return strcmp(s1, s2) < 0;
  }
};

struct DICOMTagInfo
{
  doublebyte group;
  doublebyte element;
  DICOMParser::VRTypes datatype;
  char* description;
};

class DICOMAppHelper
{
 public:
  DICOMAppHelper();
  ~DICOMAppHelper();
  
  void SetFileName(const char* filename);
  
  void ArrayCallback(doublebyte group,
                     doublebyte element,
                     DICOMParser::VRTypes type,
                     unsigned char* val,
                     quadbyte len);
  
  void SliceNumberCallback(doublebyte group,
                           doublebyte element,
                           DICOMParser::VRTypes type,
                           unsigned char* val,
                           quadbyte len) ;
  
  void SeriesUIDCallback(doublebyte group,
                         doublebyte element,
                         DICOMParser::VRTypes type,
                         unsigned char* val,
                         quadbyte len) ;
  
  void WriteImageData(doublebyte group,
                      doublebyte element,
                      DICOMParser::VRTypes type,
                      unsigned char* val,
                      quadbyte len);
  
  void TransferSyntaxCallback(doublebyte group,
                              doublebyte element,
                              DICOMParser::VRTypes type,
                              unsigned char* val,
                              quadbyte len) ;
  
  void BitsAllocatedCallback(doublebyte group,
                             doublebyte element,
                             DICOMParser::VRTypes type,
                             unsigned char* val,
                             quadbyte len) ;
  
  void OutputSeries();
  void RegisterCallbacks(DICOMParser& parser);
  void SetDICOMDataFile(DICOMFile* f)
  {
    this->DICOMDataFile = f;
  }

 protected:
  int BitsAllocated;
  bool ByteSwapData;
  
  char* GetOutputFilename()
  {
    int len = strlen(this->FileName);
    char* output = new char[len + 5];
    strcpy(output, this->FileName);
    strcat(output, ".raw");
    return output;
  }
  
  char* FileName;
  
  // map from series UID to vector of files in the series 
  std::map<char*, std::vector<char*>, ltstr > SeriesUIDMap;  
  std::map<char*, int > SliceNumberMap;  
  
  typedef std::map<std::pair<doublebyte, doublebyte>, DICOMTagInfo> TagMapType;
  TagMapType TagMap;

  std::ofstream HeaderFile;
  
  DICOMFile* DICOMDataFile;
};

#endif
