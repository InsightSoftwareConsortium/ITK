
#ifndef __DICOM_READER_H_
#define __DICOM_READER_H_

#include "DICOMTypes.h"
#include "DICOMParser.h"

class DICOMReader : public DICOMParser
{
 public:

  DICOMReader();
  ~DICOMReader();

  void TransferSyntaxUIDCallback(doublebyte group,
                                 doublebyte element,
                                 DICOMParser::VRTypes type,
                                 unsigned char* val,
                                 quadbyte len);
    
  void ImageDataCallback(doublebyte group,
                         doublebyte element,
                         DICOMParser::VRTypes type,
                         unsigned char* val,
                         quadbyte len);

  void ImageWidthCallback(doublebyte group,
                          doublebyte element,
                          DICOMParser::VRTypes type,
                          unsigned char* val,
                          quadbyte len);

  void ImageHeightCallback(doublebyte group,
                           doublebyte element,
                           DICOMParser::VRTypes type,
                           unsigned char* val,
                           quadbyte len);

  void SetFileName(const char* filename)
  {
    this->FileName = filename;
  }   

  void SetupCallbacks();

  void FormattedPrint(doublebyte group, doublebyte element, VRTypes datatype, unsigned char* data, quadbyte length);
  void FormattedStore(doublebyte group, doublebyte element, VRTypes datatype, unsigned char* data, quadbyte length) ;

 protected:
  void ParsePatientPosition(char* parse_string);
  void ParsePatientCosines(char* parse_string);

  float ComputeLambda(float q1[3], float q3[3]);
  void ComputeRASInformation();


  int Width;
  int Height;
  char* FileName;
  // bool ByteSwapData;

};

#endif
