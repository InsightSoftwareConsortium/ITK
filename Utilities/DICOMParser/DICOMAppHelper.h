
#ifndef __DICOM_APP_HELPER_H_
#define __DICOM_APP_HELPER_H_

#include <fstream>
#include "DICOMTypes.h"
#include "DICOMParser.h"
#include "DICOMCallback.h"

#include <vector>
#include <string>
#include <iomanip>
#include <iostream>

struct ltstr
{
  bool operator()(const char* s1, const char* s2) const
  {
    return strcmp(s1, s2) < 0;
  }
};

struct ltstdstr
{
  bool operator()(const std::string s1, const std::string s2) const
  {
    return s1 < s2;
  }
};

struct DICOMTagInfo
{
  doublebyte group;
  doublebyte element;
  DICOMParser::VRTypes datatype;
  char* description;
};


class DICOMOrderingElements
{
public:
  DICOMOrderingElements()
    {
      // Default values to something "valid"
      SliceNumber = -1;
      SliceLocation = 0.0;
      ImagePositionPatient[0] = 0.0;
      ImagePositionPatient[1] = 0.0;
      ImagePositionPatient[2] = 0.0;
      ImageOrientationPatient[0] = 1.0;
      ImageOrientationPatient[1] = 0.0;
      ImageOrientationPatient[2] = 0.0;
      ImageOrientationPatient[3] = 0.0;
      ImageOrientationPatient[4] = 1.0;
      ImageOrientationPatient[5] = 0.0;
    }
      
  int SliceNumber;
  float SliceLocation;
  float ImagePositionPatient[3];
  float ImageOrientationPatient[6];
};


// Main class that interfaces to the DICOMParser and caches the
// information from the DICOM file.
//
class DICOMAppHelper
{
 public:
  DICOMAppHelper();
  virtual ~DICOMAppHelper();
  
  void SetFileName(const char* filename);


  /** Callbacks that are registered with the DICOMParser.  When the
   * DICOMParser will call one of these callbacks whenever it
   * encounters a (group, element) that has an associated callback */
  virtual void RescaleSlopeCallback(doublebyte group,
                     doublebyte element,
                     DICOMParser::VRTypes type,
                     unsigned char* val,
                     quadbyte len);

  virtual void ArrayCallback(doublebyte group,
                     doublebyte element,
                     DICOMParser::VRTypes type,
                     unsigned char* val,
                     quadbyte len);
  
  virtual void SliceNumberCallback(doublebyte group,
                           doublebyte element,
                           DICOMParser::VRTypes type,
                           unsigned char* val,
                           quadbyte len) ;

  virtual void SliceLocationCallback(doublebyte group,
                           doublebyte element,
                           DICOMParser::VRTypes type,
                           unsigned char* val,
                                     quadbyte len) ;

  virtual void ImagePositionPatientCallback(doublebyte group,
                           doublebyte element,
                           DICOMParser::VRTypes type,
                           unsigned char* val,
                           quadbyte len) ;
  
  virtual void ImageOrientationPatientCallback(doublebyte group,
                           doublebyte element,
                           DICOMParser::VRTypes type,
                           unsigned char* val,
                           quadbyte len) ;
  
  virtual void SeriesUIDCallback(doublebyte group,
                         doublebyte element,
                         DICOMParser::VRTypes type,
                         unsigned char* val,
                         quadbyte len) ;
    
  virtual void TransferSyntaxCallback(doublebyte group,
                              doublebyte element,
                              DICOMParser::VRTypes type,
                              unsigned char* val,
                              quadbyte len) ;
  
  virtual void BitsAllocatedCallback(doublebyte group,
                             doublebyte element,
                             DICOMParser::VRTypes type,
                             unsigned char* val,
                             quadbyte len) ;
  
  virtual void ToggleSwapBytesCallback(doublebyte,
                               doublebyte,
                               DICOMParser::VRTypes,
                               unsigned char*,
                               quadbyte);

  virtual void PixelSpacingCallback(doublebyte group,
                                    doublebyte element,
                                    DICOMParser::VRTypes type,
                                    unsigned char* val,
                                    quadbyte len) ;

  virtual void HeightCallback(doublebyte group,
                              doublebyte element,
                              DICOMParser::VRTypes type,
                              unsigned char* val,
                              quadbyte len);

  virtual void WidthCallback( doublebyte group,
                              doublebyte element,
                              DICOMParser::VRTypes type,
                              unsigned char* val,
                              quadbyte len);

  virtual void PixelRepresentationCallback(doublebyte group,
                                           doublebyte element,
                                           DICOMParser::VRTypes type,
                                           unsigned char* val,
                                           quadbyte len);

  virtual void PhotometricInterpretationCallback(doublebyte,
                                                 doublebyte,
                                                 DICOMParser::VRTypes,
                                                 unsigned char* val,
                                                 quadbyte len);

  virtual void PixelDataCallback(doublebyte,
                                 doublebyte,
                                 DICOMParser::VRTypes,
                                 unsigned char* val,
                                 quadbyte len);

  virtual void RescaleOffsetCallback( doublebyte,
                                    doublebyte,
                                    DICOMParser::VRTypes,
                                    unsigned char* val,
                                    quadbyte);

  /** Register all the standard callbacks with the DICOM Parser.  This
   * associates a callback with each (group, element) tag pair in the
   * header of the file whose data needs to be cached. */
  virtual void RegisterCallbacks(DICOMParser* parser);

  /** Register a callback for retrieving the pixel data from a DICOM
      file */
  virtual void RegisterPixelDataCallback();

  /** Interface a DICOMFile with the AppHelper */
  virtual void SetDICOMDataFile(DICOMFile* f)
    {
    this->DICOMDataFile = f;
    }

  /** Output information associated with a DICOM series */
  void OutputSeries();


  /** The next set of methods are for accessing information which is
   * cached when a DICOM file is processed.  This allows access to
   * information from the header as well as the pixel data. */
  
  
  /** Get the pixel spacing of the last image processed by the
      DICOMParser */
  float* GetPixelSpacing()
    {
    return this->PixelSpacing;
    }

  /** Get the image width of the last image processed by the
      DICOMParser */
  int GetWidth()
    {
    return this->Width;
    }

  /** Get the image height of the last image processed by the
      DICOMParser */
  int GetHeight()
    {
    return this->Height;
    }

  /** Get the dimensions (width, height) of the last image processed
      by the DICOMParser */
  int* GetDimensions()
    {
    return this->Dimensions;
    }

  /** Get the number of bits allocated per pixel of the last image
      processed by the DICOMParser */
  int GetBitsAllocated()
    {
    return this->BitsAllocated;
    }

  /** Get the pixel representation of the last image processed by the
   * DICOMParser. A zero is a unsigned quantity.  A one indicates a
   * signed quantity. */
  int GetPixelRepresentation()
    {
    return this->PixelRepresentation;
    }

  /** Get the number of components of the last image processed by the
      DICOMParser. */
  int GetNumberOfComponents()
    {
    if (!this->PhotometricInterpretation)
      {
      return 1;
      }

    //
    // DICOM standard says that spaces (0x20) are to
    // be ignored for CS types.  We don't handle this
    // well yet.
    //
    std::string str1(*this->PhotometricInterpretation);
    std::string rgb("RGB ");

    if (str1 == rgb)
      {
      return 3;
      }
    else
      {
      return 1;
      }
    }

  /** Get the transfer syntax UID for the last image processed by the
      DICOMParser. */
  std::string GetTransferSyntaxUID()
    {
    return *(this->TransferSyntaxUID);
    }

  /** Get a textual description of the transfer syntax of the last
      image processed by the DICOMParser. */
  const char* TransferSyntaxUIDDescription(const char* uid);

  /** Get the image data from the last image processed by the
   * DICOMParser.  The data is only valid if the PixelDataCallback was
   * registered.
   * \sa RegisterPixelDataCallback()
  */
  void GetImageData(void* & data, DICOMParser::VRTypes& dataType, unsigned long& len);

  /** Determine whether the image data was rescaled (by the
      RescaleSlope tag) to be floating point. */
  bool RescaledImageDataIsFloat();

  /** Determine whether the image data was rescaled (by the
   * RescaleSlope tag) to be a signed data type. */
  bool RescaledImageDataIsSigned();

  /** Get the slice number of the last image processed by the
      DICOMParser. */
  int GetSliceNumber()
    {
    return this->SliceNumber;
    }

  /** Clear the internal databases */
  void ClearSliceOrderingMap();
  void ClearSeriesUIDMap();

  
  /** Get the filenames for a series ordered by slice number. */
  void GetSliceNumberFilenamePairs(std::vector<std::pair<int, std::string> > & v);

  /* Get the filenames for a series ordered by slice location. */
  void GetSliceLocationFilenamePairs(std::vector<std::pair<float, std::string> > & v);

  /* Get the filenames for a series ordered by image position
     patient. This is the most reliable way to order the images in a
     series. */
  void GetImagePositionPatientFilenamePairs(std::vector<std::pair<float, std::string> > & v);

 protected:
  int BitsAllocated;
  bool ByteSwapData;
  float PixelSpacing[3];
  int Width;
  int Height;
  int SliceNumber; 
  int Dimensions[2];

  char* GetOutputFilename()
  {
    int len = static_cast<int>(strlen(this->FileName));
    char* output = new char[len + 5];
    strcpy(output, this->FileName);
    strcat(output, ".raw");
    return output;
  }
  
  char* FileName;
  
  // map from series UID to vector of files in the series 
  std::map<std::string, std::vector<std::string>, ltstdstr> SeriesUIDMap;

  // map from filename to intraseries sortable tags
  std::map<std::string, DICOMOrderingElements, ltstdstr> SliceOrderingMap;

  typedef std::map<std::pair<doublebyte, doublebyte>, DICOMTagInfo> TagMapType;
  TagMapType TagMap;

  std::ofstream HeaderFile;
  
  DICOMFile* DICOMDataFile;

  DICOMParser* Parser;

  // 0 unsigned
  // 1 2s complement (signed)
  int PixelRepresentation;
  std::string* PhotometricInterpretation;
  std::string* TransferSyntaxUID;
  float RescaleOffset;
  float RescaleSlope;
  void* ImageData;
  DICOMParser::VRTypes ImageDataType;
  unsigned long ImageDataLengthInBytes;

  DICOMMemberCallback<DICOMAppHelper>* SeriesUIDCB;
  DICOMMemberCallback<DICOMAppHelper>* SliceNumberCB;
  DICOMMemberCallback<DICOMAppHelper>* SliceLocationCB;
  DICOMMemberCallback<DICOMAppHelper>* ImagePositionPatientCB;
  DICOMMemberCallback<DICOMAppHelper>* ImageOrientationPatientCB;
  DICOMMemberCallback<DICOMAppHelper>* TransferSyntaxCB;
  DICOMMemberCallback<DICOMAppHelper>* BitsAllocatedCB;
  DICOMMemberCallback<DICOMAppHelper>* PixelSpacingCB;
  DICOMMemberCallback<DICOMAppHelper>* HeightCB;
  DICOMMemberCallback<DICOMAppHelper>* WidthCB;
  DICOMMemberCallback<DICOMAppHelper>* PixelRepresentationCB;
  DICOMMemberCallback<DICOMAppHelper>* PhotometricInterpretationCB;
  DICOMMemberCallback<DICOMAppHelper>* RescaleOffsetCB;
  DICOMMemberCallback<DICOMAppHelper>* RescaleSlopeCB;
  DICOMMemberCallback<DICOMAppHelper>* PixelDataCB;

};

#endif
