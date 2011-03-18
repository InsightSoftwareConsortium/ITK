/*=========================================================================

  Program:   DICOMParser
  Module:    DICOMAppHelper.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2003 Matt Turek
  All rights reserved.
  See Copyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/

#ifndef __DICOM_APP_HELPER_H_
#define __DICOM_APP_HELPER_H_

#ifdef _MSC_VER
#pragma warning ( disable : 4514 )
#pragma warning ( push, 3 )
#endif 

#include <vector>
#include <string.h>
#include <cstring>

#include "DICOMConfig.h"
#include "DICOMTypes.h"
#include "DICOMCallback.h"

namespace DICOMPARSER_NAMESPACE
{
class DICOMParser;

// Function object for sorting strings
struct ltstdstr
{
  ltstdstr() {}
  bool operator()(const dicom_stl::string s1, const dicom_stl::string s2) const
  {
    return s1 < s2;
  }
};


// Helper structure for DICOM elements
struct DICOMTagInfo
{
  doublebyte group;
  doublebyte element;
  DICOMParser::VRTypes datatype;
  const char* description;
};

// Helper class use for ordering DICOM images based on different
// (group, element) tags.
class DICOM_EXPORT DICOMOrderingElements
{
public:
  DICOMOrderingElements()
    {
      // Default values to something "valid"
      SliceNumber = -1;
      SliceLocation = 0.0f;
      ImagePositionPatient[0] = 0.0f;
      ImagePositionPatient[1] = 0.0f;
      ImagePositionPatient[2] = 0.0f;
      ImageOrientationPatient[0] = 1.0f;
      ImageOrientationPatient[1] = 0.0f;
      ImageOrientationPatient[2] = 0.0f;
      ImageOrientationPatient[3] = 0.0f;
      ImageOrientationPatient[4] = 1.0f;
      ImageOrientationPatient[5] = 0.0f;
    }
      
  int SliceNumber;
  float SliceLocation;
  float ImagePositionPatient[3];
  float ImageOrientationPatient[6];
};

class DICOMAppHelperImplementation;

/**
 * \class DICOMAppHelper
 * \brief Class to interface an application to a DICOMParser
 *
 * DICOMAppHelper assists an application in communicating with a
 * DICOMParser. DICOMAppHelper registers a series of callbacks to the
 * DICOMParser which allows it to cache the information from a DICOM
 * file in a format that is appropriate for an application to
 * use. Once a DICOM file is read, an application can query the
 * DICOMAppHelper for the resolution, pixel size, and pixel data.
 *
 * If a DICOMParser scans more than one file, the DICOMAppHelper will
 * group filesnames by SeriesUID.  This allows an application to pass
 * a series of DICOM files to the DICOMParser (which via the callback
 * mechanism allows the DICOMAppHelper to cache information) and then
 * query the DICOMAppHelper for the files that are from the same
 * series.  The application can request the filenames for a particular
 * series to be sorted based on image number, slice location, or
 * patient position. This allows the DICOMAppHelper to assist an
 * application is collecting all the images from one series into a
 * volume.
 */
class DICOM_EXPORT DICOMAppHelper
{
public:
  /** Standard constructor */
  DICOMAppHelper();

  /** Standard destructor */
  virtual ~DICOMAppHelper();

  /** Callbacks that are registered with the DICOMParser.  The
   * DICOMParser will call one of these callbacks whenever it
   * encounters a (group, element) that has an associated callback */
  virtual void RescaleSlopeCallback(DICOMParser *parser,
                                     doublebyte group,
                                    doublebyte element,
                                    DICOMParser::VRTypes type,
                                    unsigned char* val,
                                    quadbyte len);

  virtual void ArrayCallback(DICOMParser *parser,
                             doublebyte group,
                             doublebyte element,
                             DICOMParser::VRTypes type,
                             unsigned char* val,
                             quadbyte len);
  
  virtual void SliceNumberCallback(DICOMParser *parser,
                                   doublebyte group,
                                   doublebyte element,
                                   DICOMParser::VRTypes type,
                                   unsigned char* val,
                                   quadbyte len) ;

  virtual void SliceLocationCallback(DICOMParser *parser,
                                     doublebyte group,
                                     doublebyte element,
                                     DICOMParser::VRTypes type,
                                     unsigned char* val,
                                     quadbyte len) ;

  virtual void ImagePositionPatientCallback(DICOMParser *parser,
                                            doublebyte group,
                                            doublebyte element,
                                            DICOMParser::VRTypes type,
                                            unsigned char* val,
                                            quadbyte len) ;
  
  virtual void ImageOrientationPatientCallback(DICOMParser *parser,
                                               doublebyte group,
                                               doublebyte element,
                                               DICOMParser::VRTypes type,
                                               unsigned char* val,
                                               quadbyte len) ;
  
  virtual void SeriesUIDCallback(DICOMParser *parser,
                                 doublebyte group,
                                 doublebyte element,
                                 DICOMParser::VRTypes type,
                                 unsigned char* val,
                                 quadbyte len) ;
  
  virtual void SeriesDescriptionCallback(DICOMParser *parser,
                                 doublebyte group,
                                 doublebyte element,
                                 DICOMParser::VRTypes type,
                                 unsigned char* val,
                                 quadbyte len) ;


  virtual void InstanceUIDCallback(DICOMParser *parser,
                                   doublebyte group,
                                   doublebyte element,
                                   DICOMParser::VRTypes type,
                                   unsigned char* val,
                                   quadbyte len) ;
  
  virtual void TransferSyntaxCallback(DICOMParser *parser,
                                      doublebyte group,
                                      doublebyte element,
                                      DICOMParser::VRTypes type,
                                      unsigned char* val,
                                      quadbyte len) ;
  
  virtual void BitsAllocatedCallback(DICOMParser *parser,
                                     doublebyte group,
                                     doublebyte element,
                                     DICOMParser::VRTypes type,
                                     unsigned char* val,
                                     quadbyte len) ;
  
  virtual void ToggleSwapBytesCallback(DICOMParser *parser,
                                       doublebyte,
                                       doublebyte,
                                       DICOMParser::VRTypes,
                                       unsigned char*,
                                       quadbyte);
  
  virtual void PixelSpacingCallback(DICOMParser *parser,
                                    doublebyte group,
                                    doublebyte element,
                                    DICOMParser::VRTypes type,
                                    unsigned char* val,
                                    quadbyte len) ;

  virtual void HeightCallback(DICOMParser *parser,
                              doublebyte group,
                              doublebyte element,
                              DICOMParser::VRTypes type,
                              unsigned char* val,
                              quadbyte len);

  virtual void WidthCallback( DICOMParser *parser,
                              doublebyte group,
                              doublebyte element,
                              DICOMParser::VRTypes type,
                              unsigned char* val,
                              quadbyte len);

  virtual void PixelRepresentationCallback(DICOMParser *parser,
                                           doublebyte group,
                                           doublebyte element,
                                           DICOMParser::VRTypes type,
                                           unsigned char* val,
                                           quadbyte len);

  virtual void PhotometricInterpretationCallback(DICOMParser *parser,
                                                 doublebyte,
                                                 doublebyte,
                                                 DICOMParser::VRTypes,
                                                 unsigned char* val,
                                                 quadbyte len);

  virtual void PixelDataCallback(DICOMParser *parser,
                                 doublebyte,
                                 doublebyte,
                                 DICOMParser::VRTypes,
                                 unsigned char* val,
                                 quadbyte len);

  virtual void RescaleOffsetCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);

  virtual void ROIContourSequenceCallback( DICOMParser *parser,
                                           doublebyte,
                                           doublebyte,
                                           DICOMParser::VRTypes,
                                           unsigned char* val,
                                           quadbyte);

  virtual void ContourSequenceCallback( DICOMParser *parser,
                                        doublebyte,
                                        doublebyte,
                                        DICOMParser::VRTypes,
                                        unsigned char* val,
                                        quadbyte);

  virtual void ContourImageSequenceCallback( DICOMParser *parser,
                                             doublebyte,
                                             doublebyte,
                                             DICOMParser::VRTypes,
                                             unsigned char* val,
                                             quadbyte);

  virtual void ContourGeometricTypeCallback( DICOMParser *parser,
                                             doublebyte,
                                             doublebyte,
                                             DICOMParser::VRTypes,
                                             unsigned char* val,
                                             quadbyte);

  virtual void NumberOfContourPointsCallback( DICOMParser *parser,
                                              doublebyte,
                                              doublebyte,
                                              DICOMParser::VRTypes,
                                              unsigned char* val,
                                              quadbyte);
  
  virtual void ContourDataCallback( DICOMParser *parser,
                                    doublebyte,
                                    doublebyte,
                                    DICOMParser::VRTypes,
                                    unsigned char* val,
                                    quadbyte);

  virtual void ReferencedInstanceUIDCallback( DICOMParser *parser,
                                              doublebyte,
                                              doublebyte,
                                              DICOMParser::VRTypes,
                                              unsigned char* val,
                                              quadbyte);

  virtual void PatientNameCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);

  virtual void PatientIDCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);

  virtual void PatientSexCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);

  virtual void PatientAgeCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);
  virtual void PatientDOBCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);

   virtual void StudyIDCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);

   virtual void StudyDescriptionCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);
  
   virtual void BodyPartCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);
   
   virtual void NumberOfSeriesInStudyCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);
    
   virtual void NumberOfStudyRelatedSeriesCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);

  virtual void StudyDateCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);

  virtual void ModalityCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);

  virtual void ManufacturerCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);

  virtual void InstitutionCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);

  virtual void ModelCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);
   
  virtual void ScanOptionsCallback( DICOMParser *parser,
                                      doublebyte,
                                      doublebyte,
                                      DICOMParser::VRTypes,
                                      unsigned char* val,
                                      quadbyte);

  virtual void DefaultCallback( DICOMParser *parser,
                                doublebyte,
                                doublebyte,
                                DICOMParser::VRTypes,
                                unsigned char* val,
                                quadbyte);
  

  
  /** Register all the standard callbacks with the DICOM Parser.  This
   * associates a callback with each (group, element) tag pair in the
   * header of the file whose data needs to be cached. */
  virtual void RegisterCallbacks(DICOMParser* parser);

  /** Register a callback for retrieving the pixel data from a DICOM
   *  file */
  virtual void RegisterPixelDataCallback(DICOMParser* parser);


  /** Output information associated with a DICOM series */
  void OutputSeries();


  /** The next set of methods are for accessing information which is
   * cached when a DICOM file is processed.  This allows access to
   * information from the header as well as the pixel data. */
  
  
  /** Get the pixel spacing of the last image processed by the
   *  DICOMParser */
  float* GetPixelSpacing()
    {
    return this->PixelSpacing;
    }

  /** Get the image width of the last image processed by the
   *  DICOMParser */
  int GetWidth()
    {
    return this->Width;
    }

  /** Get the image height of the last image processed by the
   *  DICOMParser */
  int GetHeight()
    {
    return this->Height;
    }

  /** Get the dimensions (width, height) of the last image processed
   *  by the DICOMParser */
  int* GetDimensions()
    {
    return this->Dimensions;
    }

  /** Get the (DICOM) x,y,z coordinates of the first pixel in the
   * image (upper left hand corner) of the last image processed by the
   * DICOMParser */
  float *GetImagePositionPatient()
    {
      return this->ImagePositionPatient;
    }
  
  
  /** Get the number of bits allocated per pixel of the last image
   *  processed by the DICOMParser */
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
   *  DICOMParser. */
  unsigned int GetNumberOfComponents()
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
    dicom_stl::string str1(*this->PhotometricInterpretation);
    dicom_stl::string rgb("RGB ");

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
   *  DICOMParser. */
  dicom_stl::string GetTransferSyntaxUID()
    {
    return *(this->TransferSyntaxUID);
    }

  /** Get a textual description of the transfer syntax of the last
   *  image processed by the DICOMParser. */
  const char* TransferSyntaxUIDDescription(const char* uid);

  /** Get the image data from the last image processed by the
   * DICOMParser.  The data is only valid if the PixelDataCallback was
   * registered.
   * \sa RegisterPixelDataCallback()
  */
  void GetImageData(void* & data, DICOMParser::VRTypes& dataType, unsigned long& len);

  /** Determine whether the image data was rescaled (by the
   *  RescaleSlope tag) to be floating point. */
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

  /** Get the series UID for the current file. */
  std::string GetSeriesUID() { return this->CurrentSeriesUID; }

  /** Get the series description for the current file. */
  std::string GetSeriesDescription() { return this->CurrentSeriesDescription; }

  /** Get the patient name processed by the
      DICOMParser. */
  void GetPatientName(char* name)
    {
    strcpy(name, m_PatientName);
    }

  /** Get the patient ID processed by the
      DICOMParser. */
  void GetPatientID(char* id)
    {
    strcpy(id, m_PatientID);
    }

  /** Get the patient sex processed by the
      DICOMParser. */
  void GetPatientSex(char* sex)
    {
    strcpy(sex, m_PatientSex);
    }

  /** Get the patient age processed by the
      DICOMParser. */
  void GetPatientAge(char* age)
    {
    strcpy(age, m_PatientAge);
    }

  /** Get the patient date of birth processed by the
      DICOMParser. */
  void GetPatientDOB(char* dob)
    {
    strcpy(dob, m_PatientDOB);
    }

 /** Get the study ID processed by the
      DICOMParser. */
  void GetStudyID(char* id)
    {
    strcpy(id, m_StudyID);
    }

 /** Get the description of the study processed by the
      DICOMParser. */
  void GetStudyDescription(char* desc)
    {
    strcpy(desc, m_StudyDescription);
    }

 /** Get the body part processed by the
      DICOMParser. */
  void GetBodyPart(char* part)
    {
    strcpy(part, m_BodyPart);
    }

  /** Get the number of series in the study processed by the
      DICOMParser. */
  void GetNumberOfSeriesInStudy(char* number)
    {
    strcpy(number, m_NumberOfSeriesInStudy);
    }


 /** Get the number of study related series processed by the
      DICOMParser. */
  void GetNumberOfStudyRelatedSeries(char* number)
    {
    strcpy(number, m_NumberOfStudyRelatedSeries);
    }

  /** Get the study date processed by the
      DICOMParser. */
  void GetStudyDate(char* date)
    {
    strcpy(date, m_StudyDate);
    }

  /** Get the modality processed by the
      DICOMParser. */
  void GetModality(char* modality)
    {
    strcpy(modality, m_Modality);
    }

  /** Get the manufacturer processed by the
      DICOMParser. */
  void GetManufacturer(char* manu)
    {
    strcpy(manu, m_Manufacturer);
    }

  /** Get the institution processed by the
      DICOMParser. */
  void GetInstitution(char* ins)
    {
    strcpy(ins, m_Institution);
    }

  /** Get the model processed by the
      DICOMParser. */
  void GetModel(char* model)
    {
    strcpy(model, m_Model);
    }

  /** Get the scan options name processed by the
      DICOMParser. */
  void GetScanOptions(char* options)
    {
    strcpy(options, m_ScanOptions);
    }

  /** Clear the internal databases. This will reset the internal
   * databases that are grouping filenames based on SeriesUID's and
   * ordering filenames based on image locations. */
  void Clear();

  /** Get the series UIDs for the files processed since the last
   * clearing of the cache. */
  void GetSeriesUIDs(dicom_stl::vector<dicom_stl::string> &v); 

  /** Get the series Descriptions for the files processed since the last
   * clearing of the cache. */
  void GetSeriesDescriptions(dicom_stl::vector<dicom_stl::string> &v); 

  /** Get the body parts for the files processed since the last
   * clearing of the cache. */
  void GetBodyParts(dicom_stl::vector<dicom_stl::string> &v); 

  /** Get the scane options for the files processed since the last
   * clearing of the cache. */
  void GetScanOptions(dicom_stl::vector<dicom_stl::string> &v); 

  /** Get the filenames for a series ordered by slice number. */
  void GetSliceNumberFilenamePairs(const dicom_stl::string &seriesUID,
                              dicom_stl::vector<dicom_stl::pair<int, dicom_stl::string> > &v,
                                            bool ascending = true);

  /** Get the filenames for a series order by slice number.  Use the
      first series by default. */
  void GetSliceNumberFilenamePairs(dicom_stl::vector<dicom_stl::pair<int, dicom_stl::string> > &v,
                                   bool ascending = true);

  /* Get the filenames for a series ordered by slice location. */
  void GetSliceLocationFilenamePairs(const dicom_stl::string &seriesUID,
                              dicom_stl::vector<dicom_stl::pair<float, dicom_stl::string> > &v,
                                   bool ascending = true);

  /* Get the filenames for a series ordered by slice location. Use the
   * first series by default. */
  void GetSliceLocationFilenamePairs(dicom_stl::vector<dicom_stl::pair<float, dicom_stl::string> > &v,
                                   bool ascending = true);

  /* Get the filenames for a series ordered by image position
     patient. This is the most reliable way to order the images in a
     series. */
  void GetImagePositionPatientFilenamePairs(const dicom_stl::string &seriesUID,
                            dicom_stl::vector<dicom_stl::pair<float, dicom_stl::string> > &v,
                                            bool ascending = true);

  /* Get the filenames for a series ordered by image position
     patient. This is the most reliable way to order the images in a
     series. Use the first series by default. */
  void GetImagePositionPatientFilenamePairs(dicom_stl::vector<dicom_stl::pair<float, dicom_stl::string> > &v,
                                            bool ascending = true);

  /* Get the contours for a series.  A vector of contours is returned
  where each contour is a vector of floats (x, y, z, x, y, z, x, y,
  z...). */
  void GetContours(const dicom_stl::string &seriesUID,
                   dicom_stl::vector<dicom_stl::vector<float> > &v);

  /* Get the contours for a series.  A vector of contours is returned
  where each contour is a vector of floats (x, y, z, x, y, z, x, y,
  z...). Use the first series by default. */
  void GetContours(dicom_stl::vector<dicom_stl::vector<float> > &v);

  /* Get the referenced instance UIDs for a series.  A vector of
   * strings in returned. */
  void GetReferencedInstanceUIDs(const dicom_stl::string &seriesUID,
                                 dicom_stl::vector<dicom_stl::string> &v);
  
  /* Get the referenced instance UIDs for a series.  A vector of
   * strings in returned. Use the first series by default. */
  void GetReferencedInstanceUIDs(dicom_stl::vector<dicom_stl::string> &v);

  /** Get the filename for a specific instance UID */
  dicom_stl::string GetFileName( const dicom_stl::string &instanceUID );
  
 protected:
  int FileCount;
  int BitsAllocated;
  bool ByteSwapData;
  float PixelSpacing[3];
  int Width;
  int Height;
  int SliceNumber; 
  int Dimensions[2];
  float ImagePositionPatient[3];

  short VolumeSliceSize;
  short VolumeSliceCount;
  long VolumeVoxelCount;
  long VolumeSegmentCount;

  char m_PatientName[512];
  char m_PatientID[512];
  char m_PatientDOB[512];
  char m_StudyID[512];   
  char m_StudyDescription[512];
  char m_BodyPart[512];
  char m_NumberOfSeriesInStudy[512];
  char m_NumberOfStudyRelatedSeries[512];
  char m_PatientSex[512];
  char m_PatientAge[512];
  char m_StudyDate[512];
  char m_Modality[512];
  char m_Manufacturer[512];
  char m_Institution[512];
  char m_Model[512];
  char m_ScanOptions[512];

  typedef dicom_stl::map<dicom_stl::pair<doublebyte, doublebyte>, DICOMTagInfo> TagMapType;
  // TagMapType TagMap;

  dicom_stream::ofstream HeaderFile;
  
  // 0 unsigned
  // 1 2s complement (signed)
  int PixelRepresentation;
  dicom_stl::string* PhotometricInterpretation;
  dicom_stl::string* TransferSyntaxUID;
  dicom_stl::string CurrentSeriesUID;
  dicom_stl::string CurrentSeriesDescription;
  dicom_stl::string CurrentBodyPart;
  dicom_stl::string CurrentScanOptions;
  dicom_stl::string InstanceUID;
  
  float RescaleOffset;
  float RescaleSlope;
  void* ImageData;
  DICOMParser::VRTypes ImageDataType;
  unsigned long ImageDataLengthInBytes;

  DICOMMemberCallback<DICOMAppHelper>* SeriesUIDCB;
  DICOMMemberCallback<DICOMAppHelper>* SeriesDescriptionCB;
  DICOMMemberCallback<DICOMAppHelper>* InstanceUIDCB;
  DICOMMemberCallback<DICOMAppHelper>* SliceNumberCB;
  DICOMMemberCallback<DICOMAppHelper>* SliceLocationCB;
  DICOMMemberCallback<DICOMAppHelper>* ImagePositionPatientCB;
  DICOMMemberCallback<DICOMAppHelper>* ImageOrientationPatientCB;
  DICOMMemberCallback<DICOMAppHelper>* TransferSyntaxCB;
  DICOMMemberCallback<DICOMAppHelper>* ToggleSwapBytesCB;
  DICOMMemberCallback<DICOMAppHelper>* BitsAllocatedCB;
  DICOMMemberCallback<DICOMAppHelper>* PixelSpacingCB;
  DICOMMemberCallback<DICOMAppHelper>* HeightCB;
  DICOMMemberCallback<DICOMAppHelper>* WidthCB;
  DICOMMemberCallback<DICOMAppHelper>* PixelRepresentationCB;
  DICOMMemberCallback<DICOMAppHelper>* PhotometricInterpretationCB;
  DICOMMemberCallback<DICOMAppHelper>* RescaleOffsetCB;
  DICOMMemberCallback<DICOMAppHelper>* RescaleSlopeCB;
  DICOMMemberCallback<DICOMAppHelper>* PixelDataCB;
  DICOMMemberCallback<DICOMAppHelper>* ROIContourSequenceCB;
  DICOMMemberCallback<DICOMAppHelper>* ContourSequenceCB;
  DICOMMemberCallback<DICOMAppHelper>* ContourDataCB;
  DICOMMemberCallback<DICOMAppHelper>* NumberOfContourPointsCB;
  DICOMMemberCallback<DICOMAppHelper>* ContourGeometricTypeCB;
  DICOMMemberCallback<DICOMAppHelper>* ContourImageSequenceCB;
  DICOMMemberCallback<DICOMAppHelper>* ReferencedInstanceUIDCB;
  DICOMMemberCallback<DICOMAppHelper>* DefaultCB;

  DICOMMemberCallback<DICOMAppHelper>* PatientNameCB;
  DICOMMemberCallback<DICOMAppHelper>* PatientIDCB;
  DICOMMemberCallback<DICOMAppHelper>* PatientSexCB;
  DICOMMemberCallback<DICOMAppHelper>* PatientAgeCB;
  DICOMMemberCallback<DICOMAppHelper>* StudyDateCB;
  DICOMMemberCallback<DICOMAppHelper>* ModalityCB;
  DICOMMemberCallback<DICOMAppHelper>* ManufacturerCB;
  DICOMMemberCallback<DICOMAppHelper>* InstitutionCB;
  DICOMMemberCallback<DICOMAppHelper>* ModelCB;
  DICOMMemberCallback<DICOMAppHelper>* ScanOptionsCB;
  DICOMMemberCallback<DICOMAppHelper>* PatientDOBCB;
  DICOMMemberCallback<DICOMAppHelper>* StudyIDCB;
  DICOMMemberCallback<DICOMAppHelper>* StudyDescriptionCB;
  DICOMMemberCallback<DICOMAppHelper>* BodyPartCB;
  DICOMMemberCallback<DICOMAppHelper>* NumberOfSeriesInStudyCB;
  DICOMMemberCallback<DICOMAppHelper>* NumberOfStudyRelatedSeriesCB;

  //
  // Implementation contains stl templated classes that 
  // can't be exported from a DLL in Windows. We hide
  // them in the implementation to get rid of annoying
  // compile warnings.
  //
  DICOMAppHelperImplementation* Implementation;

 private:
  DICOMAppHelper(const DICOMAppHelper&);  
  void operator=(const DICOMAppHelper&); 
    
};
}
#ifdef _MSC_VER
#pragma warning ( pop )
#endif

#endif
