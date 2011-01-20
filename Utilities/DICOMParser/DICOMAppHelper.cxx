/*=========================================================================

  Program:   DICOMParser
  Module:    DICOMAppHelper.cxx
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

#ifdef _MSC_VER
#pragma warning ( disable : 4514 )
#pragma warning ( disable : 4786 )
#pragma warning ( disable : 4503 )
#pragma warning ( disable : 4710 )
#pragma warning ( disable : 4702 )
#pragma warning ( push, 3 )
#endif 

#include "DICOMConfig.h"
#include "DICOMAppHelper.h"
#include "DICOMCallback.h"

#include <stdlib.h>
#include <stdio.h>
#include <string>
#include <math.h>
#include <algorithm>

namespace DICOMPARSER_NAMESPACE
{
//#define DEBUG_DICOM_APP_HELPER

class DICOMAppHelperImplementation
{
public:
  // map from series UID to vector of instance UIDs in the series
  typedef dicom_stl::map<dicom_stl::string, dicom_stl::vector<dicom_stl::string>, ltstdstr> SeriesUIDToInstanceUIDMapType;
  SeriesUIDToInstanceUIDMapType SeriesUIDToInstanceUIDMap;

  // map from a series Description to an instance UID 
  typedef dicom_stl::map<dicom_stl::string, dicom_stl::string, ltstdstr> SeriesUIDToSeriesDescriptionMapType;
  SeriesUIDToSeriesDescriptionMapType SeriesUIDToSeriesDescriptionMap;

  // map from a series body part to an instance UID 
  typedef dicom_stl::map<dicom_stl::string, dicom_stl::string, ltstdstr> SeriesUIDToBodyPartMapType;
  SeriesUIDToBodyPartMapType  SeriesUIDToBodyPartMap;

  // map from a series scan option to an instance UID 
  typedef dicom_stl::map<dicom_stl::string, dicom_stl::string, ltstdstr> SeriesUIDToScanOptionsMapType;
  SeriesUIDToScanOptionsMapType  SeriesUIDToScanOptionsMap;

  // map from an instance UID to a series UID
  typedef dicom_stl::map<dicom_stl::string, dicom_stl::string, ltstdstr> InstanceUIDToSeriesUIDMapType;
  InstanceUIDToSeriesUIDMapType InstanceUIDToSeriesUIDMap;

  // map from an instance UID to a filename
  typedef dicom_stl::map<dicom_stl::string, dicom_stl::string, ltstdstr> InstanceUIDToFileNameMapType;
  InstanceUIDToFileNameMapType InstanceUIDToFileNameMap;
  
  // map from instance UID to intraseries sortable tags
  typedef dicom_stl::map<dicom_stl::string, DICOMOrderingElements, ltstdstr> InstanceUIDToSliceOrderingMapType;
  InstanceUIDToSliceOrderingMapType InstanceUIDToSliceOrderingMap;

  // map for (group, element) to tag info
  typedef dicom_stl::map<dicom_stl::pair<doublebyte, doublebyte>, DICOMTagInfo> TagMapType;
  TagMapType TagMap;

  // data structure for a contour (store points as x,y,z,x,y,z,...)
  // Do we want to have different contour types? POINT, OPEN_PLANAR, CLOSED_PLANAR
  typedef dicom_stl::vector<float> ContourType;

  // vector of contours
  typedef dicom_stl::vector<ContourType> ContoursVectorType;

  // map from series UID to vector of contours in that series
  typedef dicom_stl::map<dicom_stl::string, ContoursVectorType, ltstdstr> SeriesUIDToContoursMapType;
  SeriesUIDToContoursMapType SeriesUIDToContoursMap;

  // vector of instance UIDs
  typedef dicom_stl::vector<dicom_stl::string> InstanceUIDVectorType;
  
  // map from series UID to referenced instance uid
  typedef dicom_stl::map<dicom_stl::string, InstanceUIDVectorType, ltstdstr> SeriesUIDToReferencedInstanceUIDMapType;
  SeriesUIDToReferencedInstanceUIDMapType SeriesUIDToReferencedInstanceUIDMap;
};


struct lt_pair_int_string
{
  bool operator()(const dicom_stl::pair<int, dicom_stl::string> s1, 
                  const dicom_stl::pair<int, dicom_stl::string> s2) const
  {
    return s1.first < s2.first;
  }
};


struct lt_pair_float_string
{
  bool operator()(const dicom_stl::pair<float, dicom_stl::string> s1, 
                  const dicom_stl::pair<float, dicom_stl::string> s2) const
  {
    return s1.first < s2.first;
  }
};


struct gt_pair_int_string
{
  bool operator()(const dicom_stl::pair<int, dicom_stl::string> s1, 
                  const dicom_stl::pair<int, dicom_stl::string> s2) const
  {
    return s1.first > s2.first;
  }
};


struct gt_pair_float_string
{
  bool operator()(const dicom_stl::pair<float, dicom_stl::string> s1, 
                  const dicom_stl::pair<float, dicom_stl::string> s2) const
  {
    return s1.first > s2.first;
  }
};


DICOMAppHelper::DICOMAppHelper()
{
  this->FileCount = 0;
  this->BitsAllocated = 8;
  this->ByteSwapData = false;
  this->PixelSpacing[0] = this->PixelSpacing[1] = this->PixelSpacing[2] = 1.0f;
  this->Dimensions[0] = this->Dimensions[1] = 0;
  this->Width = this->Height = 0;
  this->PhotometricInterpretation = NULL;
  this->TransferSyntaxUID = NULL;
  this->CurrentSeriesUID = "";
  this->InstanceUID = "";
  this->RescaleOffset = 0.0f;
  this->RescaleSlope = 1.0f;
  this->ImageData = NULL;
  this->ImageDataLengthInBytes = 0;

  // Initialize the arrays
  m_NumberOfSeriesInStudy[0]='\0';
  m_PatientName[0]='\0';
  m_PatientID[0]='\0';
  m_PatientDOB[0]='\0';
  m_StudyID[0]='\0';
  m_StudyDescription[0]='\0';
  m_BodyPart[0]='\0';
  m_NumberOfSeriesInStudy[0]='\0';
  m_NumberOfStudyRelatedSeries[0]='\0';
  m_PatientSex[0]='\0';
  m_PatientAge[0]='\0';
  m_StudyDate[0]='\0';
  m_Modality[0]='\0';
  m_Manufacturer[0]='\0';
  m_Institution[0]='\0';
  m_Model[0]='\0';
  m_ScanOptions[0]='\0';


  this->SeriesUIDCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->SeriesDescriptionCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->InstanceUIDCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->SliceNumberCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->SliceLocationCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->ImagePositionPatientCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->ImageOrientationPatientCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->TransferSyntaxCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->ToggleSwapBytesCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->BitsAllocatedCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->PixelSpacingCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->HeightCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->WidthCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->PixelRepresentationCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->PhotometricInterpretationCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->RescaleOffsetCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->RescaleSlopeCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->PixelDataCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->ROIContourSequenceCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->ContourImageSequenceCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->ContourSequenceCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->ContourDataCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->NumberOfContourPointsCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->ContourGeometricTypeCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->ReferencedInstanceUIDCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->PatientNameCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->PatientIDCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->PatientSexCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->PatientAgeCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->PatientDOBCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->StudyIDCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->StudyDescriptionCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->BodyPartCB  = new DICOMMemberCallback<DICOMAppHelper>;
  this->NumberOfSeriesInStudyCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->NumberOfStudyRelatedSeriesCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->StudyDateCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->ModalityCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->ManufacturerCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->InstitutionCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->ModelCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->ScanOptionsCB = new DICOMMemberCallback<DICOMAppHelper>;
  this->DefaultCB = new DICOMMemberCallback<DICOMAppHelper>;
  
  this->Implementation = new DICOMAppHelperImplementation;
}

DICOMAppHelper::~DICOMAppHelper()
{
  this->Clear();

  this->HeaderFile.close();

  //
  // Fix warning here.
  //
  if (this->ImageData)
    {
    delete [] (static_cast<char*> (this->ImageData));
    }
  if (this->TransferSyntaxUID)
    {
    delete this->TransferSyntaxUID;
    }
  if (this->PhotometricInterpretation)
    {
    delete this->PhotometricInterpretation;
    }

  delete this->SeriesUIDCB;
  delete this->SeriesDescriptionCB;
  delete this->InstanceUIDCB;
  delete this->SliceNumberCB;
  delete this->SliceLocationCB;
  delete this->ImagePositionPatientCB;
  delete this->ImageOrientationPatientCB;
  delete this->TransferSyntaxCB;
  delete this->ToggleSwapBytesCB;
  delete this->BitsAllocatedCB;
  delete this->PixelSpacingCB;
  delete this->HeightCB;
  delete this->WidthCB;
  delete this->PixelRepresentationCB;
  delete this->PhotometricInterpretationCB;
  delete this->RescaleOffsetCB;
  delete this->RescaleSlopeCB;
  delete this->PixelDataCB;
  delete this->ROIContourSequenceCB;
  delete this->ContourImageSequenceCB;
  delete this->ContourSequenceCB;
  delete this->ContourDataCB;
  delete this->NumberOfContourPointsCB;
  delete this->ContourGeometricTypeCB;
  delete this->ReferencedInstanceUIDCB;
  delete this->PatientNameCB;
  delete this->PatientIDCB;
  delete this->PatientSexCB;
  delete this->PatientAgeCB;
  delete this->PatientDOBCB;
  delete this->StudyIDCB;
  delete this->StudyDescriptionCB;
  delete this->BodyPartCB;
  delete this->NumberOfSeriesInStudyCB;
  delete this->NumberOfStudyRelatedSeriesCB;
  delete this->StudyDateCB;
  delete this->ModalityCB;
  delete this->ManufacturerCB;
  delete this->InstitutionCB;
  delete this->ModelCB;
  delete this->ScanOptionsCB;

  delete this->DefaultCB;

  delete this->Implementation;
}

void DICOMAppHelper::RegisterCallbacks(DICOMParser* parser)
{
  if (!parser)
    {
    dicom_stream::cerr << "Null parser!" << dicom_stream::endl;
    }

  // Default callback. Typically used to register a callback for
  // sequences where we are not interested in the sequence itself but
  // an element within the sequence.
  DefaultCB->SetCallbackFunction(this, &DICOMAppHelper::DefaultCallback);

  //
  //
  //
  SeriesUIDCB->SetCallbackFunction(this, &DICOMAppHelper::SeriesUIDCallback);
  parser->AddDICOMTagCallback(0x0020, 0x000e, DICOMParser::VR_UI, SeriesUIDCB);

  InstanceUIDCB->SetCallbackFunction(this, &DICOMAppHelper::InstanceUIDCallback);
  parser->AddDICOMTagCallback(0x0008, 0x0018, DICOMParser::VR_UI, InstanceUIDCB);
  
  SeriesDescriptionCB->SetCallbackFunction(this, &DICOMAppHelper::SeriesDescriptionCallback);
  parser->AddDICOMTagCallback(0x0008, 0x103e, DICOMParser::VR_LO, SeriesDescriptionCB);

  SliceNumberCB->SetCallbackFunction(this, &DICOMAppHelper::SliceNumberCallback);
  parser->AddDICOMTagCallback(0x0020, 0x0013, DICOMParser::VR_IS, SliceNumberCB);

  SliceLocationCB->SetCallbackFunction(this, &DICOMAppHelper::SliceLocationCallback);
  parser->AddDICOMTagCallback(0x0020, 0x1041, DICOMParser::VR_DS, SliceLocationCB);

  ImagePositionPatientCB->SetCallbackFunction(this, &DICOMAppHelper::ImagePositionPatientCallback);
  parser->AddDICOMTagCallback(0x0020, 0x0032, DICOMParser::VR_SH, ImagePositionPatientCB);

  ImageOrientationPatientCB->SetCallbackFunction(this, &DICOMAppHelper::ImageOrientationPatientCallback);
  parser->AddDICOMTagCallback(0x0020, 0x0037, DICOMParser::VR_SH, ImageOrientationPatientCB);
  
  TransferSyntaxCB->SetCallbackFunction(this, &DICOMAppHelper::TransferSyntaxCallback);
  parser->AddDICOMTagCallback(0x0002, 0x0010, DICOMParser::VR_UI, TransferSyntaxCB);

  ToggleSwapBytesCB->SetCallbackFunction(this, &DICOMAppHelper::ToggleSwapBytesCallback);

  BitsAllocatedCB->SetCallbackFunction(this, &DICOMAppHelper::BitsAllocatedCallback);
  parser->AddDICOMTagCallback(0x0028, 0x0100, DICOMParser::VR_US, BitsAllocatedCB);

  PixelSpacingCB->SetCallbackFunction(this, &DICOMAppHelper::PixelSpacingCallback);
  parser->AddDICOMTagCallback(0x0028, 0x0030, DICOMParser::VR_FL, PixelSpacingCB);
  parser->AddDICOMTagCallback(0x0018, 0x0050, DICOMParser::VR_FL, PixelSpacingCB);

  WidthCB->SetCallbackFunction(this, &DICOMAppHelper::WidthCallback);
  parser->AddDICOMTagCallback(0x0028, 0x0011, DICOMParser::VR_US, WidthCB);

  HeightCB->SetCallbackFunction(this, &DICOMAppHelper::HeightCallback);
  parser->AddDICOMTagCallback(0x0028, 0x0010, DICOMParser::VR_US, HeightCB);

  PixelRepresentationCB->SetCallbackFunction(this, &DICOMAppHelper::PixelRepresentationCallback);
  parser->AddDICOMTagCallback(0x0028, 0x0103, DICOMParser::VR_US, PixelRepresentationCB);

  PhotometricInterpretationCB->SetCallbackFunction(this, &DICOMAppHelper::PhotometricInterpretationCallback);
  parser->AddDICOMTagCallback(0x0028, 0x0004, DICOMParser::VR_CS, PhotometricInterpretationCB);

  RescaleOffsetCB->SetCallbackFunction(this, &DICOMAppHelper::RescaleOffsetCallback);
  parser->AddDICOMTagCallback(0x0028, 0x1052, DICOMParser::VR_DS, RescaleOffsetCB);

  RescaleSlopeCB->SetCallbackFunction(this, &DICOMAppHelper::RescaleSlopeCallback);
  parser->AddDICOMTagCallback(0x0028, 0x1053, DICOMParser::VR_DS, RescaleSlopeCB);

  ROIContourSequenceCB->SetCallbackFunction(this, &DICOMAppHelper::ROIContourSequenceCallback);
  parser->AddDICOMTagCallback(0x3006, 0x0039, DICOMParser::VR_SQ, ROIContourSequenceCB);

  ContourSequenceCB->SetCallbackFunction(this, &DICOMAppHelper::ContourSequenceCallback);
  parser->AddDICOMTagCallback(0x3006, 0x0040, DICOMParser::VR_SQ, ContourSequenceCB);

  ContourGeometricTypeCB->SetCallbackFunction(this, &DICOMAppHelper::ContourGeometricTypeCallback);
  parser->AddDICOMTagCallback(0x3006, 0x0042, DICOMParser::VR_CS, ContourGeometricTypeCB);

  NumberOfContourPointsCB->SetCallbackFunction(this, &DICOMAppHelper::NumberOfContourPointsCallback);
  parser->AddDICOMTagCallback(0x3006, 0x0046, DICOMParser::VR_IS, NumberOfContourPointsCB);

  ContourDataCB->SetCallbackFunction(this, &DICOMAppHelper::ContourDataCallback);
  parser->AddDICOMTagCallback(0x3006, 0x0050, DICOMParser::VR_DS, ContourDataCB);

  ContourImageSequenceCB->SetCallbackFunction(this, &DICOMAppHelper::ContourImageSequenceCallback);
  parser->AddDICOMTagCallback(0x3006, 0x0016, DICOMParser::VR_SQ, ContourImageSequenceCB);

  ReferencedInstanceUIDCB->SetCallbackFunction(this, &DICOMAppHelper::ReferencedInstanceUIDCallback);
  parser->AddDICOMTagCallback(0x0008, 0x1155, DICOMParser::VR_UI, ReferencedInstanceUIDCB);

  PatientNameCB->SetCallbackFunction(this, &DICOMAppHelper::PatientNameCallback);
  parser->AddDICOMTagCallback(0x0010, 0x0010, DICOMParser::VR_PN, PatientNameCB);

  PatientIDCB->SetCallbackFunction(this, &DICOMAppHelper::PatientIDCallback);
  parser->AddDICOMTagCallback(0x0010, 0x0020, DICOMParser::VR_LO, PatientIDCB);

  PatientSexCB->SetCallbackFunction(this, &DICOMAppHelper::PatientSexCallback);
  parser->AddDICOMTagCallback(0x0010, 0x0040, DICOMParser::VR_CS, PatientSexCB);

  PatientAgeCB->SetCallbackFunction(this, &DICOMAppHelper::PatientAgeCallback);
  parser->AddDICOMTagCallback(0x0010, 0x1010, DICOMParser::VR_AS, PatientAgeCB);
 
  PatientDOBCB->SetCallbackFunction(this, &DICOMAppHelper::PatientDOBCallback);
  parser->AddDICOMTagCallback(0x0010, 0x0030, DICOMParser::VR_DA, PatientDOBCB);

  StudyIDCB->SetCallbackFunction(this, &DICOMAppHelper::StudyIDCallback);
  parser->AddDICOMTagCallback(0x0020, 0x0010, DICOMParser::VR_SH, StudyIDCB);

  StudyDescriptionCB->SetCallbackFunction(this, &DICOMAppHelper::StudyDescriptionCallback);
  parser->AddDICOMTagCallback(0x0008, 0x1030, DICOMParser::VR_LO, StudyDescriptionCB);

  BodyPartCB->SetCallbackFunction(this, &DICOMAppHelper::BodyPartCallback);
  parser->AddDICOMTagCallback(0x0018, 0x0015, DICOMParser::VR_CS, BodyPartCB);

  NumberOfSeriesInStudyCB->SetCallbackFunction(this, &DICOMAppHelper::NumberOfSeriesInStudyCallback);
  parser->AddDICOMTagCallback(0x0020, 0x1000, DICOMParser::VR_IS, NumberOfSeriesInStudyCB);

  NumberOfStudyRelatedSeriesCB->SetCallbackFunction(this, &DICOMAppHelper::NumberOfStudyRelatedSeriesCallback);
  parser->AddDICOMTagCallback(0x0020, 0x1206, DICOMParser::VR_IS, NumberOfStudyRelatedSeriesCB);

  StudyDateCB->SetCallbackFunction(this, &DICOMAppHelper::StudyDateCallback);
  parser->AddDICOMTagCallback(0x0008, 0x0020, DICOMParser::VR_DA, StudyDateCB);

  ModalityCB->SetCallbackFunction(this, &DICOMAppHelper::ModalityCallback);
  parser->AddDICOMTagCallback(0x0008, 0x0060, DICOMParser::VR_CS, ModalityCB);

  ManufacturerCB->SetCallbackFunction(this, &DICOMAppHelper::ManufacturerCallback);
  parser->AddDICOMTagCallback(0x0008, 0x0070, DICOMParser::VR_LO, ManufacturerCB);

  InstitutionCB->SetCallbackFunction(this, &DICOMAppHelper::InstitutionCallback);
  parser->AddDICOMTagCallback(0x0008, 0x0080, DICOMParser::VR_LO, InstitutionCB);

  ModelCB->SetCallbackFunction(this, &DICOMAppHelper::ModelCallback);
  parser->AddDICOMTagCallback(0x0008, 0x1090, DICOMParser::VR_LO, ModelCB);

  ScanOptionsCB->SetCallbackFunction(this, &DICOMAppHelper::ScanOptionsCallback);
  parser->AddDICOMTagCallback(0x0018, 0x0022, DICOMParser::VR_CS, ScanOptionsCB);

  // Add in default callbacks for tags we need to see but not cache
  //parser->AddDICOMTagCallback(0x3006, 0x0012, DICOMParser::VR_DS, DefaultCB);
  DICOMTagInfo dicom_tags[] = {
    {0x0002, 0x0002, DICOMParser::VR_UI, "Media storage SOP class uid"},
    {0x0002, 0x0003, DICOMParser::VR_UI, "Media storage SOP inst uid"},
    {0x0002, 0x0010, DICOMParser::VR_UI, "Transfer syntax uid"},
    {0x0002, 0x0012, DICOMParser::VR_UI, "Implementation class uid"},
    {0x0008, 0x0018, DICOMParser::VR_UI, "Image UID"},
    {0x0008, 0x0020, DICOMParser::VR_DA, "Series date"},
    {0x0008, 0x0030, DICOMParser::VR_TM, "Series time"},
    {0x0008, 0x0060, DICOMParser::VR_SH, "Modality"},
    {0x0008, 0x0070, DICOMParser::VR_SH, "Manufacturer"},
    {0x0008, 0x0080, DICOMParser::VR_LO, "Institution"},
    {0x0008, 0x1060, DICOMParser::VR_SH, "Physician"},
    {0x0008, 0x1090, DICOMParser::VR_LO, "Model"},
    {0x0008, 0x103E, DICOMParser::VR_LO, "Series description"},
    {0x0010, 0x0010, DICOMParser::VR_PN, "Patient name"},
    {0x0010, 0x0020, DICOMParser::VR_LO, "Patient ID"},
    {0x0010, 0x0040, DICOMParser::VR_CS, "Patient sex"},
    {0x0010, 0x1010, DICOMParser::VR_AS, "Patient age"},
    {0x0010, 0x0030, DICOMParser::VR_DA, "Patient Date of birth"},
    {0x0020, 0x0010, DICOMParser::VR_SH, "Study ID"},
    {0x0008, 0x1030, DICOMParser::VR_LO, "Study Description"},
    {0x0018, 0x0015, DICOMParser::VR_CS, "Body Part"},
    {0x0020, 0x1000, DICOMParser::VR_IS, "Number of series in study"},
    {0x0020, 0x1206, DICOMParser::VR_IS, "Number of study related series"},
    {0x0018, 0x0022, DICOMParser::VR_FL, "Scan options"},  
    {0x0018, 0x0050, DICOMParser::VR_FL, "slice thickness"},
    {0x0018, 0x0060, DICOMParser::VR_FL, "kV"},
    {0x0018, 0x0088, DICOMParser::VR_FL, "slice spacing"},
    {0x0018, 0x1100, DICOMParser::VR_SH, "Recon diameter"},
    {0x0018, 0x1151, DICOMParser::VR_FL, "mA"},
    {0x0018, 0x1210, DICOMParser::VR_SH, "Recon kernel"},
    {0x0020, 0x000d, DICOMParser::VR_UI, "Study UID"},
    {0x0020, 0x000e, DICOMParser::VR_UI, "Series UID"},
    {0x0020, 0x0013, DICOMParser::VR_IS, "Image number"},
    {0x0020, 0x0032, DICOMParser::VR_SH, "Patient position"},
    {0x0020, 0x0037, DICOMParser::VR_SH, "Patient position cosines"},
    {0x0020, 0x1041, DICOMParser::VR_SS, "Slice location"},
    {0x0028, 0x0010, DICOMParser::VR_FL, "Num rows"},
    {0x0028, 0x0011, DICOMParser::VR_FL, "Num cols"},
    {0x0028, 0x0030, DICOMParser::VR_FL, "pixel spacing"},
    {0x0028, 0x0100, DICOMParser::VR_US, "Bits allocated"},
    {0x0028, 0x0120, DICOMParser::VR_UL, "pixel padding"},
    {0x0028, 0x1052, DICOMParser::VR_FL, "pixel offset"},
    {0x3006, 0x0039, DICOMParser::VR_SQ, "ROI Contour Sequence"},
    {0x3006, 0x0040, DICOMParser::VR_SQ, "Contour Sequence"},
    {0x3006, 0x0046, DICOMParser::VR_IS, "Number Of Contour Points"},
    {0x3006, 0x0050, DICOMParser::VR_DS, "Contour Data"}
  };

  int num_tags = sizeof(dicom_tags)/sizeof(DICOMTagInfo);

#ifdef DEBUG_DICOM_APP_HELPER
  DICOMMemberCallback<DICOMAppHelper>** callbackArray = new DICOMMemberCallback<DICOMAppHelper>*[num_tags];
#endif

  for (int j = 0; j < num_tags; j++)
    {
    //
    // Setup internal map.
    //
    DICOMTagInfo tagStruct = dicom_tags[j];
    doublebyte group = tagStruct.group;
    doublebyte element = tagStruct.element;

    dicom_stl::pair<doublebyte, doublebyte> gePair(group, element);
    dicom_stl::pair<const dicom_stl::pair<doublebyte, doublebyte>, DICOMTagInfo> mapPair(gePair, tagStruct);
    this->Implementation->TagMap.insert(mapPair);

#ifdef aDEBUG_DICOM_APP_HELPER
    //
    // Make callback
    //
    callbackArray[j] = new DICOMMemberCallback<DICOMAppHelper>;
    callbackArray[j]->SetCallbackFunction(this, &DICOMAppHelper::ArrayCallback);
    //
    // Set callback on parser.
    //
    parser->AddDICOMTagCallback(group, element,datatype, callbackArray[j]);
#endif

    }

}

void DICOMAppHelper::DefaultCallback(DICOMParser *,
                                     doublebyte,
                                     doublebyte,
                                     DICOMParser::VRTypes,
                                     unsigned char*,
                                     quadbyte)
{
#ifdef DEBUG_DICOM_APP_HELPER  
  dicom_stream::cout << "Default callback " << dicom_stream::endl;
#endif
}

void DICOMAppHelper::InstanceUIDCallback(DICOMParser *parser,
                                       doublebyte,
                                       doublebyte,
                                       DICOMParser::VRTypes,
                                       unsigned char* val,
                                       quadbyte len) 
{
  if (len == 0)
    {
#ifdef DEBUG_DICOM_APP_HELPER  
    dicom_stream::cout << "Instance UID: (EMPTY)" << dicom_stream::endl;
#endif
    
    // empty the string
    this->InstanceUID = dicom_stl::string();

    // just return, don't bother updating the internal databases of UID's
    return;
    }
  
  char* newString = (char*) val;
  dicom_stl::string newStdString(newString);

  // An anonymized UID, generate one
  if (newStdString == "0.0.0.0")
    {
    char fakeUID[2048];
    sprintf (fakeUID, "%d.%d.%d.%d", 0, 0, 0, ++this->FileCount);
    newStdString = fakeUID;
    }
  // cache the instance UID
  this->InstanceUID = newStdString;
  
#ifdef DEBUG_DICOM_APP_HELPER  
  dicom_stream::cout << "Instance UID: " << newStdString << dicom_stream::endl;
#endif

  // Put the instance UID into the internal database mapping instance UIDs
  // to filenames
  this->Implementation
    ->InstanceUIDToFileNameMap.insert(dicom_stl::pair<const dicom_stl::string, dicom_stl::string>(this->InstanceUID, parser->GetFileName() ));
}


void DICOMAppHelper::ReferencedInstanceUIDCallback(DICOMParser *,
                                                   doublebyte,
                                                   doublebyte,
                                                   DICOMParser::VRTypes,
                                                   unsigned char* val,
                                                   quadbyte len) 
{
  if (len == 0)
    {
#ifdef DEBUG_DICOM_APP_HELPER  
    dicom_stream::cout << "Referenced Instance UID: (EMPTY)" << dicom_stream::endl;
#endif
    
    // just return, don't bother updating the internal databases of UID's
    return;
    }

  char* newString = (char*) val;
  dicom_stl::string newStdString(newString);

#ifdef DEBUG_DICOM_APP_HELPER  
  dicom_stream::cout << "Referenced Instance UID: " << newStdString << dicom_stream::endl;
#endif
  
  // store the referenced instance UID in the map for this series.
  // This vector of referenced instance UIDs should be in lock step
  // with the contours
  this->Implementation->SeriesUIDToReferencedInstanceUIDMap[this->CurrentSeriesUID].push_back( newStdString );
}

void DICOMAppHelper::ContourImageSequenceCallback(DICOMParser *,
                                                  doublebyte,
                                                  doublebyte,
                                                  DICOMParser::VRTypes,
                                                  unsigned char*,
                                                  quadbyte) 
{
  // Add a contour to the list of contours
  DICOMAppHelperImplementation::ContourType contour;
  this->Implementation->SeriesUIDToContoursMap[this->CurrentSeriesUID].push_back(contour);

#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "Contour Image Sequence. " << dicom_stream::endl;
#endif
}

void DICOMAppHelper::SeriesUIDCallback(DICOMParser *,
                                       doublebyte,
                                       doublebyte,
                                       DICOMParser::VRTypes,
                                       unsigned char* val,
                                       quadbyte len) 
{
  if (len == 0)
    {
#ifdef DEBUG_DICOM_APP_HELPER  
    dicom_stream::cout << "Series UID: (EMPTY)" << dicom_stream::endl;
#endif

    // clear the cached series uid
    this->CurrentSeriesUID = dicom_stl::string();
    
    // just return, don't bother updating the internal databases of UID's
    return;
    }

  char* newString = (char*) val;
  dicom_stl::string newStdString(newString);

  // An anonymized UID, set the the UID to 0.0.0,1
  if (newStdString == "0.0.0.0")
    {
    char fakeUID[2048];
    sprintf (fakeUID, "%d.%d.%d.%d", 0, 0, 0, 1);
    newStdString = fakeUID;
    }

#ifdef DEBUG_DICOM_APP_HELPER  
  dicom_stream::cout << "Series UID: " << newStdString << dicom_stream::endl;
#endif

  // Put the series UID into the internal database mapping instance
  // UIDs to SeriesUIDs
  this->Implementation
    ->InstanceUIDToSeriesUIDMap.insert(dicom_stl::pair<const dicom_stl::string, dicom_stl::string>(this->InstanceUID, newStdString ));
  
  // Put the series UID into the internal database mapping series UIDs
  // to InstanceUIDs
  DICOMAppHelperImplementation::SeriesUIDToInstanceUIDMapType::iterator iiter = this->Implementation->SeriesUIDToInstanceUIDMap.find(newStdString);
  if ( iiter == this->Implementation->SeriesUIDToInstanceUIDMap.end())
    {
    dicom_stl::vector<dicom_stl::string> newVector;
    
    newVector.push_back(this->InstanceUID);
    this->Implementation->SeriesUIDToInstanceUIDMap.insert(dicom_stl::pair<const dicom_stl::string, dicom_stl::vector<dicom_stl::string> > (newStdString, newVector));
    }
  else
    {
    (*iiter).second.push_back(this->InstanceUID);
    }
  
  // Put the series UID into the internal database mapping series UIDs
  // to contours
  DICOMAppHelperImplementation::SeriesUIDToContoursMapType::iterator citer = this->Implementation->SeriesUIDToContoursMap.find(newStdString);
  if ( citer == this->Implementation->SeriesUIDToContoursMap.end())
    {
    DICOMAppHelperImplementation::ContoursVectorType newVector;
    
    this->Implementation->SeriesUIDToContoursMap.insert(dicom_stl::pair<const dicom_stl::string, DICOMAppHelperImplementation::ContoursVectorType> (newStdString, newVector));
    }
  // Put the series UID into the internal database mapping series UIDs
  // to referenced instance UIDs
  DICOMAppHelperImplementation::SeriesUIDToReferencedInstanceUIDMapType::iterator riter = this->Implementation->SeriesUIDToReferencedInstanceUIDMap.find(newStdString);
  if ( riter == this->Implementation->SeriesUIDToReferencedInstanceUIDMap.end())
    {
    DICOMAppHelperImplementation::InstanceUIDVectorType newVector;
    
    this->Implementation->SeriesUIDToReferencedInstanceUIDMap.insert(dicom_stl::pair<const dicom_stl::string, DICOMAppHelperImplementation::InstanceUIDVectorType> (newStdString, newVector));
    }
  // cache the current series UID
  this->CurrentSeriesUID = newStdString;
}


void DICOMAppHelper::SeriesDescriptionCallback(DICOMParser *,
                                       doublebyte,
                                       doublebyte,
                                       DICOMParser::VRTypes,
                                       unsigned char* val,
                                       quadbyte len ) 
{
  if (len == 0)
    {
#ifdef DEBUG_DICOM_APP_HELPER  
    dicom_stream::cout << "Series Description: (EMPTY)" << dicom_stream::endl;
#endif

    // clear the cached series uid
    this->CurrentSeriesDescription = dicom_stl::string();
    
    // just return, don't bother updating the internal databases of UID's
    return;
    }

  char* newString = (char*) val;
  dicom_stl::string newStdString(newString);

#ifdef DEBUG_DICOM_APP_HELPER  
  dicom_stream::cout << "Series Description: " << newStdString << dicom_stream::endl;
#endif
  // Put the series description into the internal database mapping series UIDs
  // to InstanceUIDs
  DICOMAppHelperImplementation::SeriesUIDToSeriesDescriptionMapType::iterator iiter = this->Implementation->SeriesUIDToSeriesDescriptionMap.find(this->CurrentSeriesUID);
  if ( iiter == this->Implementation->SeriesUIDToSeriesDescriptionMap.end()) // if not found we insert
    {
    this->Implementation->SeriesUIDToSeriesDescriptionMap.insert(dicom_stl::pair<const dicom_stl::string, dicom_stl::string> (this->CurrentSeriesUID, newStdString));
    }
  this->CurrentSeriesDescription = newStdString;
}

void DICOMAppHelper::OutputSeries()
{
  dicom_stream::cout << dicom_stream::endl << dicom_stream::endl;
        
  for (DICOMAppHelperImplementation::SeriesUIDToInstanceUIDMapType::iterator iter = this->Implementation->SeriesUIDToInstanceUIDMap.begin();
       iter != this->Implementation->SeriesUIDToInstanceUIDMap.end();
       iter++)
    {
    dicom_stream::cout << "SERIES: " << (*iter).first.c_str() << dicom_stream::endl;
    dicom_stl::vector<dicom_stl::string>& v_ref = (*iter).second;
             
    for (dicom_stl::vector<dicom_stl::string>::iterator v_iter = v_ref.begin();
         v_iter != v_ref.end();
         v_iter++)
      {
      DICOMAppHelperImplementation::InstanceUIDToSliceOrderingMapType::iterator sn_iter = Implementation->InstanceUIDToSliceOrderingMap.find(*v_iter);

      int slice = -1;
      if (sn_iter != Implementation->InstanceUIDToSliceOrderingMap.end())
        {
        slice = (*sn_iter).second.SliceNumber;
        }
      dicom_stream::cout << "\t" << (*v_iter).c_str() << " : "
                         << this->Implementation->InstanceUIDToFileNameMap[*v_iter] << " : ";
      if (slice != -1)
        {
        dicom_stream::cout << " [SliceNumber = " << slice<< "] ";
        }
      if (this->Implementation->SeriesUIDToContoursMap[(*iter).first].size() != 0)
        {
        dicom_stream::cout << " [Number of contours = "
                           << this->Implementation->SeriesUIDToContoursMap[(*iter).first].size() << "] ";
        }
      dicom_stream::cout << dicom_stream::endl;
      }
                
    }
}


void DICOMAppHelper::ArrayCallback(DICOMParser *parser,
                                   doublebyte group,
                                   doublebyte element,
                                   DICOMParser::VRTypes datatype,
                                   unsigned char* val,
                                   quadbyte len) 
{
  const char* desc = "No description";
  
  TagMapType::iterator iter = this->Implementation->TagMap.find(dicom_stl::pair<doublebyte, doublebyte> (group, element));
  if (iter != this->Implementation->TagMap.end())
    {
    desc = (*iter).second.description;
    }

  int t2 = int((0x0000FF00 & datatype) >> 8);
  int t1 = int((0x000000FF & datatype));

  char ct2(t2);
  char ct1(t1);

    
  HeaderFile << "(0x";

  HeaderFile.width(4);
  char prev = HeaderFile.fill('0');

  HeaderFile << dicom_stream::hex << group;
  HeaderFile << ",0x";

  HeaderFile.width(4);
  HeaderFile.fill('0');
    
  HeaderFile << dicom_stream::hex << element;
  HeaderFile << ") ";

  HeaderFile.fill(prev);
  HeaderFile << dicom_stream::dec;
  HeaderFile << " " << ct1 << ct2 << " ";
  HeaderFile << "[" << len << " bytes] ";
  
  HeaderFile << desc << " : ";
  
  unsigned int uival = 0;
  float fval = 0;
  double dval = 0;
  int ival = 0;

  if (val)
    {
    switch (datatype)
      {
      case DICOMParser::VR_AE:
      case DICOMParser::VR_AS:
      case DICOMParser::VR_CS:
      case DICOMParser::VR_UI:
      case DICOMParser::VR_DA:
      case DICOMParser::VR_DS:
      case DICOMParser::VR_DT:
      case DICOMParser::VR_LO:
      case DICOMParser::VR_LT:
      case DICOMParser::VR_OB: // ordered bytes
      case DICOMParser::VR_OW: // ordered words
      case DICOMParser::VR_PN:
      case DICOMParser::VR_ST:
      case DICOMParser::VR_TM:
      case DICOMParser::VR_UN:
      case DICOMParser::VR_UT:
      case DICOMParser::VR_SQ: // sequence
      case DICOMParser::VR_SH: // strings
      case DICOMParser::VR_IS:
        HeaderFile << val;
        break;
      case DICOMParser::VR_FL: // float
        fval = static_cast<float> (atof((char*) val));
        HeaderFile << fval;
        break;
      case DICOMParser::VR_FD: // float double
        fval = static_cast<float> (atof((char*) val));
        HeaderFile << dval;
        break;
      case DICOMParser::VR_UL: // unsigned long
      case DICOMParser::VR_SL: // signed long
      case DICOMParser::VR_AT:
        HeaderFile << uival;
        break;
      //case DICOMParser::VR_IS:
      //  ival = DICOMFile::ReturnAsSignedLong(val, parser->GetDICOMFile()->GetPlatformIsBigEndian()); 
      //  HeaderFile << ival;
      //  break;
      case DICOMParser::VR_SS:
        ival = DICOMFile::ReturnAsSignedShort(val, parser->GetDICOMFile()->GetPlatformIsBigEndian()); 
        HeaderFile << ival;
        break;
      case DICOMParser::VR_US: // unsigned short
        uival = DICOMFile::ReturnAsUnsignedShort(val, parser->GetDICOMFile()->GetPlatformIsBigEndian()); 
        HeaderFile << uival;
        break;
      default:
        HeaderFile << val << dicom_stream::endl;
        break;
      }
    }
  else
    {
    HeaderFile << "NULL";
    }
  
  HeaderFile << dicom_stream::dec << dicom_stream::endl;
  HeaderFile.fill(prev);

  delete [] val;
}
    
void DICOMAppHelper::SliceNumberCallback(DICOMParser *,
                                         doublebyte,
                                         doublebyte,
                                         DICOMParser::VRTypes,
                                         unsigned char* val,
                                         quadbyte len) 
{
  // Look for the current instance UID in the map of slice ordering data
  DICOMAppHelperImplementation::InstanceUIDToSliceOrderingMapType::iterator it;
  it = this->Implementation->InstanceUIDToSliceOrderingMap.find( this->InstanceUID );
  if (it == Implementation->InstanceUIDToSliceOrderingMap.end())
    {
    // instance UID not found, create a new entry
    DICOMOrderingElements ord;
    if (len > 0)
      {
      ord.SliceNumber = atoi( (char *) val);
      }

    // insert into the map
    this->Implementation->InstanceUIDToSliceOrderingMap.insert(dicom_stl::pair<const dicom_stl::string, DICOMOrderingElements>(this->InstanceUID, ord));
    }
  else
    {
    // file found, add new values
    if (len > 0)
      {
      (*it).second.SliceNumber = atoi( (char *)val );
      }
    }

  // cache the slice number
  if (len > 0)
    {
    this->SliceNumber = atoi( (char *) val);
    }
  else
    {
    this->SliceNumber = -1; // default to an unset slice number
    }
}


void DICOMAppHelper::SliceLocationCallback(DICOMParser *,
                                           doublebyte,
                                           doublebyte,
                                           DICOMParser::VRTypes,
                                           unsigned char* val,
                                           quadbyte len) 
{
  // Look for the current instance UID in the map of slice ordering data
  DICOMAppHelperImplementation::InstanceUIDToSliceOrderingMapType::iterator it;
  it = this->Implementation->InstanceUIDToSliceOrderingMap.find( this->InstanceUID );
  if (it == Implementation->InstanceUIDToSliceOrderingMap.end())
    {
    // instance UID not found, create a new entry
    DICOMOrderingElements ord;
    if (len > 0)
      {
      ord.SliceLocation = (float)atof( (char *) val);
      }

    // insert into the map
    this->Implementation->InstanceUIDToSliceOrderingMap.insert(dicom_stl::pair<const dicom_stl::string, DICOMOrderingElements>(this->InstanceUID, ord));
    }
  else
    {
    // file found, add new values
    if (len > 0)
      {
      (*it).second.SliceLocation = (float)atof( (char *)val );
      }
    }
}

void DICOMAppHelper::ImagePositionPatientCallback(DICOMParser *,
                                                  doublebyte,
                                                  doublebyte,
                                                  DICOMParser::VRTypes,
                                                  unsigned char* val,
                                                  quadbyte) 
{
  // Look for the current instance UID in the map of slice ordering data
  DICOMAppHelperImplementation::InstanceUIDToSliceOrderingMapType::iterator it;
  it = this->Implementation->InstanceUIDToSliceOrderingMap.find( this->InstanceUID );
  if (it == Implementation->InstanceUIDToSliceOrderingMap.end())
    {
    // instance UID not found, create a new entry
    DICOMOrderingElements ord;

    if (val)
      {
      sscanf( (char*)(val), "%f\\%f\\%f",
              &ord.ImagePositionPatient[0],
              &ord.ImagePositionPatient[1],
              &ord.ImagePositionPatient[2] );
      }
    else
      {
      // no actual position specified, default to the origin
      ord.ImagePositionPatient[0] = 0.0f;
      ord.ImagePositionPatient[1] = 0.0f;
      ord.ImagePositionPatient[2] = 0.0f;
      }
      
    // insert into the map
    this->Implementation->InstanceUIDToSliceOrderingMap.insert(dicom_stl::pair<const dicom_stl::string, DICOMOrderingElements>(this->InstanceUID, ord));

    // cache the value
    memcpy( this->ImagePositionPatient, ord.ImagePositionPatient,
            3*sizeof(float) );
    }
  else
    {
    if (val)
      {
      // file found, add new values
      sscanf( (char*)(val), "%f\\%f\\%f",
              &(*it).second.ImagePositionPatient[0],
              &(*it).second.ImagePositionPatient[1],
              &(*it).second.ImagePositionPatient[2] );
      }
    else
      {
      // no actual position specified, default to the origin
      (*it).second.ImagePositionPatient[0] = 0.0f;
      (*it).second.ImagePositionPatient[1] = 0.0f;
      (*it).second.ImagePositionPatient[2] = 0.0f;
      }
    
    // cache the value
    memcpy( this->ImagePositionPatient, (*it).second.ImagePositionPatient,
            3*sizeof(float) );
    }
}


void DICOMAppHelper::ImageOrientationPatientCallback(DICOMParser *,
                                                     doublebyte,
                                                     doublebyte,
                                                     DICOMParser::VRTypes,
                                                     unsigned char* val,
                                                     quadbyte) 
{
  // Look for the current instance UID in the map of slice ordering data
  DICOMAppHelperImplementation::InstanceUIDToSliceOrderingMapType::iterator it;
  it = this->Implementation->InstanceUIDToSliceOrderingMap.find(this->InstanceUID);
  if (it == Implementation->InstanceUIDToSliceOrderingMap.end())
    {
    // instance UID not found, create a new entry
    DICOMOrderingElements ord;
    if (val)
      {
      sscanf( (char*)(val), "%f\\%f\\%f\\%f\\%f\\%f",
              &ord.ImageOrientationPatient[0],
              &ord.ImageOrientationPatient[1],
              &ord.ImageOrientationPatient[2],
              &ord.ImageOrientationPatient[3],
              &ord.ImageOrientationPatient[4],
              &ord.ImageOrientationPatient[5] );
      }
    else
      {
      // no orientation defined, default to an standard axial orientation
      ord.ImageOrientationPatient[0] = 1.0f;
      ord.ImageOrientationPatient[1] = 0.0f;
      ord.ImageOrientationPatient[2] = 0.0f;
      ord.ImageOrientationPatient[3] = 0.0f;
      ord.ImageOrientationPatient[4] = 1.0f;
      ord.ImageOrientationPatient[5] = 0.0f;
      }
    
    // insert into the map
    this->Implementation->InstanceUIDToSliceOrderingMap.insert(dicom_stl::pair<const dicom_stl::string, DICOMOrderingElements>(this->InstanceUID, ord));
    }
  else
    {
    // file found, add new values
    if (val)
      {
      sscanf( (char*)(val), "%f\\%f\\%f\\%f\\%f\\%f",
              &(*it).second.ImageOrientationPatient[0],
              &(*it).second.ImageOrientationPatient[1],
              &(*it).second.ImageOrientationPatient[2],
              &(*it).second.ImageOrientationPatient[3],
              &(*it).second.ImageOrientationPatient[4],
              &(*it).second.ImageOrientationPatient[5] );
      }
    else
      {
      // no orientation defined, default to an standard axial orientation
      (*it).second.ImageOrientationPatient[0] = 1.0f;
      (*it).second.ImageOrientationPatient[1] = 0.0f;
      (*it).second.ImageOrientationPatient[2] = 0.0f;
      (*it).second.ImageOrientationPatient[3] = 0.0f;
      (*it).second.ImageOrientationPatient[4] = 1.0f;
      (*it).second.ImageOrientationPatient[5] = 0.0f;
      }
    }
}


void DICOMAppHelper::TransferSyntaxCallback(DICOMParser *parser,
                                            doublebyte,
                                            doublebyte,
                                            DICOMParser::VRTypes,
                                            unsigned char* val,
                                            quadbyte) 
{

#ifdef DEBUG_DICOM_APP_HELPER
#ifdef WIN32
  char platformByteOrder = 'L';
#else
  char platformByteOrder = 'B';
#endif
  dicom_stream::cout << "Platform byte order: " << platformByteOrder << dicom_stream::endl;
#endif

  static const char* TRANSFER_UID_EXPLICIT_BIG_ENDIAN = "1.2.840.10008.1.2.2";

  // Only add the ToggleSwapBytes callback when we need it.
  if (strcmp(TRANSFER_UID_EXPLICIT_BIG_ENDIAN, (char*) val) == 0)
    {
    this->ByteSwapData = true;
    parser->AddDICOMTagCallback(0x0800, 0x0000, DICOMParser::VR_UNKNOWN, ToggleSwapBytesCB);
#ifdef DEBUG_DICOM_APP_HELPER
    dicom_stream::cerr <<"Registering callback for swapping bytes." << dicom_stream::endl;
#endif
    }
  
  if (this->TransferSyntaxUID)
    {
    delete this->TransferSyntaxUID;
    }
  this->TransferSyntaxUID = new dicom_stl::string((char*) val);

#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "Transfer Syntax UID: " << *this->TransferSyntaxUID;
  dicom_stream::cout << " " << this->TransferSyntaxUIDDescription(this->TransferSyntaxUID->c_str()) << dicom_stream::endl;
#endif
}

void DICOMAppHelper::BitsAllocatedCallback(DICOMParser *parser,
                                           doublebyte,
                                           doublebyte,
                                           DICOMParser::VRTypes,
                                           unsigned char* val,
                                           quadbyte len) 
{
  if (len == 0)
    {
    // no value, clear the cached field, return to default BitsAllocated
    this->BitsAllocated = 8;
    return;
    }
  
  this->BitsAllocated = parser->GetDICOMFile()->ReturnAsUnsignedShort(val, parser->GetDICOMFile()->GetPlatformIsBigEndian());
#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "Bits allocated: " << this->BitsAllocated << dicom_stream::endl;
#endif
}


void DICOMAppHelper::ToggleSwapBytesCallback(DICOMParser *parser,
                                             doublebyte,
                                             doublebyte,
                                             DICOMParser::VRTypes,
                                             unsigned char* ,
                                             quadbyte len) 
{
#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "ToggleSwapBytesCallback" << dicom_stream::endl;
#endif
  bool bs = parser->GetDICOMFile()->GetPlatformIsBigEndian();
  parser->GetDICOMFile()->SetPlatformIsBigEndian(!bs);

#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "Set byte swap to: " << parser->GetDICOMFile()->GetPlatformIsBigEndian() << dicom_stream::endl;
#endif

  long pos = parser->GetDICOMFile()->Tell();

  //
  // The +4 is probably a hack, but it's a guess at the length of the previous field.
  //
  parser->GetDICOMFile()->SkipToPos(pos - len + 4);
}


void DICOMAppHelper::PixelSpacingCallback(DICOMParser *parser,
                                          doublebyte group,
                                          doublebyte element,
                                          DICOMParser::VRTypes,
                                          unsigned char* val,
                                          quadbyte len) 
{

  if (group == 0x0028 && element == 0x0030)
    {
    float fval = 1.0f; // defaul of 1mm
    if (len > 0)
      {
      fval = DICOMFile::ReturnAsFloat(val, parser->GetDICOMFile()->GetPlatformIsBigEndian());
      this->PixelSpacing[1] = fval;
      unsigned char * val2 = val;  // search for separator
      while( *val2 != '\\' && *val2 != 0 ) val2++;
      if( *val2 == 0 )
        {
        dicom_stream::cerr << "Pixel spacing is missing separator!" << dicom_stream::endl;
        }
      else
        {
        val2++; // start in the character after the delimiter.
        fval = DICOMFile::ReturnAsFloat(val2, parser->GetDICOMFile()->GetPlatformIsBigEndian());
        this->PixelSpacing[0] = fval;
        }
      }
    else
      {
      this->PixelSpacing[0] = this->PixelSpacing[1] = fval;
      }
    }
  else if (group == 0x0018 && element == 0x0050)
    {
    float fval = 1.0f; // defaul of 1mm
    if (len > 0)
      {
      fval = DICOMFile::ReturnAsFloat(val, parser->GetDICOMFile()->GetPlatformIsBigEndian());
      }
    this->PixelSpacing[2] = fval;
    }
}

void DICOMAppHelper::WidthCallback(DICOMParser *parser,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  unsigned short uival = 0;

  if (len > 0)
    {
    uival = DICOMFile::ReturnAsUnsignedShort(val, parser->GetDICOMFile()->GetPlatformIsBigEndian());
    }
  
#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "Width: " << uival << dicom_stream::endl;
#endif

  this->Width = uival;
  this->Dimensions[0] = this->Width;
}

void DICOMAppHelper::HeightCallback(DICOMParser *parser,
                                    doublebyte,
                                    doublebyte,
                                    DICOMParser::VRTypes,
                                    unsigned char* val,
                                    quadbyte len) 
{
  unsigned short uival = 0;

  if (len > 0)
    {
    uival = DICOMFile::ReturnAsUnsignedShort(val, parser->GetDICOMFile()->GetPlatformIsBigEndian());
    }
  
#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "Height: " << uival << dicom_stream::endl;
#endif
  this->Height = uival;
  this->Dimensions[1] = this->Height;
}


void DICOMAppHelper::PixelRepresentationCallback( DICOMParser *parser,
                                                  doublebyte,
                                                  doublebyte,
                                                  DICOMParser::VRTypes,
                                                  unsigned char* val,
                                                  quadbyte len)
{
  unsigned short uival = 1; // default of unsigned

  if (len > 0)
    {
    uival = DICOMFile::ReturnAsUnsignedShort(val, parser->GetDICOMFile()->GetPlatformIsBigEndian());
    }
  
#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "Pixel Representation: " << (uival ? "Signed" : "Unsigned") << dicom_stream::endl;
#endif
  this->PixelRepresentation = uival;
}

void DICOMAppHelper::PhotometricInterpretationCallback( DICOMParser *,
                                                        doublebyte,
                                                        doublebyte,
                                                        DICOMParser::VRTypes,
                                                        unsigned char* val,
                                                        quadbyte len)
{
#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "Photometric Interpretation: " << (char*) val << dicom_stream::endl;
#endif
  if (this->PhotometricInterpretation)
    {
    delete this->PhotometricInterpretation;
    }

  if (len > 0)
    {
    this->PhotometricInterpretation = new dicom_stl::string((char*) val);
    }
  else
    {
    this->PhotometricInterpretation = NULL;
    }
}

void DICOMAppHelper::PixelDataCallback( DICOMParser *,
                                        doublebyte,
                                        doublebyte,
                                        DICOMParser::VRTypes,
                                        unsigned char* data,
                                        quadbyte len)
{
  int numPixels = this->Dimensions[0] * this->Dimensions[1] * this->GetNumberOfComponents();

  // if length was undefined, i.e. 0xffff, then use numpixels, otherwise...
  if (len != 0xffff)
    {
    // length was specified, but only read up to len bytes (as
    // opposed to the image size times number of components)
    if (len < numPixels)
      {
      numPixels = len;
      }
    if (numPixels < 0)
      {
      numPixels = 0;
      }
    }

#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "numPixels : " << numPixels << dicom_stream::endl;
#endif

  int ptrIncr = int(this->BitsAllocated/8.0);

  unsigned short* ushortInputData = reinterpret_cast<unsigned short*>(data);
  unsigned char* ucharInputData = data;
  short* shortInputData = reinterpret_cast<short*> (data);

  float* floatOutputData; // = NULL;
  
  bool isFloat = this->RescaledImageDataIsFloat();

  if (isFloat)
    {
#ifdef DEBUG_DICOM_APP_HELPER
    dicom_stream::cout << "Slope and offset are not integer valued : ";
    dicom_stream::cout << this->RescaleSlope << ", " << this->RescaleOffset << dicom_stream::endl;
#endif
    if (this->ImageData)
      {
      delete [] (static_cast<char*> (this->ImageData));
      }
    this->ImageData = new float[numPixels];
    floatOutputData = static_cast<float*> (this->ImageData);

    this->ImageDataType = DICOMParser::VR_FL;
    this->ImageDataLengthInBytes = numPixels * sizeof(float);
    float newFloatPixel;

    if (ptrIncr == 1)
      {
      for (int i = 0; i < numPixels; i++)
        {
        newFloatPixel = float(this->RescaleSlope * ucharInputData[i] + this->RescaleOffset);
        floatOutputData[i] = newFloatPixel;
        }
#ifdef DEBUG_DICOM_APP_HELPER
      dicom_stream::cout << "Did rescale, offset to float from char." << dicom_stream::endl;
      dicom_stream::cout << numPixels << " pixels." << dicom_stream::endl;
#endif
      }
    else if (ptrIncr == 2)
      {
      for (int i = 0; i < numPixels; i++)
        {
        newFloatPixel = float(this->RescaleSlope * ushortInputData[i] + this->RescaleOffset);
        floatOutputData[i] = newFloatPixel;
        }
#ifdef DEBUG_DICOM_APP_HELPER
      dicom_stream::cout << "Did rescale, offset to float from short." << dicom_stream::endl;
      dicom_stream::cout << numPixels << " pixels." << dicom_stream::endl;
#endif
      }
    }
  else
    {
#ifdef DEBUG_DICOM_APP_HELPER
    dicom_stream::cout << "Slope and offset are integer valued : ";
    dicom_stream::cout << this->RescaleSlope << ", " << this->RescaleOffset << dicom_stream::endl;
#endif

    if (ptrIncr == 1)
      {
      if (this->ImageData)
        {
        delete [] (static_cast<char*> (this->ImageData));
        }
      this->ImageData = new char[numPixels];
  
      char*  charOutputData =  static_cast<char*>  (this->ImageData);

      this->ImageDataType = DICOMParser::VR_OB;
      this->ImageDataLengthInBytes = numPixels * sizeof(char);
      char newCharPixel;

      for (int i = 0; i < numPixels; i++)
        {
        newCharPixel = char(this->RescaleSlope * ucharInputData[i] + this->RescaleOffset);
        charOutputData[i] = newCharPixel;
        }
#ifdef DEBUG_DICOM_APP_HELPER
      dicom_stream::cout << "Did rescale, offset to char from char." << dicom_stream::endl;
      dicom_stream::cout << numPixels << " pixels." << dicom_stream::endl;
#endif
      }
    else if (ptrIncr == 2)
      {
      if (this->ImageData)
        {
        delete [] (static_cast<char*> (this->ImageData));
        }
      this->ImageData = new short[numPixels];
      short* shortOutputData = static_cast<short*> (this->ImageData);

      this->ImageDataType = DICOMParser::VR_OW;
      this->ImageDataLengthInBytes = numPixels * sizeof(short);
      short newShortPixel;
      for (int i = 0; i < numPixels; i++)
        {
        newShortPixel = short(this->RescaleSlope * shortInputData[i] + this->RescaleOffset);
        shortOutputData[i] = newShortPixel;
        }
#ifdef DEBUG_DICOM_APP_HELPER
      dicom_stream::cout << "Did rescale, offset to short from short." << dicom_stream::endl;
      dicom_stream::cout << numPixels << " pixels." << dicom_stream::endl;
#endif
      }
    }
}


void DICOMAppHelper::ROIContourSequenceCallback( DICOMParser *,
                                        doublebyte,
                                        doublebyte,
                                        DICOMParser::VRTypes,
                                        unsigned char*,
                                        quadbyte )
{

#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "ROIContourSequence. " << dicom_stream::endl;
#endif

}

void DICOMAppHelper::ContourSequenceCallback( DICOMParser *,
                                        doublebyte,
                                        doublebyte,
                                        DICOMParser::VRTypes,
                                        unsigned char*,
                                        quadbyte)
{
  
#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "ContourSequence." << dicom_stream::endl;
#endif

}

void DICOMAppHelper::ContourGeometricTypeCallback( DICOMParser *,
                                                   doublebyte,
                                                   doublebyte,
                                                   DICOMParser::VRTypes,
                                                   unsigned char*,
                                                   quadbyte)
{
  
#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "ContourGeometricType." << dicom_stream::endl;
#endif

}

void DICOMAppHelper::ContourDataCallback( DICOMParser *,
                                        doublebyte,
                                        doublebyte,
                                        DICOMParser::VRTypes,
                                        unsigned char* data,
                                        quadbyte len)
{

  // If we haven't added a contour yet, then we must have missed the tag
  // to start the contour
  if (this->Implementation->SeriesUIDToContoursMap[this->CurrentSeriesUID].size() == 0)
    {
    dicom_stream::cerr << "DICOMAppHelper:: Found contour data tag (0x3006, 0x0050) without a matching contour sequence tag (0x3006, 0x0040)." << dicom_stream::endl;
    }
  // If the number of points in the contour is zero, then we were not
  // expecting any data or we missed the tag for the number of contour
  // points.
  else if (this->Implementation->SeriesUIDToContoursMap[this->CurrentSeriesUID].back().size() == 0)  // Note the test for equality
    {
    dicom_stream::cerr << "DICOMAppHelper:: Found contour data tag (0x3006, 0x0050) without a matching number of contour points tag (0x3006, 0x0046)." << dicom_stream::endl;
    }
  else
    {
    // read the number of points Contours.back().size()
    //
    //
    unsigned int i;
    float p;
    
    // get a reference to the contour 
    DICOMAppHelperImplementation::ContourType
      &contour = this->Implementation->SeriesUIDToContoursMap[this->CurrentSeriesUID].back();

    // read the coordinates. the space for the points has already been
    // allocated in NumberOfContourPointsCallback().  we just need to
    // parse the (x, y, z) values and put them in the contour vector.
    // Points are stored in a vector of floats (x, y, z, x, y, z, ...)
    //

    // create a temporary (null terminated) buffer that we can tokenize
    unsigned char *tdata = new unsigned char[len+1];
    memcpy((char *) tdata, (char *)data, len);
    tdata[len] = '\0';

    // tokenize and parse the buffer
    unsigned char *tPtr;
    tPtr = (unsigned char *)strtok((char *)tdata, "\\");
    for (i=0; i < contour.size(); i += 3)
      {
      sscanf( (char*)(tPtr), "%f", &p);
      contour[i] = p;
      tPtr = (unsigned char *)strtok(NULL, "\\");
      sscanf( (char*)(tPtr), "%f", &p);
      contour[i+1] = p;
      tPtr = (unsigned char *)strtok(NULL, "\\");
      sscanf( (char*)(tPtr), "%f", &p);
      contour[i+2] = p;
      tPtr = (unsigned char *)strtok(NULL, "\\");
      }

    delete [] tdata;
    }


#ifdef DEBUG_DICOM_APP_HELPER
  DICOMAppHelperImplementation::ContourType contour = this->Implementation->SeriesUIDToContoursMap[this->CurrentSeriesUID].back();
  dicom_stream::cout << "Contour with " << contour.size() / 3 << " points." << dicom_stream::endl;
  for (unsigned int i = 0; i < contour.size(); i+=3)
    {
    dicom_stream::cout << "[" << contour[i] << ", " << contour[i+1] << ", " << contour[i+2] << "]"
                      << dicom_stream::endl;
    }
#endif

}

void DICOMAppHelper::NumberOfContourPointsCallback( DICOMParser *,
                                                    doublebyte,
                                                    doublebyte,
                                                    DICOMParser::VRTypes,
                                                    unsigned char* data,
                                                    quadbyte)
{

  int n;
  sscanf( (char*)(data), "%d", &n);


  // If we haven't added a contour yet, then we must have missed the tag
  // to start the contour
  if (this->Implementation->SeriesUIDToContoursMap[this->CurrentSeriesUID].size() == 0)
    {
    dicom_stream::cerr << "DICOMAppHelper:: Found number of contour points tag (0x3006, 0x0046) without a matching contour sequence tag (0x3006, 0x0040)." << dicom_stream::endl;
    }
  // If the last contour is not empty, then we haven't started the
  // current contour.  A preceding call to ContourSequenceCallback()
  // would have created an empty contour at the end of the Contour
  // list.
  else if (this->Implementation->SeriesUIDToContoursMap[this->CurrentSeriesUID].back().size() != 0) 
    {
    dicom_stream::cerr << "DICOMAppHelper:: Found number of contour points tag (0x3006, 0x0046) without a matching contour geometric type tag (0x3006, 0x0042)." << dicom_stream::endl;
    }
  else
    {
    // reserve enough space for the points in the contour (3 floats
    // per point).  note that we trigger off this size later to parse
    // the coordinates of the contours
    this->Implementation->SeriesUIDToContoursMap[this->CurrentSeriesUID].back().resize( 3*n ); 
    }
  
#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "NumberOfContourPoints : ";
  dicom_stream::cout << n << dicom_stream::endl;
#endif

}


void DICOMAppHelper::RegisterPixelDataCallback(DICOMParser* parser)
{
  this->PixelDataCB->SetCallbackFunction(this, &DICOMAppHelper::PixelDataCallback);
  parser->AddDICOMTagCallback(0x7FE0, 0x0010, DICOMParser::VR_OW, this->PixelDataCB);
}


const char* DICOMAppHelper::TransferSyntaxUIDDescription(const char* uid)
{
  static const char* DICOM_IMPLICIT_VR_LITTLE_ENDIAN = "1.2.840.10008.1.2";
  static const char* DICOM_LOSSLESS_JPEG = "1.2.840.10008.1.2.4.70";
  static const char* DICOM_LOSSY_JPEG_8BIT = "1.2.840.10008.1.2.4.50";
  static const char* DICOM_LOSSY_JPEG_16BIT = "1.2.840.10008.1.2.4.51";
  static const char* DICOM_EXPLICIT_VR_LITTLE_ENDIAN = "1.2.840.10008.1.2.1";
  static const char* DICOM_EXPLICIT_VR_BIG_ENDIAN = "1.2.840.10008.1.2.2";
  static const char* DICOM_GE_PRIVATE_IMPLICIT_BIG_ENDIAN = "1.2.840.113619.5.2";

  if (!strcmp(DICOM_IMPLICIT_VR_LITTLE_ENDIAN, uid))
    {
    return "Implicit VR, Little Endian";
    }
  else if (!strcmp(DICOM_LOSSLESS_JPEG, uid))
    {
    return "Lossless JPEG";
    }
  else if (!strcmp(DICOM_LOSSY_JPEG_8BIT, uid))
    {
    return "Lossy JPEG 8 bit";
    }
  else if (!strcmp(DICOM_LOSSY_JPEG_16BIT, uid))
    {
    return "Lossy JPEG 16 bit.";
    }
  else if (!strcmp(DICOM_EXPLICIT_VR_LITTLE_ENDIAN, uid))
    {
    return "Explicit VR, Little Endian.";
    }
  else if (!strcmp(DICOM_EXPLICIT_VR_BIG_ENDIAN, uid))
    {
    return "Explicit VR, Big Endian.";
    }
  else if (!strcmp(DICOM_GE_PRIVATE_IMPLICIT_BIG_ENDIAN, uid))
    {
    return "GE Private, Implicit VR, Big Endian Image Data.";
    }
  else
    {
    return "Unknown.";
    }

}


void DICOMAppHelper::RescaleOffsetCallback( DICOMParser *parser,
                                            doublebyte,
                                            doublebyte,
                                            DICOMParser::VRTypes,
                                            unsigned char* val,
                                            quadbyte len)
{
  float fval = 0.0f;

  if (len > 0)
    {
    fval = DICOMFile::ReturnAsFloat(val, parser->GetDICOMFile()->GetPlatformIsBigEndian());
    }
  
  this->RescaleOffset = fval;
#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "Pixel offset: " << this->RescaleOffset << dicom_stream::endl;
#endif
}

void DICOMAppHelper::RescaleSlopeCallback(DICOMParser *parser,
                                          doublebyte,
                                          doublebyte ,
                                          DICOMParser::VRTypes ,
                                          unsigned char* val,
                                          quadbyte len)
{
  float fval = 1.0f;

  if (len > 0)
    {
    fval = DICOMFile::ReturnAsFloat(val,
                      parser->GetDICOMFile()->GetPlatformIsBigEndian ());
    }
  
#ifdef DEBUG_DICOM_APP_HELPER
  dicom_stream::cout << "Rescale slope: " << fval << dicom_stream::endl;
#endif
  this->RescaleSlope = fval;
}

void DICOMAppHelper::PatientNameCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_PatientName, (const char*)val, len1);
    m_PatientName[len1] = '\0';
    }
  else
    {
    m_PatientName[0] = '\0';
    }
}

void DICOMAppHelper::PatientIDCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_PatientID, (const char*)val, len1);
    m_PatientID[len1] = '\0';
    }
  else
    {
    m_PatientID[0] = '\0';
    }
}

void DICOMAppHelper::PatientSexCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_PatientSex, (const char*)val, len1);
    m_PatientSex[len1] = '\0';
    }
  else
    {
    m_PatientSex[0] = '\0';
    }
}

void DICOMAppHelper::PatientAgeCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_PatientAge, (const char*)val, len1);
    m_PatientAge[len1] = '\0';
    }
  else
    {
    m_PatientAge[0] = '\0';
    }
}

void DICOMAppHelper::PatientDOBCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_PatientDOB, (const char*)val, len1);
    m_PatientDOB[len1] = '\0';
    }
  else
    {
    m_PatientDOB[0] = '\0';
    }
}


void DICOMAppHelper::StudyIDCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_StudyID, (const char*)val, len1);
    m_StudyID[len1] = '\0';
    }
  else
    {
    m_StudyID[0] = '\0';
    }
}


void DICOMAppHelper::StudyDescriptionCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_StudyDescription, (const char*)val, len1);
    m_StudyDescription[len1] = '\0';
    }
  else
    {
    m_StudyDescription[0] = '\0';
    }
}

void DICOMAppHelper::BodyPartCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_BodyPart, (const char*)val, len1);
    m_BodyPart[len1] = '\0';
    }
  else
    {
    m_BodyPart[0] = '\0';
    return;
    }

  char* newString = (char*) val;
  dicom_stl::string newStdString(newString);

#ifdef DEBUG_DICOM_APP_HELPER  
  dicom_stream::cout << "Body Part: " << newStdString << dicom_stream::endl;
#endif
  // Put the series description into the internal database mapping series UIDs
  // to InstanceUIDs
  DICOMAppHelperImplementation::SeriesUIDToBodyPartMapType::iterator iiter = this->Implementation->SeriesUIDToBodyPartMap.find(this->CurrentSeriesUID);
  if ( iiter == this->Implementation->SeriesUIDToBodyPartMap.end()) // if not found we insert
    {
    this->Implementation->SeriesUIDToBodyPartMap.insert(dicom_stl::pair<const dicom_stl::string, dicom_stl::string> (this->CurrentSeriesUID, newStdString));
    }
  this->CurrentBodyPart = newStdString;
} 


void DICOMAppHelper::NumberOfSeriesInStudyCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_NumberOfSeriesInStudy, (const char*)val, len1);
    m_NumberOfSeriesInStudy[len1] = '\0';
    }
  else
    {
    m_NumberOfSeriesInStudy[0] = '\0';
    }
}

void DICOMAppHelper::NumberOfStudyRelatedSeriesCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_NumberOfStudyRelatedSeries, (const char*)val, len1);
    m_NumberOfStudyRelatedSeries[len1] = '\0';
    }
  else
    {
    m_NumberOfStudyRelatedSeries[0] = '\0';
    }
} 

void DICOMAppHelper::StudyDateCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_StudyDate, (const char*)val, len1);
    m_StudyDate[len1] = '\0';
    }
  else
    {
    m_StudyDate[0] = '\0';
    }
}

void DICOMAppHelper::ModalityCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_Modality, (const char*)val, len1);
    m_Modality[len1] = '\0';
    }
  else
    {
    m_Modality[0] = '\0';
    }
}

void DICOMAppHelper::ManufacturerCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_Manufacturer, (const char*)val, len1);
    m_Manufacturer[len1] = '\0';
    }
  else
    {
    m_Manufacturer[0] = '\0';
    }
}

void DICOMAppHelper::InstitutionCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_Institution, (const char*)val, len1);
    m_Institution[len1] = '\0';
    }
  else
    {
    m_Institution[0] = '\0';
    }
}

void DICOMAppHelper::ModelCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_Model, (const char*)val, len1);
    m_Model[len1] = '\0';
    }
  else
    {
    m_Model[0] = '\0';
    }
}


void DICOMAppHelper::ScanOptionsCallback(DICOMParser *,
                                   doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte len)
{
  if (val)
    {
    quadbyte len1 = (len<512) ? len : 511;
    strncpy(m_ScanOptions, (const char*)val, len1);
    m_ScanOptions[len1] = '\0';
    }
  else
    {
    m_ScanOptions[0] = '\0';
    return;
    }

  char* newString = (char*) val;
  dicom_stl::string newStdString(newString);

#ifdef DEBUG_DICOM_APP_HELPER  
  dicom_stream::cout << "Scan Options: " << newStdString << dicom_stream::endl;
#endif
  // Put the series description into the internal database mapping series UIDs
  // to InstanceUIDs
  DICOMAppHelperImplementation::SeriesUIDToScanOptionsMapType::iterator iiter = this->Implementation->SeriesUIDToScanOptionsMap.find(this->CurrentSeriesUID);
  if ( iiter == this->Implementation->SeriesUIDToScanOptionsMap.end()) // if not found we insert
    {
    this->Implementation->SeriesUIDToScanOptionsMap.insert(dicom_stl::pair<const dicom_stl::string, dicom_stl::string> (this->CurrentSeriesUID, newStdString));
    }
  this->CurrentScanOptions = newStdString;
}

bool DICOMAppHelper::RescaledImageDataIsFloat()
{
  int s = int(this->RescaleSlope);
  int o = int(this->RescaleOffset);

  float sf = float(s);
  float of = float(o);

  double d1 = fabs(sf - this->RescaleSlope);
  double d2 = fabs(of - this->RescaleOffset);

  if (d1 > 0.0 || d2 > 0.0)
    {
    return true;
    }
  else
    {
    return false;
    }
}

void DICOMAppHelper::GetImageData(void*& data, DICOMParser::VRTypes& dataType, unsigned long& len)
{
  data = this->ImageData;
  dataType = this->ImageDataType;
  len = this->ImageDataLengthInBytes;
}

bool DICOMAppHelper::RescaledImageDataIsSigned()
{
  bool rescaleSigned = (this->RescaleSlope < 0.0);
  bool pixelRepSigned = (this->PixelRepresentation == 1);
  bool offsetSigned = (this->RescaleOffset < 0.0);
 
  return (rescaleSigned || pixelRepSigned || offsetSigned);
}

dicom_stl::string
DICOMAppHelper::GetFileName( const dicom_stl::string &instanceUID )
{
  dicom_stl::string ret("");

  DICOMAppHelperImplementation::InstanceUIDToFileNameMapType::iterator
    it = this->Implementation->InstanceUIDToFileNameMap.find( instanceUID );

  if (it != this->Implementation->InstanceUIDToFileNameMap.end() )
    {
    ret = (*it).second;
    }

  return ret;
}


void DICOMAppHelper::GetSliceNumberFilenamePairs(const dicom_stl::string &seriesUID,
                                                 dicom_stl::vector<dicom_stl::pair<int, dicom_stl::string> >& v,
                                                 bool ascending)
{
  v.clear();

  DICOMAppHelperImplementation::SeriesUIDToInstanceUIDMapType::iterator miter  = this->Implementation->SeriesUIDToInstanceUIDMap.find(seriesUID);

  if (miter == this->Implementation->SeriesUIDToInstanceUIDMap.end() )
    {
    return;
    }

  // grab the instance uids for the specified series
  dicom_stl::vector<dicom_stl::string> instanceUIDs = (*miter).second;

  for (dicom_stl::vector<dicom_stl::string>::iterator instanceIter = instanceUIDs.begin();
       instanceIter != instanceUIDs.end();
       instanceIter++)
       {
       dicom_stl::pair<int, dicom_stl::string> p;
       p.second = dicom_stl::string(this->Implementation->InstanceUIDToFileNameMap[*instanceIter]);
       int slice_number;
       DICOMAppHelperImplementation::InstanceUIDToSliceOrderingMapType::iterator sn_iter = Implementation->InstanceUIDToSliceOrderingMap.find(*instanceIter);
       // Only store files that have a valid slice number
       if (sn_iter != Implementation->InstanceUIDToSliceOrderingMap.end())
        {
        slice_number = (*sn_iter).second.SliceNumber;
        p.first = slice_number;
        v.push_back(p);
        }
       }
  if (ascending)
    {
    dicom_stl::sort(v.begin(), v.end(), lt_pair_int_string());
    }
  else
    {
    dicom_stl::sort(v.begin(), v.end(), gt_pair_int_string());
    }
}

void DICOMAppHelper::GetSliceNumberFilenamePairs(dicom_stl::vector<dicom_stl::pair<int, dicom_stl::string> >& v, bool ascending)
{
  // Default to using the first series
  if (this->Implementation->SeriesUIDToInstanceUIDMap.size() > 0)
    {
    this->GetSliceNumberFilenamePairs( (*this->Implementation->SeriesUIDToInstanceUIDMap.begin()).first, v, ascending );
    }
  else
    {
    v.clear();
    }
}

void DICOMAppHelper::GetSliceLocationFilenamePairs(const dicom_stl::string &seriesUID,
                                                   dicom_stl::vector<dicom_stl::pair<float, dicom_stl::string> >& v,
                                                   bool ascending)
{
  v.clear();

  DICOMAppHelperImplementation::SeriesUIDToInstanceUIDMapType::iterator miter  = this->Implementation->SeriesUIDToInstanceUIDMap.find(seriesUID);

  if (miter == this->Implementation->SeriesUIDToInstanceUIDMap.end() )
    {
    return;
    }

  // grab the instance UIDs for the specified series
  dicom_stl::vector<dicom_stl::string> instanceUIDs = (*miter).second;

  for (dicom_stl::vector<dicom_stl::string>::iterator instanceIter = instanceUIDs.begin();
       instanceIter != instanceUIDs.end();
       instanceIter++)
       {
       dicom_stl::pair<float, dicom_stl::string> p;
       p.second = dicom_stl::string(this->Implementation->InstanceUIDToFileNameMap[*instanceIter]);
       float slice_location;
       DICOMAppHelperImplementation::InstanceUIDToSliceOrderingMapType::iterator sn_iter = Implementation->InstanceUIDToSliceOrderingMap.find(*instanceIter);

       if (sn_iter != Implementation->InstanceUIDToSliceOrderingMap.end())
        {
        slice_location = (*sn_iter).second.SliceLocation;
        p.first = slice_location;
        v.push_back(p);
        }
       }
  if (ascending)
    {
    dicom_stl::sort(v.begin(), v.end(), lt_pair_float_string());
    }
  else
    {
    dicom_stl::sort(v.begin(), v.end(), gt_pair_float_string());
    }
}

void DICOMAppHelper::GetSliceLocationFilenamePairs(dicom_stl::vector<dicom_stl::pair<float, dicom_stl::string> >& v,
                                                   bool ascending)
{
  // Default to using the first series
  if (this->Implementation->SeriesUIDToInstanceUIDMap.size() > 0)
    {
    this->GetSliceLocationFilenamePairs( (*this->Implementation->SeriesUIDToInstanceUIDMap.begin()).first,
                                         v,
                                         ascending );
    }
  else
    {
    v.clear();
    }
}

void DICOMAppHelper::GetImagePositionPatientFilenamePairs(const dicom_stl::string &seriesUID, dicom_stl::vector<dicom_stl::pair<float, dicom_stl::string> >& v,
                                                          bool ascending)
{
  v.clear();

  DICOMAppHelperImplementation::SeriesUIDToInstanceUIDMapType::iterator miter  = this->Implementation->SeriesUIDToInstanceUIDMap.find(seriesUID);

  if (miter == this->Implementation->SeriesUIDToInstanceUIDMap.end() )
    {
    return;
    }

  // grab the instance UIDs for the specified series
  dicom_stl::vector<dicom_stl::string> instanceUIDs = (*miter).second;

  for (dicom_stl::vector<dicom_stl::string>::iterator instanceIter = instanceUIDs.begin();
       instanceIter != instanceUIDs.end();
       instanceIter++)
       {
       dicom_stl::pair<float, dicom_stl::string> p;
       p.second = dicom_stl::string(this->Implementation->InstanceUIDToFileNameMap[*instanceIter]);

       float image_position;
       float normal[3];
       
       DICOMAppHelperImplementation::InstanceUIDToSliceOrderingMapType::iterator sn_iter = Implementation->InstanceUIDToSliceOrderingMap.find(*instanceIter);

       if (sn_iter != Implementation->InstanceUIDToSliceOrderingMap.end())
        {
        // compute the image patient position wrt to the slice image
        // plane normal

        normal[0] = ((*sn_iter).second.ImageOrientationPatient[1]
                     * (*sn_iter).second.ImageOrientationPatient[5])
          - ((*sn_iter).second.ImageOrientationPatient[2]
             * (*sn_iter).second.ImageOrientationPatient[4]);
        normal[1] = ((*sn_iter).second.ImageOrientationPatient[2]
                     * (*sn_iter).second.ImageOrientationPatient[3])
          - ((*sn_iter).second.ImageOrientationPatient[0]
             *(*sn_iter).second.ImageOrientationPatient[5]);
        normal[2] = ((*sn_iter).second.ImageOrientationPatient[0]
                     * (*sn_iter).second.ImageOrientationPatient[4])
          - ((*sn_iter).second.ImageOrientationPatient[1]
             * (*sn_iter).second.ImageOrientationPatient[3]);
        
        image_position = (normal[0]*(*sn_iter).second.ImagePositionPatient[0])
          + (normal[1]*(*sn_iter).second.ImagePositionPatient[1])
          + (normal[2]*(*sn_iter).second.ImagePositionPatient[2]);
        p.first = image_position;
        v.push_back(p);
        }
       }
  if (ascending)
    {
    dicom_stl::sort(v.begin(), v.end(), lt_pair_float_string());
    }
  else
    {
    dicom_stl::sort(v.begin(), v.end(), gt_pair_float_string());
    }
}

void DICOMAppHelper::GetImagePositionPatientFilenamePairs(dicom_stl::vector<dicom_stl::pair<float, dicom_stl::string> >& v,
                                                          bool ascending)
{
  // Default to using the first series
  if (this->Implementation->SeriesUIDToInstanceUIDMap.size() > 0)
    {
    this->GetImagePositionPatientFilenamePairs( (*this->Implementation->SeriesUIDToInstanceUIDMap.begin()).first, v, ascending );
    }
  else
    {
    v.clear();
    }
}


void DICOMAppHelper::GetContours(const dicom_stl::string &seriesUID,
                                 dicom_stl::vector<dicom_stl::vector<float> >& v)
{
  v.clear();

  DICOMAppHelperImplementation::SeriesUIDToContoursMapType::iterator iter = this->Implementation->SeriesUIDToContoursMap.find(seriesUID);
  
  if (iter == this->Implementation->SeriesUIDToContoursMap.end() )
    {
    return;
    }

  // All we need to do is copy the contours to the output
  v = (*iter).second;
}

void DICOMAppHelper::GetContours(dicom_stl::vector<dicom_stl::vector<float> >& v)
{
  // Default to using the first series
  if (this->Implementation->SeriesUIDToInstanceUIDMap.size() > 0)
    {
    this->GetContours( (*this->Implementation->SeriesUIDToInstanceUIDMap.begin()).first, v );
    }
  else
    {
    v.clear();
    }
}


void DICOMAppHelper::GetReferencedInstanceUIDs(const dicom_stl::string &seriesUID,
                                 dicom_stl::vector<dicom_stl::string>& v)
{
  v.clear();

  DICOMAppHelperImplementation::SeriesUIDToReferencedInstanceUIDMapType::iterator iter = this->Implementation->SeriesUIDToReferencedInstanceUIDMap.find(seriesUID);
  
  if (iter == this->Implementation->SeriesUIDToReferencedInstanceUIDMap.end() )
    {
    return;
    }

  // All we need to do is copy the referenced instanced to the output
  v = (*iter).second;
}

void DICOMAppHelper::GetReferencedInstanceUIDs(dicom_stl::vector<dicom_stl::string>& v)
{
  // Default to using the first series
  if (this->Implementation->SeriesUIDToInstanceUIDMap.size() > 0)
    {
    this->GetReferencedInstanceUIDs( (*this->Implementation->SeriesUIDToReferencedInstanceUIDMap.begin()).first, v );
    }
  else
    {
    v.clear();
    }
}


void DICOMAppHelper::GetSeriesUIDs(dicom_stl::vector<dicom_stl::string> &v)
{
  v.clear();

  DICOMAppHelperImplementation::SeriesUIDToInstanceUIDMapType::iterator miter;

  for (miter = this->Implementation->SeriesUIDToInstanceUIDMap.begin(); miter != this->Implementation->SeriesUIDToInstanceUIDMap.end();
       ++miter)
    {
    v.push_back( (*miter).first );
    }
}

void DICOMAppHelper::GetSeriesDescriptions(dicom_stl::vector<dicom_stl::string> &v)
{
  v.clear();

  DICOMAppHelperImplementation::SeriesUIDToInstanceUIDMapType::iterator miter;

  for (miter = this->Implementation->SeriesUIDToInstanceUIDMap.begin(); miter != this->Implementation->SeriesUIDToInstanceUIDMap.end();
       ++miter)
    {
    DICOMAppHelperImplementation::SeriesUIDToSeriesDescriptionMapType::iterator iiter = this->Implementation->SeriesUIDToSeriesDescriptionMap.find((*miter).first);
    if ( iiter != this->Implementation->SeriesUIDToSeriesDescriptionMap.end()) // if found we insert
      {
      v.push_back( (*iiter).second );
      }
    else
      {
      dicom_stl::string value = "";
      v.push_back( value );
      }
   }
}


void DICOMAppHelper::GetBodyParts(dicom_stl::vector<dicom_stl::string> &v)
{
  v.clear();

  DICOMAppHelperImplementation::SeriesUIDToInstanceUIDMapType::iterator miter;

  for (miter = this->Implementation->SeriesUIDToInstanceUIDMap.begin(); miter != this->Implementation->SeriesUIDToInstanceUIDMap.end();
       ++miter)
    {
    DICOMAppHelperImplementation::SeriesUIDToBodyPartMapType::iterator iiter = this->Implementation->SeriesUIDToBodyPartMap.find((*miter).first);
    if ( iiter != this->Implementation->SeriesUIDToBodyPartMap.end()) // if found we insert
      {
      v.push_back( (*iiter).second );
      }
    else
      {
      dicom_stl::string value = "";
      v.push_back( value );
      }
   }
}


void DICOMAppHelper::GetScanOptions(dicom_stl::vector<dicom_stl::string> &v)
{
  v.clear();

  DICOMAppHelperImplementation::SeriesUIDToInstanceUIDMapType::iterator miter;

  for (miter = this->Implementation->SeriesUIDToInstanceUIDMap.begin(); miter != this->Implementation->SeriesUIDToInstanceUIDMap.end();
       ++miter)
    {
    DICOMAppHelperImplementation::SeriesUIDToScanOptionsMapType::iterator iiter = this->Implementation->SeriesUIDToScanOptionsMap.find((*miter).first);
    if ( iiter != this->Implementation->SeriesUIDToScanOptionsMap.end()) // if found we insert
      {
      v.push_back( (*iiter).second );
      }
    else
      {
      dicom_stl::string value = "";
      v.push_back( value );
      }
   }
}


void DICOMAppHelper::Clear()
{ 
  this->Implementation->InstanceUIDToSeriesUIDMap.clear();
  this->Implementation->InstanceUIDToSliceOrderingMap.clear();
  this->Implementation->SeriesUIDToInstanceUIDMap.clear();
  this->Implementation->SeriesUIDToSeriesDescriptionMap.clear();
  this->Implementation->SeriesUIDToContoursMap.clear();
  this->Implementation->SeriesUIDToBodyPartMap.clear();
  this->Implementation->SeriesUIDToScanOptionsMap.clear();

  this->CurrentSeriesUID = "";
  this->CurrentSeriesDescription = "";
  this->InstanceUID = "";
}
}
#ifdef _MSC_VER
#pragma warning ( pop )
#endif
