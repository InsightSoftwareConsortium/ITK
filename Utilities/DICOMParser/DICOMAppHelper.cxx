
#ifdef WIN32
#pragma warning(disable:4786)
#endif


#include "DICOMAppHelper.h"
#include "DICOMCallback.h"

#include <iomanip>
#include <iostream>
#include <stdlib.h>
#include <string>
#include <math.h>

DICOMAppHelper::DICOMAppHelper()
{
  this->FileName = NULL;
  this->DICOMDataFile = NULL;
  this->BitsAllocated = 8;
  this->ByteSwapData = false;
  this->PixelSpacing[0] = this->PixelSpacing[1] = 1.0;
  this->Dimensions[0] = this->Dimensions[1] = 0;
  this->PhotometricInterpretation = NULL;
  this->TransferSyntaxUID = NULL;
  this->RescaleOffset = 0.0;
  this->RescaleSlope = 1.0;
  this->ImageData = NULL;
  this->ImageDataLengthInBytes = 0;
}

DICOMAppHelper::~DICOMAppHelper()
{
  this->HeaderFile.close();
}

void DICOMAppHelper::RegisterCallbacks(DICOMParser* parser)
{
  if (!parser)
    {
    std::cerr << "Null parser!" << std::endl;
    }

  this->Parser = parser;
  
  DICOMMemberCallback<DICOMAppHelper>* cb2 = new DICOMMemberCallback<DICOMAppHelper>;
  cb2->SetCallbackFunction(this, &DICOMAppHelper::SeriesUIDCallback);
  parser->AddDICOMTagCallback(0x0020, 0x000e, DICOMParser::VR_UI, cb2);

  DICOMMemberCallback<DICOMAppHelper>* cb3 = new DICOMMemberCallback<DICOMAppHelper>;
  cb3->SetCallbackFunction(this, &DICOMAppHelper::SliceNumberCallback);
  parser->AddDICOMTagCallback(0x0020, 0x0013, DICOMParser::VR_IS, cb3);

  DICOMMemberCallback<DICOMAppHelper>* cb4 = new DICOMMemberCallback<DICOMAppHelper>;
  cb4->SetCallbackFunction(this, &DICOMAppHelper::TransferSyntaxCallback);
  parser->AddDICOMTagCallback(0x0002, 0x0010, DICOMParser::VR_UI, cb4);

  DICOMMemberCallback<DICOMAppHelper>* cb5 = new DICOMMemberCallback<DICOMAppHelper>;
  cb5->SetCallbackFunction(this, &DICOMAppHelper::BitsAllocatedCallback);
  parser->AddDICOMTagCallback(0x0028, 0x0100, DICOMParser::VR_US, cb5);

  DICOMMemberCallback<DICOMAppHelper>* cb6 = new DICOMMemberCallback<DICOMAppHelper>;
  cb6->SetCallbackFunction(this, &DICOMAppHelper::PixelSpacingCallback);
  parser->AddDICOMTagCallback(0x0028, 0x0030, DICOMParser::VR_FL, cb6);
  parser->AddDICOMTagCallback(0x0018, 0x0050, DICOMParser::VR_FL, cb6);

  DICOMMemberCallback<DICOMAppHelper>* cb7 = new DICOMMemberCallback<DICOMAppHelper>;
  cb7->SetCallbackFunction(this, &DICOMAppHelper::WidthCallback);
  parser->AddDICOMTagCallback(0x0028, 0x0011, DICOMParser::VR_US, cb7);

  DICOMMemberCallback<DICOMAppHelper>* cb8 = new DICOMMemberCallback<DICOMAppHelper>;
  cb8->SetCallbackFunction(this, &DICOMAppHelper::HeightCallback);
  parser->AddDICOMTagCallback(0x0028, 0x0010, DICOMParser::VR_US, cb8);

  DICOMMemberCallback<DICOMAppHelper>* cb9 = new DICOMMemberCallback<DICOMAppHelper>;
  cb9->SetCallbackFunction(this, &DICOMAppHelper::PixelRepresentationCallback);
  parser->AddDICOMTagCallback(0x0028, 0x0103, DICOMParser::VR_US, cb9);

  DICOMMemberCallback<DICOMAppHelper>* cb10 = new DICOMMemberCallback<DICOMAppHelper>;
  cb10->SetCallbackFunction(this, &DICOMAppHelper::PhotometricInterpretationCallback);
  parser->AddDICOMTagCallback(0x0028, 0x0004, DICOMParser::VR_CS, cb10);

  DICOMMemberCallback<DICOMAppHelper>* cb11 = new DICOMMemberCallback<DICOMAppHelper>;
  cb11->SetCallbackFunction(this, &DICOMAppHelper::RescaleOffsetCallback);
  parser->AddDICOMTagCallback(0x0028, 0x1052, DICOMParser::VR_CS, cb11);

  DICOMMemberCallback<DICOMAppHelper>* cb12 = new DICOMMemberCallback<DICOMAppHelper>;
  cb12->SetCallbackFunction(this, &DICOMAppHelper::RescaleSlopeCallback);
  parser->AddDICOMTagCallback(0x0028, 0x1053, DICOMParser::VR_FL, cb12);

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
    {0x0008, 0x1060, DICOMParser::VR_SH, "Physician"},
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
    {0x0028, 0x0010, DICOMParser::VR_FL, "Num rows"},
    {0x0028, 0x0011, DICOMParser::VR_FL, "Num cols"},
    {0x0028, 0x0030, DICOMParser::VR_FL, "pixel spacing"},
    {0x0028, 0x0100, DICOMParser::VR_US, "Bits allocated"},
    {0x0028, 0x0120, DICOMParser::VR_UL, "pixel padding"},
    {0x0028, 0x1052, DICOMParser::VR_FL, "pixel offset"}
  };

  int num_tags = sizeof(dicom_tags)/sizeof(DICOMTagInfo);
  DICOMMemberCallback<DICOMAppHelper>** callbackArray = new DICOMMemberCallback<DICOMAppHelper>*[num_tags];

  for (int j = 0; j < num_tags; j++)
    {
    //
    // Make callback
    //
    callbackArray[j] = new DICOMMemberCallback<DICOMAppHelper>;
    callbackArray[j]->SetCallbackFunction(this, &DICOMAppHelper::ArrayCallback);
    //
    // Setup internal map.
    //
    DICOMTagInfo tagStruct = dicom_tags[j];
    doublebyte group = tagStruct.group;
    doublebyte element = tagStruct.element;
    DICOMParser::VRTypes datatype = tagStruct.datatype;

    std::pair<doublebyte, doublebyte> gePair(group, element);
    std::pair<std::pair<doublebyte, doublebyte>, DICOMTagInfo> mapPair(gePair, tagStruct);
    this->TagMap.insert(mapPair);

    //
    // Set callback on parser.
    //
    parser->AddDICOMTagCallback(group, element,datatype, callbackArray[j]);

    }

}

void DICOMAppHelper::SeriesUIDCallback(doublebyte,
                                       doublebyte,
                                       DICOMParser::VRTypes,
                                       unsigned char* val,
                                       quadbyte) 
{
  char* newString = (char*) val;
  std::map<char*, std::vector<char*>, ltstr >::iterator iter = SeriesUIDMap.find(newString);
  std::vector<char*>* newVector = new std::vector<char*>;
  if ( iter == SeriesUIDMap.end())
    {
    newVector->push_back(this->FileName);
    SeriesUIDMap.insert(std::pair<char*, std::vector<char*> > (newString, *newVector));
    }
  else
    {
    (*iter).second.push_back(this->FileName);
    }
}

void DICOMAppHelper::OutputSeries()
{
  std::cout << std::endl << std::endl;

  for (std::map<char*, std::vector<char*>, ltstr >::iterator iter = SeriesUIDMap.begin();
       iter != SeriesUIDMap.end();
       iter++)
    {
    std::cout << "SERIES: " << (*iter).first << std::endl;
    std::vector<char*>& v_ref = (*iter).second;

    for (std::vector<char*>::iterator v_iter = v_ref.begin();
         v_iter != v_ref.end();
         v_iter++)
      {
      std::map<char*, int>::iterator sn_iter = SliceNumberMap.find(*v_iter);
      int slice = -1;
      if (sn_iter != SliceNumberMap.end())
        {
        slice = (*sn_iter).second;
        }
      std::cout << "\t" << *v_iter << " [" << slice << "]" <<  std::endl;
      }
    }
}

void DICOMAppHelper::SetFileName(const char* filename)
{
  if (this->HeaderFile.is_open())
    {
    this->HeaderFile.close();
    }
  //NOTE: const_cast is usually a bad idea, but
  //the number of changes necessary to make this correct are too
  //numerous to deal with.
  this->FileName = const_cast<char*>(filename);

  std::string myfilename(std::string((char*) this->FileName) + ".header.txt");

  this->HeaderFile.open(myfilename.c_str());
}

void DICOMAppHelper::ArrayCallback(doublebyte group,
                                   doublebyte element,
                                   DICOMParser::VRTypes datatype,
                                   unsigned char* val,
                                   quadbyte len) 
{
  char* desc = "No description";
  
  TagMapType::iterator iter = this->TagMap.find(std::pair<doublebyte, doublebyte> (group, element));
  if (iter != this->TagMap.end())
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

  HeaderFile << std::hex << group;
  HeaderFile << ",0x";

  HeaderFile.width(4);
  HeaderFile.fill('0');
    
  HeaderFile << std::hex << element;
  HeaderFile << ") ";

  HeaderFile.fill(prev);
  HeaderFile << std::dec;
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
      case DICOMParser::VR_IS:
        ival = DICOMFile::ReturnAsSignedLong(val, this->DICOMDataFile->GetByteSwap()); 
        HeaderFile << ival;
        break;
      case DICOMParser::VR_SS:
        ival = DICOMFile::ReturnAsSignedShort(val, this->DICOMDataFile->GetByteSwap()); 
        HeaderFile << ival;
        break;
      case DICOMParser::VR_US: // unsigned short
        uival = DICOMFile::ReturnAsUnsignedShort(val, this->DICOMDataFile->GetByteSwap()); 
        HeaderFile << uival;
        break;
      default:
        HeaderFile << val << std::endl;
        break;
      }
    }
  else
    {
    HeaderFile << "NULL";
    }
  
  HeaderFile << std::dec << std::endl;
  HeaderFile.fill(prev);
}
    
void DICOMAppHelper::SliceNumberCallback(doublebyte,
                                         doublebyte,
                                         DICOMParser::VRTypes,
                                         unsigned char* val,
                                         quadbyte) 
{
  char* newString = (char*) val;
  int sliceNumber = atoi(newString);
  std::cout << "Slice number: " << sliceNumber << std::endl;

  // SliceNumberMap.insert(std::pair<char*, int> (this->FileName, sliceNumber));
}

void DICOMAppHelper::TransferSyntaxCallback(doublebyte,
                                            doublebyte,
                                            DICOMParser::VRTypes,
                                            unsigned char* val,
                                            quadbyte) 
{
#ifdef WIN32
  char platformByteOrder = 'L';
#else
  char platformByteOrder = 'B';
#endif
  std::cout << "Platform byte order: " << platformByteOrder << std::endl;

  static char* TRANSFER_UID_EXPLICIT_BIG_ENDIAN = "1.2.840.10008.1.2.2";


  DICOMMemberCallback<DICOMAppHelper>* cb = new DICOMMemberCallback<DICOMAppHelper>;
  cb->SetCallbackFunction(this, &DICOMAppHelper::ToggleSwapBytesCallback);

  if (strcmp(TRANSFER_UID_EXPLICIT_BIG_ENDIAN, (char*) val) == 0)
    {
    this->ByteSwapData = true;
    this->Parser->AddDICOMTagCallback(0x0800, 0x0000, DICOMParser::VR_UNKNOWN, cb);
    std::cerr <<"Registering callback for swapping bytes." << std::endl;
    }
  
  this->TransferSyntaxUID = new std::string((char*) val);
  std::cout << "Transfer Syntax UID: " << *this->TransferSyntaxUID;
  std::cout << " " << this->TransferSyntaxUIDDescription(this->TransferSyntaxUID->c_str()) << std::endl;
}

void DICOMAppHelper::BitsAllocatedCallback(doublebyte,
                                           doublebyte,
                                           DICOMParser::VRTypes,
                                           unsigned char* val,
                                           quadbyte) 
{
  this->BitsAllocated = this->DICOMDataFile->ReturnAsUnsignedShort(val, this->DICOMDataFile->GetByteSwap());
  std::cout << "Bits allocated: " << this->BitsAllocated << std::endl;
}


void DICOMAppHelper::ToggleSwapBytesCallback(doublebyte,
                                             doublebyte,
                                             DICOMParser::VRTypes,
                                             unsigned char*,
                                             quadbyte len) 
{
  std::cout << "ToggleSwapBytesCallback" << std::endl;
  bool bs = this->DICOMDataFile->GetByteSwap();
  this->DICOMDataFile->SetByteSwap(!bs);

  std::cout << "Set byte swap to: " << this->DICOMDataFile->GetByteSwap() << std::endl;

  long pos = this->DICOMDataFile->Tell();

  //
  // The +4 is probably a hack, but it's a guess at the length of the previous field.
  //
  this->DICOMDataFile->SkipToPos(pos - len + 4);
}


void DICOMAppHelper::PixelSpacingCallback(doublebyte group,
                                          doublebyte element,
                                          DICOMParser::VRTypes,
                                          unsigned char* val,
                                          quadbyte) 
{
  float fval = DICOMFile::ReturnAsFloat(val, this->DICOMDataFile->GetByteSwap());

  if (group == 0x0028 && element == 0x0030)
    {
    this->PixelSpacing[0] = this->PixelSpacing[1] = fval;
    }
  else if (group == 0x0018 && element == 0x0050)
    {
    this->PixelSpacing[2] = fval;
    }
}

void DICOMAppHelper::WidthCallback(doublebyte,
                                   doublebyte,
                                   DICOMParser::VRTypes,
                                   unsigned char* val,
                                   quadbyte)
{
  unsigned short uival = DICOMFile::ReturnAsUnsignedShort(val, this->DICOMDataFile->GetByteSwap()); 
  std::cout << "Width: " << uival << std::endl;

  this->Width = uival;
  this->Dimensions[0] = this->Width;
}

void DICOMAppHelper::HeightCallback(doublebyte,
                                    doublebyte,
                                    DICOMParser::VRTypes,
                                    unsigned char* val,
                                    quadbyte) 
{
  unsigned short uival = DICOMFile::ReturnAsUnsignedShort(val, this->DICOMDataFile->GetByteSwap()); 

  std::cout << "Height: " << uival << std::endl;

  this->Height = uival;
  this->Dimensions[1] = this->Height;
}


void DICOMAppHelper::PixelRepresentationCallback( doublebyte,
                                                  doublebyte,
                                                  DICOMParser::VRTypes,
                                                  unsigned char* val,
                                                  quadbyte)
{
  unsigned short uival = DICOMFile::ReturnAsUnsignedShort(val, this->DICOMDataFile->GetByteSwap());
  std::cout << "Pixel Representation: " << (uival ? "Signed" : "Unsigned") << std::endl;
  this->PixelRepresentation = uival;
}

void DICOMAppHelper::PhotometricInterpretationCallback( doublebyte,
                                                        doublebyte,
                                                        DICOMParser::VRTypes,
                                                        unsigned char* val,
                                                        quadbyte)
{
  std::cout << "Photometric Interpretation: " << (char*) val << std::endl;
  this->PhotometricInterpretation = new std::string((char*) val);
}

void DICOMAppHelper::PixelDataCallback( doublebyte,
                                        doublebyte,
                                        DICOMParser::VRTypes,
                                        unsigned char* data,
                                        quadbyte len)
{
  int numPixels = this->Dimensions[0] * this->Dimensions[1];
  if (len < numPixels)
    {
    numPixels = len;
    }
  if (numPixels < 0)
    {
    numPixels = 0;
    }

  std::cout << "numPixels : " << numPixels << std::endl;

  int ptrIncr = int(this->BitsAllocated/8.0);

  unsigned short* ushortInputData = reinterpret_cast<unsigned short*>(data);
  unsigned char* ucharInputData = data;
  short* shortInputData = reinterpret_cast<short*> (data);

  float* floatOutputData = NULL;
  
  bool isFloat = this->RescaledImageDataIsFloat();

  //float* tempData = new float[numPixels];

  if (isFloat)
    {
    std::cout << "Slope and offset are not integer valued : ";
    std::cout << this->RescaleSlope << ", " << this->RescaleOffset << std::endl;

    this->ImageData = new float[numPixels];
    floatOutputData = static_cast<float*> (this->ImageData);

    this->ImageDataType = DICOMParser::VR_FL;
    this->ImageDataLengthInBytes = numPixels * sizeof(float);
    float newFloatPixel = 0.0;

    if (ptrIncr == 1)
      {
      for (int i = 0; i < numPixels; i++)
        {
        newFloatPixel = float(this->RescaleSlope * ucharInputData[i] + this->RescaleOffset);
        floatOutputData[i] = newFloatPixel;
        }
      std::cout << "Did rescale, offset to float from char." << std::endl;
      std::cout << numPixels << " pixels." << std::endl;
      }
    else if (ptrIncr == 2)
      {
      for (int i = 0; i < numPixels; i++)
        {
        newFloatPixel = float(this->RescaleSlope * ushortInputData[i] + this->RescaleOffset);
        floatOutputData[i] = newFloatPixel;
        }
      std::cout << "Did rescale, offset to float from short." << std::endl;
      std::cout << numPixels << " pixels." << std::endl;
      }
    }
  else
    {
    std::cout << "Slope and offset are integer valued : ";
    std::cout << this->RescaleSlope << ", " << this->RescaleOffset << std::endl;

    if (ptrIncr == 1)
      {
      this->ImageData = new char[numPixels];
  
      char*  charOutputData =  static_cast<char*>  (this->ImageData);

      this->ImageDataType = DICOMParser::VR_OB;
      this->ImageDataLengthInBytes = numPixels * sizeof(char);
      char newCharPixel = 0;

      for (int i = 0; i < numPixels; i++)
        {
        newCharPixel = char(this->RescaleSlope * ucharInputData[i] + this->RescaleOffset);
        charOutputData[i] = newCharPixel;
        }
      std::cout << "Did rescale, offset to char from char." << std::endl;
      std::cout << numPixels << " pixels." << std::endl;
      }
    else if (ptrIncr == 2)
      {
      this->ImageData = new short[numPixels];

      short* shortOutputData = static_cast<short*> (this->ImageData);

      this->ImageDataType = DICOMParser::VR_OW;
      this->ImageDataLengthInBytes = numPixels * sizeof(short);
      short newShortPixel = 0;
      for (int i = 0; i < numPixels; i++)
        {
        newShortPixel = short(this->RescaleSlope * shortInputData[i] + this->RescaleOffset);
        shortOutputData[i] = newShortPixel;
        }
      std::cout << "Did rescale, offset to short from short." << std::endl;
      std::cout << numPixels << " pixels." << std::endl;
      }
    }
}

void DICOMAppHelper::RegisterPixelDataCallback()
{
  DICOMMemberCallback<DICOMAppHelper>* cb = new DICOMMemberCallback<DICOMAppHelper>;
  cb->SetCallbackFunction(this, &DICOMAppHelper::PixelDataCallback);
  this->Parser->AddDICOMTagCallback(0x7FE0, 0x0010, DICOMParser::VR_OW, cb);
}

void DICOMAppHelper::RescaleOffsetCallback( doublebyte,
                                          doublebyte,
                                          DICOMParser::VRTypes,
                                          unsigned char* val,
                                          quadbyte)
{
  float fval = DICOMFile::ReturnAsFloat(val, this->DICOMDataFile->GetByteSwap());
  this->RescaleOffset = fval;
  std::cout << "Pixel offset: " << this->RescaleOffset << std::endl;
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


void DICOMAppHelper::RescaleSlopeCallback(doublebyte,
                     doublebyte ,
                     DICOMParser::VRTypes ,
                     unsigned char* val,
                     quadbyte )
{
  float fval = DICOMFile::ReturnAsFloat(val, this->DICOMDataFile->GetByteSwap());
  std::cout << "Rescale slope: " << fval << std::endl;
  this->RescaleSlope = fval;
}

bool DICOMAppHelper::RescaledImageDataIsFloat()
{
  int s = int(this->RescaleSlope);
  int o = int(this->RescaleOffset);

  float sf = float(s);
  float of = float(o);

  float d1 = fabs(sf - this->RescaleSlope);
  float d2 = fabs(of - this->RescaleOffset);

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

