
#ifdef WIN32
#pragma warning(disable:4786)
#endif


#include "DICOMAppHelper.h"
#include "DICOMCallback.h"

#include <iomanip>
#include <iostream>
#include <stdlib.h>
#include <string>


DICOMAppHelper::DICOMAppHelper()
{
  this->FileName = NULL;
  this->DICOMDataFile = NULL;
  this->BitsAllocated = 16;
  this->ByteSwapData = false;
}

DICOMAppHelper::~DICOMAppHelper()
{
  this->HeaderFile.close();
}

void DICOMAppHelper::RegisterCallbacks(DICOMParser& parser)
{
  DICOMMemberCallback<DICOMAppHelper>* cb = new DICOMMemberCallback<DICOMAppHelper>;
  cb->SetCallbackFunction(this, &DICOMAppHelper::WriteImageData);
  parser.AddDICOMTagCallback(0x7FE0, 0x0010, DICOMParser::VR_OW, cb);
        
  DICOMMemberCallback<DICOMAppHelper>* cb2 = new DICOMMemberCallback<DICOMAppHelper>;
  cb2->SetCallbackFunction(this, &DICOMAppHelper::SeriesUIDCallback);
  parser.AddDICOMTagCallback(0x0020, 0x000e, DICOMParser::VR_UI, cb2);

  DICOMMemberCallback<DICOMAppHelper>* cb3 = new DICOMMemberCallback<DICOMAppHelper>;
  cb3->SetCallbackFunction(this, &DICOMAppHelper::SliceNumberCallback);
  parser.AddDICOMTagCallback(0x0020, 0x0013, DICOMParser::VR_IS, cb3);

  DICOMMemberCallback<DICOMAppHelper>* cb4 = new DICOMMemberCallback<DICOMAppHelper>;
  cb4->SetCallbackFunction(this, &DICOMAppHelper::TransferSyntaxCallback);
  parser.AddDICOMTagCallback(0x0002, 0x0010, DICOMParser::VR_UI, cb4);

  DICOMMemberCallback<DICOMAppHelper>* cb5 = new DICOMMemberCallback<DICOMAppHelper>;
  cb5->SetCallbackFunction(this, &DICOMAppHelper::BitsAllocatedCallback);
  parser.AddDICOMTagCallback(0x0028, 0x0100, DICOMParser::VR_US, cb5);


  DICOMTagInfo dicom_tags[] = {
    // {0x0002, 0x0002, DICOMParser::VR_UI, "Media storage SOP class uid"},
    // {0x0002, 0x0003, DICOMParser::VR_UI, "Media storage SOP inst uid"},
    {0x0002, 0x0010, DICOMParser::VR_UI, "Transfer syntax uid"},
    // {0x0002, 0x0012, DICOMParser::VR_UI, "Implementation class uid"},
    // {0x0008, 0x0018, DICOMParser::VR_UI, "Image UID"},
    // {0x0008, 0x0020, DICOMParser::VR_DA, "Series date"},
    // {0x0008, 0x0030, DICOMParser::VR_TM, "Series time"},
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
    {0x0028, 0x0010, DICOMParser::VR_US, "Num rows"},
    {0x0028, 0x0011, DICOMParser::VR_US, "Num cols"},
    {0x0028, 0x0030, DICOMParser::VR_FL, "pixel spacing"},
    {0x0028, 0x0100, DICOMParser::VR_US, "Bits allocated"},
    {0x0028, 0x0120, DICOMParser::VR_UL, "pixel padding"},
    {0x0028, 0x1052, DICOMParser::VR_FL, "pixel offset"}
    //{0x7FE0, 0x0010, DICOMParser::VR_OW, "pixel data"}
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
    parser.AddDICOMTagCallback(group, element,datatype, callbackArray[j]);

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
    
void DICOMAppHelper::WriteImageData(doublebyte group,
                                    doublebyte element,
                                    DICOMParser::VRTypes type,
                                    unsigned char* val,
                                    quadbyte len)
{
#if 0
  //FILE* fptr = fopen(this->GetOutputFilename(), "wb");

  /*  
      if (this->ByteSwapData)
      {
      if (this->BitsAllocated >= 8 && this->BitsAllocated <=16)
      {
      this->DICOMDataFile->swapShorts((ushort*) val,(ushort*) val, len/2);
      }
      else if (this->BitsAllocated > 16)
      {
      this->DICOMDataFile->swapLongs((ulong*) val,(ulong*) val, len/4);
      }
      }
  */

  // fwrite(val,len, 1,fptr);
  // fclose(fptr);
#endif
}

void DICOMAppHelper::SetFileName(const char* filename)
{
  if (this->HeaderFile.is_open())
    {
    this->HeaderFile.close();
    }
  this->FileName = (char*) filename;
  /*
    #ifdef WIN32
    char myfilename[_MAX_PATH];
    #else
    char myfilename[PATH_MAX];
    #endif
  */

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
        fval = atof((char*) val);
        HeaderFile << fval;
        break;
      case DICOMParser::VR_FD: // float double
        fval = atof((char*) val);
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
                                         doublebyte element,
                                         DICOMParser::VRTypes type,
                                         unsigned char* val,
                                         quadbyte len) 
{
  char* newString = (char*) val;
  int sliceNumber = atoi(newString);
  std::cout << "Slice number: " << sliceNumber << std::endl;

  SliceNumberMap.insert(std::pair<char*, int> (this->FileName, sliceNumber));
}

void DICOMAppHelper::TransferSyntaxCallback(doublebyte,
                                            doublebyte,
                                            DICOMParser::VRTypes,
                                            unsigned char* val,
                                            quadbyte) 
{
  static char* TRANSFER_UID_LITTLE_ENDIAN = "1.2.840.10008.1.2";
  if (!strcmp(TRANSFER_UID_LITTLE_ENDIAN, (char*) val))
    {
#ifdef WIN32
    this->ByteSwapData = false;   
#else
    this->ByteSwapData = true;
#endif
    }
  else
    {
#ifdef WIN32
    this->ByteSwapData = true;    
#else
    this->ByteSwapData = false;
#endif
    }
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
