

#include "DICOMReader.h"
#include "DICOMCallback.h"
#include <iostream>

//
// Define this via the compiler for other little endian platforms
// e.g. Linux on Intel.
//
#ifdef _WIN32
#define WORDS_LITTLEENDIAN
#endif


static const char* TRANSFER_UID_BIG_ENDIAN = "1.2.840.113619.5.2";

//
// DICOM Implicit VR Little Endian
//
static const char* TRANSFER_UID_LITTLE_ENDIAN = "1.2.840.10008.1.2";

DICOMReader::DICOMReader() 
{
  Width = 0;
  Height = 0;
  FileName = NULL;
#ifdef WORDS_LITTLEENDIAN
  ByteSwapData = false;
#else
  ByteSwapData = true;
#endif  
}

DICOMReader::~DICOMReader()
{

}


void DICOMReader::TransferSyntaxUIDCallback(doublebyte group,
                                            doublebyte element,
                                            DICOMParser::VRTypes type,
                                            unsigned char* val,
                                            quadbyte len)
{
  // {0x0002, 0x0010, DICOMParser::VR_UI}
#ifdef WORDS_LITTLEENDIAN
  if (!strcmp((char*) val, TRANSFER_UID_LITTLE_ENDIAN))
    {
    ByteSwapData = false;
    }
  else if (!strcmp((char*) val, TRANSFER_UID_BIG_ENDIAN))
    {
    ByteSwapData = true;
    }
#else
  if (!strcmp((char*) val, TRANSFER_UID_LITTLE_ENDIAN))
    {
    ByteSwapData = true;
    }
  else if (!strcmp((char*) val, TRANSFER_UID_BIG_ENDIAN))
    {
    ByteSwapData = false;
    }
#endif  
}



    
void DICOMReader::ImageDataCallback(doublebyte group,
                                    doublebyte element,
                                    DICOMParser::VRTypes type,
                                    unsigned char* val,
                                    quadbyte len)
{

}

void DICOMReader::ImageWidthCallback(doublebyte group,
                                     doublebyte element,
                                     DICOMParser::VRTypes type,
                                     unsigned char* val,
                                     quadbyte len)
{
  this->Width = this->DataFile->ReturnAsUnsignedShort(val, this->DataFile->GetByteSwap());
}

void DICOMReader::ImageHeightCallback(doublebyte group,
                                      doublebyte element,
                                      DICOMParser::VRTypes type,
                                      unsigned char* val,
                                      quadbyte len)
{
  this->Height = this->DataFile->ReturnAsUnsignedShort(val, this->DataFile->GetByteSwap());
}

void DICOMReader::SetupCallbacks()
{

  DICOMMemberCallback<DICOMReader>* widthCB = new DICOMMemberCallback<DICOMReader>;
  widthCB->SetCallbackFunction(this, &DICOMReader::ImageWidthCallback);
  this->AddDICOMTagCallback(0x7FE0, 0x0010, DICOMParser::VR_OW, widthCB);


  DICOMMemberCallback<DICOMReader>* heightCB = new DICOMMemberCallback<DICOMReader>;
  heightCB->SetCallbackFunction(this, &DICOMReader::ImageHeightCallback);


  DICOMMemberCallback<DICOMReader>* transferSyntaxCB = new DICOMMemberCallback<DICOMReader>;
  transferSyntaxCB->SetCallbackFunction(this, &DICOMReader::TransferSyntaxUIDCallback);

}



void DICOMReader::FormattedPrint(doublebyte group, doublebyte element, VRTypes datatype, unsigned char* data, quadbyte length)
{
  if (!data)
    {
    return;
    }

  unsigned int uival = 0;
  float fval = 0;
  double dval = 0;
  int ival = 0;

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
      std::cout << "String: " << data << std::endl;
      break;
    case DICOMParser::VR_FL: // float
      fval = atof((char*) data);
      std::cout << "Float: " << fval << std::endl;
      break;
    case DICOMParser::VR_FD: // float double
      fval = atof((char*) data);
      std::cout << "Double: " << dval << std::endl;
      break;
    case DICOMParser::VR_UL: // unsigned long
    case DICOMParser::VR_SL: // signed long
    case DICOMParser::VR_AT:
    case DICOMParser::VR_US: // unsigned short
      uival = atol((char*) data);
      std::cout << "Unsigned int: " << uival << std::endl;
      break;
    case DICOMParser::VR_IS:
      // sscanf((char*) data, "%d", &ival);
      ival = DataFile->ReturnAsInteger(data, this->DataFile->GetByteSwap());

      std::cout << "Integer: " << ival << std::endl;
      break;
    case DICOMParser::VR_SS:
      ival = atoi((char*) data);
      std::cout << "Signed short: " << ival << std::endl;
      break;
    default:
      std::cout << data << std::endl;
      break;
    }
}

void DICOMReader::FormattedStore(doublebyte group, doublebyte element, VRTypes datatype, unsigned char* data_arr, quadbyte length) 
{
  if (!data_arr)
    {
    return;
    }

  quadbyte ulval = 0;
  doublebyte usval = 0;
  float fval = 0;
  char* sval = NULL;
  short int ssval = 0;
  //
  // Not used right now, get rid of compiler warning.
  //
  ssval = ssval;
  int ival = 0;

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
      sval = (char*) data_arr;
      break;
    case DICOMParser::VR_FL: // float
      fval = atof((char*) data_arr);
      break;
    case DICOMParser::VR_SL: // signed long
    case DICOMParser::VR_AT:
    case DICOMParser::VR_UL: // unsigned long
      ulval = DataFile->ReturnAsUnsignedLong(data_arr, this->DataFile->GetByteSwap());
      break;
    case DICOMParser::VR_US: // unsigned short
      usval = DataFile->ReturnAsUnsignedShort(data_arr, this->DataFile->GetByteSwap());
      break;
    case DICOMParser::VR_SS: // signed short
      ssval = DataFile->ReturnAsSignedShort(data_arr, this->DataFile->GetByteSwap());
      break;
    case DICOMParser::VR_IS:
      // sscanf((char*) data_arr, "%d", &ival);
      ival = DataFile->ReturnAsInteger(data_arr, this->DataFile->GetByteSwap());
      break;
    default:
      break;
    }

  switch(group)
    {
    case 0x0002:
      switch (element)
        {
        case 0x0010:
          Header.transferSyntaxUID = sval;
#ifdef WORDS_LITTLEENDIAN
          if (!strcmp(Header.transferSyntaxUID, TRANSFER_UID_LITTLE_ENDIAN))
            {
            ByteSwapData = false;
            }
          else if (!strcmp(Header.transferSyntaxUID, TRANSFER_UID_BIG_ENDIAN))
            {
            ByteSwapData = true;
            }
#else
          if (!strcmp(Header.transferSyntaxUID, TRANSFER_UID_LITTLE_ENDIAN))
            {
            ByteSwapData = true;
            }
          else if (!strcmp(Header.transferSyntaxUID, TRANSFER_UID_BIG_ENDIAN))
            {
            ByteSwapData = false;
            }
#endif
          break;
        default:
          break;
        }
      break;
    case 0x0008:
      switch (element)
        {
        case 0x0018:
          Header.imageUID = sval;
          break;
        case 0x0070:
          Header.manufacturer = sval;
          break;
        default:
          break;
        }
      break;
    case 0x0018:
      switch (element)
        {
        case 0x0050:
          Header.thickness = fval;
          break;
        case 0x0060:
          //Header.kV = fval;
          break;
        case 0x0088:
          //Header.slice_spacing = fval;
          break;
        case 0x1100:
          break;
        case 0x1151:
          //Header.mA = fval;
          break;
        case 0x1210:
          //Header.reconType = sval;
        default:
          break;
        }
      break;
    case 0x0020:
      switch (element)
        {
        case 0x000d:
          Header.studyUID = sval;
          break;
        case 0x000e:
          Header.seriesUID = sval;
          break;
        case 0x0013:
          Header.image_num = ival;
          break;
        case 0x0032:
          Header.patient_position_ul = sval;
          this->ParsePatientPosition(Header.patient_position_ul);
          break;
        case 0x0037:
          Header.patient_position_cosines = sval;
          this->ParsePatientCosines(Header.patient_position_cosines);
          break;
        default:
          break;
        }
      break;
    case 0x0028:
      switch (element)
        {
        case 0x0010:
          Header.height = usval;
          break;
        case 0x0011:
          Header.width = usval;
          break;
        case 0x0030:
          Header.pix_spacing = fval;
          break;
        case 0x0100:
          Header.depth = usval;
          break;
        case 0x0120:
          //Header.bgshade = ulval;
          ulval = ulval;
        case 0x1052:
          //Header.pixel_offset = fval;
        default:
          break;
        }
      break;
    case 0x7FE0:
      switch (element)
        {
        case 0x0010: // image data
          Header.header_size = this->DataFile->Tell() - length;
          break;
        default:
          break;
        }
      break;
    default:
      break;
    }
}



void DICOMReader::ParsePatientPosition(char* parse_string)
{
  char* tempstr = NULL;
  char* ul[3] = {0, 0, 0};
  int count = 0;

  while((tempstr = strtok(parse_string, "\\"))) {
  parse_string = NULL;
  ul[count] = tempstr;
  count++;
  if (count == 3) {
  break;
  }
  }
  Header.upper_left_R = -1.0 * atof(ul[0]);
  Header.upper_left_A = -1.0 * atof(ul[1]);
  Header.upper_left_S = atof(ul[2]);
}

void DICOMReader::ParsePatientCosines(char* parse_string)
{
  char* tempstr = NULL;
  char* dircos[6] = {0, 0, 0, 0, 0, 0};  // row L,P,S; col L,P,S
  int count = 0;

  while((tempstr = strtok(parse_string, "\\"))) {
  parse_string = NULL;
  dircos[count] = tempstr;
  count++;
  if (count == 6)
    {
    break;
    }
  }
  Header.dir_cos_row_L = atof(dircos[0]);
  Header.dir_cos_row_P = atof(dircos[1]);
  Header.dir_cos_row_S = atof(dircos[2]);
  Header.dir_cos_col_L = atof(dircos[3]);
  Header.dir_cos_col_P = atof(dircos[4]);
  Header.dir_cos_col_S = atof(dircos[5]); 
}

float DICOMReader::ComputeLambda(float q1[3], float q3[3])
{
  //
  // Lambda is the position in a line segment parameterized as:
  // x = (1-lambda) * q1 + lambda * q3
  //
  // Lambda should be between 0 and 1 if the origin is on the line segment
  //
  float lambda = (q1[0]*q1[0] + q1[1]*q1[1] + q1[2]*q1[2] - q1[0]*q3[0] - q1[1]*q3[1] - q1[2]*q3[2])/(q1[0]*q1[0] + q1[1]*q1[1] + q1[2]*q1[2] - 2.0*q1[0]*q3[0] - 2.0*q1[1]*q3[1] - 2.0*q1[2]*q3[2] + q3[0]*q3[0] + q3[1]*q3[1] + q3[2]*q3[2]);
  return lambda;
}

void DICOMReader::ComputeRASInformation()
{
  // 
  // The following calculation is from vtkDICOMVolSlice.cxx
  // 
  float norm_row = Header.pix_spacing * Header.width;
  float norm_col = Header.pix_spacing * Header.height;

  Header.upper_right_R = Header.upper_left_R + Header.dir_cos_row_L * norm_row * -1.0;
  Header.upper_right_A = Header.upper_left_A + Header.dir_cos_row_P * norm_row * -1.0;
  Header.upper_right_S = Header.upper_left_S + Header.dir_cos_row_S * norm_row * -1.0;

  Header.lower_right_R = Header.upper_right_R + Header.dir_cos_col_L * norm_col * -1.0;
  Header.lower_right_A = Header.upper_right_A + Header.dir_cos_col_P * norm_col * -1.0;
  Header.lower_right_S = Header.upper_right_S + Header.dir_cos_col_S * norm_col * -1.0;

  Header.lower_left_R = Header.upper_left_R + Header.dir_cos_col_L * norm_col * -1.0;
  Header.lower_left_A = Header.upper_left_A + Header.dir_cos_col_P * norm_col * -1.0;
  Header.lower_left_S = Header.upper_left_S + Header.dir_cos_col_S * norm_col * -1.0;

  float R_x_incr = Header.pix_spacing * Header.dir_cos_row_L * -1.0;
  float A_x_incr = Header.pix_spacing * Header.dir_cos_row_P * -1.0;
  float S_x_incr = Header.pix_spacing * Header.dir_cos_row_S;

  std::cout << "RAS x incr: " << R_x_incr << "," << A_x_incr << "," << S_x_incr << std::endl;

  float R_y_incr = Header.pix_spacing * Header.dir_cos_col_L * -1.0;
  float A_y_incr = Header.pix_spacing * Header.dir_cos_col_P * -1.0;
  float S_y_incr = Header.pix_spacing * Header.dir_cos_col_S;

  std::cout << "RAS y incr: " << R_y_incr << "," << A_y_incr << "," << S_y_incr << std::endl;

  float q1[3] = {0.0, 0.0, 0.0};
  float q2[3] = {0.0, 0.0, 0.0};
  float q3[3] = {0.0, 0.0, 0.0};
  float q4[3] = {0.0, 0.0, 0.0};
  float p1[3] = {0.0, 0.0, 0.0};
  float p2[3] = {0.0, 0.0, 0.0};
  float iso[3] = {0.0, 0.0, 0.0};
  int p[3] = {0, 0, 0};
 
  //  Image coordinates at corners.
  //  q's are in RAS.
  // 
  //   q1  *--------------* q2
  //       |              | 
  //       |              |
  //       |              |
  //       |              |
  //       |              |
  //   q3  *--------------* q4
  //
  q1[0] = Header.upper_left_R;
  q1[1] = Header.upper_left_A;
  q1[2] = Header.upper_left_S;

  q3[0] = Header.lower_left_R;
  q3[1] = Header.lower_left_A;
  q3[2] = Header.lower_left_S;

  q4[0] = Header.lower_right_R;
  q4[1] = Header.lower_right_A;
  q4[2] = Header.lower_right_S;

  q2[0] = Header.upper_right_R;
  q2[1] = Header.upper_right_A;
  q2[2] = Header.upper_right_S;

  //
  // The following computations assume that the RAS coordinate system is
  // not skewed over the image, i.e. R is along x and A is along y.
  // 
  float lambda1 = this->ComputeLambda(q1, q3);

  p1[0] = (1.0 - lambda1) * q1[0] + lambda1 * q3[0];
  p1[1] = (1.0 - lambda1) * q1[1] + lambda1 * q3[1];
  p1[2] = (1.0 - lambda1) * q1[2] + lambda1 * q3[2];
  
  p2[0] = (1.0 - lambda1) * q2[0] + lambda1 * q4[0];
  p2[1] = (1.0 - lambda1) * q2[1] + lambda1 * q4[1];
  p2[2] = (1.0 - lambda1) * q2[2] + lambda1 * q4[2];


  float lambda2 = this->ComputeLambda(p1, p2);
  
  iso[0] = (1.0 - lambda2) * p1[0] + lambda2 * p2[0];
  iso[1] = (1.0 - lambda2) * p1[1] + lambda2 * p2[1];
  iso[2] = (1.0 - lambda2) * p1[2] + lambda2 * p2[2];

  std::cout << "Iso: " << iso[0] << "," << iso[1] << "," << iso[2] << std::endl;

  // 
  // lambda1 is between q1 and q3 => therefore, lambda1 scales height.
  // lambda2 is between q2 and q4 => therefore, lambda2 scales width.
  p[0] = int(lambda2 * Header.width + 0.5);
  p[1] = int(lambda1 * Header.height + 0.5);
  p[2] = 0;

  Header.RA_origin_x = p[0];
  Header.RA_origin_y = p[1];

}

