#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <fstream>
#include <string>
#include <math.h>


#include <metaUtils.h>
#include <metaObject.h>
#include <metaImage.h>

//
// MetaImage Constructors
//
MetaImage::
MetaImage()
:MetaObject()
{
  if(META_DEBUG) std::cout << "MetaImage()" << std::endl;
  MetaImage::Clear();
  m_AutoFreeElementData = 0;
  m_ElementData = NULL;
}

//
MetaImage::
MetaImage(const char *_headerName)
:MetaObject()
  {
  if(META_DEBUG) std::cout << "MetaImage()" << std::endl;
  Clear();
  MetaImage::Read(_headerName);
  }

//
MetaImage::
MetaImage(MetaImage *_im)
:MetaObject()
  {
  if(META_DEBUG) std::cout << "MetaImage()" << std::endl;
  MetaImage::Clear();
  InitializeEssential(_im->NDims(), 
                      _im->DimSize(),
                      _im->ElementSpacing(),
                      _im->ElementType(),
                      _im->ElementNumberOfChannels(),
                      _im->ElementData());
  CopyInfo(_im);
  }

//
MetaImage::
MetaImage(int _nDims, 
          const int * _dimSize,
          const float * _elementSpacing,
          MET_ValueEnumType _elementType,
          int _elementNumberOfChannels,
          void *_elementData)
:MetaObject()
  {
  if(META_DEBUG) std::cout << "MetaImage()" << std::endl;
  MetaImage::Clear();
  InitializeEssential(_nDims, 
                      _dimSize, 
                      _elementSpacing,
                      _elementType,
                      _elementNumberOfChannels,
                      _elementData);
  }

//
MetaImage::
MetaImage(int _x, int _y, 
          float _elementSpacingX, float _elementSpacingY,
          MET_ValueEnumType _elementType,
          int _elementNumberOfChannels, void *_elementData)
:MetaObject()
  {
  if(META_DEBUG) std::cout << "MetaImage()" << std::endl;
  int ds[2];
  ds[0] = _x;
  ds[1] = _y;
  float es[2];
  es[0] = _elementSpacingX;
  es[1] = _elementSpacingY;
  MetaImage::Clear();
  InitializeEssential(2, ds, es, _elementType, 
                      _elementNumberOfChannels,
                      _elementData);
  }

//
MetaImage::
MetaImage(int _x, int _y, int _z, 
          float _elementSpacingX, 
          float _elementSpacingY,
          float _elementSpacingZ, 
          MET_ValueEnumType _elementType,
          int _elementNumberOfChannels, 
          void *_elementData)
:MetaObject()
  {
  if(META_DEBUG) std::cout << "MetaImage()" << std::endl;
  int ds[3];
  ds[0] = _x;
  ds[1] = _y;
  ds[2] = _z;
  float es[3];
  es[0] = _elementSpacingX;
  es[1] = _elementSpacingY;
  es[2] = _elementSpacingZ;
  MetaImage::Clear();
  InitializeEssential(3, ds, es, _elementType, 
                      _elementNumberOfChannels,
                      _elementData);
  }

//
MetaImage::
~MetaImage()
  {
  M_Destroy();
  }

//
void MetaImage::
PrintInfo() const
  {
  int i;

  MetaObject::PrintInfo();

  char s[255];
  MET_ImageModalityToString(m_Modality, s);
  std::cout << "Modality = " << s << std::endl;

  std::cout << "Quantity = " << m_Quantity << std::endl;

  std::cout << "DimSize = ";
  for(i=0; i<m_NDims; i++)
    {
    std::cout << m_DimSize[i] << " ";
    }
  std::cout << std::endl;
  std::cout << "SubQuantity = ";
  for(i=0; i<m_NDims; i++)
    {
    std::cout << m_SubQuantity[i] << " ";
    }
  std::cout << std::endl;

  std::cout << "HeaderSize = " << m_HeaderSize << std::endl;

  std::cout << "SequenceID = ";
  for(i=0; i<m_NDims; i++)
    {
    std::cout << m_SequenceID[i] << " ";
    }
  std::cout << std::endl;

  std::cout << "ElementSizeValid = " << (int)m_ElementSizeValid
            << std::endl;
  std::cout << "ElementSize = ";
  for(i=0; i<m_NDims; i++)
    {
    std::cout << m_ElementSize[i] << " ";
    }
  std::cout << std::endl;

  char str[255];
  MET_TypeToString(m_ElementType, str);
  std::cout << "ElementType = " << str << std::endl;

  std::cout << "ElementNumberOfChannels = "
            << m_ElementNumberOfChannels << std::endl;

  if(m_ElementMinMaxValid)
    {
    std::cout << "Min and Max are valid" << std::endl;
    std::cout << "   Min = " << m_ElementMin << std::endl;
    std::cout << "   Max = " << m_ElementMax << std::endl;
    }
  else
    {
    std::cout << "Min and Max are not valid" << std::endl;
    }

  std::cout << "AutoFreeElementData = " 
            << ((m_AutoFreeElementData)?"True":"False") << std::endl;

  std::cout << "ElementData = " << ((m_ElementData==NULL)?"NULL":"Valid")
            << std::endl;
  }

void MetaImage::
CopyInfo(const MetaImage * _im)
  {
  MetaObject::CopyInfo(_im);

  if(_im->ElementSizeValid())
    {
    ElementSize(_im->ElementSize());
    }
  HeaderSize(_im->HeaderSize());
  Modality(_im->Modality());
  SequenceID(_im->SequenceID());
  ElementMin(_im->ElementMin());
  ElementMax(_im->ElementMax());
  }

int MetaImage::
HeaderSize(void) const
  {
  return m_HeaderSize;
  }

void MetaImage::
HeaderSize(int _headerSize)
  {
  m_HeaderSize = _headerSize;
  }

MET_ImageModalityEnumType MetaImage::
Modality(void) const
  {
  return m_Modality;
  }

void MetaImage::
Modality(MET_ImageModalityEnumType _modality)
  {
  m_Modality = _modality;
  }

const int * MetaImage::
DimSize(void) const
  {
  return m_DimSize;
  }

int MetaImage:: 
DimSize(int _i) const
  {
  return m_DimSize[_i];
  }

int MetaImage::
Quantity(void) const
  {
  return m_Quantity;
  }

const int * MetaImage::
SubQuantity(void) const
  {
  return m_SubQuantity;
  }

int MetaImage::
SubQuantity(int _i) const
  {
  return m_SubQuantity[_i];
  }

const float * MetaImage::
SequenceID(void) const
  {
  return m_SequenceID;
  }

float MetaImage::
SequenceID(int _i) const
  {
  return m_SequenceID[_i];
  }

void MetaImage::
SequenceID(const float *_sequenceID)
  {
  memcpy(m_SequenceID, _sequenceID, m_NDims*sizeof(float));
  }

void MetaImage::
SequenceID(int _i, float _value)
  {
  m_SequenceID[_i] = _value;
  }

bool MetaImage::
ElementSizeValid(void) const
  {
  return m_ElementSizeValid;
  }

void MetaImage::
ElementSizeValid(bool _elementSizeValid)
  {
  m_ElementSizeValid = _elementSizeValid;
  }

const float * MetaImage::
ElementSize(void) const
  {
  return m_ElementSize;
  }

float MetaImage::
ElementSize(int _i) const
  {
  return m_ElementSize[_i];
  }

void MetaImage::
ElementSize(const float *_elementSize)
  {
  memcpy(m_ElementSize, _elementSize, m_NDims*sizeof(float));
  m_ElementSizeValid = true;
  }

void MetaImage::
ElementSize(int _i, float _value)
  {
  m_ElementSize[_i] = _value;
  m_ElementSizeValid = true;
  }

MET_ValueEnumType MetaImage::
ElementType(void) const
  {
  return m_ElementType;
  }

void MetaImage::
ElementType(MET_ValueEnumType _elementType)
  {
  m_ElementType = _elementType;
  }

int MetaImage::
ElementNumberOfChannels(void) const
  {
  return m_ElementNumberOfChannels;
  }

void MetaImage::
ElementNumberOfChannels(int _elementNumberOfChannels)
  {
  m_ElementNumberOfChannels = _elementNumberOfChannels;
  }

void MetaImage::
ElementByteOrderSwap(void)
  {
  std::cout << "MetaImage: ElementByteOrderSwap" << std::endl;

  int eSize;
  MET_SizeOfType(m_ElementType, &eSize);    
  switch(eSize)
    {
    default:
    case 0:
    case 1: 
      {
      break;
      }
    case 2:
      {
      int i;
      for(i=0; i<m_Quantity*m_ElementNumberOfChannels; i++)
        {
        ((unsigned short *)m_ElementData)[i] = 
              MET_ByteOrderSwapShort(((unsigned short *)m_ElementData)[i]);
        }
      break;
      }
    case 4:
    case 8:
      {
      int i;
      for(i=0; i<m_Quantity*m_ElementNumberOfChannels; i++)
        {
        ((unsigned int *)m_ElementData)[i] =
              MET_ByteOrderSwapLong(((unsigned int *)m_ElementData)[i]);
        }
      break;
      }
    }
  m_BinaryDataByteOrderMSB = !m_BinaryDataByteOrderMSB;
  }

bool MetaImage::
ElementByteOrderFix(void)
  {
  if(m_BinaryDataByteOrderMSB != MET_SystemByteOrderMSB())
    {
    ElementByteOrderSwap();
    return true;
    }
  return true;
  }

bool MetaImage::
ElementMinMaxValid(void) const
  {
  return m_ElementMinMaxValid;
  }

void MetaImage::
ElementMinMaxValid(bool _elementMinMaxValid)
  {
  m_ElementMinMaxValid = _elementMinMaxValid;
  }

void MetaImage::
ElementMinMaxRecalc(void)
  {
  int i;
  double tf;
  
  if(m_ElementData == NULL)
    return;

  ElementByteOrderFix();

  MET_ValueToDouble(m_ElementType, m_ElementData, 0, &tf);
  m_ElementMin = tf;
  m_ElementMax = tf;
  for(i=1; i<m_Quantity*m_ElementNumberOfChannels; i++)
    {
    MET_ValueToDouble(m_ElementType, m_ElementData, i, &tf);
    if(tf<m_ElementMin)
      {
      m_ElementMin = tf;
      }
    else if(tf>m_ElementMax)
        {
        m_ElementMax = tf;
        }
    }

  m_ElementMinMaxValid = true;

  std::cout << "MetaImage: ElementMinMaxRecalc: min = "
            << m_ElementMin << " : max = " 
            << m_ElementMax << std::endl;

  }

double MetaImage::
ElementMin(void) const
  {
  return m_ElementMin;
  }

void MetaImage::
ElementMin(double _elementMin)
  {
  m_ElementMin = _elementMin;
  }

double MetaImage::
ElementMax(void) const
  {
  return m_ElementMax;
  }

void MetaImage::
ElementMax(double _elementMax)
  {
  m_ElementMax = _elementMax;
  }

bool MetaImage::
ConvertElementDataTo(MET_ValueEnumType _elementType,
                     double _toMin, double _toMax)
  {
  int eSize;
  MET_SizeOfType(_elementType, &eSize);
  void * newElementData = calloc(m_Quantity*m_ElementNumberOfChannels, eSize);

  ElementByteOrderFix();
  if(!ElementMinMaxValid())
    {
    ElementMinMaxRecalc();
    }

  int i;
  for(i=0; i<m_Quantity*m_ElementNumberOfChannels; i++)
    {
    MET_ValueToValue(m_ElementType, m_ElementData, i, _elementType, 
                     newElementData, m_ElementMin, m_ElementMax,
                     _toMin, _toMax);
    }

  if(m_AutoFreeElementData)
    delete [] (char *)m_ElementData;
  m_ElementData = newElementData;
  m_ElementType = _elementType;
  m_ElementMinMaxValid = true;
  m_ElementMin = _toMin;
  m_ElementMax = _toMax;
  m_AutoFreeElementData = true;

  return true;
  }

void * MetaImage::
ElementData(void)
  {
  return m_ElementData;
  }

bool MetaImage::
ElementData(int _i, double _v)
  {
  if(_i<m_Quantity)
    {
    MET_DoubleToValue(_v, m_ElementType, m_ElementData, _i);
    return true;
    }
  return false;
  }

void MetaImage::
ElementData(void * _elementData)
  {
  if(m_AutoFreeElementData)
     delete [] (char *)m_ElementData;
  m_ElementData = _elementData;
  m_AutoFreeElementData = true;
  }

double MetaImage::
ElementData(int _i) const
  {
  double tf = 0;
  MET_ValueToDouble(m_ElementType, m_ElementData, _i, &tf);

  return tf;
  }

bool MetaImage::
AutoFreeElementData(void) const
  {
  return m_AutoFreeElementData;
  }

void MetaImage::
AutoFreeElementData(bool _autoFreeElementData)
  {
  m_AutoFreeElementData = _autoFreeElementData;
  }

//
//
//
const char * MetaImage::
ElementDataFileName(void) const
  {
  return m_ElementDataFileName;
  }

void MetaImage::
ElementDataFileName(const char * _elementDataFileName)
  {
  strcpy(m_ElementDataFileName, _elementDataFileName);
  }


bool MetaImage::
Read(const char *_headerName, bool _readElements, void * _buffer)
  {
  M_Destroy();

  MetaImage::Clear();

  M_SetupReadFields();

  if(_headerName != NULL)
    {
    strcpy(m_FileName, _headerName);
    }
  
  if(!m_ReadStream)
  {
    m_ReadStream = new std::ifstream;
  }
  m_ReadStream->open(m_FileName, std::ios::binary | std::ios::in);
  if(!m_ReadStream->is_open())
    {
    std::cout << "MetaImage: Read: Cannot open file" << std::endl;
    return false;
    }

  if(!M_Read())
    {
    std::cout << "MetaImage: Read: Cannot parse file" << std::endl;
    m_ReadStream->close();
    return false;
    }

  MetaImage::InitializeEssential(m_NDims, 
                                 m_DimSize,
                                 m_ElementSpacing, 
                                 m_ElementType, 
                                 m_ElementNumberOfChannels, 
                                 _buffer, 
                                 _readElements);

 
  if(_headerName != NULL)
    {
    strcpy(m_FileName, _headerName);
    }

  int i, j;
  bool usePath;
  char pathName[255];
  char fName[255];
  usePath = MET_GetFilePath(m_FileName, pathName);

  if(_readElements)
    {
    if(!strcmp("Local", m_ElementDataFileName) || 
       !strcmp("LOCAL", m_ElementDataFileName) ||
       !strcmp("local", m_ElementDataFileName))
      {
      M_ReadElements(m_ReadStream, m_ElementData, m_Quantity);
      }
    else if(!strncmp("LIST", m_ElementDataFileName,4))
      {
      int fileImageDim = 0;
      char junk[255];
      sscanf( m_ElementDataFileName,"%s %d",junk, &fileImageDim);
      if ( (fileImageDim == 0) || (fileImageDim > m_NDims) )
        {
        // if optional file dimension size is not give or is larger than
        // overall dimension then default to a size of m_NDims - 1.
        fileImageDim = m_NDims-1;
        }
      char s[255];
      std::ifstream* readStreamTemp = new std::ifstream;
      int elementSize;
      MET_SizeOfType(m_ElementType, &elementSize);
      elementSize *= m_ElementNumberOfChannels;
      int totalFiles = 1;
      for (i = m_NDims; i > fileImageDim; i--)
        {
          totalFiles *= m_DimSize[i-1];
        }
      for(i=0; i< totalFiles && !m_ReadStream->eof(); i++)
        {
        m_ReadStream->getline(s, 255);
        if(!m_ReadStream->eof())
          {
          j = strlen(s)-1;
          while(j>0 && (isspace(s[j]) || !isprint(s[j])))
            {
            s[j--] = '\0';
            }
          if(usePath)
            {
            sprintf(fName, "%s%s", pathName, s);
            }
          else
            {
            strcpy(fName, s);
            }

          readStreamTemp->open(fName, std::ios::binary | std::ios::in);
          if(!readStreamTemp->is_open())
            {
            std::cout << "MetaImage: Read: cannot open slice" << std::endl;
            continue;
            }

          M_ReadElements(readStreamTemp,
                         &(((char *)m_ElementData)[i*m_SubQuantity[fileImageDim]*
                                                   elementSize]),
                         m_SubQuantity[fileImageDim]);

          readStreamTemp->close();
          }
        }
        delete readStreamTemp;
      }
    else if(strstr(m_ElementDataFileName, "%"))
      {
      int nWrds;
      char **wrds;
      int minV = 1;
      int maxV = m_DimSize[m_NDims-1];
      int stepV = 1;
      char s[255];
      std::ifstream* readStreamTemp = new std::ifstream;
      MET_StringToWordArray(m_ElementDataFileName, &nWrds, &wrds);
      int elementSize;
      MET_SizeOfType(m_ElementType, &elementSize);
      elementSize *= m_ElementNumberOfChannels;
      if(nWrds >= 2)
        {
        minV = (int)atof(wrds[1]);
        maxV = minV + m_DimSize[m_NDims-1] - 1;
        }
      if(nWrds >= 3)
        {
        maxV = (int)atof(wrds[2]);
        stepV = (maxV-minV)/(m_DimSize[m_NDims-1]);
        }
      if(nWrds >= 4)
        {
        stepV = (int)atof(wrds[3]);
        }
      std::cout << "Using string '" << wrds[0] << "' with values " 
        << minV << " to " << maxV << " stepping " << stepV << std::endl;
      int cnt = 0;
      for(i=minV; i<=maxV; i += stepV)
        {
        sprintf(s, wrds[0], i);
        if(usePath)
          {
          sprintf(fName, "%s%s", pathName, s);
          }
        else
          {
          strcpy(fName, s);
          }
        std::cout << "  file = _" << fName << "_" << std::endl;
        readStreamTemp->open(fName, std::ios::binary | std::ios::in);
        if(!readStreamTemp->is_open())
          {
          std::cout << "MetaImage: Read: cannot construct file _" 
            << fName << "_" << std::endl;
          continue;
          }
      
        M_ReadElements(readStreamTemp,
                       &(((char *)m_ElementData)[cnt*m_SubQuantity[m_NDims-1]*
                                                 elementSize]),
                       m_SubQuantity[m_NDims-1]);
        cnt++;

        readStreamTemp->close();
        }
        delete readStreamTemp;
      }
    else
      {
      if(usePath)
        {
        sprintf(fName, "%s%s", pathName, m_ElementDataFileName);
        }
      else
        {
        strcpy(fName, m_ElementDataFileName);
        }
      std::ifstream* readStreamTemp = new std::ifstream;
      readStreamTemp->open(fName, std::ios::binary | std::ios::in);
      if(!readStreamTemp->is_open())
        {
        std::cout << "MetaImage: Read: Cannot open data file" << std::endl;
        m_ReadStream->close();
        return false;
        }
      M_ReadElements(readStreamTemp, m_ElementData, m_Quantity);
      readStreamTemp->close();
      delete readStreamTemp;
      }
    }

  m_ReadStream->close();
  
  return true;
  }

//
//
//
bool MetaImage::
Write(const char *_headName, const char *_dataName, bool _writeElements)
  {
  if(_dataName == NULL)
    {
    if(strlen(m_ElementDataFileName)==0)
      {
      ElementDataFileName("LOCAL");
      }
    }
  else
    {
    ElementDataFileName(_dataName);
    }
  
  if(_headName != NULL)
    {
    FileName(_headName);
    }

  char pathName[255];
  bool usePath = MET_GetFilePath(m_FileName, pathName);
  if(usePath)
    {
    char elementPathName[255];
    MET_GetFilePath(m_ElementDataFileName, elementPathName);
    if(!strcmp(pathName, elementPathName))
      {
      strcpy(elementPathName, &m_ElementDataFileName[strlen(pathName)]);
      strcpy(m_ElementDataFileName, elementPathName);
      }
    }

  M_SetupWriteFields();

  if(!m_WriteStream)
  {
    m_WriteStream = new std::ofstream;
  }
  m_WriteStream->open(m_FileName, std::ios::binary | std::ios::out);
  if(!m_WriteStream->is_open())
    {
    return false;
    }

  M_Write();

  if(_writeElements)
    {
    int elementSize;
    MET_SizeOfType(m_ElementType, &elementSize);
    int elementNumberOfBytes = elementSize*m_ElementNumberOfChannels;
    if(!strcmp(m_ElementDataFileName, "LOCAL"))
      {
      m_WriteStream->write( (char *)m_ElementData,
                            m_Quantity * elementNumberOfBytes ); 
      m_WriteStream->close();
      return true;
      }
    else
      {
      m_WriteStream->close();

      std::ofstream* writeStreamTemp = new std::ofstream;
      char dataFileName[255];
      if(usePath)
        {
        sprintf(dataFileName, "%s%s", pathName, m_ElementDataFileName);
        }
      else
        {
        strcpy(dataFileName, m_ElementDataFileName);
        }
      if(strstr(dataFileName, "%"))
        {
        int i;
        char fName[255];
        int sliceNumberOfBytes = m_SubQuantity[m_NDims-1]*elementNumberOfBytes;
        for(i=1; i<=m_DimSize[m_NDims-1]; i++)
          {
          sprintf(fName, dataFileName, i);
          writeStreamTemp->open(fName, std::ios::binary | std::ios::out);
          writeStreamTemp->write(&(((char *)m_ElementData)[i*sliceNumberOfBytes]),
                                sliceNumberOfBytes);
          writeStreamTemp->close();
          }
        }
      else
        {
        writeStreamTemp->open(dataFileName, std::ios::binary | std::ios::out);
        writeStreamTemp->write( (char *)m_ElementData, 
                                m_Quantity * elementNumberOfBytes );
        writeStreamTemp->close();
        return true;
        }
      delete writeStreamTemp; 
      }
    }
      
  return true;
  }
  
void MetaImage::
Clear(void)
  {
  if(META_DEBUG) std::cout << "MetaImage: Clear" << std::endl;

  strcpy(m_ElementDataFileName, "");

  m_ElementType = MET_NONE;
  m_ElementNumberOfChannels = 1;
  m_ElementData = NULL;

  m_HeaderSize = 0;

  memset(m_SequenceID, 0, 4*sizeof(float));
  memset(m_ElementSize, 0, 10*sizeof(float));
  m_ElementSizeValid = false;

  m_Modality = MET_MOD_UNKNOWN;

  m_ElementMinMaxValid = false;
  m_ElementMin = 0;
  m_ElementMax = 0;
  m_Quantity = 0;
  m_SubQuantity[0] = 0;
  m_DimSize[0] = 0;

  MetaObject::Clear();

  m_BinaryData = true;
  }
        
        
bool MetaImage::
InitializeEssential(int _nDims,
                    const int * _dimSize,
                    const float * _elementSpacing, 
                    MET_ValueEnumType _elementType,
                    int _elementNumberOfChannels,
                    void * _elementData,
                    bool _allocElementMemory)
  {
  if(META_DEBUG) std::cout << "MetaImage: Initialize" << std::endl;

  MetaObject::InitializeEssential(_nDims);

  int i;
  m_Quantity = 1;
  m_SubQuantity[0] = 1;
  m_ElementSizeValid = false;
  for(i=0; i<m_NDims; i++)
    {
    m_DimSize[i] = _dimSize[i];
    m_Quantity *= _dimSize[i];
    if(i>0)
      {
      m_SubQuantity[i] = m_SubQuantity[i-1]*m_DimSize[i-1];
      }
    m_ElementSpacing[i] = _elementSpacing[i];
    if(m_ElementSize[i] == 0)
      {
      m_ElementSize[i] = m_ElementSpacing[i];
      }
    else
      {
      m_ElementSizeValid = true;
      }
    }

  m_ElementType = _elementType;

  m_ElementNumberOfChannels = _elementNumberOfChannels;

  if(_elementData != NULL)
    {
    m_AutoFreeElementData = false;
    m_ElementData = (void *)_elementData;
    }
  else if(_allocElementMemory)
    {
    m_AutoFreeElementData = true;
    MET_SizeOfType(m_ElementType, &i);
    m_ElementData = calloc(m_Quantity*m_ElementNumberOfChannels, i);
    if(m_ElementData == NULL)
      {
      m_AutoFreeElementData = false;
      std::cout << "MetaImage:: M_Allocate:: Insufficient memory" << std::endl;
      return false;
      }
    }
  else
    {
    m_AutoFreeElementData = false;
    m_ElementData = NULL;
    }

  m_BinaryData = true;

  return true;
  }

void MetaImage::
M_Destroy(void)
  {
  if(m_AutoFreeElementData && m_ElementData != NULL)
    {
    delete [] (char *)m_ElementData;
    }

  m_ElementData = NULL;

  MetaObject::M_Destroy();
  }

void MetaImage::
M_SetupReadFields(void)
  {
  if(META_DEBUG) std::cout << "MetaImage: M_SetupReadFields" << std::endl;

  MetaObject::M_SetupReadFields();

  MET_FieldRecordType * mF;

  int nDimsRecNum = MET_GetFieldRecordNumber("NDims", &m_Fields);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "DimSize", MET_INT_ARRAY, true, nDimsRecNum);
  mF->required = true;
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "HeaderSize", MET_INT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Modality", MET_STRING, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "SequenceID", MET_INT_ARRAY, false, nDimsRecNum);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementMin", MET_FLOAT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementMax", MET_FLOAT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementNumberOfChannels", MET_INT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementSize", MET_FLOAT_ARRAY, false, nDimsRecNum);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementType", MET_STRING, true);
  mF->required = true;
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementDataFile", MET_STRING, true);
  mF->required = true;
  mF->terminateRead = true;
  m_Fields.push_back(mF);
  }

void MetaImage::
M_SetupWriteFields(void)
  {
  m_BinaryData = true;

  strcpy(m_ObjectTypeName,"Image");
  MetaObject::M_SetupWriteFields();

  MET_FieldRecordType * mF;

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "DimSize", MET_INT_ARRAY, m_NDims, m_DimSize);
  m_Fields.push_back(mF);

  char s[255];
  if(m_HeaderSize > 0 || m_HeaderSize == -1)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "HeaderSize", MET_INT);
    m_Fields.push_back(mF);
    }

  int i;
  if(m_Modality != MET_MOD_UNKNOWN)
    {
    mF = new MET_FieldRecordType;
    strcpy(s, MET_ValueTypeName[m_Modality]);
    MET_InitWriteField(mF, "Modality", MET_STRING, strlen(s), s);
    m_Fields.push_back(mF);
    }

  bool valid = false;
  for(i=0; i<4; i++)
    {
    if(m_SequenceID[i] != 0)
      {
      valid = true;
      break;
      }
    }
  if(valid)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "SequenceID", MET_FLOAT_ARRAY, m_NDims,
                       m_SequenceID);
    m_Fields.push_back(mF);
    }

  if(m_ElementMinMaxValid)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ElementMin", MET_FLOAT, m_ElementMin);
    m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ElementMax", MET_FLOAT, m_ElementMax);
    m_Fields.push_back(mF);
    }

  if(m_ElementNumberOfChannels>1)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ElementNumberOfChannels", MET_INT, 
                       m_ElementNumberOfChannels);
    m_Fields.push_back(mF);
    }

  if(m_ElementSizeValid)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ElementSize", MET_FLOAT_ARRAY, m_NDims,
                       m_ElementSize);
    m_Fields.push_back(mF);
    }

  mF = new MET_FieldRecordType;
  MET_TypeToString(m_ElementType, s);
  MET_InitWriteField(mF, "ElementType", MET_STRING, strlen(s), s);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "ElementDataFile", MET_STRING, 
                     strlen(m_ElementDataFileName),
                     m_ElementDataFileName);
  mF->terminateRead = true;
  m_Fields.push_back(mF);
  }

//
//
//
bool MetaImage::
M_ReadElements(std::ifstream * _fstream, void * _data, int _dataQuantity)
  {
  if(META_DEBUG) std::cout << "MetaImage: M_ReadElements" << std::endl;

  if(m_HeaderSize>(int)0)
    {
    _fstream->seekg(m_HeaderSize, std::ios::cur);
    if((int)_fstream->gcount() != m_HeaderSize)
      {
      std::cout << "MetaImage: Read: header not read correctly" << std::endl;
      return false;
      }
    }

  int elementSize;
  MET_SizeOfType(m_ElementType, &elementSize);
  int readSize = _dataQuantity*m_ElementNumberOfChannels*elementSize;
  if(META_DEBUG)
    std::cout << "MetaImage: M_ReadElements: ReadSize = " 
              << readSize << std::endl;

  if(m_HeaderSize == -1)
    {
    if(META_DEBUG) 
      std::cout << "MetaImage: M_ReadElements: Skipping header" << std::endl;
    _fstream->seekg(-readSize, std::ios::end);
    }

  _fstream->read((char *)_data, readSize);
  int gc = _fstream->gcount();
  if(gc != readSize)
    {
    std::cout << "MetaImage: M_ReadElements: data not read completely" 
              << std::endl;
    std::cout << "   ideal = " << readSize << " : actual = " << gc << std::endl;
    return false;
    }

  return true;
  }

bool MetaImage
::Append(const char *_headName)
  {
  if(META_DEBUG) std::cout << "MetaImage: Append" << std::endl;

  if(strlen(m_ElementDataFileName)==0)
    {
    ElementDataFileName("LOCAL");
    }

  if(_headName != NULL)
    {
    FileName(_headName);
    }

  M_SetupWriteFields();

  if(!m_WriteStream)
  {
    m_WriteStream = new std::ofstream;
  }

  m_WriteStream->open(m_FileName, std::ios::binary | std::ios::app | std::ios::out);
  if(!m_WriteStream->is_open())
    {
    return false;
    }

  M_Write();
      
  int elementSize;
  MET_SizeOfType(m_ElementType, &elementSize);
  int elementNumberOfBytes = elementSize*m_ElementNumberOfChannels;
  if(!strcmp(m_ElementDataFileName, "LOCAL"))
    {
    m_WriteStream->write((char *)m_ElementData,
                        m_Quantity * elementNumberOfBytes); 
    m_WriteStream->close();
    return true;
    }
  else
    {
    char pathName[255];
    bool usePath = MET_GetFilePath(m_FileName, pathName);
    std::ofstream* writeStreamTemp = new std::ofstream;
    m_WriteStream->close();
    char dataFileName[255];
    if(usePath)
      {
      sprintf(dataFileName, "%s%s", pathName, m_ElementDataFileName);
      }
    else
      {
      strcpy(dataFileName, m_ElementDataFileName);
      }
    if(strstr(dataFileName, "%"))
      {
      int i;
      char fName[255];
      int sliceNumberOfBytes = m_SubQuantity[m_NDims-1]*elementNumberOfBytes;
      for(i=1; i<=m_DimSize[m_NDims-1]; i++)
        {
        sprintf(fName, dataFileName, i);
        writeStreamTemp->open(fName, std::ios::binary | std::ios::out);
        writeStreamTemp->write(&(((char *)m_ElementData)[i*sliceNumberOfBytes]),
                              sliceNumberOfBytes);
        writeStreamTemp->close();
        }
      }
    else
      {
      writeStreamTemp->open(dataFileName, std::ios::binary | std::ios::out);
      writeStreamTemp->write( (char *)m_ElementData,
                              m_Quantity * elementNumberOfBytes);
      writeStreamTemp->close();
      return true;
      }
    delete writeStreamTemp;
    }
  return true;

  }

bool MetaImage::
M_Read(void)
  {
  if(META_DEBUG) std::cout << "MetaImage: M_Read: Loading Header" << std::endl;
  if(!MetaObject::M_Read())
    {
    std::cout << "MetaImage: M_Read: Error parsing file" << std::endl;
    return false;
    }

  if(META_DEBUG) std::cout << "MetaImage: M_Read: Parsing Header" << std::endl;
  MET_FieldRecordType * mF;
     
  if(META_DEBUG) 
    std::cout << "metaImage: M_Read: elementSpacing[" << 0 << "] = "
              << m_ElementSpacing[0] << std::endl;

  mF = MET_GetFieldRecord("DimSize", &m_Fields);
  if(mF && mF->defined)
    {
    int i;
    for(i=0; i<m_NDims; i++)
      {
      m_DimSize[i] = (int)mF->value[i];
      }
    }

  mF = MET_GetFieldRecord("HeaderSize", &m_Fields);
  if(mF && mF->defined)
    {
    m_HeaderSize = (int)mF->value[0];
    }

  mF = MET_GetFieldRecord("Modality", &m_Fields);
  if(mF && mF->defined)
    {
    MET_StringToImageModality((char *)mF->value, &m_Modality);
    }

  mF = MET_GetFieldRecord("SequenceID", &m_Fields);
  if(mF && mF->defined)
    {
    int i;
    for(i=0; i<m_NDims; i++)
      {
      m_SequenceID[i] = mF->value[i];
      }
    }

  mF = MET_GetFieldRecord("ElementMin", &m_Fields);
  if(mF && mF->defined)
    {
    m_ElementMin = mF->value[0];
    }

  mF = MET_GetFieldRecord("ElementMax", &m_Fields);
  if(mF && mF->defined)
    {
    m_ElementMax = mF->value[0];
    }

  mF = MET_GetFieldRecord("ElementNumberOfChannels", &m_Fields);
  if(mF && mF->defined)
    {
    m_ElementNumberOfChannels = (int)mF->value[0];
    }


  mF = MET_GetFieldRecord("ElementSize", &m_Fields);
  if(mF && mF->defined)
    {
    m_ElementSizeValid = true;
    int i;
    for(i=0; i<m_NDims; i++)
      {
      m_ElementSize[i] = mF->value[i];
      }
    mF = MET_GetFieldRecord("ElementSpacing", &m_Fields);
    if(mF && !mF->defined)
      {
      for(i=0; i<m_NDims; i++)
        {
        m_ElementSpacing[i] = m_ElementSize[i];
        }
      }
    }
  else
    {
    int i;
    m_ElementSizeValid = false;
    for(i=0; i<m_NDims; i++)
      {
      m_ElementSize[i] = m_ElementSpacing[i];
      }
    }

  mF = MET_GetFieldRecord("ElementType", &m_Fields);
  if(mF && mF->defined)
    {
    MET_StringToType((char *)(mF->value), &m_ElementType);
    }

  mF = MET_GetFieldRecord("ElementDataFile", &m_Fields);
  if(mF && mF->defined)
    {
    strcpy(m_ElementDataFileName, (char *)(mF->value));
    }

  return true;
  }

bool MetaImage::
ReadStream(int ndims, std::ifstream * stream)
{
  if(META_DEBUG)  std::cout << "MetaImage: ReadStream" << std::endl;

  M_Destroy();
  Clear();

  M_SetupReadFields();

  MET_FieldRecordType * mF = MET_GetFieldRecord("NDims", &m_Fields);
  mF->value[0] = ndims;
  mF->defined = true;

  if(m_ReadStream)
  {
    delete m_ReadStream;
  }

  m_ReadStream = stream;

  if(!M_Read())
  {
    std::cout << "MetaImage: Read: Cannot parse file" << std::endl;
    return false;
  }

  InitializeEssential(m_NDims, 
                      m_DimSize, 
                      m_ElementSpacing,
                      m_ElementType, 
                      m_ElementNumberOfChannels, 
                      NULL);

  int i, j;
  bool usePath;
  char pathName[255];
  char fName[255];
  usePath = MET_GetFilePath(m_FileName, pathName);

  if(!strcmp("Local", m_ElementDataFileName) || 
     !strcmp("LOCAL", m_ElementDataFileName) ||
     !strcmp("local", m_ElementDataFileName))
  {
    M_ReadElements(m_ReadStream, m_ElementData, m_Quantity);
  }
  else if(!strcmp("LIST", m_ElementDataFileName))
  {
    char s[255];
    std::ifstream* readStreamTemp = new std::ifstream;
    for(i=0; i<m_DimSize[m_NDims-1] && !m_ReadStream->eof(); i++)
    {
      m_ReadStream->getline(s, 255);
      if(!m_ReadStream->eof())
      {
        j = strlen(s)-1;
        while(j>0 && (isspace(s[j]) || !isprint(s[j])))
        {
          s[j--] = '\0';
        }
        if(usePath)
        {
          sprintf(fName, "%s%s", pathName, s);
        }
        else
        {
          strcpy(fName, s);
        }

        readStreamTemp->open(fName, std::ios::binary | std::ios::in);
        if(!readStreamTemp->is_open())
        {
          std::cout << "MetaImage: Read: cannot open slice" << std::endl;
          continue;
        }

        M_ReadElements(readStreamTemp,
                       &(((char *)m_ElementData)[m_SubQuantity[m_NDims-1]*i]),
                       m_SubQuantity[m_NDims-1]);

        readStreamTemp->close();
      }
    }
    delete readStreamTemp;
  }
  else if(strstr(m_ElementDataFileName, "%"))
  {
    int nWrds;
    char **wrds;
    int minV = 1;
    int maxV = m_DimSize[m_NDims-1];
    int stepV = 1;
    char s[255];
    std::ifstream* readStreamTemp = new std::ifstream;
    MET_StringToWordArray(m_ElementDataFileName, &nWrds, &wrds);
    if(nWrds > 2)
    {
      minV = (int)atof(wrds[1]);
    }
    if(nWrds > 3)
    {
      maxV = (int)atof(wrds[2]);
    }
    if(nWrds > 4)
    {
      stepV = (int)atof(wrds[3]);
    }
    for(i=minV; i<=maxV; i += stepV)
    {
      sprintf(s, wrds[0], i);
      if(usePath)
      {
        sprintf(fName, "%s%s", pathName, fName);
      }
      else
      {
        strcpy(fName, s);
      }
      readStreamTemp->open(fName, std::ios::binary | std::ios::in);
      if(!readStreamTemp->is_open())
      {
        std::cout << "MetaImage: Read: cannot construct file" << std::endl;
        continue;
      }
      
      M_ReadElements(readStreamTemp,
                     &(((char *)m_ElementData)[m_SubQuantity[m_NDims-1]*i]),
                     m_SubQuantity[m_NDims-1]);

      readStreamTemp->close();
    }
    delete readStreamTemp;
  }
  else
  {
    if(usePath)
    {
      sprintf(fName, "%s%s", pathName, m_ElementDataFileName);
      std::cout << "MetaImage: Read: Element file with path = " << fName 
                << std::endl;
    }
    else
    {
      strcpy(fName, m_ElementDataFileName);
      if(META_DEBUG) std::cout << "MetaImage: Read: Element file = " << fName << std::endl;
    }
    std::ifstream* readStreamTemp = new std::ifstream;
    readStreamTemp->open(fName, std::ios::binary | std::ios::in);
    if(!readStreamTemp->is_open())
    {
      std::cout << "MetaImage: Read: Cannot open data file" << std::endl;
      return false;
    }
    M_ReadElements(readStreamTemp, m_ElementData, m_Quantity);
    readStreamTemp->close();
    delete readStreamTemp;
  }

  m_ReadStream=NULL;

  return true;
}
