#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <fstream>
#include <string>

#include <metaUtils.h>
#include <metaObject.h>
#include <metaBlob.h>

//
// MedImage Constructors
//
MetaBlob::
MetaBlob()
:MetaObject()
{
  if(META_DEBUG) std::cout << "MetaBlob()" << std::endl;
  m_NPoints = 0;
  m_PointList.clear();
  Clear();
}

//
MetaBlob::
MetaBlob(const char *_headerName)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaBlob()" << std::endl;
  m_NPoints = 0;
  m_PointList.clear();
  Clear();
  Read(_headerName);
}

//
MetaBlob::
MetaBlob(const MetaBlob *_tube)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaBlob()" << std::endl;
  m_NPoints = 0;
  m_PointList.clear();
  Clear();
  CopyInfo(_tube);
}



//
MetaBlob::
MetaBlob(unsigned int dim)
:MetaObject(dim)
{
  if(META_DEBUG) std::cout << "MetaBlob()" << std::endl;
  m_NPoints = 0;
  m_PointList.clear();
  Clear();
}

//
MetaBlob::
~MetaBlob()
{
  M_Destroy();
}

//
void MetaBlob::
PrintInfo() const
{
  MetaObject::PrintInfo();
  std::cout << "PointDim = " << m_PointDim << std::endl;
  std::cout << "NPoints = " << m_NPoints << std::endl;
  char str[255];
  MET_TypeToString(m_ElementType, str);
  std::cout << "ElementType = " << str << std::endl;
}

void MetaBlob::
CopyInfo(const MetaBlob * _tube)
{
  MetaObject::CopyInfo(_tube);
}

    

void MetaBlob::
PointDim(const char* pointDim)
{
  strcpy(m_PointDim,pointDim);
}
    
const char* MetaBlob::
PointDim(void) const
{
  return m_PointDim;
}

void MetaBlob::
NPoints(int npnt)
{
  m_NPoints = npnt;
}

int MetaBlob::
NPoints(void) const
{
  return m_NPoints;
}


bool MetaBlob::
ReadStream(int ndims, std::ifstream * stream)
{
  
  if(META_DEBUG)  std::cout << "MetaBlob: ReadStream" << std::endl;

  M_Destroy();
  Clear();

  M_SetupReadFields();

  MET_FieldRecordType * mF = MET_GetFieldRecord("NDims", &m_Fields);
  mF->value[0] = ndims;
  mF->defined = true;

  m_ReadStream = stream;
  bool result = M_Read();
  return result;
}

bool MetaBlob::
Read(const char *_headerName)
{
  if(META_DEBUG) std::cout << "MetaBlob: Read" << std::endl;

  M_Destroy();

  Clear();

  M_SetupReadFields();

  if(_headerName != NULL)
  {
    strcpy(m_FileName, _headerName);
  }

  if(META_DEBUG) std::cout << "MetaBlob: Read: Opening stream" << std::endl;
 
  m_ReadStream->open(m_FileName, std::ios::binary | std::ios::in);
  
  if(!m_ReadStream->is_open())
  {
    std::cout << "MetaBlob: Read: Cannot open file" << std::endl;
    return false;
  }

  if(!M_Read())
  {
    std::cout << "MetaBlob: Read: Cannot parse file" << std::endl;
    return false;
  }

  if(_headerName != NULL)
  {
    strcpy(m_FileName, _headerName);
  }

  m_ReadStream->close();

  return true;
}

//
//
//
bool MetaBlob::
Write(const char *_headName)
{
  if(META_DEBUG) std::cout << "MetaBlob: Write" << std::endl;

  if(_headName != NULL)
    {
    FileName(_headName);
    }

  m_NPoints = m_PointList.size();

  M_SetupWriteFields();

  m_WriteStream->open(m_FileName, std::ios::binary | std::ios::out);
  if(!m_WriteStream->is_open())
    {
    return false;
    }

  M_Write();
      
  m_WriteStream->close();

  return true;
}
  

bool MetaBlob
::Append(const char *_headName)
{
  if(META_DEBUG) std::cout << "MetaBlob: Append" << std::endl;

  if(_headName != NULL)
  {
    FileName(_headName);
  }

  m_NPoints = m_PointList.size();

  M_SetupWriteFields();

  m_WriteStream->open(m_FileName, std::ios::binary | std::ios::app | std::ios::out);
  if(!m_WriteStream->is_open())
  {
    return false;
  }

  M_Write();
  
  m_WriteStream->close();
  return true;

}




/** Clear tube information */
void MetaBlob::
Clear(void)
{
  if(META_DEBUG) std::cout << "MetaBlob: Clear" << std::endl;
  MetaObject::Clear();
  if(META_DEBUG) std::cout << "MetaBlob: Clear: m_NPoints" << std::endl;
  m_PointList.clear();
  m_NPoints = 0;
  strcpy(m_PointDim, "x y z red green blue alpha");
  m_ElementType = MET_FLOAT;
}
        
/** Destroy tube information */
void MetaBlob::
M_Destroy(void)
{
  MetaObject::M_Destroy();
}

/** Set Read fields */
void MetaBlob::
M_SetupReadFields(void)
{
  if(META_DEBUG) std::cout << "MetaBlob: M_SetupReadFields" << std::endl;

  MetaObject::M_SetupReadFields();

  MET_FieldRecordType * mF;

  //int nDimsRecNum = MET_GetFieldRecordNumber("NDims", &m_Fields);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "PointDim", MET_STRING, true);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "NPoints", MET_INT, true);
  m_Fields.push_back(mF);
 
  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementType", MET_STRING, true);
  mF->required = true;
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Points", MET_NONE, true);
  mF->terminateRead = true;
  m_Fields.push_back(mF);

}

MET_ValueEnumType MetaBlob::
ElementType(void) const
{
  return m_ElementType;
}

void MetaBlob::
ElementType(MET_ValueEnumType _elementType)
{
  m_ElementType = _elementType;
}


void MetaBlob::
M_SetupWriteFields(void)
{
  strcpy(m_ObjectTypeName,"Blob");
  MetaObject::M_SetupWriteFields();

  MET_FieldRecordType * mF;

  char s[255];
  mF = new MET_FieldRecordType;
  MET_TypeToString(m_ElementType, s);
  MET_InitWriteField(mF, "ElementType", MET_STRING, strlen(s), s);
  m_Fields.push_back(mF);

  if(strlen(m_PointDim)>0)
  {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "PointDim", MET_STRING,
                           strlen(m_PointDim),m_PointDim);
    m_Fields.push_back(mF);
  }
  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "NPoints", MET_INT,m_NPoints);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "Points", MET_NONE);
  m_Fields.push_back(mF);

}



bool MetaBlob::
M_Read(void)
{
  if(META_DEBUG) std::cout << "MetaBlob: M_Read: Loading Header" << std::endl;

  if(!MetaObject::M_Read())
  {
    std::cout << "MetaBlob: M_Read: Error parsing file" << std::endl;
    return false;
  }

  if(META_DEBUG) std::cout << "MetaBlob: M_Read: Parsing Header" << std::endl;
 
  MET_FieldRecordType * mF;
 
  mF = MET_GetFieldRecord("NPoints", &m_Fields);
  if(mF->defined)
  {
    m_NPoints= (int)mF->value[0];
  }

  mF = MET_GetFieldRecord("ElementType", &m_Fields);
  if(mF->defined)
  {
    MET_StringToType((char *)(mF->value), &m_ElementType);
  }


  mF = MET_GetFieldRecord("PointDim", &m_Fields);
  if(mF->defined)
  {
    strcpy(m_PointDim,(char *)(mF->value));
  }

  int* posDim= new int[m_NDims];
  for(int i= 0; i < m_NDims; i++)
  {
    posDim[i] = -1;
  }

  int pntDim;
  char** pntVal = NULL;
  MET_StringToWordArray(m_PointDim, &pntDim, &pntVal); 
 
    
  int j;
  for(j = 0; j < pntDim; j++) 
  {
    if(!strcmp(pntVal[j], "x") || !strcmp(pntVal[j], "X"))
    {
      posDim[0] = j;
    }
    if(!strcmp(pntVal[j], "y") || !strcmp(pntVal[j], "Y"))
    {
    posDim[1] = j;
    }
    if(!strcmp(pntVal[j], "z") || !strcmp(pntVal[j], "Z"))
    {
     posDim[2] = j;
    }

  }

  float v[16];
  BlobPnt* pnt;
  
  if(m_BinaryData)
  {
    int elementSize;
    MET_SizeOfType(m_ElementType, &elementSize);
    int readSize = m_NPoints*(m_NDims+4)*elementSize;
    
    char* _data = new char[readSize];
    m_ReadStream->read((char *)_data, readSize);

    int gc = m_ReadStream->gcount();
    if(gc != readSize)
    {
      std::cout << "MetaBlob: m_Read: data not read completely" 
                << std::endl;
      std::cout << "   ideal = " << readSize << " : actual = " << gc << std::endl;
      return false;
    }

    int i=0;
    int d;
    double td;
    for(j=0; j<m_NPoints; j++) 
    {
      pnt = new BlobPnt(m_NDims);
      float* x = new float[m_NDims];
      for(d=0; d<m_NDims; d++)
      {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        x[d] = (float)td;
      }

      pnt->m_X = x;   

      for(d=0; d<4; d++)
      {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_Color[d] = (float)td;
      }

      m_PointList.push_back(pnt);
    }
  }
  else
  {
    for(j=0; j<m_NPoints; j++) 
    {

      if(j%100000 == 0) {std::cout << "Reading " << j << std::endl;}

      pnt = new BlobPnt(m_NDims);

      for(int k=0; k<pntDim; k++)
      {
        *m_ReadStream >> v[k];
        m_ReadStream->get(); // char c =
      }

      float* x = new float[m_NDims];
      int d;
      for(d=0; d<m_NDims; d++)
      {
        x[d] = v[posDim[d]];
      }

      pnt->m_X = x; 

      for(d=0; d<4; d++)
      {
        pnt->m_Color[d] = v[d+m_NDims];
      }
     
      m_PointList.push_back(pnt);
    }

   
      
    char c = ' ';
    while( (c!='\n') && (!m_ReadStream->eof()))
    {
      c = m_ReadStream->get();// to avoid unrecognize charactere
    }
  }

  return true;
}


bool MetaBlob::
M_Write(void)
{

  if(!MetaObject::M_Write())
  {
    std::cout << "MetaBlob: M_Read: Error parsing file" << std::endl;
    return false;
  }

  /** Then copy all points */
  
  if(m_BinaryData)
  {
    PointListType::const_iterator it = m_PointList.begin();
    int elementSize;
    MET_SizeOfType(m_ElementType, &elementSize);

    char* data = new char[(m_NDims+4)*m_NPoints*elementSize];
    int i=0;
    int d;
    while(it != m_PointList.end())
    {
      for(d = 0; d < m_NDims; d++)
      {
        MET_DoubleToValue((double)(*it)->m_X[d],m_ElementType,data,i++);
      }

      for(d = 0; d < 4; d++)
      {
        MET_DoubleToValue((double)(*it)->m_Color[d],m_ElementType,data,i++);
      }
      it++;
    }
     
    m_WriteStream->write((char *)data,(m_NDims+4)*m_NPoints*elementSize);  
  }
  else
  {
    PointListType::const_iterator it = m_PointList.begin();
  
    int d;
    while(it != m_PointList.end())
    {
      for(d = 0; d < m_NDims; d++)
      {
        *m_WriteStream << (*it)->m_X[d] << " ";
      }

      for(d = 0; d < 4; d++)
      {
        *m_WriteStream << (*it)->m_Color[d] << " ";
      }

      *m_WriteStream << std::endl;
      it++;
    }
  }

  return true;

}
