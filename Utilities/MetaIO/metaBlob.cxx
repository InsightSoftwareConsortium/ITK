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
  Clear();
}

//
MetaBlob::
MetaBlob(const char *_headerName)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaBlob()" << std::endl;
  m_NPoints = 0;
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
  Clear();
}

//
MetaBlob::
~MetaBlob()
{
  Clear();
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


/** Clear tube information */
void MetaBlob::
Clear(void)
{
  if(META_DEBUG) std::cout << "MetaBlob: Clear" << std::endl;
  MetaObject::Clear();
  if(META_DEBUG) std::cout << "MetaBlob: Clear: m_NPoints" << std::endl;
  // Delete the list of pointers to tubes.
  PointListType::iterator it = m_PointList.begin();
  while(it != m_PointList.end())
  {
    BlobPnt* pnt = *it;
    it++;
    delete pnt;
  }  
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

  m_NPoints = m_PointList.size();
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

  delete [] pntVal;

  float v[16];
  
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
      BlobPnt* pnt = new BlobPnt(m_NDims);

      for(d=0; d<m_NDims; d++)
      {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_X[d] = (float)td;
      }

      for(d=0; d<4; d++)
      {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_Color[d] = (float)td;
      }

      m_PointList.push_back(pnt);
    }
    delete [] _data;
  }
  else
  {
    for(j=0; j<m_NPoints; j++) 
    {

      if(j%100000 == 0) {std::cout << "Reading " << j << std::endl;}

      BlobPnt* pnt = new BlobPnt(m_NDims);
      
      for(int k=0; k<pntDim; k++)
      {
        *m_ReadStream >> v[k];
        m_ReadStream->get(); 
      }

      int d;
      for(d=0; d<m_NDims; d++)
      {
        pnt->m_X[d] = v[posDim[d]];
      }

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
  
  delete [] posDim;
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
    delete data;
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
