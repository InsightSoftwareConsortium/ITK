#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <fstream>
#include <string>

#include <metaUtils.h>
#include <metaObject.h>
#include <metaLine.h>

//
// MetaLine Constructors
//
MetaLine::
MetaLine()
:MetaObject()
{
  if(META_DEBUG) std::cout << "MetaLine()" << std::endl;
  Clear();
}

//
MetaLine::
MetaLine(const char *_headerName)
:MetaObject(_headerName)
{
  if(META_DEBUG)  std::cout << "MetaLine()" << std::endl;
  Clear();
  Read(_headerName);
}

//
MetaLine::
MetaLine(const MetaLine *_line)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaLine()" << std::endl;
  Clear();
  CopyInfo(_line);
}



//
MetaLine::
MetaLine(unsigned int dim)
:MetaObject(dim)
{
  if(META_DEBUG) std::cout << "MetaLine()" << std::endl;
  Clear();
}

//
MetaLine::
~MetaLine()
{
  Clear();
  M_Destroy();
}

//
void MetaLine::
PrintInfo() const
{
  MetaObject::PrintInfo();
  std::cout << "PointDim = " << m_PointDim << std::endl;
  std::cout << "NPoints = " << m_NPoints << std::endl;
  char str[255];
  MET_TypeToString(m_ElementType, str);
  std::cout << "ElementType = " << str << std::endl;
}

void MetaLine::
CopyInfo(const MetaLine * _tube)
{
  MetaObject::CopyInfo(_tube);
}

    

void MetaLine::
PointDim(const char* pointDim)
{
  strcpy(m_PointDim,pointDim);
}
    
const char* MetaLine::
PointDim(void) const
{
  return m_PointDim;
}

void MetaLine::
NPoints(int npnt)
{
  m_NPoints = npnt;
}

int MetaLine::
NPoints(void) const
{
  return m_NPoints;
}

/** Clear tube information */
void MetaLine::
Clear(void)
{
  if(META_DEBUG) std::cout << "MetaLine: Clear" << std::endl;
  MetaObject::Clear();
  m_NPoints = 0;
    // Delete the list of pointers to tubes.
  PointListType::iterator it = m_PointList.begin();
  while(it != m_PointList.end())
  {
    LinePnt* pnt = *it;
    it++;
    delete pnt;
  }  
  m_PointList.clear();

  strcpy(m_PointDim, "x y z v1x v1y v1z");
  m_ElementType = MET_FLOAT;
}
        
/** Destroy tube information */
void MetaLine::
M_Destroy(void)
{
  MetaObject::M_Destroy();
}

/** Set Read fields */
void MetaLine::
M_SetupReadFields(void)
{
  if(META_DEBUG) std::cout << "MetaLine: M_SetupReadFields" << std::endl;

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

void MetaLine::
M_SetupWriteFields(void)
{
  strcpy(m_ObjectTypeName,"Line");
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

MET_ValueEnumType MetaLine::
ElementType(void) const
{
  return m_ElementType;
}

void MetaLine::
ElementType(MET_ValueEnumType _elementType)
{
  m_ElementType = _elementType;
}


bool MetaLine::
M_Read(void)
{
  if(META_DEBUG) std::cout << "MetaLine: M_Read: Loading Header" << std::endl;

  if(!MetaObject::M_Read())
  {
    std::cout << "MetaLine: M_Read: Error parsing file" << std::endl;
    return false;
  }

  if(META_DEBUG) std::cout << "MetaLine: M_Read: Parsing Header" << std::endl;
 
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

  int pntDim;
  char** pntVal = NULL;
  MET_StringToWordArray(m_PointDim, &pntDim, &pntVal);
  
  delete [] pntVal;

  float v[16];

  if(m_BinaryData)
  {
    int elementSize;
    MET_SizeOfType(m_ElementType, &elementSize);
    int readSize = m_NPoints*(m_NDims*m_NDims+4)*elementSize;

    char* _data = new char[readSize];
    m_ReadStream->read((char *)_data, readSize);

    int gc = m_ReadStream->gcount();
    if(gc != readSize)
    {
      std::cout << "MetaLine: m_Read: data not read completely" 
                << std::endl;
      std::cout << "   ideal = " << readSize << " : actual = " << gc << std::endl;
      return false;
    }

    int i=0;
    double td;
    for(int j=0; j<m_NPoints; j++) 
    {
      LinePnt* pnt = new LinePnt(m_NDims);
      
      for(int d=0; d<m_NDims; d++)
      {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_X[d] = (float)td;
      }

      for(int l=0;l<m_NDims-1;l++)
      {

        for(int d=0; d<m_NDims; d++)
        {
          MET_ValueToDouble(m_ElementType, _data, i++, &td);
          pnt->m_V[l][d] = (float)td;
        }
        //pnt.m_V[l] = n; 
      }
      m_PointList.push_back(pnt);
    }
    delete [] _data;
  }
  else
  {
    for(int j=0; j<m_NPoints; j++) 
    {
      LinePnt* pnt = new LinePnt(m_NDims);
      
      int k;
      int d;
      for(k=0; k<m_NDims; k++)
      {
        *m_ReadStream >> v[k];
        m_ReadStream->get();
      }

      //float* x = new float[m_NDims];
      for(d=0; d<m_NDims; d++)
      {
        pnt->m_X[d] = v[d];
      }

      //pnt.m_X = x;

      for(k=0; k<m_NDims-1; k++)
      {
        for(int j=0; j<m_NDims; j++)
        {
          *m_ReadStream >> v[j];
          m_ReadStream->get();
        }

        //float* n = new float[m_NDims];
        for(d=0; d<m_NDims; d++)
        {
          pnt->m_V[k][d] = v[d];
        }
        //pnt.m_V[k] = n;
      }
      for(k=0; k<4; k++)
      {
        *m_ReadStream >> v[k];
        m_ReadStream->get();
        pnt->m_Color[k] = v[k];
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


bool MetaLine::
M_Write(void)
{

  if(!MetaObject::M_Write())
  {
    std::cout << "MetaLine: M_Read: Error parsing file" << std::endl;
    return false;
  }

  /** Then copy all points */
  
  if(m_BinaryData)
  {
    PointListType::const_iterator it = m_PointList.begin();
    int elementSize;
    MET_SizeOfType(m_ElementType, &elementSize);

    char* data = new char[(m_NDims*m_NDims+4)*m_NPoints*elementSize];
    int i=0;
    int d;
    while(it != m_PointList.end())
    {
      for(d = 0; d < m_NDims; d++)
      {
        MET_DoubleToValue((double)(*it)->m_X[d],m_ElementType,data,i++);  
      }

      for(int j=0;j<m_NDims-1;j++)
      {
        for(d=0; d<m_NDims; d++)
        {
          MET_DoubleToValue((double)(*it)->m_V[j][d],m_ElementType,data,i++);
        }
      }

      for(d=0; d<4; d++)
      {
        MET_DoubleToValue((double)(*it)->m_Color[d],m_ElementType,data,i++);
      }

      it++;
    }

    m_WriteStream->write((char *)data,(m_NDims*m_NDims+4)*m_NPoints*elementSize); 
    delete [] data;
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

      for(d = 0; d < m_NDims-1; d++)
      {
        for(int i = 0; i < m_NDims; i++)
        {
          *m_WriteStream << (*it)->m_V[d][i] << " ";
        }
      }

      for(d=0;d<4;d++)
      {
        *m_WriteStream << (*it)->m_Color[d] << " ";
      }

      *m_WriteStream << std::endl;
      it++;
    }
  }

  return true;

}
