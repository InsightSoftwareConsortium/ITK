#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <fstream>
#include <string>

#include <metaUtils.h>
#include <metaObject.h>
#include <metaCoef.h>

//
// MetaCoef Constructors
//
MetaCoef::
MetaCoef()
:MetaObject()
{
  if(META_DEBUG) std::cout << "MetaCoef()" << std::endl;
  Clear();
}

//
MetaCoef::
MetaCoef(const char *_headerName)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaCoef()" << std::endl;
  Clear();
  Read(_headerName);
}

//
MetaCoef::
MetaCoef(const MetaCoef *_coef)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaCoef()" << std::endl;
  Clear();
  CopyInfo(_coef);
}



//
MetaCoef::
MetaCoef(unsigned int dim)
:MetaObject(dim)
{
  if(META_DEBUG) std::cout << "MetaCoef()" << std::endl;
  Clear();
}

//
MetaCoef::
~MetaCoef()
{
  Clear();
  
  M_Destroy();
}

//
void MetaCoef::
PrintInfo() const
{
  MetaObject::PrintInfo();
  std::cout << "PointDim = " << m_PointDim << std::endl;
  std::cout << "NPoints = " << m_NPoints << std::endl;
  char str[255];
  MET_TypeToString(m_ElementType, str);
  std::cout << "ElementType = " << str << std::endl;
}

void MetaCoef::
CopyInfo(const MetaCoef * _coef)
{
  MetaObject::CopyInfo(_coef);
}


void MetaCoef::
PointDim(const char* pointDim)
{
  strcpy(m_PointDim,pointDim);
}
    
const char* MetaCoef::
PointDim(void) const
{
  return m_PointDim;
}

void MetaCoef::
NPoints(int npnt)
{
  m_NPoints = npnt;
}

int MetaCoef::
NPoints(void) const
{
  return m_NPoints;
}

/** Clear Coef information */
void MetaCoef::
Clear(void)
{
  if(META_DEBUG) std::cout << "MetaCoef: Clear" << std::endl;
  MetaObject::Clear();
  m_NPoints = 0;
  // Delete the list of pointers to tubes.
  PointListType::iterator it = m_PointList.begin();
  while(it != m_PointList.end())
  {
    CoefPoint* pnt = *it;
    it++;
    delete pnt;
  }
  m_PointList.clear();
  strcpy(m_PointDim, "x y z ...");
  m_ElementType = MET_DOUBLE;
}

/** Destroy Coef information */
void MetaCoef::
M_Destroy(void)
{
  MetaObject::M_Destroy();
}

/** Set Read fields */
void MetaCoef::
M_SetupReadFields(void)
{
  if(META_DEBUG) std::cout << "MetaCoef: M_SetupReadFields" << std::endl;

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

void MetaCoef::
M_SetupWriteFields(void)
{
  if(META_DEBUG) std::cout << "MetaCoef: M_SetupWriteFields" << std::endl;

  strcpy(m_ObjectTypeName,"Coef");
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

MET_ValueEnumType MetaCoef::
ElementType(void) const
{
  return m_ElementType;
}

void MetaCoef::
ElementType(MET_ValueEnumType _elementType)
{
  m_ElementType = _elementType;
}

bool MetaCoef::
M_Read(void)
{
  if(META_DEBUG) std::cout << "MetaCoef: M_Read: Loading Header" << std::endl;

  if(!MetaObject::M_Read())
  {
    std::cout << "MetaCoef: M_Read: Error parsing file" << std::endl;
    return false;
  }

  if(META_DEBUG) std::cout << "MetaCoef: M_Read: Parsing Header" << std::endl;
 
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

  int i;
  for(i=0;i<pntDim;i++)
    {
      delete [] pntVal[i];
    }
  delete [] pntVal;


  float v[16];

  if(m_BinaryData)
  {
    int elementSize;
    MET_SizeOfType(m_ElementType, &elementSize);
    int readSize = m_NPoints*m_NDims*elementSize;

    char* _data = new char[readSize];
    m_ReadStream->read((char *)_data, readSize);

    int gc = m_ReadStream->gcount();
    if(gc != readSize)
    {
      std::cout << "MetaCoef: m_Read: data not read completely" 
                << std::endl;
      std::cout << "   ideal = " << readSize << " : actual = " << gc << std::endl;
      return false;
    }

    int i=0;
    int d;
    double td;
    for(int j=0; j<m_NPoints; j++) 
    {
      CoefPoint* pnt = new CoefPoint(m_NDims);
     
      for(d=0; d<m_NDims; d++)
      {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_X[d] = (float)td;
      }


      m_PointList.push_back(pnt);
    }
    delete [] _data;
  }
  else
  {
    for(int j=0; j<m_NPoints; j++) 
    {
      CoefPoint* pnt = new CoefPoint(m_NDims);

      for(int k=0; k<pntDim; k++)
      {
        *m_ReadStream >> v[k];
        m_ReadStream->get(); // char c =
      }

      int d;
      for(d=0; d<m_NDims; d++)
      {
        pnt->m_X[d] = v[d];
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


bool MetaCoef::
M_Write(void)
{

  if(META_DEBUG) std::cout << "MetaCoef: M_Write" << std::endl;

  if(!MetaObject::M_Write())
  {
    std::cout << "MetaCoef: M_Write: Error parsing file" << std::endl;
    return false;
  }

  /** Then copy all points */
  
  if(m_BinaryData)
  {
    PointListType::const_iterator it = m_PointList.begin();
    int elementSize;
    MET_SizeOfType(m_ElementType, &elementSize);

    char* data = new char[m_NDims*m_NPoints*elementSize];
    int i=0;
    int d;
    while(it != m_PointList.end())
    {
      for(d = 0; d < m_NDims; d++)
      {
        MET_DoubleToValue((double)(*it)->m_X[d],m_ElementType,data,i++);
      }

      it++;
    }
   
    m_WriteStream->write((char *)data,m_NDims*m_NPoints*elementSize);
    m_WriteStream->write("\n",1);
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

      *m_WriteStream << std::endl;
      it++;
    }
  }

  return true;

}


