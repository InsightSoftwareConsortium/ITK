#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <fstream>
#include <string>

#include <metaUtils.h>
#include <metaObject.h>
#include <metaEllipse.h>

//
// MedImage Constructors
//
MetaEllipse::
MetaEllipse()
:MetaObject()
{
  if(META_DEBUG) std::cout << "MetaEllipse()" << std::endl;
  Clear();

}

//
MetaEllipse::
MetaEllipse(const char *_headerName)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaEllipse()" << std::endl;
  Clear();
  Read(_headerName);
}

//
MetaEllipse::
MetaEllipse(const MetaEllipse *_ellipse)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaEllipse()" << std::endl;
  Clear();
  CopyInfo(_ellipse);
}

MetaEllipse::
MetaEllipse(unsigned int dim)
:MetaObject(dim)
{
  if(META_DEBUG) std::cout << "MetaEllipse()" << std::endl;
  Clear();
}

//
MetaEllipse::
~MetaEllipse()
{
  M_Destroy();
}

//
void MetaEllipse::
PrintInfo() const
{
  MetaObject::PrintInfo();
  std::cout << "Radius = ";
  for(int i=0;i<m_NDims;i++)
  {
    std::cout << m_Radius[i] << " ";
  }
  std::cout << std::endl;
}

void MetaEllipse::
CopyInfo(const MetaEllipse * _ellipse)
{
  MetaObject::CopyInfo(_ellipse);
}

void  MetaEllipse::
Radius(const float* radius)
{
  for(int i=0; i<m_NDims; i++)
  {
    m_Radius[i] = radius[i];
  }
}


void  MetaEllipse::
Radius(float radius)
{
  for(int i=0;i<m_NDims;i++)
  {
    m_Radius[i] = radius;
  }
}

void  MetaEllipse::
Radius(float r1,float r2)
{
  m_Radius[0] = r1;
  m_Radius[1] = r2;
}

void  MetaEllipse::
Radius(float r1,float r2, float r3)
{
  m_Radius[0] = r1;
  m_Radius[1] = r2;
  m_Radius[2] = r3;
}

const float*  MetaEllipse::
Radius(void)
{
  return m_Radius;
}
  
/** Clear ellipse information */
void MetaEllipse::
Clear(void)
{
  if(META_DEBUG) std::cout << "MetaEllipse: Clear" << std::endl;
  MetaObject::Clear();
  memset(m_Radius, 0, 100*sizeof(float));
  
  for(int i=0; i<m_NDims; i++)
  {
    m_Radius[i] = 1;
  }
}
        
/** Destroy ellipse information */
void MetaEllipse::
M_Destroy(void)
{
  MetaObject::M_Destroy();
}

/** Set Read fields */
void MetaEllipse::
M_SetupReadFields(void)
{
  if(META_DEBUG) std::cout << "MetaEllipse: M_SetupReadFields" << std::endl;

  MetaObject::M_SetupReadFields();

  MET_FieldRecordType * mF;

  int nDimsRecNum = MET_GetFieldRecordNumber("NDims", &m_Fields);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Radius", MET_FLOAT_ARRAY, true,nDimsRecNum);
  mF->terminateRead = true;
  m_Fields.push_back(mF);

}

void MetaEllipse::
M_SetupWriteFields(void)
{
  strcpy(m_ObjectTypeName,"Ellipse");
  MetaObject::M_SetupWriteFields();

  MET_FieldRecordType * mF;

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "Radius", MET_FLOAT_ARRAY, m_NDims,m_Radius);
  m_Fields.push_back(mF);
}


bool MetaEllipse::
M_Read(void)
{
  if(META_DEBUG) std::cout << "MetaEllipse: M_Read: Loading Header" << std::endl;
  
  if(!MetaObject::M_Read())
  {
    std::cout << "MetaEllipse: M_Read: Error parsing file" << std::endl;
    return false;
  }

  if(META_DEBUG) std::cout << "MetaEllipse: M_Read: Parsing Header" << std::endl;
 
  MET_FieldRecordType * mF;
 
  mF = MET_GetFieldRecord("Radius", &m_Fields);
  if(mF->defined)
  {
    for(int i=0;i<m_NDims;i++)
    {
      m_Radius[i]= (float)mF->value[i];
    }
  }

  return true;
}

