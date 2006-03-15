#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <fstream>
#include <string>

#include "metaUtils.h"
#include "metaObject.h"
#include "metaGaussian.h"

//
// MedImage Constructors
//
MetaGaussian::
MetaGaussian()
:MetaObject( )
{
  if(META_DEBUG) std::cout << "MetaGaussian()" << std::endl;
  Clear();

}

//
MetaGaussian::
MetaGaussian(const char *_headerName)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaGaussian()" << std::endl;
  Clear();
  Read(_headerName);
}

//
MetaGaussian::
MetaGaussian(const MetaGaussian *_gaussian)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaGaussian()" << std::endl;
  Clear();
  CopyInfo(_gaussian);
}

MetaGaussian::
MetaGaussian(unsigned int dim)
:MetaObject(dim)
{
  if(META_DEBUG) std::cout << "MetaGaussian()" << std::endl;
  Clear();
}

//
MetaGaussian::
~MetaGaussian()
{
  M_Destroy();
}

//
void MetaGaussian::
PrintInfo() const
{
  MetaObject::PrintInfo();
  std::cout << "\n"
            << "Maximum = " << m_Maximum << "\n"
            << "Radius = " << m_Radius
            << std::endl;
}

void MetaGaussian::
CopyInfo(const MetaGaussian * _gaussian)
{
  MetaObject::CopyInfo(_gaussian);
}
  
/** Clear gaussian information */
void MetaGaussian::
Clear(void)
{
  if(META_DEBUG) std::cout << "MetaGaussian: Clear" << std::endl;
  MetaObject::Clear();
  m_Maximum = 1;
  m_Radius = 1;
}
        
/** Destroy gaussian information */
void MetaGaussian::
M_Destroy(void)
{
  MetaObject::M_Destroy();
}

/** Set Read fields */
void MetaGaussian::
M_SetupReadFields(void)
{
  if(META_DEBUG) std::cout << "MetaGaussian: M_SetupReadFields" << std::endl;

  MetaObject::M_SetupReadFields();

  MET_FieldRecordType * mF;

  MET_GetFieldRecordNumber("NDims", &m_Fields);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Maximum", MET_FLOAT, true);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Radius", MET_FLOAT, true);
  m_Fields.push_back(mF);

}

void MetaGaussian::
M_SetupWriteFields(void)
{
  strcpy(m_ObjectTypeName,"Gaussian");
  MetaObject::M_SetupWriteFields();

  MET_FieldRecordType * mF;

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "Maximum", MET_FLOAT, m_Maximum);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "Radius", MET_FLOAT,
                     m_Radius);
  m_Fields.push_back(mF);

}


bool MetaGaussian::
M_Read(void)
{
  if(META_DEBUG) std::cout << "MetaGaussian: M_Read: Loading Header"
                           << std::endl;

  if(!MetaObject::M_Read())
  {
    std::cout << "MetaGaussian: M_Read: Error parsing file" << std::endl;
    return false;
  }

  if(META_DEBUG) std::cout << "MetaGaussian: M_Read: Parsing Header"
                           << std::endl;

  MET_FieldRecordType * mF;

  mF = MET_GetFieldRecord("Maximum", &m_Fields);
  if( mF->defined )
  {
    m_Maximum = (float)mF->value[0];
  }

  mF = MET_GetFieldRecord("Radius", &m_Fields);
  if( mF->defined )
  {
    m_Radius = (float)mF->value[0];
  }

  return true;
}

