#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <fstream>
#include <string>

#include <metaUtils.h>
#include <metaObject.h>
#include <metaArrow.h>

//
// Constructors
//
MetaArrow::
MetaArrow()
:MetaObject()
{
  if(META_DEBUG) std::cout << "MetaArrow()" << std::endl;
  Clear();
}

//
MetaArrow::
MetaArrow(const char *_headerName)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaArrow()" << std::endl;
  Clear();
  Read(_headerName);
}

//
MetaArrow::
MetaArrow(const MetaArrow *_Arrow)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaArrow()" << std::endl;
  Clear();
  CopyInfo(_Arrow);
}

MetaArrow::
MetaArrow(unsigned int dim)
:MetaObject(dim)
{
  if(META_DEBUG) std::cout << "MetaArrow()" << std::endl;
  Clear();
}

//
MetaArrow::
~MetaArrow()
{
  M_Destroy();
}

//
void MetaArrow::
PrintInfo() const
{
  MetaObject::PrintInfo();
  std::cout << "Lenght = " << m_Lenght << std::endl;
}

void MetaArrow::
CopyInfo(const MetaArrow * _Arrow)
{
  MetaObject::CopyInfo(_Arrow);
  m_Lenght = _Arrow->Lenght();
}


void  MetaArrow::
Lenght(float lenght)
{
  m_Lenght = lenght;
}

float  MetaArrow::
Lenght(void) const 
{
  return m_Lenght;
}
  
/** Clear Arrow information */
void MetaArrow::
Clear(void)
{
  if(META_DEBUG) std::cout << "MetaArrow: Clear" << std::endl;
  MetaObject::Clear();
  m_Lenght = 1;
}
        
/** Destroy Arrow information */
void MetaArrow::
M_Destroy(void)
{
  MetaObject::M_Destroy();
}

/** Set Read fields */
void MetaArrow::
M_SetupReadFields(void)
{
  if(META_DEBUG) std::cout << "MetaArrow: M_SetupReadFields" << std::endl;

  MetaObject::M_SetupReadFields();

  MET_FieldRecordType * mF;

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Lenght", MET_FLOAT, true);
  mF->terminateRead = true;
  m_Fields.push_back(mF);
}

void MetaArrow::
M_SetupWriteFields(void)
{
  strcpy(m_ObjectTypeName,"Arrow");
  MetaObject::M_SetupWriteFields();

  MET_FieldRecordType * mF;

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "Lenght", MET_FLOAT, m_Lenght);
  m_Fields.push_back(mF);
}


bool MetaArrow::
M_Read(void)
{
  if(META_DEBUG) std::cout << "MetaArrow: M_Read: Loading Header" << std::endl;
  
  if(!MetaObject::M_Read())
  {
    std::cout << "MetaArrow: M_Read: Error parsing file" << std::endl;
    return false;
  }

  if(META_DEBUG) std::cout << "MetaArrow: M_Read: Parsing Header" << std::endl;
 
  MET_FieldRecordType * mF;
 
  mF = MET_GetFieldRecord("Lenght", &m_Fields);
  if(mF->defined)
    {
    m_Lenght= (float)mF->value[0];
    }

  return true;
}

