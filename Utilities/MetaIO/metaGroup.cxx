#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <fstream>
#include <string>

#include <metaUtils.h>
#include <metaObject.h>
#include <metaGroup.h>

//
// MedImage Constructors
//
MetaGroup::
MetaGroup()
:MetaObject()
{
  if(META_DEBUG) std::cout << "MetaGroup()" << std::endl;
  Clear();

}

//
MetaGroup::
MetaGroup(const char *_headerName)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaGroup()" << std::endl;
  Clear();
  Read(_headerName);
}

//
MetaGroup::
MetaGroup(const MetaGroup *_group)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaGroup()" << std::endl;
  Clear();
  CopyInfo(_group);
}

MetaGroup::
MetaGroup(unsigned int dim)
:MetaObject(dim)
{
  if(META_DEBUG) std::cout << "MetaGroup()" << std::endl;
  Clear();
}

//
MetaGroup::
~MetaGroup()
{
  M_Destroy();
}

//
void MetaGroup::
PrintInfo() const
{
  MetaObject::PrintInfo();
}

void MetaGroup::
CopyInfo(const MetaGroup * _group)
{
  MetaObject::CopyInfo(_group);
}

/** Clear group information */
void MetaGroup::
Clear(void)
{
  if(META_DEBUG) std::cout << "MetaGroup: Clear" << std::endl;
  MetaObject::Clear();
}
        
/** Destroy group information */
void MetaGroup::
M_Destroy(void)
{
  MetaObject::M_Destroy();
}

/** Set Read fields */
void MetaGroup::
M_SetupReadFields(void)
{
  if(META_DEBUG) std::cout << "MetaGroup: M_SetupReadFields" << std::endl;

  MetaObject::M_SetupReadFields();

  MET_FieldRecordType * mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "EndGroup", MET_NONE, true);
  mF->terminateRead = true;
  m_Fields.push_back(mF);

  mF = MET_GetFieldRecord("ElementSpacing", &m_Fields);
  mF->required = false;
}

void MetaGroup::
M_SetupWriteFields(void)
{
  strcpy(m_ObjectTypeName,"Group");
  MetaObject::M_SetupWriteFields();

  MET_FieldRecordType * mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "EndGroup", MET_NONE);
  m_Fields.push_back(mF);
}


bool MetaGroup::
M_Read(void)
{
  if(META_DEBUG) 
    {
    std::cout << "MetaGroup: M_Read: Loading Header" << std::endl;
    }
  
  if(!MetaObject::M_Read())
    {
    std::cout << "MetaGroup: M_Read: Error parsing file" << std::endl;
    return false;
    }

  if(META_DEBUG)
    {
    std::cout << "MetaGroup: M_Read: Parsing Header" << std::endl;
    }
 
  return true;
}

