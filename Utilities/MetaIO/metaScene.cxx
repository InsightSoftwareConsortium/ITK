#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <fstream>
#include <string>

#include <metaUtils.h>
#include <metaObject.h>
#include <metaScene.h>
#include <metaTube.h>
#include <metaEllipse.h>
#include <metaImage.h>
#include <metaBlob.h>
#include <metaLine.h>
#include <metaGroup.h>
#include <metaSurface.h>
//
// MetaScene Constructors
//
MetaScene::
MetaScene()
:MetaObject()
{
  if(META_DEBUG) std::cout << "MetaScene()" << std::endl;
  Clear();
}


//
MetaScene::
MetaScene(const MetaScene *_scene)
:MetaObject()
{
  if(META_DEBUG) std::cout << "MetaScene()" << std::endl;
  Clear();
  CopyInfo(_scene);
}

//
MetaScene::
MetaScene(unsigned int dim)
:MetaObject(dim)
{
  if(META_DEBUG) std::cout << "MetaScene()" << std::endl;
  Clear();
}


/** Destructor */
MetaScene::
~MetaScene()
{
  Clear();
  M_Destroy();
}

//
void MetaScene::
PrintInfo() const
{
  MetaObject::PrintInfo();
  std::cout << "Number of Objects = " << m_NObjects << std::endl;
}

void MetaScene::
CopyInfo(const MetaScene * _tube)
{
  MetaObject::CopyInfo(_tube);
}


void MetaScene::
NObjects(int nobjects)
{
  m_NObjects = nobjects;
}
 
int MetaScene:: 
NObjects(void) const
{
  return m_NObjects;
}

void MetaScene:: 
AddObject(MetaObject* object)
{
  m_ObjectList.push_back(object);
}

bool MetaScene::
Read(const char *_headerName)
{
  if(META_DEBUG) std::cout << "MetaScene: Read" << std::endl;

  M_Destroy();

  Clear();

  M_SetupReadFields();

  if(_headerName != NULL)
  {
    strcpy(m_FileName, _headerName);
  }

  if(META_DEBUG) std::cout << "MetaScene: Read: Opening stream" << std::endl;
 
  if(!m_ReadStream)
  {
    m_ReadStream = new std::ifstream;
  }
  
  m_ReadStream->open(m_FileName, std::ios::binary | std::ios::in);
  
  if(!m_ReadStream->is_open())
  {
    std::cout << "MetaScene: Read: Cannot open file" << std::endl;
    return false;
  }

  if(!M_Read())
  {
    std::cout << "MetaScene: Read: Cannot parse file" << std::endl;
    m_ReadStream->close();
    return false;
  }

  if(_headerName != NULL)
  {
    strcpy(m_FileName, _headerName);
  }

  /** Objects should be added here */
  for(int i=0;i<m_NObjects;i++)
  {
    
    if(!strncmp(MET_ReadType(*m_ReadStream),"Tube",4))
    {
      MetaTube* tube = new MetaTube();
      tube->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(tube);
    }

    else if(!strncmp(MET_ReadType(*m_ReadStream),"Ellipse",7))
    {
      MetaEllipse* ellipse = new MetaEllipse();
      ellipse->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(ellipse);
    }
    
    else if(!strncmp(MET_ReadType(*m_ReadStream),"Image",5))
    {
      MetaImage* image = new MetaImage();
      image->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(image);
    }
    
    else if(!strncmp(MET_ReadType(*m_ReadStream),"Blob",5))
    {
      MetaBlob* blob = new MetaBlob();
      blob->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(blob);
    }
      
    else if(!strncmp(MET_ReadType(*m_ReadStream),"Surface",5))
    {
      MetaSurface* surface = new MetaSurface();
      surface->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(surface);
    }
     
    else if(!strncmp(MET_ReadType(*m_ReadStream),"Line",5))
    {
      MetaLine* line = new MetaLine();
      line->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(line);
    }

    else if(!strncmp(MET_ReadType(*m_ReadStream),"Group",5))
    {
      MetaGroup* group = new MetaGroup();
      group->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(group);
    }
  }

  m_ReadStream->close();

  return true;
}


//
//
//
bool MetaScene::
Write(const char *_headName)
{
  if(META_DEBUG) std::cout << "MetaScene: Write" << std::endl;

  if(_headName != NULL)
  {
    FileName(_headName);
  }

  // Set the number of objects based on the net list
  //ObjectListType::const_iterator itNet = m_ObjectList.begin();
  m_NObjects = m_ObjectList.size();

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

  m_WriteStream->close();

  /** Then we write all the objects in the scene */
  ObjectListType::const_iterator it = m_ObjectList.begin();
  while(it != m_ObjectList.end())
  {
    (*it)->Append(_headName);
    it++;
  }

  return true;
}
  
/** Clear tube information */
void MetaScene::
Clear(void)
{
  if(META_DEBUG) std::cout << "MetaScene: Clear" << std::endl;
  MetaObject::Clear();
  // Delete the list of pointers to objects in the scene.
  ObjectListType::iterator it = m_ObjectList.begin();
  while(it != m_ObjectList.end())
  {
    MetaObject* object = *it;
    it++;
    delete object;
  }

  m_ObjectList.clear();

}
        
/** Destroy tube information */
void MetaScene::
M_Destroy(void)
{
  MetaObject::M_Destroy();
}

/** Set Read fields */
void MetaScene::
M_SetupReadFields(void)
{
  if(META_DEBUG) std::cout << "MetaScene: M_SetupReadFields" << std::endl;

  MetaObject::M_SetupReadFields();

  MET_FieldRecordType * mF;

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "NObjects", MET_INT, false);
  mF->required = true;
  mF->terminateRead = true;
  m_Fields.push_back(mF);

  mF = MET_GetFieldRecord("ElementSpacing", &m_Fields);
  mF->required = false;
}

void MetaScene::
M_SetupWriteFields(void)
{
  strcpy(m_ObjectTypeName,"Scene");
  MetaObject::M_SetupWriteFields();

  MET_FieldRecordType * mF;

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "NObjects", MET_INT, m_NObjects);
  m_Fields.push_back(mF);
}



bool MetaScene::
M_Read(void)
{
  if(META_DEBUG) std::cout << "MetaScene: M_Read: Loading Header" << std::endl;
  if(!MetaObject::M_Read())
  {
    std::cout << "MetaScene: M_Read: Error parsing file" << std::endl;
    return false;
  }

  if(META_DEBUG) std::cout << "MetaScene: M_Read: Parsing Header" << std::endl;
 
  MET_FieldRecordType * mF;

  mF = MET_GetFieldRecord("NObjects", &m_Fields);
  if(mF->defined)
  {
    m_NObjects= (int)mF->value[0];
  }

  return true;
}

bool MetaScene::
M_Write(void)
{
  if(!MetaObject::M_Write())
  {
    std::cout << "MetaScene: M_Write: Error parsing file" << std::endl;
    return false;
  }

  return true;
}
