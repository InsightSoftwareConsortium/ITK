#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <fstream>
#include <string>

#include <metaUtils.h>
#include <metaObject.h>
#include <metaScene.h>
#include <metaTube.h>
#include <metaDTITube.h>
#include <metaVesselTube.h>
#include <metaEllipse.h>
#include <metaGaussian.h>
#include <metaImage.h>
#include <metaBlob.h>
#include <metaLandmark.h>
#include <metaLine.h>
#include <metaGroup.h>
#include <metaSurface.h>
#include <metaLandmark.h>
#include <metaMesh.h>
#include <metaArrow.h>
#include <metaTransform.h>

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

  int i = 0;
  char suf[80];
  suf[0] = '\0';
  if(MET_GetFileSuffixPtr(_headerName, &i))
    {
    strcpy(suf, &_headerName[i]);
    }

  M_Destroy();

  Clear();

  M_SetupReadFields();

  if(_headerName != NULL)
  {
    strcpy(m_FileName, _headerName);
  }

  if(META_DEBUG) std::cout << "MetaScene: Read: Opening stream" << std::endl;
 
  M_PrepareNewReadStream();
  
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

  if(m_Event)
   {
   m_Event->StartReading(m_NObjects);
   }

  /** Objects should be added here */
  for(i=0;i<m_NObjects;i++)
  {
    if(META_DEBUG) std::cout << MET_ReadType(*m_ReadStream) << std::endl;

    if(m_Event)
      {
      m_Event->SetCurrentIteration(i+1);
      }

    if(!strncmp(MET_ReadType(*m_ReadStream),"Tube",4) || 
       (MET_ReadType(*m_ReadStream) == NULL && !strcmp(suf, "tre")))
      {
      char* subtype = MET_ReadSubType(*m_ReadStream);
      if(!strncmp(subtype,"Vessel",6))
        {
        MetaVesselTube* vesseltube = new MetaVesselTube();
        vesseltube->SetEvent(m_Event);
        vesseltube->ReadStream(m_NDims,m_ReadStream);
        m_ObjectList.push_back(vesseltube);
        }
      else if(!strncmp(subtype,"DTI",3))
        {
        MetaDTITube* dtitube = new MetaDTITube();
        dtitube->SetEvent(m_Event);
        dtitube->ReadStream(m_NDims,m_ReadStream);
        m_ObjectList.push_back(dtitube);
        }
      else
        {
        MetaTube* tube = new MetaTube();
        tube->SetEvent(m_Event);
        tube->ReadStream(m_NDims,m_ReadStream);
        m_ObjectList.push_back(tube);
        }
      delete subtype;
      }

    else if(!strncmp(MET_ReadType(*m_ReadStream),"Transform",9))
    {
      MetaTransform* transform = new MetaTransform();
      transform->SetEvent(m_Event);
      transform->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(transform);
    }

    else if(!strncmp(MET_ReadType(*m_ReadStream),"Ellipse",7) ||
            (MET_ReadType(*m_ReadStream) == NULL && !strcmp(suf, "elp")))
    {
      MetaEllipse* ellipse = new MetaEllipse();
      ellipse->SetEvent(m_Event);
      ellipse->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(ellipse);
    }

    else if(!strncmp(MET_ReadType(*m_ReadStream),"Arrow",5))
    {
      MetaArrow* arrow = new MetaArrow();
      arrow->SetEvent(m_Event);
      arrow->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(arrow);
    }

    else if(!strncmp(MET_ReadType(*m_ReadStream),"Gaussian",8) ||
            (MET_ReadType(*m_ReadStream) == NULL && !strcmp(suf, "gau")))
    {
      MetaGaussian* gaussian = new MetaGaussian();
      gaussian->SetEvent(m_Event);
      gaussian->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(gaussian);
    }
    
    else if(!strncmp(MET_ReadType(*m_ReadStream),"Image",5) ||
            (MET_ReadType(*m_ReadStream) == NULL && 
             (!strcmp(suf, "mhd") || !strcmp(suf, "mha"))))
    {
      MetaImage* image = new MetaImage();
      image->SetEvent(m_Event);
      image->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(image);
    }
    
    else if(!strncmp(MET_ReadType(*m_ReadStream),"Blob",4) ||
            (MET_ReadType(*m_ReadStream) == NULL && !strcmp(suf, "blb")))
    {
      MetaBlob* blob = new MetaBlob();
      blob->SetEvent(m_Event);
      blob->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(blob);
    }
      
    else if(!strncmp(MET_ReadType(*m_ReadStream),"Landmark",8) ||
            (MET_ReadType(*m_ReadStream) == NULL && !strcmp(suf, "ldm")))
    {
      MetaLandmark* landmark = new MetaLandmark();
      landmark->SetEvent(m_Event);
      landmark->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(landmark);
    }
      
    else if(!strncmp(MET_ReadType(*m_ReadStream),"Surface",5) ||
            (MET_ReadType(*m_ReadStream) == NULL && !strcmp(suf, "suf")))
    {
      MetaSurface* surface = new MetaSurface();
      surface->SetEvent(m_Event);
      surface->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(surface);
    }
     
    else if(!strncmp(MET_ReadType(*m_ReadStream),"Line",5) ||
            (MET_ReadType(*m_ReadStream) == NULL && !strcmp(suf, "lin")))
    {
      MetaLine* line = new MetaLine();
      line->SetEvent(m_Event);
      line->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(line);
    }

    else if(!strncmp(MET_ReadType(*m_ReadStream),"Group",5) ||
            (MET_ReadType(*m_ReadStream) == NULL && !strcmp(suf, "grp")))
    {
      MetaGroup* group = new MetaGroup();      
      group->SetEvent(m_Event);
      group->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(group);
    }

    else if(!strncmp(MET_ReadType(*m_ReadStream),"AffineTransform",15) ||
            (MET_ReadType(*m_ReadStream) == NULL && !strcmp(suf, "trn")))
    {
      MetaGroup* group = new MetaGroup();
      group->SetEvent(m_Event);
      group->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(group);
    }
    else if(!strncmp(MET_ReadType(*m_ReadStream),"Mesh",4) ||
            (MET_ReadType(*m_ReadStream) == NULL && !strcmp(suf, "msh")))
    {
      MetaMesh* mesh = new MetaMesh();
      mesh->SetEvent(m_Event);
      mesh->ReadStream(m_NDims,m_ReadStream);
      m_ObjectList.push_back(mesh);
    }
  }

  if(m_Event)
    {
     m_Event->StopReading();
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

#ifdef __sgi
  // Create the file. This is required on some older sgi's
  std::ofstream tFile(m_FileName,std::ios::out);
  tFile.close();                    
#endif

  m_WriteStream->open(m_FileName, std::ios::binary | std::ios::out);
  if(!m_WriteStream->is_open())
    {
    return false;
    delete m_WriteStream;
    m_WriteStream = 0;
    }

  M_Write();

  m_WriteStream->close();
  delete m_WriteStream;
  m_WriteStream = 0;

  /** Then we write all the objects in the scene */
  ObjectListType::iterator it = m_ObjectList.begin();
  while(it != m_ObjectList.end())
  {
    (*it)->BinaryData(this->BinaryData());
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
  this->ClearFields();

  MET_FieldRecordType * mF;

  if(strlen(m_Comment)>0)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "Comment", MET_STRING, strlen(m_Comment), m_Comment);
    m_Fields.push_back(mF);
    }

  strcpy(m_ObjectTypeName,"Scene");
  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "ObjectType", MET_STRING, strlen(m_ObjectTypeName),
                    m_ObjectTypeName);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "NDims", MET_INT, m_NDims);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "NObjects", MET_INT, m_NObjects);
  m_Fields.push_back(mF);
}



bool MetaScene::
M_Read(void)
{
  if(META_DEBUG) std::cout<<"MetaScene: M_Read: Loading Header"<<std::endl;
  if(strncmp(MET_ReadType(*m_ReadStream),"Scene",5))
    {
    m_NObjects = 1;
    return true;
    }

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
