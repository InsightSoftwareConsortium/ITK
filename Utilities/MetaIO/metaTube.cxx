#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <fstream>
#include <string>

#include <metaUtils.h>
#include <metaObject.h>
#include <metaTube.h>

//
// MedImage Constructors
//
MetaTube::
MetaTube()
:MetaObject()
{
  if(META_DEBUG) std::cout << "MetaTube()" << std::endl;
  Clear();

}

//
MetaTube::
MetaTube(const char *_headerName)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaTube()" << std::endl;
  Clear();
  Read(_headerName);
}

//
MetaTube::
MetaTube(const MetaTube *_tube)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaTube()" << std::endl;
  Clear();
  CopyInfo(_tube);
}



//
MetaTube::
MetaTube(unsigned int dim)
:MetaObject(dim)
{
  if(META_DEBUG) std::cout << "MetaTube()" << std::endl;
  Clear();
}

//
MetaTube::
~MetaTube()
{
  M_Destroy();
}

//
void MetaTube::
PrintInfo() const
{
  MetaObject::PrintInfo();
  std::cout << "ParentPoint = " << m_ParentPoint << std::endl;
  std::cout << "Root = " << m_Root << std::endl;
  std::cout << "PointDim = " << m_PointDim << std::endl;
  std::cout << "NPoints = " << m_NPoints << std::endl;

}

void MetaTube::
CopyInfo(const MetaTube * _tube)
{
  MetaObject::CopyInfo(_tube);
}

    

void MetaTube::
PointDim(const char* pointDim)
{
  strcpy(m_PointDim,pointDim);
}
    
const char* MetaTube::
PointDim(void) const
{
  return m_PointDim;
}

void MetaTube::
NPoints(int npnt)
{
  m_NPoints = npnt;
}

int MetaTube::
NPoints(void) const
{
  return m_NPoints;
}

void MetaTube::
Root(int root)
{
  m_Root = root;
}
    
int MetaTube:: 
Root(void) const
{
  return m_Root;
}


void  MetaTube::
ParentPoint(int root)
{
  m_Root = root;
}

int MetaTube::
ParentPoint(void) const
{
  return m_ParentPoint;
}


bool MetaTube::
ReadStream(int ndims, std::ifstream * stream)
{
  
  if(META_DEBUG)  std::cout << "MetaTube: ReadStream" << std::endl;

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

bool MetaTube::
Read(const char *_headerName)
{
  if(META_DEBUG) std::cout << "MetaTube: Read" << std::endl;

  M_Destroy();

  Clear();

  M_SetupReadFields();

  if(_headerName != NULL)
  {
    strcpy(m_FileName, _headerName);
  }

  if(META_DEBUG) std::cout << "MetaTube: Read: Opening stream" << std::endl;
 
  m_ReadStream->open(m_FileName, std::ios::binary | std::ios::in);
  
  if(!m_ReadStream->is_open())
  {
    std::cout << "MetaTube: Read: Cannot open file" << std::endl;
    return false;
  }

  if(!M_Read())
  {
    std::cout << "MetaTube: Read: Cannot parse file" << std::endl;
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
bool MetaTube::
Write(const char *_headName)
{
  if(META_DEBUG) std::cout << "MetaTube: Write" << std::endl;

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
  

bool MetaTube
::Append(const char *_headName)
{
  if(META_DEBUG) std::cout << "MetaTube: Append" << std::endl;

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
void MetaTube::
Clear(void)
{
  if(META_DEBUG) std::cout << "MetaTube: Clear" << std::endl;
  MetaObject::Clear();
  m_ParentPoint= -1;
  m_Root = 0;
  m_NPoints = 0;
  strcpy(m_PointDim, "x y z r rn mn bn mk v1x v1y v1z v2x v2y v2z a1 a2 red green blue alpha");
}
        
/** Destroy tube information */
void MetaTube::
M_Destroy(void)
{
  MetaObject::M_Destroy();
}

/** Set Read fields */
void MetaTube::
M_SetupReadFields(void)
{
  if(META_DEBUG) std::cout << "MetaTube: M_SetupReadFields" << std::endl;

  MetaObject::M_SetupReadFields();

  MET_FieldRecordType * mF;

  // int nDimsRecNum = MET_GetFieldRecordNumber("NDims", &m_Fields);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ParentPoint", MET_INT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Root", MET_INT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "PointDim", MET_STRING, true);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "NPoints", MET_INT, true);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Points", MET_NONE, true);
  mF->terminateRead = true;
  m_Fields.push_back(mF);

}

void MetaTube::
M_SetupWriteFields(void)
{
  strcpy(m_ObjectTypeName,"Tube");
  MetaObject::M_SetupWriteFields();

  MET_FieldRecordType * mF;

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "ParentPoint", MET_INT,m_ParentPoint);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "Root", MET_INT,m_Root);
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



bool MetaTube::
M_Read(void)
{
  if(META_DEBUG) std::cout << "MetaTube: M_Read: Loading Header" << std::endl;

  if(!MetaObject::M_Read())
  {
    std::cout << "MetaTube: M_Read: Error parsing file" << std::endl;
    return false;
  }

  if(META_DEBUG) std::cout << "MetaTube: M_Read: Parsing Header" << std::endl;
 
  MET_FieldRecordType * mF;
 
  mF = MET_GetFieldRecord("ParentPoint", &m_Fields);
  if(mF->defined)
  {
    m_ParentPoint= (int)mF->value[0];
  }

  mF = MET_GetFieldRecord("Root", &m_Fields);
  if(mF->defined)
  {
    m_Root= (int)mF->value[0];
  }

  mF = MET_GetFieldRecord("NPoints", &m_Fields);
  if(mF->defined)
  {
    m_NPoints= (int)mF->value[0];
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
  int posR = -1;
  int posRn = -1;
  int posMn = -1;
  int posBn = -1;
  int posMk = -1;
  int posV1x = -1;
  int posV1y = -1;
  int posV1z = -1;
  int posV2x = -1;
  int posV2y = -1;
  int posV2z = -1;
  int posA1 = -1;
  int posA2 = -1;
  int posRed = -1;
  int posGreen = -1;
  int posBlue = -1;
  int posAlpha = -1;

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
    if(((char *)pntVal[j])[0] == 'w' || ((char *)pntVal[j])[0] == 'W')
    {
     posDim[(int)pntVal[j][1]+3] = j;
    }
    if(!strcmp(pntVal[j], "s") || !strcmp(pntVal[j], "S") ||
      !strcmp(pntVal[j], "r") || !strcmp(pntVal[j], "R") ||
      !strcmp(pntVal[j], "rad") || !strcmp(pntVal[j], "Rad") ||
      !strcmp(pntVal[j], "radius") || !strcmp(pntVal[j], "Radius"))
    {
      posR = j;
    }
  
    if(!strcmp(pntVal[j], "rn") || !strcmp(pntVal[j], "RN"))
    {
      posRn = j;
    }
    if(!strcmp(pntVal[j], "mn") || !strcmp(pntVal[j], "MN"))
    {
      posMn = j;
    }
    if(!strcmp(pntVal[j], "bn") || !strcmp(pntVal[j], "BN"))
    {
      posBn = j;
    }
    if(!strcmp(pntVal[j], "mk") || !strcmp(pntVal[j], "MK"))
    {
      posMk = j;
    }
    if(!strcmp(pntVal[j], "v1x"))
    {
      posV1x = j;
    }
    if(!strcmp(pntVal[j], "v1y"))
    {
      posV1y = j;
    }
    if(!strcmp(pntVal[j], "v1z"))
    {
      posV1z = j;
    }
    if(!strcmp(pntVal[j], "v2x"))
    {
      posV2x = j;
    }
    if(!strcmp(pntVal[j], "v2y"))
    {
      posV2y = j;
    }
    if(!strcmp(pntVal[j], "v2z"))
    {
      posV2z = j;
    }
    if(!strcmp(pntVal[j], "a1"))
    {
      posA1 = j;
    }
    if(!strcmp(pntVal[j], "a2"))
    {
      posA2 = j;
    }
    
    if(!strcmp(pntVal[j], "red"))
    {
      posRed = j;
    }
    if(!strcmp(pntVal[j], "green"))
    {
      posGreen = j;
    }
    
    if(!strcmp(pntVal[j], "blue"))
    {
      posBlue = j;
    }
    if(!strcmp(pntVal[j], "alpha"))
    {
      posAlpha = j;
    }
  }

  float v[20];
  TubePnt* pnt;

  for(j=0; j<m_NPoints; j++) 
  {
    pnt = new TubePnt(m_NDims);

    for(int k=0; k<pntDim; k++)
    {
      *m_ReadStream >> v[k];
      m_ReadStream->get();
    }

    float* x = new float[m_NDims];
    for(int d=0; d<m_NDims; d++)
    {
      x[d] = v[posDim[d]];
    }

    pnt->m_X = x;

    pnt->m_R = v[posR];

    if(posMn >= (int)0 && posMn < pntDim)
    {
     pnt->m_Medialness = v[posMn];
    }

      if(posRn >= (int)0 && posRn < pntDim)
    {
     pnt->m_Ridgeness = v[posRn];
    }

    if(posBn >= (int)0 && posBn < pntDim)
    {
     pnt->m_Branchness = v[posBn];
    }

    if(posMk >= 0 && posMk < pntDim)
    {
     pnt->m_Mark = (v[posMk] > 0) ? true:false;
    }

    pnt->m_V1 = new float[m_NDims];
    if(posV1x>=0 && posV1x<pntDim)
    {
      pnt->m_V1[0] = v[posV1x]; 
      if(posV1y >= 0) { pnt->m_V1[1] = v[posV1y]; }
      if(posV1z >= 0 && m_NDims>2) { pnt->m_V1[2] = v[posV1z]; }
      if(posV2x >= 0) { pnt->m_V2[0] = v[posV2x]; }
      if(posV2y >= 0) { pnt->m_V2[1] = v[posV2y]; }
      if(posV2z >= 0 && m_NDims>2) { pnt->m_V2[2] = v[posV2z]; }
      pnt->m_Alpha1 = v[posA1];    
      pnt->m_Alpha2 = v[posA2];
    }
    
    if(posRed >= 0 && posRed < pntDim)
    {
      pnt->m_Color[0] = v[posRed];
    }

    if(posGreen >= 0 && posGreen < pntDim)
    {
      pnt->m_Color[1] = v[posGreen];
    }
    
    if(posBlue >= 0 && posBlue < pntDim)
    {
      pnt->m_Color[2] = v[posBlue];
    }
    
    if(posAlpha >= 0 && posAlpha < pntDim)
    {
      pnt->m_Color[3] = v[posAlpha];
    }
 
    m_PointList.push_back(pnt);
  }

      
  char c = ' ';
  while( (c!='\n') && (!m_ReadStream->eof()))
  {
    c = m_ReadStream->get();// to avoid unrecognize charactere
  }
  
  return true;
}


bool MetaTube::
M_Write(void)
{

  if(!MetaObject::M_Write())
  {
    std::cout << "MetaTube: M_Read: Error parsing file" << std::endl;
    return false;
  }

  /** Then copy all tubes points */
  PointListType::const_iterator it = m_PointList.begin();
  
  int d;
  while(it != m_PointList.end())
  {
    for(d = 0; d < m_NDims; d++)
    {
      *m_WriteStream << (*it)->m_X[d] << " ";
    }
      
    *m_WriteStream << (*it)->m_R << " ";
    *m_WriteStream << (*it)->m_Ridgeness << " ";
    *m_WriteStream << (*it)->m_Medialness << " ";
    *m_WriteStream << (*it)->m_Branchness << " ";
    if((*it)->m_Mark)
    {
      *m_WriteStream << 1 << " ";
    }
    else
    {
      *m_WriteStream << 0 << " ";
    }

    for(d = 0; d < m_NDims; d++)
    {
       *m_WriteStream << (*it)->m_V1[d] << " ";
    }
   
    for(d = 0; d < m_NDims; d++)
    {
       *m_WriteStream << (*it)->m_V2[d] << " ";
    }
    
    *m_WriteStream << (*it)->m_Alpha1 << " ";
    *m_WriteStream << (*it)->m_Alpha2 << " ";
    
    for(d=0;d<4;d++)
    {
      *m_WriteStream << (*it)->m_Color[d] << " ";
    }
    *m_WriteStream << std::endl;
    it++;
  }

  return true;

}

