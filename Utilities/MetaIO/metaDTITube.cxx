#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <fstream>
#include <string>

#include <metaUtils.h>
#include <metaObject.h>
#include <metaDTITube.h>

/** MetaDTITube Constructors */
MetaDTITube::
MetaDTITube()
:MetaObject()
{
  if(META_DEBUG) std::cout << "MetaDTITube()" << std::endl;
  Clear();
}


MetaDTITube::
MetaDTITube(const char *_headerName)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaDTITube()" << std::endl;
  Clear();
  Read(_headerName);
}


MetaDTITube::
MetaDTITube(const MetaDTITube *_DTITube)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaDTITube()" << std::endl;
  Clear();
  CopyInfo(_DTITube);
}


MetaDTITube::
MetaDTITube(unsigned int dim)
:MetaObject(dim)
{
  if(META_DEBUG) std::cout << "MetaDTITube()" << std::endl;
  Clear();
}

/** Destructor */
MetaDTITube::
~MetaDTITube()
{
  // Delete the list of pointers to DTITubes.
  PointListType::iterator it = m_PointList.begin();
  while(it != m_PointList.end())
  {
    DTITubePnt* pnt = *it;
    it++;
    delete pnt;
  }  
  m_PointList.clear();
  M_Destroy();
}

//
void MetaDTITube::
PrintInfo() const
{
  MetaObject::PrintInfo();
  std::cout << "ParentPoint = " << m_ParentPoint << std::endl;
  std::cout << "Root = " << m_Root << std::endl;
  std::cout << "PointDim = " << m_PointDim << std::endl;
  std::cout << "NPoints = " << m_NPoints << std::endl;
  char str[255];
  MET_TypeToString(m_ElementType, str);
  std::cout << "ElementType = " << str << std::endl;
}

void MetaDTITube::
CopyInfo(const MetaDTITube * _DTITube)
{
  MetaObject::CopyInfo(_DTITube);
}

    

void MetaDTITube::
PointDim(const char* pointDim)
{
  strcpy(m_PointDim,pointDim);
}
    
const char* MetaDTITube::
PointDim(void) const
{
  return m_PointDim;
}

void MetaDTITube::
NPoints(int npnt)
{
  m_NPoints = npnt;
}

int MetaDTITube::
NPoints(void) const
{
  return m_NPoints;
}

void MetaDTITube::
Root(int root)
{
  m_Root = root;
}
    
int MetaDTITube:: 
Root(void) const
{
  return m_Root;
}


void  MetaDTITube::
ParentPoint(int parentpoint)
{
  m_ParentPoint = parentpoint;
}

int MetaDTITube::
ParentPoint(void) const
{
  return m_ParentPoint;
}

/** Clear DTITube information */
void MetaDTITube::
Clear(void)
{
  if(META_DEBUG) std::cout << "MetaDTITube: Clear" << std::endl;
  MetaObject::Clear();
  // Delete the list of pointers to DTITubes.
  PointListType::iterator it = m_PointList.begin();
  while(it != m_PointList.end())
  {
    DTITubePnt* pnt = *it;
    it++;
    delete pnt;
  }  
  m_PointList.clear();

  m_ParentPoint= -1;
  m_Root = 0;
  m_NPoints = 0;
  strcpy(m_PointDim, "x y z fa adc ga i l1 l2 l3 xevmin yevmin zevmin xevmed yevmed zevmed xevmax yevmax zevmax mri1 mri2 mri3 mri4 mri5 tensor1 tensor2 tensor3 tensor4 tensor5 tensor6 v1x v1y v1z v2x v2y v2z tx ty tz r red green blue alpha id");
  m_ElementType = MET_FLOAT;
}
        
/** Destroy DTITube information */
void MetaDTITube::
M_Destroy(void)
{
  MetaObject::M_Destroy();
}

/** Set Read fields */
void MetaDTITube::
M_SetupReadFields(void)
{
  if(META_DEBUG) std::cout << "MetaDTITube: M_SetupReadFields" << std::endl;

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

void MetaDTITube::
M_SetupWriteFields(void)
{
  strcpy(m_ObjectTypeName,"Tube");
  strcpy(m_ObjectSubTypeName,"DTI");
  MetaObject::M_SetupWriteFields();

  MET_FieldRecordType * mF;

  if(m_ParentPoint>=0 && m_ParentID>=0)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ParentPoint", MET_INT,m_ParentPoint);
    m_Fields.push_back(mF);
    }

  if(m_Root>0)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "Root", MET_INT,m_Root);
    m_Fields.push_back(mF);
    }

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


bool MetaDTITube::
M_Read(void)
{
  if(META_DEBUG) std::cout << "MetaDTITube: M_Read: Loading Header" << std::endl;

  if(!MetaObject::M_Read())
  {
    std::cout << "MetaDTITube: M_Read: Error parsing file" << std::endl;
    return false;
  }

  if(META_DEBUG) std::cout << "MetaDTITube: M_Read: Parsing Header" << std::endl;
 
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
  int i;
  for(i= 0; i < m_NDims; i++)
  {
    posDim[i] = -1;
  }
  int posR = -1;
  int posV1x = -1;
  int posV1y = -1;
  int posV1z = -1;
  int posV2x = -1;
  int posV2y = -1;
  int posV2z = -1;
  int posTx = -1;
  int posTy = -1;
  int posTz = -1;
  int posRed = -1;
  int posGreen = -1;
  int posBlue = -1;
  int posAlpha = -1;
  int posID = -1;
  int posFA = -1;
  int posADC = -1;
  int posGA = -1;
  int posI = -1;
  int posl1 = -1;
  int posl2 = -1;
  int posl3 = -1;
  int posxevmin = -1;
  int posyevmin = -1;
  int poszevmin = -1;
  int posxevmed = -1;
  int posyevmed = -1;
  int poszevmed = -1;
  int posxevmax = -1;
  int posyevmax = -1;
  int poszevmax = -1;
  int posmri1 = -1;
  int posmri2 = -1;
  int posmri3 = -1;
  int posmri4 = -1;
  int posmri5 = -1;
  int postensor1 = -1;
  int postensor2 = -1;
  int postensor3 = -1;
  int postensor4 = -1;
  int postensor5 = -1;
  int postensor6 = -1;

  int pntDim;
  char** pntVal = NULL;
  MET_StringToWordArray(m_PointDim, &pntDim, &pntVal); 
 
  if(META_DEBUG)
    { std::cout << "MetaDTITube: Parsing point dim" << std::endl; }

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
    if(!strcmp(pntVal[j], "r") || !strcmp(pntVal[j], "R") ||
      !strcmp(pntVal[j], "rad") || !strcmp(pntVal[j], "Rad") ||
      !strcmp(pntVal[j], "radius") || !strcmp(pntVal[j], "Radius"))
    {
      posR = j;
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
    if(!strcmp(pntVal[j], "tx"))
    {
      posTx = j;
    }
    if(!strcmp(pntVal[j], "ty"))
    {
      posTy = j;
    }
    if(!strcmp(pntVal[j], "tz"))
    {
      posTz = j;
    }
    if(!strcmp(pntVal[j], "l1"))
    {
      posl1 = j;
    }
    if(!strcmp(pntVal[j], "l2"))
    {
      posl2 = j;
    }
    if(!strcmp(pntVal[j], "l3"))
    {
      posl3 = j;
    }    
    if(!strcmp(pntVal[j], "fa"))
    {
      posFA = j;
    }
    if(!strcmp(pntVal[j], "adc"))
    {
      posADC = j;
    }
    if(!strcmp(pntVal[j], "ga"))
    {
      posGA = j;
    }
    if(!strcmp(pntVal[j], "i"))
    {
      posI = j;
    }
    if(!strcmp(pntVal[j], "xevmin"))
    {
      posxevmin = j;
    }
    if(!strcmp(pntVal[j], "yevmin"))
    {
      posyevmin = j;
    }
    if(!strcmp(pntVal[j], "zevmin"))
    {
      poszevmin = j;
    }
    if(!strcmp(pntVal[j], "xevmed"))
    {
      posxevmed = j;
    }
    if(!strcmp(pntVal[j], "yevmed"))
    {
      posyevmed = j;
    }
    if(!strcmp(pntVal[j], "zevmed"))
    {
      poszevmed = j;
    }
    if(!strcmp(pntVal[j], "xevmax"))
    {
      posxevmax = j;
    }
    if(!strcmp(pntVal[j], "yevmax"))
    {
      posyevmax = j;
    }
    if(!strcmp(pntVal[j], "zevmax"))
    {
      poszevmax = j;
    }
    if(!strcmp(pntVal[j], "mri1"))
    {
      posmri1 = j;
    }
    if(!strcmp(pntVal[j], "mri2"))
    {
      posmri1 = j;
    }
    if(!strcmp(pntVal[j], "mri3"))
    {
      posmri1 = j;
    }
    if(!strcmp(pntVal[j], "mri4"))
    {
      posmri1 = j;
    }
    if(!strcmp(pntVal[j], "mri5"))
    {
      posmri1 = j;
    }
    if(!strcmp(pntVal[j], "tensor1"))
    {
      postensor1 = j;
    }
    if(!strcmp(pntVal[j], "tensor2"))
    {
      postensor2 = j;
    }
    if(!strcmp(pntVal[j], "tensor3"))
    {
      postensor3 = j;
    }
    if(!strcmp(pntVal[j], "tensor4"))
    {
      postensor4 = j;
    }
    if(!strcmp(pntVal[j], "tensor5"))
    {
      postensor5 = j;
    }
    if(!strcmp(pntVal[j], "tensor6"))
    {
      postensor6 = j;
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
    if(!strcmp(pntVal[j], "id") || !strcmp(pntVal[j], "ID"))
    {
      posID = j;
    }
  }

  for(i=0;i<pntDim;i++)
    {
      delete [] pntVal[i];
    }
  delete [] pntVal;

  float v[50];
  
  if(m_Event)
    {
     m_Event->StartReading(m_NPoints);
    }

  if(m_BinaryData)
  {
    int elementSize;
    MET_SizeOfType(m_ElementType, &elementSize);
    int readSize = m_NPoints*(m_NDims*(4)+13+9+5+6)*elementSize;

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
    int d;
    for(int j=0; j<m_NPoints; j++) 
    {
      DTITubePnt* pnt = new DTITubePnt(m_NDims);
      
      for(d=0; d<m_NDims; d++)
      {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_X[d] = (float)td;
      }

      MET_ValueToDouble(m_ElementType, _data, i++, &td);
      pnt->m_FA = (float)td;
      MET_ValueToDouble(m_ElementType, _data, i++, &td);
      pnt->m_ADC = (float)td;
      MET_ValueToDouble(m_ElementType, _data, i++, &td);
      pnt->m_GA = (float)td;
      MET_ValueToDouble(m_ElementType, _data, i++, &td);
      pnt->m_Interpolation = (int)td;
      MET_ValueToDouble(m_ElementType, _data, i++, &td);
      pnt->m_Lambda1 = (float)td;
      MET_ValueToDouble(m_ElementType, _data, i++, &td);
      pnt->m_Lambda2 = (float)td;
      MET_ValueToDouble(m_ElementType, _data, i++, &td);
      pnt->m_Lambda3 = (float)td;

      for(d = 0; d < 3; d++)
      {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_MinEV[d] = (float)td;
      }
      for(d = 0; d < 3; d++)
      {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_MedEV[d] = (float)td;
      }
      for(d = 0; d < 3; d++)
      {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_MaxEV[d] = (float)td;
      }
      for(d = 0; d < 5; d++)
      {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_MRI[d] = (float)td;
      }
      for(d = 0; d < 6; d++)
      {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_TensorMatrix[d] = (float)td;
      }

      for(d = 0; d < m_NDims; d++)
      {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_V1[d] = (float)td;
      }
    
      if(m_NDims==3)
      {
        for(d = 0; d < m_NDims; d++)
        {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_V2[d] = (float)td;
        }
      }
      
      for(d = 0; d < m_NDims; d++)
      {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_T[d] = (float)td;
      }
     
      MET_ValueToDouble(m_ElementType, _data, i++, &td);
      pnt->m_R = (float)td;

      for(d=0; d<4; d++)
      {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_Color[d] = (float)td;
      }

      MET_ValueToDouble(m_ElementType,_data,i++,&td);
      pnt->m_ID=(int)td;

      m_PointList.push_back(pnt);
    }
    delete [] _data;
  }
  else
  {
    for(j=0; j<m_NPoints; j++) 
    {
      if(m_Event)
        {
        m_Event->SetCurrentIteration(j+1);
        }

     DTITubePnt* pnt = new DTITubePnt(m_NDims);

      for(int k=0; k<pntDim; k++)
      {
        *m_ReadStream >> v[k];
        m_ReadStream->get();
      }

      for(int d=0; d<m_NDims; d++)
      {
        pnt->m_X[d] = v[posDim[d]];
      }

      pnt->m_R = v[posR];

      if(posV1x>=0 && posV1x<pntDim)
        {
        pnt->m_V1[0] = v[posV1x]; 
        if(posV1y >= 0 && posV1y<pntDim) 
          { pnt->m_V1[1] = v[posV1y]; }
        if(posV1z >= 0 && m_NDims>2 && posV1z<pntDim) 
          { pnt->m_V1[2] = v[posV1z]; }
        }
      if(posV2x >= 0 && posV2x<pntDim) 
        {
        pnt->m_V2[0] = v[posV2x]; 
        if(posV2y >= 0 && posV2y<pntDim) 
          { pnt->m_V2[1] = v[posV2y]; }
        if(posV2z >= 0 && m_NDims>2 && posV2z<pntDim) 
          { pnt->m_V2[2] = v[posV2z]; }
        }
      if(posTx >= 0 && posTx<pntDim) 
        {
        pnt->m_T[0] = v[posTx]; 
        if(posTy >= 0 && posTy<pntDim) 
          { pnt->m_T[1] = v[posTy]; }
        if(posTz >= 0 && m_NDims>2 && posTz<pntDim) 
          { pnt->m_T[2] = v[posTz]; }
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
 
      if(posID >= 0 && posID < pntDim)
      {
        pnt->m_ID = (int)v[posID];
      }

      // Read FA
      if(posFA >= 0 && posFA < pntDim)
      {
        pnt->m_FA = v[posFA];
      }
      
      // Read ADC
      if(posADC >= 0 && posADC < pntDim)
      {
        pnt->m_ADC = v[posADC];
      }  
      // Read GA
      if(posGA >= 0 && posGA < pntDim)
      {
        pnt->m_GA = v[posGA];
      }  
      // Read interpolation
      if(posI >= 0 && posI < pntDim)
      {
        pnt->m_Interpolation = v[posI];
      }  
      // Read lambda 1
      if(posl1 >= 0 && posl1 < pntDim)
      {
        pnt->m_Lambda1 = v[posl1];
      }  
      // Read lambda 2
      if(posl2 >= 0 && posl2 < pntDim)
      {
        pnt->m_Lambda2 = v[posl2];
      }  
      // Read lambda 3
      if(posl3 >= 0 && posl3 < pntDim)
      {
        pnt->m_Lambda3 = v[posl3];
      }  
      // Read xevmin
      if(posxevmin >= 0 && posxevmin < pntDim)
      {
        pnt->m_MinEV[0] = v[posxevmin];
      } 
      // Read yevmin
      if(posyevmin >= 0 && posyevmin < pntDim)
      {
        pnt->m_MinEV[1] = v[posyevmin];
      } 
      // Read zevmin
      if(poszevmin >= 0 && poszevmin < pntDim)
      {
        pnt->m_MinEV[2] = v[poszevmin];
      } 
      // Read xevmed
      if(posxevmed >= 0 && posxevmed < pntDim)
      {
        pnt->m_MedEV[0] = v[posxevmed];
      } 
      // Read yevmed
      if(posyevmed >= 0 && posyevmed < pntDim)
      {
        pnt->m_MedEV[1] = v[posyevmed];
      } 
      // Read zevmed
      if(poszevmed >= 0 && poszevmed < pntDim)
      {
        pnt->m_MedEV[2] = v[poszevmed];
      } 
      // Read xevmax
      if(posxevmax >= 0 && posxevmax < pntDim)
      {
        pnt->m_MaxEV[0] = v[posxevmax];
      } 
      // Read yevmax
      if(posyevmax >= 0 && posyevmax < pntDim)
      {
        pnt->m_MaxEV[1] = v[posyevmax];
      } 
      // Read zevmax
      if(poszevmax >= 0 && poszevmax < pntDim)
      {
        pnt->m_MaxEV[2] = v[poszevmax];
      } 
      // Read MRI1
      if(posmri1 >= 0 && posmri1 < pntDim)
      {
        pnt->m_MRI[0] = v[posmri1];
      } 
      // Read MRI2
      if(posmri2 >= 0 && posmri2 < pntDim)
      {
        pnt->m_MRI[1] = v[posmri2];
      }
      // Read MRI3
      if(posmri3 >= 0 && posmri3 < pntDim)
      {
        pnt->m_MRI[2] = v[posmri3];
      } 
      // Read MRI4
      if(posmri4 >= 0 && posmri4 < pntDim)
      {
        pnt->m_MRI[3] = v[posmri4];
      } 
      // Read MRI5
      if(posmri5 >= 0 && posmri5 < pntDim)
      {
        pnt->m_MRI[4] = v[posmri5];
      }
      // Read tensor1
      if(postensor1 >= 0 && postensor1 < pntDim)
      {
        pnt->m_TensorMatrix[0] = v[postensor1];
      }
      // Read tensor2
      if(postensor2 >= 0 && postensor2 < pntDim)
      {
        pnt->m_TensorMatrix[2] = v[postensor2];
      }
      // Read tensor3
      if(postensor3 >= 0 && postensor3 < pntDim)
      {
        pnt->m_TensorMatrix[2] = v[postensor3];
      }
      // Read tensor4
      if(postensor4 >= 0 && postensor4 < pntDim)
      {
        pnt->m_TensorMatrix[3] = v[postensor4];
      }
      // Read tensor5
      if(postensor5 >= 0 && postensor5 < pntDim)
      {
        pnt->m_TensorMatrix[4] = v[postensor5];
      }
      // Read tensor6
      if(postensor6 >= 0 && postensor6 < pntDim)
      {
        pnt->m_TensorMatrix[5] = v[postensor6];
      }
      m_PointList.push_back(pnt);
    }

      
    char c = ' ';
    while( (c!='\n') && (!m_ReadStream->eof()))
    {
      c = m_ReadStream->get();// to avoid unrecognize charactere
    }
  }
  
  if(m_Event)
    {
    m_Event->StopReading();
    }

  delete posDim;
  return true;
}

MET_ValueEnumType MetaDTITube::
ElementType(void) const
{
  return m_ElementType;
}

void MetaDTITube::
ElementType(MET_ValueEnumType _elementType)
{
  m_ElementType = _elementType;
}

bool MetaDTITube::
M_Write(void)
{

  if(!MetaObject::M_Write())
  {
    std::cout << "MetaDTITube: M_Read: Error parsing file" << std::endl;
    return false;
  }

  /** Then copy all DTITubes points */
  if(m_BinaryData)
  {
    PointListType::const_iterator it = m_PointList.begin();
    int elementSize;
    MET_SizeOfType(m_ElementType, &elementSize);

    char* data = new char[(m_NDims*(4)+13+9+5+6)*m_NPoints*elementSize];
    int i=0;
    int d;
    while(it != m_PointList.end())
    {
      for(d = 0; d < m_NDims; d++)
      {
        MET_DoubleToValue((double)(*it)->m_X[d],m_ElementType,data,i++);  
      }

      MET_DoubleToValue((double)(*it)->m_FA,m_ElementType,data,i++);  
      MET_DoubleToValue((double)(*it)->m_ADC,m_ElementType,data,i++);
      MET_DoubleToValue((double)(*it)->m_GA,m_ElementType,data,i++);
      MET_DoubleToValue((double)(*it)->m_Interpolation,m_ElementType,data,i++);
      MET_DoubleToValue((double)(*it)->m_Lambda1,m_ElementType,data,i++);
      MET_DoubleToValue((double)(*it)->m_Lambda2,m_ElementType,data,i++);
      MET_DoubleToValue((double)(*it)->m_Lambda3,m_ElementType,data,i++);

      for(d = 0; d < 3; d++)
      {
        MET_DoubleToValue((double)(*it)->m_MinEV[d],m_ElementType,data,i++);  
      }
      for(d = 0; d < 3; d++)
      {
        MET_DoubleToValue((double)(*it)->m_MedEV[d],m_ElementType,data,i++);  
      }
      for(d = 0; d < 3; d++)
      {
        MET_DoubleToValue((double)(*it)->m_MaxEV[d],m_ElementType,data,i++);  
      }
      for(d = 0; d < 5; d++)
      {
        MET_DoubleToValue((double)(*it)->m_MRI[d],m_ElementType,data,i++);  
      }
      for(d = 0; d < 6; d++)
      {
        MET_DoubleToValue((double)(*it)->m_TensorMatrix[d],m_ElementType,data,i++);  
      }
      
      for(d = 0; d < m_NDims; d++)
      {
        MET_DoubleToValue((double)(*it)->m_V1[d],m_ElementType,data,i++);  
      }
    
      if(m_NDims==3)
      {
        for(d = 0; d < m_NDims; d++)
        {
          MET_DoubleToValue((double)(*it)->m_V2[d],m_ElementType,data,i++);  
        }
      }
      
      for(d = 0; d < m_NDims; d++)
      {
        MET_DoubleToValue((double)(*it)->m_T[d],m_ElementType,data,i++);  
      }
     
      MET_DoubleToValue((double)(*it)->m_R,m_ElementType,data,i++);  

      for(d=0; d<4; d++)
      {
        MET_DoubleToValue((double)(*it)->m_Color[d],m_ElementType,data,i++);
      }

      MET_DoubleToValue((double)(*it)->m_ID,m_ElementType,data,i++);

      it++;
    }

    m_WriteStream->write((char *)data,i*elementSize);
    m_WriteStream->write("\n",1);
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
      
      *m_WriteStream << (*it)->m_FA << " ";
      *m_WriteStream << (*it)->m_ADC << " ";
      *m_WriteStream << (*it)->m_GA << " ";
      *m_WriteStream << (*it)->m_Interpolation << " ";
      *m_WriteStream << (*it)->m_Lambda1 << " ";
      *m_WriteStream << (*it)->m_Lambda2 << " ";
      *m_WriteStream << (*it)->m_Lambda3 << " ";

      for(d = 0; d < 3; d++)
        {
        *m_WriteStream << (*it)->m_MinEV[d] << " ";
        }
      for(d = 0; d < 3; d++)
        {
        *m_WriteStream << (*it)->m_MedEV[d] << " ";
        }
      for(d = 0; d < 3; d++)
        {
        *m_WriteStream << (*it)->m_MaxEV[d] << " ";
        }
      for(d = 0; d < 5; d++)
        {
        *m_WriteStream << (*it)->m_MRI[d] << " ";
        }
      for(d = 0; d < 6; d++)
        {
        *m_WriteStream << (*it)->m_TensorMatrix[d] << " ";
        }

      for(d = 0; d < m_NDims; d++)
      {
         *m_WriteStream << (*it)->m_V1[d] << " ";
      }
   
      if(m_NDims>=3)
      {
        for(d = 0; d < m_NDims; d++)
        {
           *m_WriteStream << (*it)->m_V2[d] << " ";
        }
      }
      
      for(d = 0; d < m_NDims; d++)
      {
         *m_WriteStream << (*it)->m_T[d] << " ";
      }
            
      *m_WriteStream << (*it)->m_R << " ";

      for(d=0;d<4;d++)
      {
        *m_WriteStream << (*it)->m_Color[d] << " ";
      }

      *m_WriteStream << (*it)->m_ID << " ";

      *m_WriteStream << std::endl;
      it++;
    }
  }
  return true;

}

