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
  if(m_Root)
    {
    std::cout << "Root = " << "True" << std::endl;
    }
  else
    {
    std::cout << "Root = " << "True" << std::endl;
    }
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
  m_PointDim = pointDim;
}
    
const char* MetaDTITube::
PointDim(void) const
{
  return m_PointDim.c_str();
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
Root(bool root)
{
  m_Root = root;
}
    
bool MetaDTITube:: 
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
  m_Root = false;
  m_NPoints = 0;
  m_PointDim = "x y z tensor1 tensor2 tensor3 tensor4 tensor5 tensor6";
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
  MET_InitReadField(mF, "Root", MET_STRING, false);
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

  if(m_Root)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "Root", MET_STRING, strlen("True"), "True");
    m_Fields.push_back(mF);
    }
  else
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "Root", MET_STRING, strlen("False"), "False");
    m_Fields.push_back(mF);
    }

  // Create the new PointDim field
  m_PointDim = "x y z tensor1 tensor2 tensor3 tensor4 tensor5 tensor6";
  
  // All the points in the tube have the same number of fields
  const DTITubePnt::FieldListType & extraList = (*(m_PointList.begin()))->GetExtraFields();
  DTITubePnt::FieldListType::const_iterator itFields = extraList.begin();
  while(itFields !=  extraList.end())
    {
    m_PointDim += " ";
    m_PointDim += (*itFields).first;
    itFields++;
    }

  if(m_PointDim.size()>0)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "PointDim", MET_STRING,
                           m_PointDim.size(),m_PointDim.c_str());
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

/** Return the position given the name of the field */
int MetaDTITube::GetPosition(const char* name) const
{
  std::vector<PositionType>::const_iterator it = m_Positions.begin();
  while(it != m_Positions.end())
    {
    if(!strcmp((*it).first.c_str(),name))
      {
      return (*it).second;
      }
    ++it;
    }

  return -1;
}

bool MetaDTITube::
M_Read(void)
{
  if(META_DEBUG)
    {
    std::cout << "MetaDTITube: M_Read: Loading Header" << std::endl;
    }

  if(!MetaObject::M_Read())
    {
    std::cout << "MetaDTITube: M_Read: Error parsing file" << std::endl;
    return false;
    }

  if(META_DEBUG)
    {
    std::cout << "MetaDTITube: M_Read: Parsing Header" << std::endl;
    }
 
  MET_FieldRecordType * mF;
 
  mF = MET_GetFieldRecord("ParentPoint", &m_Fields);
  if(mF->defined)
    {
    m_ParentPoint= (int)mF->value[0];
    }

  m_Root = false;
  mF = MET_GetFieldRecord("Root", &m_Fields);
  if(mF->defined)
    {
    if(*((char *)(mF->value)) == 'T' 
      || *((char*)(mF->value)) == 't'
      || *((char*)(mF->value)) == '1')
      {
      m_Root = true;
      }
    else
      {
      m_Root = false;
      }
    }

  mF = MET_GetFieldRecord("NPoints", &m_Fields);
  if(mF->defined)
    {
    m_NPoints= (int)mF->value[0];
    }

  mF = MET_GetFieldRecord("PointDim", &m_Fields);
  if(mF->defined)
    {
    m_PointDim = (char *)(mF->value);
    }

  int i;
  
  int pntDim;
  char** pntVal = NULL;
  char pointDim[255];

  for(unsigned t = 0;t<m_PointDim.size();t++)
    {
    pointDim[t] = m_PointDim[t];
    }
  pointDim[m_PointDim.size()] = '\0';

  MET_StringToWordArray(pointDim, &pntDim, &pntVal); 

  if(META_DEBUG)
    { 
    std::cout << "MetaDTITube: Parsing point dim" << std::endl; 
    }

  int j;
  m_Positions.clear();
  for(j = 0; j < pntDim; j++) 
    {
    PositionType p(pntVal[j],j);
    m_Positions.push_back(p);
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
    int readSize = m_NPoints*pntDim*elementSize;

    char* _data = new char[readSize];
    m_ReadStream->read((char *)_data, readSize);

    int gc = m_ReadStream->gcount();
    if(gc != readSize)
      {
      std::cout << "MetaLine: m_Read: data not read completely" 
                << std::endl;
      std::cout << "   ideal = " << readSize 
                << " : actual = " << gc << std::endl;
      return false;
      }

    i=0;
    double td;
    int d;
    for(j=0; j<m_NPoints; j++) 
      {
      DTITubePnt* pnt = new DTITubePnt(m_NDims);
      
      for(d=0; d<m_NDims; d++)
        {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_X[d] = (float)td;
        } 
    
      for(d=0; d<6; d++)
        {
        MET_ValueToDouble(m_ElementType, _data, i++, &td);
        pnt->m_TensorMatrix[d] = (float)td;
        }

      std::vector<PositionType>::const_iterator itFields = m_Positions.begin();
      while(itFields !=  m_Positions.end())
        {
        if(strcmp((*itFields).first.c_str(),"x") 
          && strcmp((*itFields).first.c_str(),"y") 
          && strcmp((*itFields).first.c_str(),"z") 
          && strcmp((*itFields).first.c_str(),"tensor1") 
          && strcmp((*itFields).first.c_str(),"tensor2") 
          && strcmp((*itFields).first.c_str(),"tensor3") 
          && strcmp((*itFields).first.c_str(),"tensor4") 
          && strcmp((*itFields).first.c_str(),"tensor5") 
          && strcmp((*itFields).first.c_str(),"tensor6") 
          )
          {
          MET_ValueToDouble(m_ElementType, _data, i++, &td);
          pnt->AddField((*itFields).first.c_str(),(float)td);
          }
        itFields++;
        }

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

     
      pnt->m_X[0] = v[this->GetPosition("x")];
      pnt->m_X[1] = v[this->GetPosition("y")];

      if(m_NDims == 3)
        {
        pnt->m_X[2] = v[this->GetPosition("z")];
        }

      // Read tensor1
      if(this->GetPosition("tensor1") >= 0 
         && this->GetPosition("tensor1") < pntDim)
        {
        pnt->m_TensorMatrix[0] = v[this->GetPosition("tensor1")];
        }
      // Read tensor2
      if(this->GetPosition("tensor2") >= 0 
         && this->GetPosition("tensor2") < pntDim)
        {
        pnt->m_TensorMatrix[1] = v[this->GetPosition("tensor2")];
        }
      // Read tensor3
      if(this->GetPosition("tensor3") >= 0 
         && this->GetPosition("tensor3") < pntDim)
        {
        pnt->m_TensorMatrix[2] = v[this->GetPosition("tensor3")];
        }
      // Read tensor4
      if(this->GetPosition("tensor4") >= 0 
         && this->GetPosition("tensor4") < pntDim)
        {
        pnt->m_TensorMatrix[3] = v[this->GetPosition("tensor4")];
        }
      // Read tensor5
      if(this->GetPosition("tensor5") >= 0 
         && this->GetPosition("tensor5") < pntDim)
        {
        pnt->m_TensorMatrix[4] = v[this->GetPosition("tensor5")];
        }
      // Read tensor6
      if(this->GetPosition("tensor6") >= 0 
         && this->GetPosition("tensor6") < pntDim)
        {
        pnt->m_TensorMatrix[5] = v[this->GetPosition("tensor6")];
        }

      // Add the extrafields
      std::vector<PositionType>::const_iterator itFields = m_Positions.begin();
      while(itFields !=  m_Positions.end())
        {
        if(strcmp((*itFields).first.c_str(),"x") 
          && strcmp((*itFields).first.c_str(),"y") 
          && strcmp((*itFields).first.c_str(),"z") 
          && strcmp((*itFields).first.c_str(),"tensor1") 
          && strcmp((*itFields).first.c_str(),"tensor2") 
          && strcmp((*itFields).first.c_str(),"tensor3") 
          && strcmp((*itFields).first.c_str(),"tensor4") 
          && strcmp((*itFields).first.c_str(),"tensor5") 
          && strcmp((*itFields).first.c_str(),"tensor6") 
          )
          {
          pnt->AddField((*itFields).first.c_str(),
                        v[this->GetPosition((*itFields).first.c_str())]);
          }
        itFields++;
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

    unsigned int pntDim = m_NDims+6; 
    const DTITubePnt::FieldListType & extraList = (*(m_PointList.begin()))->GetExtraFields();
    pntDim += extraList.size();

    char* data = new char[pntDim*m_NPoints*elementSize];
    int i=0;
    int d;
    while(it != m_PointList.end())
      {
      for(d = 0; d < m_NDims; d++)
        {
        MET_DoubleToValue((double)(*it)->m_X[d],m_ElementType,data,i++);  
        }

      for(d = 0; d < 6; d++)
        {
        MET_DoubleToValue((double)(*it)->m_TensorMatrix[d],
                           m_ElementType, data, i++);  
        }

      // Add the extra fields
      extraList = (*it)->GetExtraFields();
      DTITubePnt::FieldListType::const_iterator itFields = extraList.begin();
      while(itFields !=  extraList.end())
        {
        MET_DoubleToValue((double)(*itFields).second,m_ElementType,data,i++);  
        itFields++;
        }

      it++;
      }

    m_WriteStream->write((char *)data,i*elementSize);
    m_WriteStream->write("\n",1);
    delete []data;
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
      
      for(d = 0; d < 6; d++)
        {
        *m_WriteStream << (*it)->m_TensorMatrix[d] << " ";
        }

      // Add the extra fields
      const DTITubePnt::FieldListType & extraList = (*it)->GetExtraFields();
      DTITubePnt::FieldListType::const_iterator itFields = extraList.begin();
      while(itFields !=  extraList.end())
        {
        *m_WriteStream << (*itFields).second << " ";
        itFields++;
        }

      *m_WriteStream << std::endl;
      it++;
      }
    }
  return true;

}

