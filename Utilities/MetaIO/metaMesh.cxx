#include <stdio.h>
#include <ctype.h>
#include <iostream>
#include <fstream>
#include <string>

#include <metaUtils.h>
#include <metaObject.h>
#include <metaMesh.h>

//
// MetaMesh Constructors
//
MetaMesh::
MetaMesh()
:MetaObject()
{
  if(META_DEBUG) std::cout << "MetaMesh()" << std::endl;
  m_NPoints = 0;
 
  for(unsigned int i=0;i<MET_NUM_CELL_TYPES;i++)
    {
    m_CellListArray[i] = NULL;
    }
  Clear();
}

//
MetaMesh::
MetaMesh(const char *_headerName)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaMesh()" << std::endl;
  m_NPoints = 0;
  
  for(unsigned int i=0;i<MET_NUM_CELL_TYPES;i++)
    {
    m_CellListArray[i] = NULL;
    }
  Clear();
  Read(_headerName);
}

//
MetaMesh::
MetaMesh(const MetaMesh *_mesh)
:MetaObject()
{
  if(META_DEBUG)  std::cout << "MetaMesh()" << std::endl;
  m_NPoints = 0;
  for(unsigned int i=0;i<MET_NUM_CELL_TYPES;i++)
    {
    m_CellListArray[i] = NULL;
    }
  Clear();
  CopyInfo(_mesh);
}



//
MetaMesh::
MetaMesh(unsigned int dim)
:MetaObject(dim)
{
  if(META_DEBUG) std::cout << "MetaMesh()" << std::endl;
  m_NPoints = 0;
  for(unsigned int i=0;i<MET_NUM_CELL_TYPES;i++)
    {
    m_CellListArray[i] = NULL;
    }
  Clear();
}

/** Destructor */
MetaMesh::
~MetaMesh()
{
  Clear();
  for(unsigned int i=0;i<MET_NUM_CELL_TYPES;i++)
    {
    delete m_CellListArray[i];
    m_CellListArray[i] = NULL;
    }

  M_Destroy();
}

//
void MetaMesh::
PrintInfo() const
{
  MetaObject::PrintInfo();
  std::cout << "PointDim = " << m_PointDim << std::endl;
  std::cout << "NPoints = " << m_NPoints << std::endl;
  char str[255];
  MET_TypeToString(m_PointType, str);
  std::cout << "PointType = " << str << std::endl;
  MET_TypeToString(m_PointDataType, str);
  std::cout << "PointDataType = " << str << std::endl;
  MET_TypeToString(m_CellDataType, str);
  std::cout << "CellDataType = " << str << std::endl;
}

void MetaMesh::
CopyInfo(const MetaMesh * _tube)
{
  MetaObject::CopyInfo(_tube);
}

int MetaMesh::
NPoints(void) const
{
  return m_NPoints;
}

int MetaMesh::
NCells(void) const
{
  return m_NCells;
}

int MetaMesh::
NCellLinks(void) const
{
  return m_NCellLinks;
}

/** Clear tube information */
void MetaMesh::
Clear(void)
{
  if(META_DEBUG) std::cout << "MetaMesh: Clear" << std::endl;
  MetaObject::Clear();
  if(META_DEBUG) std::cout << "MetaMesh: Clear: m_NPoints" << std::endl;
  
  // Delete the list of pointers to points.
  PointListType::iterator it_pnt = m_PointList.begin();
  while(it_pnt != m_PointList.end())
  {
    MeshPoint* pnt = *it_pnt;
    it_pnt++;
    delete pnt;
  }

  // Delete the list of pointers to celllinks
  CellLinkListType::iterator it_celllinks = m_CellLinks.begin();
  while(it_celllinks != m_CellLinks.end())
  {
    MeshCellLink* link = *it_celllinks;
    it_celllinks++;
    delete link;
  }

  // Delete the list of pointers to pointdata
  PointDataListType::iterator it_pointdata = m_PointData.begin();
  while(it_pointdata != m_PointData.end())
  {
    MeshDataBase* data = *it_pointdata;
    it_pointdata++;
    delete data;
  }

  // Delete the list of pointers to celldata
  CellDataListType::iterator it_celldata = m_CellData.begin();
  while(it_celldata != m_CellData.end())
  {
    MeshDataBase* data = *it_celldata;
    it_celldata++;
    delete data;
  }

  // Initialize the new array
  for(unsigned int i=0;i<MET_NUM_CELL_TYPES;i++)
    {
    if(m_CellListArray[i])
      {
      // Delete the list of pointers to cells.
      CellListType::iterator it_cell = m_CellListArray[i]->begin();
      while(it_cell != m_CellListArray[i]->end())
      {
        MeshCell* cell = *it_cell;
        it_cell++;
        delete cell;
      }
      delete m_CellListArray[i];
      }
    m_CellListArray[i] = new CellListType;
    }

  m_PointList.clear();
  m_PointData.clear();
  m_CellData.clear();

  m_NPoints = 0;
  m_NCells = 0;
  m_NCellLinks=0;
  m_NCellData = 0;
  m_NPointData = 0;
  strcpy(m_PointDim, "ID x y ...");
  m_PointType = MET_FLOAT;
  m_PointDataType = MET_FLOAT;
  m_CellDataType = MET_FLOAT;
}
        
/** Destroy tube information */
void MetaMesh::
M_Destroy(void)
{
  MetaObject::M_Destroy();
}

/** Set Read fields */
void MetaMesh::
M_SetupReadFields(void)
{
  if(META_DEBUG) std::cout << "MetaMesh: M_SetupReadFields" << std::endl;

  MetaObject::M_SetupReadFields();

  MET_FieldRecordType * mF;
  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "NCellTypes", MET_INT,true);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "PointDim", MET_STRING, true);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "NPoints", MET_INT, true);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "PointType", MET_STRING, true);
  mF->required = true;
  m_Fields.push_back(mF);
 
  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "PointDataType", MET_STRING, true);
  mF->required = true;
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "CellDataType", MET_STRING, true);
  mF->required = true;
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Points", MET_NONE, true);
  mF->terminateRead = true;
  m_Fields.push_back(mF);

 
}

void MetaMesh::
M_SetupWriteFields(void)
{
  strcpy(m_ObjectTypeName,"Mesh");
  MetaObject::M_SetupWriteFields();

  MET_FieldRecordType * mF;
  
  char s[255];
  mF = new MET_FieldRecordType;
  MET_TypeToString(m_PointType, s);
  MET_InitWriteField(mF, "PointType", MET_STRING, strlen(s), s);
  m_Fields.push_back(mF);

  // Find the pointDataType
  if(m_PointData.size()>0)
    {
    m_PointDataType = (*m_PointData.begin())->GetMetaType();
    }

  char s1[255];
  mF = new MET_FieldRecordType;
  MET_TypeToString(m_PointDataType, s1);
  MET_InitWriteField(mF, "PointDataType", MET_STRING, strlen(s1), s1);
  m_Fields.push_back(mF);

  char s2[255];
  mF = new MET_FieldRecordType;
  MET_TypeToString(m_CellDataType, s2);
  MET_InitWriteField(mF, "CellDataType", MET_STRING, strlen(s2), s2);
  m_Fields.push_back(mF);

  unsigned int numberOfCellTypes = 0;
  for(unsigned int i=0;i<MET_NUM_CELL_TYPES;i++)
    {
    if(m_CellListArray[i]->size()>0)
      {
      numberOfCellTypes++;
      }
    }
  if(numberOfCellTypes)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "NCellTypes", MET_INT,numberOfCellTypes);
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



bool MetaMesh::
M_Read(void)
{
  if(META_DEBUG) std::cout << "MetaMesh: M_Read: Loading Header" << std::endl;

  if(!MetaObject::M_Read())
  {
    std::cout << "MetaMesh: M_Read: Error parsing file" << std::endl;
    return false;
  }

  if(META_DEBUG) std::cout << "MetaMesh: M_Read: Parsing Header" << std::endl;
 
  MET_FieldRecordType * mF;

  unsigned int numberOfCellTypes =0;
  mF = MET_GetFieldRecord("NCellTypes", &m_Fields);
  if(mF->defined)
  {
    numberOfCellTypes= (int)mF->value[0];
  }
 
  mF = MET_GetFieldRecord("NPoints", &m_Fields);
  if(mF->defined)
  {
    m_NPoints= (int)mF->value[0];
  }

  mF = MET_GetFieldRecord("PointType", &m_Fields);
  if(mF->defined)
  {
    MET_StringToType((char *)(mF->value), &m_PointType);
  }

  mF = MET_GetFieldRecord("PointDataType", &m_Fields);
  if(mF->defined)
  {
    MET_StringToType((char *)(mF->value), &m_PointDataType);
  }

  mF = MET_GetFieldRecord("CellDataType", &m_Fields);
  if(mF->defined)
  {
    MET_StringToType((char *)(mF->value), &m_CellDataType);
  }


  mF = MET_GetFieldRecord("PointDim", &m_Fields);
  if(mF->defined)
  {
    strcpy(m_PointDim,(char *)(mF->value));
  }

  int j;

  if(m_BinaryData)
  {
    int elementSize;
    MET_SizeOfType(m_PointType, &elementSize);
    int readSize = m_NPoints*(m_NDims)*elementSize+m_NPoints*sizeof(int);
    
    char* _data = new char[readSize];
    m_ReadStream->read((char *)_data, readSize);

    int gc = m_ReadStream->gcount();
    if(gc != readSize)
    {
      std::cout << "MetaMesh: m_Read: data not read completely" 
                << std::endl;
      std::cout << "   ideal = " << readSize << " : actual = " << gc << std::endl;
      return false;
    }

    int i=0;
    int d;
    double td;
    for(j=0; j<m_NPoints; j++) 
    {
      MeshPoint* pnt = new MeshPoint(m_NDims);

      MET_ValueToDouble(MET_INT, _data, i++, &td);
      pnt->m_Id=static_cast<int>(td);
      
      for(d=0; d<m_NDims; d++)
      {
        MET_ValueToDouble(m_PointType, _data, i++, &td);
        pnt->m_X[d] = (float)td;
      }

      m_PointList.push_back(pnt);
    }
    delete [] _data;
  }
  else
  {
    for(j=0; j<m_NPoints; j++) 
    {
      MeshPoint* pnt = new MeshPoint(m_NDims);
      
      float v[10];
      for(int k=0; k<m_NDims+1; k++)
      {
        *m_ReadStream >> v[k];
        m_ReadStream->get(); 
      }

      int d;
      pnt->m_Id=(int)v[0];
      for(d=0; d<m_NDims; d++)
      {
        pnt->m_X[d] = v[d+1];
      }
     
      m_PointList.push_back(pnt);
    }

      
    char c = ' ';
    while( (c!='\n') && (!m_ReadStream->eof()))
    {
      c = m_ReadStream->get();// to avoid unrecognize charactere
    }
  }
  

  // Now reading the cells
  for(unsigned int nCellType=0;nCellType<numberOfCellTypes;nCellType++)
    {
    MetaObject::ClearFields();
    mF = new MET_FieldRecordType;
    MET_InitReadField(mF, "CellType", MET_STRING, true);
    m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    MET_InitReadField(mF, "NCells", MET_INT, true);
    m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    MET_InitReadField(mF, "Cells", MET_NONE, true);
    mF->terminateRead = true;
    m_Fields.push_back(mF);
  
   if(!MET_Read(*m_ReadStream, & m_Fields))
      {
      std::cout << "MetaObject: Read: MET_Read Failed" << std::endl;
      return false;
      }

    mF = MET_GetFieldRecord("NCells", &m_Fields);
    if(mF->defined)
    {
      m_NCells= (int)mF->value[0];
    }

    MET_CellGeometry celltype = MET_VERTEX_CELL;
  
    mF = MET_GetFieldRecord("CellType", &m_Fields);
    if(mF->defined)
      {
      for(j=0;j<MET_NUM_CELL_TYPES;j++)
        {
        if(!strncmp((char *)(mF->value),MET_CellTypeName[j],3))
          {
          celltype=(MET_CellGeometry)j;
          }
        }
      }

    if(m_BinaryData)
    {
      unsigned int totalcellsize = (MET_CellSize[celltype]+1)*m_NCells;
      int readSize = totalcellsize*sizeof(int);
    
      char* _data = new char[readSize];
      m_ReadStream->read((char *)_data, readSize);

      int gc = m_ReadStream->gcount();
      if(gc != readSize)
      {
        std::cout << "MetaMesh: m_Read: data not read completely" 
                  << std::endl;
        std::cout << "   ideal = " << readSize << " : actual = " << gc << std::endl;
        return false;
      }

      int i=0;
      int d;
      double td;
      for(j=0; j<m_NCells; j++) 
      {
        int n = MET_CellSize[celltype];
        MeshCell* cell = new MeshCell(n);
      
        MET_ValueToDouble(MET_INT, _data, i++, &td);
        cell->m_Id = (int)td;

        for(d=0; d<n; d++)
        {
          MET_ValueToDouble(MET_INT, _data, i++, &td);
          cell->m_PointsId[d] = (int)td;
        }

        m_CellListArray[celltype]->push_back(cell);
      }
      delete [] _data;
    }
    else
      {
      for(j=0; j<m_NCells; j++) 
        {     
        int v;   
        int n = MET_CellSize[celltype];
        MeshCell* cell = new MeshCell(MET_CellSize[celltype]);
        
        *m_ReadStream >> v;
        m_ReadStream->get();
        cell->m_Id = v;

        for(int k=0; k<n; k++)
          {
          *m_ReadStream >> v;
          m_ReadStream->get();
          cell->m_PointsId[k] = v;
          }
        m_CellListArray[celltype]->push_back(cell);
        }

      
      char c = ' ';
      while( (c!='\n') && (!m_ReadStream->eof()))
      {
        c = m_ReadStream->get();// to avoid unrecognized characters
      }
    }
    }

  long int pos = m_ReadStream->tellg();

  // Now reading the cell links
  MetaObject::ClearFields();

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "NCellLinks", MET_INT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "CellLinksSize", MET_INT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "CellLinks", MET_NONE, false);
  mF->terminateRead = true;
  m_Fields.push_back(mF);
  
  if(!MET_Read(*m_ReadStream, & m_Fields,'=',false,false))
    {
    std::cout << "MetaObject: Read: MET_Read Failed" << std::endl;
    return false;
    }

  mF = MET_GetFieldRecord("NCellLinks", &m_Fields);
  if(mF->defined)
    {
    m_NCellLinks= (int)mF->value[0];
    }

  unsigned int totalCellLink=0;
  mF = MET_GetFieldRecord("CellLinksSize", &m_Fields);
  if(m_BinaryData)
    {
    if(mF->defined)
      {
      totalCellLink= (int)mF->value[0];
      }
    }

  if(m_BinaryData)
    {
    int readSize = totalCellLink*sizeof(int);
    
    char* _data = new char[readSize];
    m_ReadStream->read((char *)_data, readSize);

    int gc = m_ReadStream->gcount();
    if(gc != readSize)
      {
      std::cout << "MetaMesh: m_Read: data not read completely" 
                << std::endl;
      std::cout << "   ideal = " << readSize << " : actual = " << gc << std::endl;
      return false;
      }
    int i=0;
    int d;
    double td;
    for(j=0; j<m_NCellLinks; j++) 
      {
      MeshCellLink* link = new MeshCellLink();
    
      MET_ValueToDouble(MET_INT, _data, i++, &td);
      link->m_Id = (int)td;
        
      MET_ValueToDouble(MET_INT, _data, i++, &td);
      int n = (int)td;

      for(d=0; d<n; d++)
        {
        MET_ValueToDouble(MET_INT, _data, i++, &td);
        link->m_Links.push_back((int)td);
        }
      m_CellLinks.push_back(link);
      }
    delete [] _data;
    }
  else
    {
    for(j=0; j<m_NCellLinks; j++) 
      {     
      int v;   
      MeshCellLink* link = new MeshCellLink();
       
      *m_ReadStream >> v;
      m_ReadStream->get();
      link->m_Id = v;

      *m_ReadStream >> v;
      m_ReadStream->get();
      int count = v;

      for(int i=0;i<count;i++)
        {
        *m_ReadStream >> v;
        m_ReadStream->get();
        link->m_Links.push_back(v);
        }
      m_CellLinks.push_back(link);
      }
      
    if(m_NCellLinks > 0)
      {
      char c = ' ';
      while( (c!='\n') && (!m_ReadStream->eof()))
        {
        c = m_ReadStream->get();// to avoid unrecognized characters
        }
      }
    }

  if(m_NCellLinks == 0)
    {
    m_ReadStream->clear();
    m_ReadStream->seekg(pos,std::ios::beg);
    }
  pos = m_ReadStream->tellg();

   // Now reading the point data
  MetaObject::ClearFields();

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "NPointData", MET_INT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "PointDataSize", MET_INT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "PointData", MET_NONE, false);
  mF->terminateRead = true;
  m_Fields.push_back(mF);
  
  if(!MET_Read(*m_ReadStream, & m_Fields,'=',false,false))
    {
    std::cout << "MetaObject: Read: MET_Read Failed" << std::endl;
    return false;
    }
 
  mF = MET_GetFieldRecord("NPointData", &m_Fields);
  if(mF->defined)
    {
    m_NPointData= (int)mF->value[0];
    }

  unsigned int pointDataSize=0;
  mF = MET_GetFieldRecord("PointDataSize", &m_Fields);
  if(mF->defined)
    {
    pointDataSize= (int)mF->value[0];
    }

  char* _data = new char[pointDataSize];
  m_ReadStream->read((char *)_data, pointDataSize);

  unsigned int gc = m_ReadStream->gcount();
  if(gc != pointDataSize)
    {
    std::cout << "MetaMesh: m_Read: data not read completely" 
              << std::endl;
    std::cout << "   ideal = " << pointDataSize << " : actual = " << gc << std::endl;
    return false;
    }
  int i=0;
  double td;

  for(j=0; j<m_NPointData; j++)  
    {
    MeshDataBase* pd;
    
    unsigned int k;
    char* num = new char[sizeof(int)];
    for(k=0;k<sizeof(int);k++)
      {
      num[k] = _data[i+k];
      }
    td = (double)((int*)num)[0];

    delete [] num;
    i+=sizeof(int);

    int elementSize;
    MET_SizeOfType(m_PointDataType, &elementSize);
    num = new char[elementSize];
    for(k=0;k<static_cast<unsigned int>(elementSize);k++)
      {
      num[k] = _data[i+k];
      }
    i+=elementSize;

    if(m_PointDataType == MET_CHAR)
      {
      double val = (double)((char*)num)[0];
      pd = new MeshData<char>(); 
      static_cast<MeshData<char>*>(pd)->m_Data = (char)val; 
      }
    else if(m_PointDataType == MET_UCHAR)
      {
      double val = (double)((unsigned char*)num)[0];
      pd = new MeshData<unsigned char>();
      static_cast<MeshData<unsigned char>*>(pd)->m_Data = (unsigned char)val; 
      }
    else if(m_PointDataType == MET_SHORT)
      {
      double val = (double)((short*)num)[0];
      pd = new MeshData<short>();
      static_cast<MeshData<short>*>(pd)->m_Data = (short)val; 
      }
    else if(m_PointDataType == MET_USHORT)
      { 
      double val = (double)((unsigned short*)num)[0];
      pd = new MeshData<unsigned short>();
      static_cast<MeshData<unsigned short>*>(pd)->m_Data = (unsigned short)val; 
      }
    else if(m_PointDataType == MET_INT)
      {
      double val = (double)((int*)num)[0];
      pd = new MeshData<int>();
      static_cast<MeshData<int>*>(pd)->m_Data = (int)val; 
      }
    else if(m_PointDataType == MET_UINT)
      { 
      double val = (double)((char*)num)[0];
      pd = new MeshData<unsigned int>();
      static_cast<MeshData<unsigned int>*>(pd)->m_Data = (unsigned int)val; 
      }
    else if(m_PointDataType == MET_LONG)
      { 
      double val = (double)((long*)num)[0];
      pd = new MeshData<long>();
      static_cast<MeshData<long>*>(pd)->m_Data = (long)val; 
      }
    else if(m_PointDataType == MET_ULONG)
      { 
      double val = (double)((unsigned long*)num)[0];
      pd = new MeshData<unsigned long>();
      static_cast<MeshData<unsigned long>*>(pd)->m_Data = (unsigned long)val; 
      }
    else if(m_PointDataType == MET_FLOAT)
      { 
      double val = (double)((float*)num)[0];
      pd = new MeshData<float>();
      static_cast<MeshData<float>*>(pd)->m_Data = (float)val; 
      }
    else if(m_PointDataType == MET_DOUBLE)
      { 
      double val = (double)((double*)num)[0];
      pd = new MeshData<double>();
      static_cast<MeshData<double>*>(pd)->m_Data = val; 
      }
       
    delete [] num;
    pd->m_Id = (int)td;
    m_PointData.push_back(pd);
    }
  delete [] _data;

  // If no point data, reset the pointer to the stream to the previous position
  if(m_NPointData == 0)
    {
    m_ReadStream->clear();
    m_ReadStream->seekg(pos,std::ios::beg);
    }
  pos = m_ReadStream->tellg();

  // Now reading the cell data
  MetaObject::ClearFields();

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "NCellData", MET_INT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "CellDataSize", MET_INT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "CellData", MET_NONE, false);
  mF->terminateRead = true;
  m_Fields.push_back(mF);
  
  if(!MET_Read(*m_ReadStream, & m_Fields,'=',false,false))
    {
    std::cout << "MetaObject: Read: MET_Read Failed" << std::endl;
    return false;
    }

 
  mF = MET_GetFieldRecord("NCellData", &m_Fields);
  if(mF->defined)
    {
    m_NCellData= (int)mF->value[0];
    }

  unsigned int cellDataSize=0;
  mF = MET_GetFieldRecord("CellDataSize", &m_Fields);
  if(mF->defined)
    {
    cellDataSize= (int)mF->value[0];
    }

  char* _celldata = new char[cellDataSize];
  m_ReadStream->read((char *)_celldata, cellDataSize);

  unsigned int gcCell = m_ReadStream->gcount();
  if(gcCell != cellDataSize)
    {
    std::cout << "MetaMesh: m_Read: data not read completely" 
              << std::endl;
    std::cout << "   ideal = " << cellDataSize << " : actual = " << gcCell << std::endl;
    return false;
    }
  
  i=0;
  for(j=0; j<m_NCellData; j++)  
    {
    MeshDataBase* cd;

    unsigned int k;
    char* num = new char[sizeof(int)];
    for(k=0;k<sizeof(int);k++)
      {
      num[k] = _celldata[i+k];
      }
    td = (double)((int*)num)[0];

    delete [] num;
    i+=sizeof(int);

    int elementSize;
    MET_SizeOfType(m_CellDataType, &elementSize);
    num = new char[elementSize];
    for(k=0;k<static_cast<unsigned int>(elementSize);k++)
      {
      num[k] = _celldata[i+k];
      }
    i+=elementSize;

    if(m_CellDataType == MET_CHAR)
      { 
      double val = (double)((char*)num)[0];
      cd = new MeshData<char>(); 
      static_cast<MeshData<char>*>(cd)->m_Data = (char)val;
      }
    else if(m_CellDataType == MET_UCHAR)
      {
      double val = (double)((unsigned char*)num)[0];
      cd = new MeshData<unsigned char>();
      static_cast<MeshData<unsigned char>*>(cd)->m_Data = (unsigned char)val; 
      }
    else if(m_CellDataType == MET_SHORT)
      {
      double val = (double)((short*)num)[0];
      cd = new MeshData<short>();
      static_cast<MeshData<short>*>(cd)->m_Data = (short)val; 
      }
    else if(m_CellDataType == MET_USHORT)
      { 
      double val = (double)((unsigned short*)num)[0];
      cd = new MeshData<unsigned short>();
      static_cast<MeshData<unsigned short>*>(cd)->m_Data = (unsigned short)val; 
      }
    else if(m_CellDataType == MET_INT)
      {
      double val = (double)((int*)num)[0];
      cd = new MeshData<int>();
      static_cast<MeshData<int>*>(cd)->m_Data = (int)val; 
      }
    else if(m_CellDataType == MET_UINT)
      { 
      double val = (double)((unsigned int*)num)[0];
      cd = new MeshData<unsigned int>();
      static_cast<MeshData<unsigned int>*>(cd)->m_Data = (unsigned int)val; 
      }
    else if(m_CellDataType == MET_LONG)
      { 
      double val = (double)((long*)num)[0];
      cd = new MeshData<long>();
      static_cast<MeshData<long>*>(cd)->m_Data = (long)val; 
      }
    else if(m_CellDataType == MET_ULONG)
      { 
      double val = (double)((unsigned long*)num)[0];
      cd = new MeshData<unsigned long>();
      static_cast<MeshData<unsigned long>*>(cd)->m_Data = (unsigned long)val; 
      }
    else if(m_CellDataType == MET_FLOAT)
      { 
      double val = (double)((float*)num)[0];
      cd = new MeshData<float>();
      static_cast<MeshData<float>*>(cd)->m_Data = (float)val; 
      }
    else if(m_CellDataType == MET_DOUBLE)
      { 
      double val = (double)((double*)num)[0];
      cd = new MeshData<double>();
      static_cast<MeshData<double>*>(cd)->m_Data = val; 
      }
       
    delete [] num;

    cd->m_Id = (int)td;
    m_CellData.push_back(cd);
    }

  delete [] _celldata;

 // If no cell data, reset the pointer to the stream to the previous position
  if(m_NCellData == 0)
    {
    m_ReadStream->clear();
    m_ReadStream->seekg(pos,std::ios::beg);
    }

  return true;
}


bool MetaMesh::
M_Write(void)
{

  if(!MetaObject::M_Write())
  {
    std::cout << "MetaMesh: M_Write: Error parsing file" << std::endl;
    return false;
  }

  /** Then copy all points */
  if(m_BinaryData)
  {
    PointListType::const_iterator it = m_PointList.begin();
    int elementSize;
    MET_SizeOfType(m_PointType, &elementSize);

    char* data = new char[(m_NDims)*m_NPoints*elementSize+m_NPoints*sizeof(int)];
    int i=0;
    int d;
    while(it != m_PointList.end())
    {
      MET_DoubleToValue((double)(*it)->m_Id,MET_INT,data,i++);
      for(d = 0; d < m_NDims; d++)
      {
        MET_DoubleToValue((double)(*it)->m_X[d],m_PointType,data,i++);
      }
      it++;
    }  
    m_WriteStream->write((char *)data,(m_NDims+1)*m_NPoints*elementSize);
    m_WriteStream->write("\n",1);
    delete []data;
  }
  else
  {
    PointListType::const_iterator it = m_PointList.begin();
  
    int d;
    while(it != m_PointList.end())
    {
      *m_WriteStream << (*it)->m_Id << " ";
      for(d = 0; d < m_NDims; d++)
      {
        *m_WriteStream << (*it)->m_X[d] << " ";
      }

      *m_WriteStream << std::endl;
      it++;
    }
  }

  // Loop trough the array of cell types and write them if they exists
  for(unsigned int i=0;i<MET_NUM_CELL_TYPES;i++)
    {
    if(m_CellListArray[i]->size()>0)
      {
      // clear the fields and add new fields for a new write
      MetaObject::ClearFields();
      MET_FieldRecordType * mF;
      if(strlen(MET_CellTypeName[i])>0)
        {
        mF = new MET_FieldRecordType;
        MET_InitWriteField(mF, "CellType", MET_STRING,
                               strlen(MET_CellTypeName[i]),MET_CellTypeName[i]);
        m_Fields.push_back(mF);
        }

      m_NCells = m_CellListArray[i]->size();
      mF = new MET_FieldRecordType;
      MET_InitWriteField(mF, "NCells", MET_INT,m_NCells);
      m_Fields.push_back(mF);

      mF = new MET_FieldRecordType;
      MET_InitWriteField(mF, "Cells", MET_NONE);
      m_Fields.push_back(mF);


      if(!MetaObject::M_Write())
        {
        std::cout << "MetaMesh: M_Write: Error parsing file" << std::endl;
        return false;
        }

      /** Then copy all cells */
      if(m_BinaryData)
        {
        unsigned int totalCellsSize = m_CellListArray[i]->size()*(MET_CellSize[i]+1);
        char* data = new char[totalCellsSize*sizeof(int)];
        unsigned int d;
        int j=0;
        CellListType::const_iterator it = m_CellListArray[i]->begin();
        while(it != m_CellListArray[i]->end())
        {
        MET_DoubleToValue((double)(*it)->m_Id,MET_INT,data,j++);
        for(d = 0; d < (*it)->m_Dim; d++)
            {
            MET_DoubleToValue((double)(*it)->m_PointsId[d],MET_INT,data,j++);
            }
          it++;
        }
        m_WriteStream->write((char *)data,totalCellsSize*sizeof(int));
        m_WriteStream->write("\n",1);
        delete []data;
        }
      else
        {
        CellListType::const_iterator it = m_CellListArray[i]->begin();
  
        unsigned int d;
        while(it != m_CellListArray[i]->end())
          {
           *m_WriteStream << (*it)->m_Id << " ";
          for(d = 0; d < (*it)->m_Dim; d++)
            {
            *m_WriteStream << (*it)->m_PointsId[d] << " ";
            }

          *m_WriteStream << std::endl;
          it++;
          }
        }
    }
  }

  // Now write the cell links
  if(m_CellLinks.size()>0)
    {
    MetaObject::ClearFields();
    m_NCellLinks = m_CellLinks.size();
    MET_FieldRecordType * mF;
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "NCellLinks", MET_INT,m_NCellLinks);
    m_Fields.push_back(mF);

    int cellLinksSize = 0;
    if(m_BinaryData)
      {    
      CellLinkListType::const_iterator it = m_CellLinks.begin();
      while(it != m_CellLinks.end())
        {
        cellLinksSize += 2+(*it)->m_Links.size();
        it++;
        }
      mF = new MET_FieldRecordType;
      MET_InitWriteField(mF, "CellLinksSize", MET_INT,cellLinksSize);
      m_Fields.push_back(mF);
      }


    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "CellLinks", MET_NONE);
    m_Fields.push_back(mF);


    if(!MetaObject::M_Write())
      {
      std::cout << "MetaMesh: M_Write: Error parsing file" << std::endl;
      return false;
      }

     /** Then copy all cell links */
    if(m_BinaryData)
      {
      char* data = new char[cellLinksSize*sizeof(int)];
      int j=0;
      CellLinkListType::const_iterator it = m_CellLinks.begin();
      while(it != m_CellLinks.end())
        {
        MET_DoubleToValue((double)(*it)->m_Id,MET_INT,data,j++);
        MET_DoubleToValue((double)(*it)->m_Links.size(),MET_INT,data,j++);
        std::list<int>::const_iterator it2 = (*it)->m_Links.begin();
        while(it2 != (*it)->m_Links.end())
          {
          MET_DoubleToValue((double)(*it2),MET_INT,data,j++);
          it2++;
          }
        it++;
        }
        m_WriteStream->write((char *)data,cellLinksSize*sizeof(int));
        m_WriteStream->write("\n",1);
        delete []data;
      }
    else
      {
      CellLinkListType::const_iterator it = m_CellLinks.begin();
  
          while(it != m_CellLinks.end())
            {
             *m_WriteStream << (*it)->m_Id << " ";
             *m_WriteStream << (*it)->m_Links.size() << " ";
             std::list<int>::const_iterator it2 = (*it)->m_Links.begin();
             while(it2 != (*it)->m_Links.end())
              {
              *m_WriteStream << (*it2) << " ";
              it2++;
              }
            *m_WriteStream << std::endl;
            it++;
            }
          }
    }

  // Now write the point data
  // Point Data type is the same for the whole mesh
  if(m_PointData.size()>0)
    {
    MetaObject::ClearFields();
    m_NPointData = m_PointData.size();
    MET_FieldRecordType * mF;
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "NPointData", MET_INT,m_NPointData);
    m_Fields.push_back(mF);

    int pointDataSize = 0;    
    PointDataListType::const_iterator it = m_PointData.begin();
    while(it != m_PointData.end())
      {
      pointDataSize += (*it)->GetSize();
      it++;
      }

    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "PointDataSize", MET_INT,pointDataSize);
    m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "PointData", MET_NONE);
    m_Fields.push_back(mF);


    if(!MetaObject::M_Write())
      {
      std::cout << "MetaMesh: M_Write: Error parsing file" << std::endl;
      return false;
      }

    // Then copy all Point data : 
    // Always binary to be compatible with everything
    it = m_PointData.begin();
    while(it != m_PointData.end())
      {
      (*it)->Write(m_WriteStream);
      it++;
      }
    m_WriteStream->write("\n",1);

    }

  // Now write the cell data
  // Cell Data type is the same for the whole mesh
  if(m_CellData.size()>0)
    {
    MetaObject::ClearFields();
    m_NCellData = m_CellData.size();
    MET_FieldRecordType * mF;
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "NCellData", MET_INT,m_NCellData);
    m_Fields.push_back(mF);

    int cellDataSize = 0;    
    CellDataListType::const_iterator it = m_CellData.begin();
    while(it != m_CellData.end())
      {
      cellDataSize += (*it)->GetSize();
      it++;
      }

    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "CellDataSize", MET_INT,cellDataSize);
    m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "CellData", MET_NONE);
    m_Fields.push_back(mF);


    if(!MetaObject::M_Write())
      {
      std::cout << "MetaMesh: M_Write: Error parsing file" << std::endl;
      return false;
      }

    // Then copy all Cell data : 
    // Always binary to be compatible with everything
    it = m_CellData.begin();
    while(it != m_CellData.end())
      {
      (*it)->Write(m_WriteStream);
      it++;
      }
    m_WriteStream->write("\n",1);
    }

  return true;

}
