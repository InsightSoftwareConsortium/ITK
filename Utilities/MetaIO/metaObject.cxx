#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <string.h>
#include <math.h>


#include <metaUtils.h>
#include <metaObject.h>

int META_DEBUG = 0;
//
// MetaObject Constructors
//
MetaObject::
MetaObject(void)
  {
  m_NDims = 0;
  this->ClearFields();
  this->ClearUserFields();
  MetaObject::Clear();
  m_ReadStream = NULL;
  m_WriteStream = NULL;
  m_FileName[0] = '\0';
  }

MetaObject::
MetaObject(const char * _fileName)
  {
  m_NDims = 0;
  this->ClearFields();
  this->ClearUserFields();
  MetaObject::Clear();
  m_ReadStream = NULL;
  m_WriteStream = NULL;
  this->Read(_fileName);
  }

MetaObject::
MetaObject(unsigned int dim)
  {
  m_NDims = 0;
  this->ClearFields();
  this->ClearUserFields();
  MetaObject::Clear();
  m_ReadStream = NULL;
  m_WriteStream = NULL;
  InitializeEssential(dim);
  }


MetaObject::
~MetaObject(void)
{
  M_Destroy();
  delete m_ReadStream;
  delete m_WriteStream;
  
  this->ClearFields();
  this->ClearUserFields();
}


//
// Clear Fields only, if the pointer is in the UserField list it is not deleted.
void MetaObject::
ClearFields()
{
  if(META_DEBUG) std::cout << "MetaObject:ClearFields" << std::endl;

  FieldsContainerType::iterator  it  = m_Fields.begin();
  FieldsContainerType::iterator  end = m_Fields.end();
  while( it != end )
    {
    MET_FieldRecordType* field = *it;
    it++;
    
    // Check if the pointer is not in one of the user's list
    bool exists = false;
    FieldsContainerType::iterator  it2  = m_UserDefinedWriteFields.begin();
    FieldsContainerType::iterator  end2 = m_UserDefinedWriteFields.end();
    while( it2 != end2 )
    {
      if(*it2 == field)
        {
        exists = true;
        break;
        }
      it2++;
    }

    if(!exists)
      {
      FieldsContainerType::iterator  it2  = m_UserDefinedReadFields.begin();
      FieldsContainerType::iterator  end2 = m_UserDefinedReadFields.end();
      while( it2 != end2 )
      {
        if(*it2 == field)
          {
          exists = true;
          break;
          }
        it2++;
      }
    }
    
    if(!exists)
      {
      delete field;
      }
    }
  m_Fields.clear();
}


// Clear UserFields
void MetaObject
::ClearUserFields()
{
  // Clear write field
  FieldsContainerType::iterator  it  = m_UserDefinedWriteFields.begin();
  FieldsContainerType::iterator  end = m_UserDefinedWriteFields.end();
  while( it != end )
  {
    MET_FieldRecordType* field = *it;
    it++;
    delete field;
  }
   

  // Clear read field
  it  = m_UserDefinedReadFields.begin();
  end = m_UserDefinedReadFields.end();
  while( it != end )
  {
    MET_FieldRecordType* field = *it;
    
    // check if the pointer has not been deleted yet
    bool deleted = false;
    FieldsContainerType::iterator  it2  = m_UserDefinedWriteFields.begin();
    FieldsContainerType::iterator  end2 = m_UserDefinedWriteFields.end();
    while( it2 != end2 )
      {
      if(*it2 == *it)
        {
        deleted = true;  
        break;
        }
      it2++; 
      }
    
    it++;

    if(!deleted)
      {
      delete field;
      }
  }

  m_UserDefinedWriteFields.clear();
  m_UserDefinedReadFields.clear();
}
 
//
//
void MetaObject::
FileName(const char *_fileName)
  {
  if(_fileName != NULL)
    {
    if(_fileName[0] != '\0')
      {
      strcpy(m_FileName, _fileName);
      }
    }
  }

const char * MetaObject::
FileName(void) const
  {
  return m_FileName;
  }

void MetaObject::
CopyInfo(const MetaObject * _object)
  {
  if(NDims() != _object->NDims())
    {
    std::cout << "MetaObject: CopyInfo: Warning: NDims not same size" 
              << std::endl;
    }

  FileName(_object->FileName());
  Comment(_object->Comment());
  ObjectTypeName(_object->ObjectTypeName());
  ObjectSubTypeName(_object->ObjectSubTypeName());
  CenterOfRotation(_object->CenterOfRotation());
  Offset(_object->Offset());
  Rotation(_object->Rotation());
  ElementSpacing(_object->ElementSpacing());
  ID(_object->ID());
  Color(_object->Color());
  ParentID(_object->ParentID());
  Name(_object->Name());
  BinaryData(_object->BinaryData());
  BinaryDataByteOrderMSB(_object->BinaryDataByteOrderMSB());
  }

bool MetaObject::
Read(const char *_fileName)
  {
  if(META_DEBUG)  std::cout << "MetaObject: Read" << std::endl;

  if(_fileName != NULL)
    {
    strcpy(m_FileName, _fileName);
    }

  M_Destroy();

  Clear();

  M_SetupReadFields();

  if(!m_ReadStream)
  {
    m_ReadStream = new std::ifstream;
  }

  m_ReadStream->open(m_FileName);
  if(!m_ReadStream->is_open())
    {
    std::cout << "MetaObject: Read: Cannot open file" << std::endl;
    return false;
    }

  bool result = M_Read();

  m_ReadStream->close();
  m_ReadStream->clear();
  return result;
  }

bool MetaObject::
ReadStream(int _nDims, std::ifstream * _stream)
{
  if(META_DEBUG) std::cout << "MetaObject: ReadStream" << std::endl;

  M_Destroy();
  
  fflush(NULL);
  
  Clear();
  
  M_SetupReadFields();
 
  MET_FieldRecordType * mF = MET_GetFieldRecord("NDims", &m_Fields);
  mF->value[0] = _nDims;
  mF->defined = true;
 
  if(m_ReadStream)
  {
    delete m_ReadStream;
  }

  m_ReadStream = _stream;

  bool result = M_Read();
  m_ReadStream= NULL;
  return result;
}



bool MetaObject::
Write(const char *_fileName)
  {
  if(_fileName != NULL)
    {
    FileName(_fileName);
    }

  M_SetupWriteFields();

  if(!m_WriteStream)
  {
    m_WriteStream = new std::ofstream;
  }  
 
  m_WriteStream->open(m_FileName);
  if(!m_WriteStream->is_open())
  {
    return false;
  }

  bool result = M_Write();

  m_WriteStream->close();

  return result;
  }

//
//
void MetaObject::
PrintInfo(void) const
  {
  int i, j;

  std::cout << "FileName = _" << m_FileName << "_" << std::endl;
  std::cout << "Comment = _" << m_Comment << "_" << std::endl;
  std::cout << "ObjectType = _" << m_ObjectTypeName << "_" << std::endl;
  std::cout << "ObjectSubType = _" << m_ObjectSubTypeName << "_" << std::endl;
  std::cout << "NDims = " << m_NDims << std::endl;
  std::cout << "Name = " << m_Name << std::endl;
  std::cout << "ID = " << m_ID << std::endl;
  std::cout << "ParentID = " << m_ParentID << std::endl;
  if(m_BinaryData)
    std::cout << "BinaryData = True" << std::endl;
  else
    std::cout << "BinaryData = False" << std::endl;
  if(m_BinaryData && m_BinaryDataByteOrderMSB)
    std::cout << "BinaryDataByteOrderMSB = True" << std::endl;
  else
    std::cout << "BinaryDataByteOrderMSB = False" << std::endl;
  std::cout << "Color = " ;
  for(i=0; i<4; i++)
    {
    std::cout << m_Color[i] << " ";
    }
  std::cout << std::endl;

  std::cout << "Offset = ";
  for(i=0; i<m_NDims; i++)
    {
    std::cout << m_Offset[i] << " ";
    }
  std::cout << std::endl;
  
  std::cout << "Rotation = ";
  std::cout << std::endl;
  for(i=0; i<m_NDims; i++)
    {
    for(j=0; j<m_NDims; j++)
      { 
      std::cout << m_Rotation[i*m_NDims+j] << " ";
      }
    std::cout << std::endl;
    }

  std::cout << "CenterOfRotation = ";
  std::cout << std::endl;
  for(i=0; i<m_NDims; i++)
    {
    std::cout << m_CenterOfRotation[i] << " ";
    }
  std::cout << std::endl;

  std::cout << "ElementSpacing = ";
  for(i=0; i<m_NDims; i++)
    {
    std::cout << m_ElementSpacing[i] << " ";
    }
  std::cout << std::endl;


  // Print User's fields : 
  FieldsContainerType::const_iterator  itw = m_UserDefinedWriteFields.begin();
  FieldsContainerType::const_iterator  itr  = m_UserDefinedReadFields.begin();
  FieldsContainerType::const_iterator  endw = m_UserDefinedWriteFields.end();
  FieldsContainerType::const_iterator it;
   while( itw != endw )
   { 

     if((*itw)->defined)
     {
       it=itw;
     }
     else
     {
       it=itr; 
     }
     
     printf("%s: ",(*it)->name);

     if((*it)->type == MET_STRING)
     {
       printf("%s",(char *) (*it)->value);
     }
     else if(
       (*it)->type == MET_ASCII_CHAR ||
       (*it)->type == MET_CHAR ||
       (*it)->type == MET_UCHAR ||
       (*it)->type == MET_SHORT ||
       (*it)->type == MET_USHORT ||
       (*it)->type == MET_INT ||
       (*it)->type == MET_UINT ||
       (*it)->type == MET_FLOAT ||
       (*it)->type == MET_DOUBLE
     )
     {
       printf("%s : %f\n",(*it)->name,(*it)->value[0]);
     }
     else if(
       (*it)->type ==MET_CHAR_ARRAY || 
       (*it)->type ==MET_UCHAR_ARRAY ||
       (*it)->type ==MET_SHORT_ARRAY ||
       (*it)->type ==MET_USHORT_ARRAY ||
       (*it)->type ==MET_INT_ARRAY ||
       (*it)->type ==MET_UINT_ARRAY ||
       (*it)->type ==MET_FLOAT_ARRAY ||
       (*it)->type ==MET_DOUBLE_ARRAY
     )
     {
        for(i=0; i<(*it)->length; i++)
        {
          printf("%f ",(*it)->value[i]);
        }    
     }
     else if((*it)->type == MET_FLOAT_MATRIX)
     {
       std::cout << std::endl;
       for(i=0; i<(*it)->length*(*it)->length; i++)
       {
         printf("%f ",(*it)->value[i]);
         if(i==(*it)->length-1)
         {
           std::cout << std::endl;
         }
       }
     }

     std::cout << std::endl;
     
     itw++;
     itr++;
   }

  }

const char * MetaObject::
Comment(void) const
  {
  return m_Comment;
  }

void MetaObject::
Comment(const char * _comment)
  {
  strcpy(m_Comment, _comment);
  }

const char * MetaObject::
ObjectTypeName(void) const
  {
  return (char *)m_ObjectTypeName;
  }

void MetaObject::
ObjectTypeName(const char * _objectTypeName)
  {
  strcpy(m_ObjectTypeName, _objectTypeName);
  }

const char * MetaObject::
ObjectSubTypeName(void) const
  {
  return m_ObjectSubTypeName;
  }

void MetaObject::
ObjectSubTypeName(const char * _objectSubTypeName)
  {
  strcpy(m_ObjectSubTypeName, _objectSubTypeName);
  }

int MetaObject::
NDims(void) const
  {
  return m_NDims;
  }

const float * MetaObject::
Offset(void) const
  {
  return m_Offset;
  }

float MetaObject::
Offset(int _i) const
  {
  return m_Offset[_i];
  }

void MetaObject::
Offset(const float * _position)
  {
  int i;
  for(i=0; i<m_NDims; i++)
    {
    m_Offset[i] = _position[i];
    }
  }

void MetaObject::
Offset(int _i, float _value)
  {
  m_Offset[_i] = _value;
  }


const float * MetaObject::
Position(void) const
  {
  return m_Offset;
  }

float MetaObject::
Position(int _i) const
  {
  return m_Offset[_i];
  }

void MetaObject::
Position(const float * _position)
  {
  int i;
  for(i=0; i<m_NDims; i++)
    {
    m_Offset[i] = _position[i];
    }
  }

void MetaObject::
Position(int _i, float _value)
  {
  m_Offset[_i] = _value;
  }

const float * MetaObject::
Origin(void) const
  {
  return m_Offset;
  }

float MetaObject::
Origin(int _i) const
  {
  return m_Offset[_i];
  }

void MetaObject::
Origin(const float * _position)
  {
  int i;
  for(i=0; i<m_NDims; i++)
    {
    m_Offset[i] = _position[i];
    }
  }

void MetaObject::
Origin(int _i, float _value)
  {
  m_Offset[_i] = _value;
  }

//
//
const float * MetaObject::
Rotation(void) const
  {
  return m_Rotation;
  }

float MetaObject::
Rotation(int _i, int _j) const
  {
  return m_Rotation[_i*m_NDims+_j];
  }

void MetaObject::
Rotation(const float * _orientation)
  {
  int i;
  for(i=0; i<m_NDims*m_NDims; i++)
    {
    m_Rotation[i] = _orientation[i];
    }
  }

void MetaObject::
Rotation(int _i, int _j, float _value)
  {
  m_Rotation[_i*m_NDims+_j] = _value;
  }

//
const float * MetaObject::
Orientation(void) const
  {
  return m_Rotation;
  }

float MetaObject::
Orientation(int _i, int _j) const
  {
  return m_Rotation[_i*m_NDims+_j];
  }

void MetaObject::
Orientation(const float * _orientation)
  {
  int i;
  for(i=0; i<m_NDims*m_NDims; i++)
    {
    m_Rotation[i] = _orientation[i];
    }
  }

void MetaObject::
Orientation(int _i, int _j, float _value)
  {
  m_Rotation[_i*m_NDims+_j] = _value;
  }

//
//
const float * MetaObject::
CenterOfRotation(void) const
  {
  return m_CenterOfRotation;
  }

float MetaObject::
CenterOfRotation(int _i) const
  {
  return m_CenterOfRotation[_i];
  }

void MetaObject::
CenterOfRotation(const float * _position)
  {
  int i;
  for(i=0; i<m_NDims; i++)
    {
    m_CenterOfRotation[i] = _position[i];
    }
  }

void MetaObject::
CenterOfRotation(int _i, float _value)
  {
  m_CenterOfRotation[_i] = _value;
  }

//
//
const char * MetaObject::
AnatomicalOrientationAcronym(void) const
  {
  static char str[10];
  int i;
  for(i=0; i<m_NDims; i++)
    {
    str[i] = MET_OrientationTypeName[m_AnatomicalOrientation[i]][0];
    }
  str[i] = '\0';
  return str;
  }

const MET_OrientationEnumType * MetaObject::
AnatomicalOrientation(void) const
  {
  return m_AnatomicalOrientation;
  }

MET_OrientationEnumType MetaObject::
AnatomicalOrientation(int _dim) const
  {
  return m_AnatomicalOrientation[_dim];
  }

void MetaObject::
AnatomicalOrientation(const char *_ao)
  {
  int i, j;
  for(i=0; i<m_NDims; i++)
    {
    for(j=0; j<MET_NUM_ORIENTATION_TYPES; j++)
      {
      if(_ao[i] == MET_OrientationTypeName[j][0])
        {
        m_AnatomicalOrientation[i] = (MET_OrientationEnumType)j;
        break;
        }
      }
    if(j == MET_NUM_ORIENTATION_TYPES)
      {
      m_AnatomicalOrientation[i] = MET_ORIENTATION_UNKNOWN;
      }
    }
  }

void MetaObject::
AnatomicalOrientation(const MET_OrientationEnumType *_ao)
  {
  int i;
  for(i=0; i<m_NDims; i++)
    {
    m_AnatomicalOrientation[i] = _ao[i];
    }
  }

void MetaObject::
AnatomicalOrientation(int _dim, MET_OrientationEnumType _ao)
  {
  m_AnatomicalOrientation[_dim] = _ao;
  }

void MetaObject::
AnatomicalOrientation(int _dim, char _ao)
  {
  int j;
  for(j=0; j<MET_NUM_ORIENTATION_TYPES; j++)
    {
    if(_ao == MET_OrientationTypeName[j][0])
      {
      m_AnatomicalOrientation[_dim] = (MET_OrientationEnumType)j;
      return;
      }
    }

  m_AnatomicalOrientation[_dim] = MET_ORIENTATION_UNKNOWN;
  }

//
//
const float * MetaObject::
ElementSpacing(void) const
  {
  return m_ElementSpacing;
  }

float MetaObject::
ElementSpacing(int _i) const
  {
  return m_ElementSpacing[_i];
  }

void MetaObject::
ElementSpacing(const float * _elementSpacing)
  {
  int i;
  for(i=0; i<m_NDims; i++)
    {
    m_ElementSpacing[i] = _elementSpacing[i];
    }
  }

void MetaObject::
ElementSpacing(int _i, float _value)
  {
  m_ElementSpacing[_i] = _value;
  }


void  MetaObject::
Name(const char *_Name)
{
  if(_Name != NULL)
  {
    strcpy(m_Name, _Name);
  }
}
      
const char  * MetaObject::
Name(void) const
{
  return m_Name;
}

   
const float * MetaObject::
Color(void) const
{
  return m_Color;
}

void  MetaObject::
Color(float _r, float _g, float _b, float _a)
{
  m_Color[0] = _r;
  m_Color[1] = _g;
  m_Color[2] = _b;
  m_Color[3] = _a;
}
void MetaObject::
Color(const float * _color)
{
  for(unsigned int i=0; i<4; i++)
  {
    m_Color[i] = _color[i];
  }
}


void  MetaObject::
ID(int _id)
{
  m_ID = _id;
}
      
int  MetaObject::
ID(void) const
{
  return m_ID;
}

void  MetaObject::
ParentID(int _parentId)
{
  m_ParentID = _parentId;
}
      
int   MetaObject::ParentID(void) const
{
  return m_ParentID;
}


void  MetaObject::BinaryData(bool _binaryData)
{
  m_BinaryData = _binaryData;
}
      
bool   MetaObject::BinaryData(void) const
{
  return m_BinaryData;
}

bool MetaObject::
BinaryDataByteOrderMSB(void) const
  {
  return m_BinaryDataByteOrderMSB;
  }

void MetaObject::
BinaryDataByteOrderMSB(bool _elementByteOrderMSB)
  {
  m_BinaryDataByteOrderMSB = _elementByteOrderMSB;
  }

void MetaObject::
Clear(void)
  {
  if(META_DEBUG)  std::cout << "MetaObject: Clear()" << std::endl;
  strcpy(m_Comment, "");
  strcpy(m_ObjectTypeName, "");
  strcpy(m_ObjectSubTypeName, "");
  strcpy(m_Name, "");

  memset(m_Offset, 0, 10*sizeof(float));
  memset(m_Rotation, 0, 100*sizeof(float));
  memset(m_CenterOfRotation, 0, 10*sizeof(float));
  memset(m_Color, 0, 4*sizeof(float));

  m_ID = -1;
  m_Color[0]=1.0;
  m_Color[1]=1.0;
  m_Color[2]=1.0;
  m_Color[3]=1.0; // white by default
  m_ParentID = -1;
  m_BinaryData = false;
  m_BinaryDataByteOrderMSB = MET_SystemByteOrderMSB();

  if(META_DEBUG) 
    {
    std::cout << "MetaObject: Clear: m_NDims=" << m_NDims << std::endl;
    }
  int i;
  for(i=0; i<10; i++)
    {
    m_ElementSpacing[i] = 1;
    m_AnatomicalOrientation[i] = MET_ORIENTATION_UNKNOWN;
    }
/*
  std::vector<MET_FieldRecordType *>::iterator fieldIter;
  for(fieldIter=m_Fields.begin(); fieldIter!=m_Fields.end(); fieldIter++)
    {
    if(META_DEBUG) std::cout << "field = " << (*fieldIter)->name << std::endl;
    MET_FieldRecordType* field = *fieldIter;
    delete field;
    field = NULL;
    if(META_DEBUG) std::cout << " has been deleted." << std::endl;
    }
  m_Fields.clear();*/
  this->ClearFields();
  }

bool MetaObject::
InitializeEssential(int _nDims)
  {
  if(META_DEBUG) std::cout << "MetaObject: Initialize" << std::endl;

  M_Destroy();

  if(_nDims > 10)
    {
    std::cout 
      << "MetaObject: Initialize: Warning: Number of dimensions limited to 10" 
      << std::endl
      << "Resetting number of dimensions to 10"
      << std::endl;
    _nDims = 10;
    }

  if(_nDims < 0)
    {
    std::cout
      << "MetaObject: Initialize: Warning: Number of dimensions must be >= 0"
      << std::endl
      << "Resetting number of dimensions to 0"
      << std::endl;
    _nDims = 0;
    }

  m_NDims = _nDims;

  return true;
  }

void MetaObject::
M_Destroy(void)
  {
  if(META_DEBUG) std::cout << "MetaObject: Destroy" << std::endl;
  }

void MetaObject::
M_SetupReadFields(void)
  {
  this->ClearFields();
  if(META_DEBUG) std::cout << "MetaObject: M_SetupReadFields" << std::endl;

  MET_FieldRecordType * mF;

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Comment", MET_STRING, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ObjectType", MET_STRING, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ObjectSubType", MET_STRING, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "NDims", MET_INT, true);
  mF->required = true;
  m_Fields.push_back(mF);

  int nDimsRecordNumber = MET_GetFieldRecordNumber("NDims", &m_Fields);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Name", MET_STRING, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ID", MET_INT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ParentID", MET_INT, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "BinaryData", MET_STRING, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementByteOrderMSB", MET_STRING, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "BinaryDataByteOrderMSB", MET_STRING, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Color", MET_FLOAT_ARRAY, false,-1,4);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Position", MET_FLOAT_ARRAY, false,
                     nDimsRecordNumber);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Origin", MET_FLOAT_ARRAY, false,
                     nDimsRecordNumber);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Offset", MET_FLOAT_ARRAY, false,
                     nDimsRecordNumber);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Orientation", MET_FLOAT_MATRIX, false,
                    nDimsRecordNumber);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "Rotation", MET_FLOAT_MATRIX, false,
                    nDimsRecordNumber);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "CenterOfRotation", MET_FLOAT_ARRAY, false,
                     nDimsRecordNumber);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "AnatomicalOrientation", MET_STRING, false);
  m_Fields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementSpacing", MET_FLOAT_ARRAY, false,
                     nDimsRecordNumber);
  mF->required = false;
  m_Fields.push_back(mF);

  // Add User's field
  FieldsContainerType::iterator  it  = m_UserDefinedReadFields.begin();
  FieldsContainerType::iterator  end = m_UserDefinedReadFields.end();
  while( it != end )
  {
    m_Fields.push_back(*it); 
    it++;
  }


 }


void MetaObject::
M_SetupWriteFields(void)
  {
  if(META_DEBUG) std::cout << "MetaObject: M_SetupWriteFields" << std::endl;

  this->ClearFields();

  if(META_DEBUG) std::cout << "MetaObject: M_SetupWriteFields: Creating Fields"
                           << std::endl;

  MET_FieldRecordType * mF;

  if(strlen(m_Comment)>0)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "Comment", MET_STRING, strlen(m_Comment), m_Comment);
    m_Fields.push_back(mF);
    }

  if(strlen(m_ObjectTypeName)>0)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ObjectType", MET_STRING, strlen(m_ObjectTypeName),
                      m_ObjectTypeName);
    m_Fields.push_back(mF);
    }

  if(strlen(m_ObjectSubTypeName)>0)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ObjectSubType", MET_STRING,
                      strlen(m_ObjectSubTypeName),
                      m_ObjectSubTypeName);
    m_Fields.push_back(mF);
    }

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "NDims", MET_INT, m_NDims);
  m_Fields.push_back(mF);
  
  if(strlen(m_Name)>0)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "Name", MET_STRING,
                      strlen(m_Name),m_Name);
    m_Fields.push_back(mF);
    }

  if(m_ID >= 0)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ID", MET_INT, m_ID);
    m_Fields.push_back(mF);
    }

  if(m_ParentID >= 0)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ParentID", MET_INT, m_ParentID);
    m_Fields.push_back(mF);
    }

  if(m_BinaryData)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "BinaryData", MET_STRING, strlen("True"), "True");
    m_Fields.push_back(mF);

    mF = new MET_FieldRecordType;
    if(m_BinaryDataByteOrderMSB)
      MET_InitWriteField(mF, "BinaryDataByteOrderMSB", MET_STRING,
                         strlen("True"), "True");
    else
      MET_InitWriteField(mF, "BinaryDataByteOrderMSB", MET_STRING,
                         strlen("False"), "False");
    m_Fields.push_back(mF);
    }

  bool valSet = false;
  int i;
  for(i=0; i<m_NDims; i++)
    {
    if(m_Offset[i] != 0)
      {
      valSet = true;
      break;
      }
    }
  if(valSet)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "Offset", MET_FLOAT_ARRAY, m_NDims,
                       m_Offset);
    m_Fields.push_back(mF);
    }

  valSet = false;
  for(i=0; i<4; i++)
    {
    if(m_Color[i] != 1)
      {
      valSet = true;
      break;
      }
    }
  if(valSet)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "Color", MET_FLOAT_ARRAY, 4,
                         m_Color);
    m_Fields.push_back(mF);
    }
  
  valSet = false;

  // check for identity matrix
  bool isIdentity = true ;
  for(i=0; i<m_NDims*m_NDims; i++)
    {
    if( (i % m_NDims) == (int) (i / m_NDims) )
      {
        if ( m_Rotation[i] != 1 )
          {
            isIdentity = false ;
            break;
          }
      }
    else
      {
        if ( m_Rotation[i] != 0 )
          {
            isIdentity = false ;
            break ;
          }
      }
    }

  // check for zero matrix
  bool isAllZero = true ;
  if(!isIdentity)
    {
      for(i=0; i<m_NDims*m_NDims; i++)
        {
          if ( m_Rotation[i] != 0 )
            {
              isAllZero = false ;
              break;
            }
        }
    }

  if(!isIdentity && !isAllZero)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "Rotation", MET_FLOAT_MATRIX, m_NDims,
                       m_Rotation);
    m_Fields.push_back(mF);
    }

  for(i=0; i<m_NDims; i++)
    {
    if(m_CenterOfRotation[i] != 0)
      {
      valSet = true;
      break;
      }
    }
  if(valSet)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "CenterOfRotation", MET_FLOAT_ARRAY, m_NDims,
                       m_CenterOfRotation);
    m_Fields.push_back(mF);
    }

  if(m_AnatomicalOrientation[0] != MET_ORIENTATION_UNKNOWN)
    {
    const char * str = AnatomicalOrientationAcronym();
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "AnatomicalOrientation",
                       MET_STRING, strlen(str), str);
    m_Fields.push_back(mF);
    }

  valSet = false;
  for(i=0; i<m_NDims; i++)
    {
    if(m_ElementSpacing[i] != 0 && m_ElementSpacing[i] != 1)
      {
      valSet = true;
      break;
      }
    }
  if(valSet)
    {
    mF = new MET_FieldRecordType;
    MET_InitWriteField(mF, "ElementSpacing", MET_FLOAT_ARRAY, m_NDims,
                       m_ElementSpacing);
    m_Fields.push_back(mF);
    }

   // Add User's field
   FieldsContainerType::iterator  it  = m_UserDefinedWriteFields.begin();
   FieldsContainerType::iterator  end = m_UserDefinedWriteFields.end();
   while( it != end )
   {
     //std::cout << "Adding " << (*it)->name << std::endl;
     m_Fields.push_back(*it); 
     it++;
   }
  }

bool MetaObject::
M_Read(void)
  {

  if(!MET_Read(*m_ReadStream, & m_Fields))
    {
    std::cout << "MetaObject: Read: MET_Read Failed" << std::endl;
    return false;
    }

  MET_FieldRecordType * mF;

  mF = MET_GetFieldRecord("Comment", &m_Fields);
  if(mF && mF->defined)
    {
    strcpy(m_Comment, (char *)(mF->value));
    }

  mF = MET_GetFieldRecord("ObjectType", &m_Fields);
  if(mF && mF->defined)
    {
    strcpy(m_ObjectTypeName, (char *)(mF->value));
    }

  mF = MET_GetFieldRecord("ObjectSubType", &m_Fields);
  if(mF && mF->defined)
    {
    strcpy(m_ObjectSubTypeName, (char *)(mF->value));
    }

  mF = MET_GetFieldRecord("NDims", &m_Fields);
  if(mF && mF->defined)
    {
    m_NDims = (int)mF->value[0];
    }

  if(m_NDims>0)
    {
    MetaObject::InitializeEssential(m_NDims);
    }
 
  mF = MET_GetFieldRecord("Name", &m_Fields);
  if(mF && mF->defined)
    {
    strcpy(m_Name, (char *)(mF->value));
    }

  mF = MET_GetFieldRecord("ID", &m_Fields);
  if(mF && mF->defined)
    {
    m_ID = (int)mF->value[0];
    }

  mF = MET_GetFieldRecord("ParentID", &m_Fields);
  if(mF && mF->defined)
    {
    m_ParentID = (int)mF->value[0];
    }

  mF = MET_GetFieldRecord("BinaryData",  &m_Fields);
  if(mF && mF->defined)
    {
    if(((char *)(mF->value))[0] == 'T' || ((char *)(mF->value))[0] == 't' 
       || ((char *)(mF->value))[0] == '1')
      m_BinaryData = true;
    else
      m_BinaryData = false;
    }
  else
    {
    m_BinaryData = false;
    }

  mF = MET_GetFieldRecord("ElementByteOrderMSB",  &m_Fields);
  if(mF && mF->defined)
    {
    if(((char *)(mF->value))[0] == 'T' || ((char *)(mF->value))[0] == 't' 
       || ((char *)(mF->value))[0] == '1')
      m_BinaryDataByteOrderMSB = true;
    else
      m_BinaryDataByteOrderMSB = false;
    }

  mF = MET_GetFieldRecord("BinaryDataByteOrderMSB",  &m_Fields);
  if(mF && mF->defined)
    {
    if(((char *)(mF->value))[0] == 'T' || ((char *)(mF->value))[0] == 't' 
       || ((char *)(mF->value))[0] == '1')
      m_BinaryDataByteOrderMSB = true;
    else
      m_BinaryDataByteOrderMSB = false;
    }

  int i;
  mF = MET_GetFieldRecord("Color", &m_Fields);
  if(mF && mF->defined)
    {
    for(i=0; i<mF->length; i++)
      {
      m_Color[i] = static_cast<float>( mF->value[i] );
      }
    }
  else
    {
    for(i=0; i<mF->length; i++)
      {
      m_Color[i] = static_cast<unsigned int>( 1 );
      }
    }

  mF = MET_GetFieldRecord("Position", &m_Fields);
  if(mF && mF->defined)
    {
    for(i=0; i<mF->length; i++)
      {
      m_Offset[i] = static_cast<float>( mF->value[i] );
      }
    }
  mF = MET_GetFieldRecord("Offset", &m_Fields);
  if(mF && mF->defined)
    {
    for(i=0; i<mF->length; i++)
      {
      m_Offset[i] = static_cast<float>( mF->value[i] );
      }
    }
  mF = MET_GetFieldRecord("Origin", &m_Fields);
  if(mF && mF->defined)
    {
    for(i=0; i<mF->length; i++)
      {
      m_Offset[i] = static_cast<float>( mF->value[i] );
      }
    }

  bool rotationDefined = false;
  mF = MET_GetFieldRecord("Orientation", &m_Fields);
  if(mF && mF->defined)
    {
    rotationDefined = true;
    int len = mF->length;
    for(i=0; i<len*len; i++)
      {
      m_Rotation[i] = static_cast<float>( mF->value[i] );
      }
    }
  mF = MET_GetFieldRecord("Rotation", &m_Fields);
  if(mF && mF->defined)
    {
    rotationDefined = true;
    int len = mF->length;
    for(i=0; i<len*len; i++)
      {
      m_Rotation[i] = static_cast<float>( mF->value[i] );
      }
    }
  if(!rotationDefined)
    {
    for(i=0; i<m_NDims; i++)
      {
      m_Rotation[i+i*m_NDims] = 1;
      }
    }

  mF = MET_GetFieldRecord("AnatomicalOrientation", &m_Fields);
  if(mF && mF->defined)
    {
    AnatomicalOrientation((char *)(mF->value));
    }

  mF = MET_GetFieldRecord("ElementSpacing", &m_Fields);
  if(mF && mF->defined)
    {
    for(i=0; i<mF->length; i++)
      {
      m_ElementSpacing[i] = static_cast<float>( mF->value[i] );
      if (META_DEBUG) 
        std::cout << "metaObject: M_Read: elementSpacing[" << i << "] = " 
                  << m_ElementSpacing[i] << std::endl;
      }
    }
  else
    {
    for(i=0; i<mF->length; i++)
      {
      m_ElementSpacing[i] = 1;
      if (META_DEBUG) 
        std::cout << "metaObject: M_Read: elementSpacing[" << i << "] = " 
                  << m_ElementSpacing[i] << std::endl;
      }
    }

   // Set the read record field in the m_UserDefinedWriteFields
   FieldsContainerType::iterator  it  = m_UserDefinedReadFields.begin();
   FieldsContainerType::iterator  end = m_UserDefinedReadFields.end();
   while( it != end )
   {
     mF = MET_GetFieldRecord((*it)->name, &m_Fields);
     m_UserDefinedWriteFields.push_back(mF);
     it++;
   }
  
  return true;
  }

bool MetaObject::
M_Write(void)
  {
  if(!MET_Write(*m_WriteStream, & m_Fields))
    {
    std::cout << "MetaObject: Write: MET_Write Failed" << std::endl;
    return false;
    }

  return true;
  }


bool MetaObject
::Append(const char *_headName)
{
  if(META_DEBUG) std::cout << "MetaObject: Append" << std::endl;

  if(_headName != NULL)
  {
    FileName(_headName);
  }

  M_SetupWriteFields();

  if(!m_WriteStream)
  {
    m_WriteStream = new std::ofstream;
  }
  m_WriteStream->open(m_FileName, std::ios::binary | std::ios::app | std::ios::out);
  if(!m_WriteStream->is_open())
  {
    return false;
  }

  M_Write();
  
  m_WriteStream->close();
  return true;

}



// Get the user field
void* MetaObject
::GetUserField(const char* _name)
{ 
  FieldsContainerType::iterator  it  = m_UserDefinedWriteFields.begin();
  FieldsContainerType::iterator  end = m_UserDefinedWriteFields.end();
  while( it != end )
  {
    int eSize;
    MET_SizeOfType((*it)->type, &eSize);
    const unsigned int itLength = 
                static_cast<unsigned int>( (*it)->length );
    void * out;
    if(!strcmp((*it)->name,_name))
      {
      if((*it)->type == MET_STRING)
        {
        out = (void*) (new char[(itLength+1)*eSize] );
        memcpy( out, (*it)->value, itLength * eSize );
        static_cast<char*>(out)[itLength]=0;
        }
      else if((*it)->type == MET_FLOAT_MATRIX)
        {
        const unsigned int numMatrixElements = itLength * itLength;
        out = (void*) (new char[numMatrixElements*eSize] );
        for( unsigned int i=0; i < numMatrixElements; i++ )
          {
          MET_DoubleToValue((*it)->value[i],(*it)->type,out,i);
          }
        }
      else
        {
        out = (void*) (new char[itLength*eSize] );
        for( unsigned int i=0; i < itLength; i++ )
          {
          MET_DoubleToValue((*it)->value[i],(*it)->type,out,i);
          }
        }
      return out;
      }
    it++;
  }
  return NULL;
}


bool MetaObject
::AddUserField(const char* _fieldName,MET_ValueEnumType _type,int _length,
                        bool _required,int _dependsOn)
{
  MET_FieldRecordType* mFr = new MET_FieldRecordType;
  MET_InitReadField(mFr,_fieldName, _type, _required,_dependsOn,_length);
  m_UserDefinedReadFields.push_back(mFr);
  return 1;
}
