#include <stdio.h>
#include <iostream>
#include <ctype.h>

#include <sys/stat.h>
#include <fcntl.h>

#ifndef _WIN32
#include <unistd.h>
#include <arpa/inet.h>
#endif

#include <metaTypes.h>
#include <metaUtils.h>


char MET_SeperatorChar = '=';

bool MET_SystemByteOrderMSB(void)
  {
  const int l = 1;
  const char * u = (char *) & l;
  if (u[0])
    {
    return false;
    }
   else
    {
    return true;
    }
  }

MET_FieldRecordType * 
MET_GetFieldRecord(const char * _fieldName,
                   std::vector<MET_FieldRecordType *> * _fields)
  {
  std::vector<MET_FieldRecordType *>::iterator fieldIter;
  for(fieldIter=_fields->begin(); fieldIter!=_fields->end(); fieldIter++)
    {
    if(!strcmp((*fieldIter)->name, _fieldName))
      {
      return *fieldIter;
      }
    }
  return NULL;
  }


int
MET_GetFieldRecordNumber(const char * _fieldName,std::vector<MET_FieldRecordType *> * _fields)
  {
  int i;
  for(i=0; i<(int)_fields->size(); i++)
    {
    if(!strcmp((*_fields)[i]->name, _fieldName))
      {
      return i;
      }
    }
  return -1;
  }


//
// Read the type of the object
//
const char* MET_ReadType(std::istream &_fp)
{
  unsigned int pos = _fp.tellg();
  MET_FieldRecordType* mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ObjectType", MET_STRING, false);
  mF->required = true;
  mF->terminateRead = true;
  std::vector<MET_FieldRecordType *> fields;
  fields.push_back(mF);
  MET_Read(_fp, &fields);
  _fp.seekg(pos);

  if(mF && mF->defined)
  {
    return (char *)(mF->value);
  }

  return NULL;
}


//
// String To Type
//
bool MET_StringToType(const char *_s, MET_ValueEnumType *_vType)
  {
  int i;
  for(i=0; i<MET_NUM_VALUE_TYPES; i++)
    if(!strcmp(_s, MET_ValueTypeName[i]))
      {
      *_vType = (MET_ValueEnumType)i;
      return true;
      }
    
  *_vType = MET_OTHER;  
  return false;
  }

//
// METType To String
//
bool MET_TypeToString(MET_ValueEnumType _vType, char *_s)
  {
  if(_vType>=0 && _vType<=MET_NUM_VALUE_TYPES)
    {
    sprintf(_s, MET_ValueTypeName[_vType]);
    return true;
    }
  
  return false;
  }


//
// Sizeof METTYPE
//
bool MET_SizeOfType(MET_ValueEnumType _vType, int *s)
  {
  *s = MET_ValueTypeSize[_vType];
  if(_vType < MET_STRING)
    return true;
  else
    return false;
  }

//
// Value to Double
//
bool MET_ValueToDouble(MET_ValueEnumType _type, const void *_data, int _index,
                       double *_value)
  {
  switch(_type)
    {
    case MET_ASCII_CHAR:
    case MET_CHAR:
    case MET_CHAR_ARRAY:
      *_value = (double)(((char *)_data)[_index]);
      return true;
    case MET_UCHAR:
    case MET_UCHAR_ARRAY:
      *_value = (double)(((unsigned char *)_data)[_index]);
      return true;
    case MET_SHORT:
    case MET_SHORT_ARRAY:
      *_value = (double)(((short *)_data)[_index]);
      return true;
    case MET_USHORT:
    case MET_USHORT_ARRAY:
      *_value = (double)(((unsigned short *)_data)[_index]);
      return true;
    case MET_INT:
    case MET_INT_ARRAY:
      *_value = (double)(((int *)_data)[_index]);
      return true;
    case MET_UINT:
    case MET_UINT_ARRAY:
      *_value = (double)(((unsigned int *)_data)[_index]);
      return true;
    case MET_FLOAT:
    case MET_FLOAT_ARRAY:
    case MET_FLOAT_MATRIX:
      *_value = (double)(((float *)_data)[_index]);
      return true;
    case MET_DOUBLE:
    case MET_DOUBLE_ARRAY:
      *_value = (double)(((double *)_data)[_index]);
      return true;
    case MET_STRING:
      *_value = atof(&(((char *)_data)[_index]));
      return true;
    default:
      *_value = 0;
      return false;
    }
  }

bool MET_DoubleToValue(double _value, MET_ValueEnumType _type, void *_data, int _index)
  {
  switch(_type)
    {
    case MET_ASCII_CHAR:
    case MET_CHAR:
    case MET_CHAR_ARRAY:
      ((char *)_data)[_index] = (char)_value;
      return true;
    case MET_UCHAR:
    case MET_UCHAR_ARRAY:
      ((unsigned char *)_data)[_index] = (unsigned char)_value;
      return true;
    case MET_SHORT:
    case MET_SHORT_ARRAY:
      ((short *)_data)[_index] = (short)_value;
      return true;
    case MET_USHORT:
    case MET_USHORT_ARRAY:
      ((unsigned short *)_data)[_index] = (unsigned short)_value;
      return true;
    case MET_INT:
    case MET_INT_ARRAY:
      ((int *)_data)[_index] = (int)_value;
      return true;
    case MET_UINT:
    case MET_UINT_ARRAY:
      ((unsigned int *)_data)[_index] = (unsigned int)_value;
      return true;
    case MET_FLOAT:
    case MET_FLOAT_ARRAY:
    case MET_FLOAT_MATRIX:
      ((float *)_data)[_index] = (float)_value;
      return true;
    case MET_DOUBLE:
    case MET_DOUBLE_ARRAY:
      ((double *)_data)[_index] = (double)_value;
      return true;
    case MET_STRING:
      sprintf(&(((char *)_data)[_index]), "%f", _value);
      return true;
    default:
      return false;
    }
  }

bool MET_ValueToValue(MET_ValueEnumType _fromType, const void *_fromData,
                      int _index,
                      MET_ValueEnumType _toType, void *_toData,
                      double _fromMin, double _fromMax,
                      double _toMin, double _toMax)
  {
  double tf;
  MET_ValueToDouble(_fromType, _fromData, _index, &tf);
  if(_toMin != _toMax && _fromMin != _fromMax)
    {
    tf = (tf-_fromMin)/(_fromMax-_fromMin) * (_toMax-_toMin) + _toMin;
    if(tf<_toMin)
      {
      tf = _toMin;
      }
    else if(tf>_toMax)
      {
      tf = _toMax;
      }
    }
  switch(_toType)
    {
    case MET_ASCII_CHAR:
    case MET_CHAR:
    case MET_CHAR_ARRAY:
      (((char *)_toData)[_index]) = (char)tf;
      return true;
    case MET_UCHAR:
    case MET_UCHAR_ARRAY:
      (((unsigned char *)_toData)[_index]) = (unsigned char)tf;
      return true;
    case MET_SHORT:
    case MET_SHORT_ARRAY:
      (((short *)_toData)[_index]) = (short)tf;
      return true;
    case MET_USHORT:
    case MET_USHORT_ARRAY:
      (((unsigned short *)_toData)[_index]) = (unsigned short)tf;
      return true;
    case MET_INT:
    case MET_INT_ARRAY:
      (((int *)_toData)[_index]) = (int)tf;
      return true;
    case MET_UINT:
    case MET_UINT_ARRAY:
      (((unsigned int *)_toData)[_index]) = (unsigned int)tf;
      return true;
    case MET_FLOAT:
    case MET_FLOAT_ARRAY:
    case MET_FLOAT_MATRIX:
      (((float *)_toData)[_index]) = (float)tf;
      return true;
    case MET_STRING:
      sprintf(&(((char *)_toData)[_index]), "%f", tf);
      return true;
    default:
      return false;
    }
  }

//
//
//
bool MET_StringToWordArray(char *s, int *n, char ***val)
{
  int l = strlen(s);

  int p = 0;
  while(p<l && s[p] == ' ')
    p++;
  
  *n = 0;
  int pp = p;
  bool space = false;
  while(pp<l)
  {
    if(s[pp] == ' ' && !space)
    {
      (*n)++;
      space = true;
    }
    else
    {
      space = false;
    }
    pp++;
  }
  pp=l-1;
  if(s[pp] == ' ')   
  {
    while(pp>=0 && s[pp] == ' ')
    {
      (*n)--;
      pp--;
    }
  }
  else
  {
    (*n)++;
  }
  
  *val = new char *[*n];
  
  int i, j;
  for(i=0; i<*n; i++) 
  {
    if(p == l)
    {
      return false;
    }
    
    (*val)[i] = new char [80];
    while(p<l && s[p] == ' ')
    {
      p++;
    }
    j = 0;
    while(p<l && s[p] != ' ')
    {
      (*val)[i][j++] = s[p++];
    }
    (*val)[i][j] = '\0';
  }
  
  return true;
}

//
//
//
bool MET_GetFilePath(const char *_fName, char *_fPath)
  {
  int i;
  
  int l = strlen(_fName);
  
  for(i=l-1; i>=0; i--)
    if(_fName[i] == '\\' || _fName[i] == '/')
      break;
    
    if(i >= 0 && (_fName[i] == '/' || _fName[i] == '\\'))
      {
      strcpy(_fPath, _fName);
      _fPath[i+1] = '\0';
      return true;
      }
    else
      {
      _fPath[0] = '\0';
      return false;
      }
  }

//
//
//
bool MET_GetFileSuffixPtr(const char *_fName, int *i)
  {
  *i = strlen(_fName);
  while(*i>0)
    if(_fName[(*i)-1] == '.')
      return true;
    else
      (*i)--;
    return false;
  }

//
//
//
bool MET_SetFileSuffix(char *_fName, const char *_suf)
  {
  int i;
  MET_GetFileSuffixPtr(_fName, &i);
  if(i>0)
    {
    if(_suf[0] == '.')
      _fName[i-1] = '\0';
    else
      _fName[i] = '\0';
    strcat(_fName, _suf);
    return true;
    }
  else
    return false;
  }

//
//
//
bool MET_InitWriteField(MET_FieldRecordType * _mf, 
                        const char *_name, 
                        MET_ValueEnumType _type, 
                        double _v)
  {
  strcpy(_mf->name, _name);
  _mf->type = _type;
  _mf->defined = true;
  _mf->length = 1;
  _mf->dependsOn = -1;
  _mf->required = false;
  _mf->terminateRead = false;
  _mf->value[0] = _v;
  return true;
  }

bool MET_InitReadField(MET_FieldRecordType * _mf, 
                                  const char *_name, 
                                  MET_ValueEnumType _type, 
                                  bool _required,
                                  int _dependsOn, 
                                  int _length)
  {
  strcpy(_mf->name, _name);
  _mf->type = _type;
  _mf->defined = false;
  _mf->dependsOn = _dependsOn;
  _mf->required = _required;
  _mf->terminateRead = false;
  _mf->length = _length;
  _mf->value[0] = 0;
  return true;
  }

//
//
//
bool MET_SkipToVal(std::istream &fp)
  {
  char c;
  if( fp.eof() )
    {
    return false;
    }
  
  c = fp.get();

  while( c != MET_SeperatorChar && !fp.eof() )
    {
    c = fp.get();
    }

  while( ( c == MET_SeperatorChar || isspace(c) ) && !fp.eof() )
    {
    c = fp.get();
    }
  
  if( fp.eof() )
    {
    std::cout << "Incomplete file record definition" << std::endl;
    return false;
    }
  
  fp.putback(c);

  return true;
  }

//
//
//
bool MET_IsComplete(std::vector<MET_FieldRecordType *> * fields)
  {
  std::vector<MET_FieldRecordType *>::iterator fieldIter;
  for(fieldIter=fields->begin(); fieldIter!=fields->end(); fieldIter++)
    {
    if((*fieldIter)->required && !(*fieldIter)->defined)
      {
      std::cout << (*fieldIter)->name << " required and not defined." 
                << std::endl;
      return false;
      }
    }
  return true;
  }

//
bool MET_Read(std::istream &fp, std::vector<MET_FieldRecordType *> * fields,
              char _MET_SeperatorChar)
  {

  char s[1024];
  int i, j;

  std::vector<MET_FieldRecordType *>::iterator fieldIter;
  
  MET_SeperatorChar = _MET_SeperatorChar;
  
  bool found;
  
  char c;
  while(!fp.eof())
    {
    i = 0;
    c = fp.get();
    while(!fp.eof() && c != MET_SeperatorChar
          && (c == '\n' || isspace(c)))
      {
      c = fp.get();
      }
    while(!fp.eof() && c != MET_SeperatorChar && c != '\n' && i<500)
      {
      s[i++] = c;
      c = fp.get();
      }
    if(fp.eof() || i >= 500)
      {
      break;
      }
    fp.putback(c);
    s[i] = '\0';
    
    i--;
    while((s[i] == ' ' || s[i] == '\t') && i>0)
      {
      s[i--] = '\0';
      }

    found = false;
    for(fieldIter=fields->begin(); fieldIter!=fields->end(); fieldIter++)
      {
      if(!strcmp((*fieldIter)->name, s))
        {
        if((*fieldIter)->dependsOn >= 0)
          if(!(*fields)[(*fieldIter)->dependsOn]->defined)
            {
            std::cout << (*fieldIter)->name << " defined prior to defining ";
            std::cout << (*fields)[(*fieldIter)->dependsOn]->name << std::endl;
            return false;
            }
        switch((*fieldIter)->type)
          {
          case MET_NONE:
            fp.getline( s, 500 );
            break;
          case MET_ASCII_CHAR:
            {
            MET_SkipToVal(fp);
            if(fp.eof())
              {
              break;
              }
            char c = fp.get();
            (*fieldIter)->value[0] = (double)c;
            fp.getline( s, 500 );
            break;
            }
          default:
          case MET_CHAR:
          case MET_UCHAR:
          case MET_SHORT:
          case MET_USHORT:
          case MET_INT:
          case MET_UINT:
          case MET_FLOAT:
          case MET_DOUBLE:
            {
            MET_SkipToVal(fp);
            if(fp.eof())
              {
              break;
              }
            fp >> (*fieldIter)->value[0];
            fp.getline( s, 500 );
            break;
            }
          case MET_STRING:
            {
            MET_SkipToVal(fp);
            if(fp.eof())
              {
              break;
              }
            char * str = (char *)((*fieldIter)->value);
            fp.getline( str, 500 );
            j = strlen(str)-1;
            while(!isprint(str[j]) || isspace(str[j]))
              str[j--] = '\0';
            (*fieldIter)->length = strlen( str );
            break;
            }
          case MET_CHAR_ARRAY:
          case MET_UCHAR_ARRAY:
          case MET_SHORT_ARRAY:
          case MET_USHORT_ARRAY:
          case MET_INT_ARRAY:
          case MET_UINT_ARRAY:
          case MET_FLOAT_ARRAY:
          case MET_DOUBLE_ARRAY:
            {
            MET_SkipToVal(fp);
            if(fp.eof())
              {
              break;
              }
            if((*fieldIter)->dependsOn >= 0)
              {
              (*fieldIter)->length =
                    (int)((*fields)[(*fieldIter)->dependsOn]->value[0]);
              for(j=0; j<(*fieldIter)->length; j++) 
                {
                fp >> (*fieldIter)->value[j];
                }
              }
            else
              {
              if((*fieldIter)->length <= 0)
                {
                std::cout << 
                  "Arrays must have dependency or pre-specified lengths"
                  << std::endl;
                return false;
                }
              for(j=0; j<(*fieldIter)->length; j++)
                {
                fp >> (*fieldIter)->value[j];
                }
              }
            fp.getline( s, 500 );
            break;
            }
          case MET_FLOAT_MATRIX:
            {
            MET_SkipToVal(fp);
            if(fp.eof())
              {
              break;
              }
            if((*fieldIter)->dependsOn >= 0)
              {
              (*fieldIter)->length =
                    (int)((*fields)[(*fieldIter)->dependsOn]->value[0]);
              for(j=0; j<(*fieldIter)->length*(*fieldIter)->length;
                  j++) 
                {
                fp >> (*fieldIter)->value[j];
                }
              }
            else
              {
              if((*fieldIter)->length <= 0)
                {
                std::cout << 
                  "Arrays must have dependency or pre-specified lengths"
                  << std::endl;
                return false;
                }
              for(j=0; j<(*fieldIter)->length*(*fieldIter)->length; j++)
                {
                fp >> (*fieldIter)->value[j];
                }
              }
            fp.getline( s, 500 );
            break;
            }
          case MET_OTHER:
            {
            fp.getline( s, 500 );
            break;
            }
          }
        found = true;
        (*fieldIter)->defined = true;
        if((*fieldIter)->terminateRead)
          {
          return MET_IsComplete(fields);
          }
        break;
        }
      }
    if(!found)
      {
      std::cout << "Skipping unrecognized field " << s << std::endl;
      fp.getline( s, 500 );
      }
    }
    
  return MET_IsComplete(fields);
  }
  
//
bool MET_Write(std::ostream &fp, std::vector<MET_FieldRecordType *> * fields,
               char _MET_SeperatorChar)
  {
  MET_SeperatorChar = _MET_SeperatorChar;
  
  int j;
  std::vector<MET_FieldRecordType *>::iterator fieldIter;
  for(fieldIter=fields->begin(); fieldIter!=fields->end(); fieldIter++)
    {
    switch((*fieldIter)->type)
      {
      case MET_NONE:
        {
        fp << (*fieldIter)->name << " " << MET_SeperatorChar << " " 
           << std::endl;
        break;
        }
      case MET_ASCII_CHAR:
        {
        fp << (*fieldIter)->name << " " << MET_SeperatorChar << " ";
        fp << (char)(*fieldIter)->value[0] << std::endl;
        break;
        }
      case MET_CHAR:
      case MET_UCHAR:
      case MET_SHORT:
      case MET_USHORT:
      case MET_INT:
      case MET_UINT:
        {
        fp << (*fieldIter)->name << " " << MET_SeperatorChar << " ";
        fp << (int)(*fieldIter)->value[0] << std::endl;
        break;
        }
      case MET_FLOAT:
      case MET_DOUBLE:
        {
        fp << (*fieldIter)->name << " " << MET_SeperatorChar << " ";
        fp << (double)(*fieldIter)->value[0] << std::endl;
        break;
        }
      case MET_STRING:
        {
        fp << (*fieldIter)->name << " " << MET_SeperatorChar << " ";
        if((*fieldIter)->dependsOn >= 0) 
          {
          if((*fieldIter)->length != 
             (*fields)[(*fieldIter)->dependsOn]->value[0])
            {
            std::cout << "Warning:";
            std::cout << "length and dependsOn values not equal in write";
            std::cout << std::endl;
            }
          }
        fp.write( (char *)((*fieldIter)->value), (*fieldIter)->length );
        fp << std::endl;
        break;
        }
      case MET_CHAR_ARRAY:
      case MET_UCHAR_ARRAY:
      case MET_SHORT_ARRAY:
      case MET_USHORT_ARRAY:
      case MET_INT_ARRAY:
      case MET_UINT_ARRAY:
        {
        fp << (*fieldIter)->name << " " << MET_SeperatorChar;
        if((*fieldIter)->dependsOn >= 0)
          {
          if((*fieldIter)->length != 
             (*fields)[(*fieldIter)->dependsOn]->value[0])
            {
            std::cout << "Warning: ";
            std::cout << "Length and dependsOn values not equal in write";
            std::cout << std::endl;
            }
          }
        for(j=0; j<(*fieldIter)->length; j++)
          {
          fp << " " << (int)(*fieldIter)->value[j];
          }
        fp << std::endl;
        break;
        }
      case MET_FLOAT_ARRAY:
      case MET_DOUBLE_ARRAY:
        {
        fp << (*fieldIter)->name << " " << MET_SeperatorChar;
        if((*fieldIter)->dependsOn >= 0)
          {
          if((*fieldIter)->length != 
             (*fields)[(*fieldIter)->dependsOn]->value[0])
            {
            std::cout << "Warning: ";
            std::cout << "length and dependsOn values not equal in write";
            std::cout << std::endl;
            }
          }
        for(j=0; j<(*fieldIter)->length; j++)
          {
          fp << " " << (double)(*fieldIter)->value[j];
          }
        fp << std::endl;
        break;
        }
      case MET_FLOAT_MATRIX:
        {
        fp << (*fieldIter)->name << " " << MET_SeperatorChar;
        if((*fieldIter)->dependsOn >= 0)
          {
          if((*fieldIter)->length != 
             (*fields)[(*fieldIter)->dependsOn]->value[0])
            {
            std::cout << "Warning: ";
            std::cout << "length and dependsOn values not equal in write";
            std::cout << std::endl;
            }
          }
        for(j=0; j<(*fieldIter)->length*(*fieldIter)->length; j++)
          {
          fp << " " << (double)(*fieldIter)->value[j];
          }
        fp << std::endl;
        break;
        }
      case MET_OTHER:
        {
        break;
        }
      }
    }
  return true;
}

bool MET_WriteFieldToFile(std::ostream & _fp, const char *_fieldName,
                          MET_ValueEnumType _pType, int _n, const void *_v)
  {
  int i;
  MET_FieldRecordType f;
  
  sprintf(f.name, "%s", _fieldName);
  f.defined = false;
  f.dependsOn = -1;
  f.length = _n;
  f.required = false;
  f.type = _pType;
  switch(_pType)
    {
    case MET_ASCII_CHAR:
    case MET_CHAR:
    case MET_CHAR_ARRAY:
      for(i = 0; i < _n; i++)
        {
        f.value[i] = (double)(((char *)_v)[i]);
        }
      break;
    case MET_UCHAR:
    case MET_UCHAR_ARRAY:
      for(i = 0; i < _n; i++)
        {
        f.value[i] = (double)(((unsigned char *)_v)[i]);
        }
      break;
    case MET_SHORT:
    case MET_SHORT_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)(((short *)_v)[i]);
        }
      break;
    case MET_USHORT:
    case MET_USHORT_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)(((unsigned short *)_v)[i]);
        }
      break;
    case MET_INT:
    case MET_INT_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)(((int *)_v)[i]);
        }
      break;
    case MET_UINT:
    case MET_UINT_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)(((unsigned int *)_v)[i]);
        }
      break;
    case MET_FLOAT:
    case MET_FLOAT_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)((float *)_v)[i];
        }
      break;
    case MET_DOUBLE:
    case MET_DOUBLE_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)(((double *)_v)[i]);
        }
      break;
    case MET_STRING:
      strcpy((char *)(f.value),(char *)_v);
      break;
    case MET_FLOAT_MATRIX:
      for(i=0; i<_n*_n; i++)
        {
        f.value[i] = (double)((float *)_v)[i];
        }
      break;
    default:
      break;
    }
  
  std::vector<MET_FieldRecordType *> l;
  l.clear();
  l.push_back(&f);
  MET_Write(_fp, &l);
  
  return true;
  }

bool MET_WriteFieldToFile(std::ostream & _fp, const char *_fieldName,
  MET_ValueEnumType _pType, double _v)
  {
  MET_FieldRecordType f;
  
  sprintf(f.name, "%s", _fieldName);
  f.defined = false;
  f.dependsOn = -1;
  f.length = 1;
  f.required = false;
  f.type = _pType;
  f.value[0] = _v;
  
  std::vector<MET_FieldRecordType *> l;
  l.clear();
  l.push_back(&f);
  MET_Write(_fp, &l);
  
  return true;
  }

