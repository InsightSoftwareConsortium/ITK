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

#include <stdlib.h>
#include <string>

char MET_SeperatorChar = '=';

bool MET_SystemByteOrderMSB(void)
  {
  const int l = 1;
  const char * u = (const char *) & l;

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
MET_GetFieldRecordNumber(const char * _fieldName,
                         std::vector<MET_FieldRecordType *> * _fields)
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
std::string MET_ReadType(std::istream &_fp)
  {
  unsigned int pos = _fp.tellg();
  std::vector<MET_FieldRecordType *> fields;
  MET_FieldRecordType* mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ObjectType", MET_STRING, false);
  mF->required = false;
  mF->terminateRead = true;
  fields.push_back(mF);

  MET_Read(_fp, &fields, '=', true);
  _fp.seekg(pos);

  std::string value;

  if(mF && mF->defined)
    {
    value = (char *)(mF->value);
    delete mF;
    return value;
    }
    
  value[0] = '\0';
  delete mF;
  return value;
  }

//
// Read the subtype of the object
//
char* MET_ReadSubType(std::istream &_fp)
  {
  unsigned int pos = _fp.tellg();
  std::vector<MET_FieldRecordType *> fields;
  MET_FieldRecordType* mF;  
  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ObjectType", MET_STRING, false);
  mF->required = false;
  fields.push_back(mF);

  MET_Read(_fp, &fields, '=', true);

  // Find the line right after the ObjectType
  char s[1024];
  _fp.getline( s, 500 );
  std::string value = s;
  int position = value.find("=");
  if(position!=-1)
    {
    value = value.substr(position+2,value.size()-position);
    }
  _fp.seekg(pos);

  char* ret = new char[value.size()+1];
  strncpy(ret,value.c_str(),value.size());
  ret[value.size()] = '\0';
  delete mF;
  return ret;
  }


//
// String To Type
//
bool MET_StringToType(const char *_s, MET_ValueEnumType *_vType)
  {
  int i;
  for(i=0; i<MET_NUM_VALUE_TYPES; i++)
    {
    if(!strcmp(_s, MET_ValueTypeName[i]))
      {
      *_vType = (MET_ValueEnumType)i;
      return true;
      }
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
    {
    return true;
    }
  else
    {
    return false;
    }
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
      *_value = (double)(((const MET_CHAR_TYPE *)_data)[_index]);
      return true;
    case MET_UCHAR:
    case MET_UCHAR_ARRAY:
      *_value = (double)(((const MET_UCHAR_TYPE *)_data)[_index]);
      return true;
    case MET_SHORT:
    case MET_SHORT_ARRAY:
      *_value = (double)(((const MET_SHORT_TYPE *)_data)[_index]);
      return true;
    case MET_USHORT:
    case MET_USHORT_ARRAY:
      *_value = (double)(((const MET_USHORT_TYPE *)_data)[_index]);
      return true;
    case MET_INT:
    case MET_INT_ARRAY:
      *_value = (double)(((const MET_INT_TYPE *)_data)[_index]);
      return true;
    case MET_LONG:
    case MET_LONG_ARRAY:
      *_value = (double)(((const MET_LONG_TYPE *)_data)[_index]);
      return true;
    case MET_UINT:
    case MET_UINT_ARRAY:
      *_value = (double)(((const MET_UINT_TYPE *)_data)[_index]);
      return true;
    case MET_ULONG:
    case MET_ULONG_ARRAY:
      *_value = (double)(((const MET_ULONG_TYPE *)_data)[_index]);
      return true;
    case MET_LONG_LONG:
    case MET_LONG_LONG_ARRAY:
      *_value = (double)(((const MET_LONG_LONG_TYPE *)_data)[_index]);
      return true;
    case MET_ULONG_LONG:
    case MET_ULONG_LONG_ARRAY:
      *_value = (double)(((const MET_ULONG_LONG_TYPE *)_data)[_index]);
      return true;
    case MET_FLOAT:
    case MET_FLOAT_ARRAY:
    case MET_FLOAT_MATRIX:
      *_value = (double)(((const MET_FLOAT_TYPE *)_data)[_index]);
      return true;
    case MET_DOUBLE:
    case MET_DOUBLE_ARRAY:
      *_value = (double)(((const MET_DOUBLE_TYPE *)_data)[_index]);
      return true;
    case MET_STRING:
      *_value = atof(&(((const MET_CHAR_TYPE *)_data)[_index]));
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
      ((MET_CHAR_TYPE *)_data)[_index] = (MET_CHAR_TYPE)_value;
      return true;
    case MET_UCHAR:
    case MET_UCHAR_ARRAY:
      ((MET_UCHAR_TYPE *)_data)[_index] = (MET_UCHAR_TYPE)_value;
      return true;
    case MET_SHORT:
    case MET_SHORT_ARRAY:
      ((MET_SHORT_TYPE *)_data)[_index] = (MET_SHORT_TYPE)_value;
      return true;
    case MET_USHORT:
    case MET_USHORT_ARRAY:
      ((MET_USHORT_TYPE *)_data)[_index] = (MET_USHORT_TYPE)_value;
      return true;
    case MET_INT:
    case MET_INT_ARRAY:
      ((MET_INT_TYPE *)_data)[_index] = (MET_INT_TYPE)_value;
      return true;
    case MET_LONG:
    case MET_LONG_ARRAY:
      ((MET_LONG_TYPE *)_data)[_index] = (MET_LONG_TYPE)_value;
      return true;
    case MET_UINT:
    case MET_UINT_ARRAY:
      ((MET_UINT_TYPE *)_data)[_index] = (MET_UINT_TYPE)_value;
      return true;
    case MET_ULONG:
    case MET_ULONG_ARRAY:
      ((MET_ULONG_TYPE *)_data)[_index] = (MET_ULONG_TYPE)_value;
      return true;
    case MET_LONG_LONG:
    case MET_LONG_LONG_ARRAY:
      ((MET_LONG_LONG_TYPE *)_data)[_index] = (MET_LONG_LONG_TYPE)_value;
      return true;
    case MET_ULONG_LONG:
    case MET_ULONG_LONG_ARRAY:
      ((MET_ULONG_LONG_TYPE *)_data)[_index] = (MET_ULONG_LONG_TYPE)_value;
      return true;
    case MET_FLOAT:
    case MET_FLOAT_ARRAY:
    case MET_FLOAT_MATRIX:
      ((MET_FLOAT_TYPE *)_data)[_index] = (MET_FLOAT_TYPE)_value;
      return true;
    case MET_DOUBLE:
    case MET_DOUBLE_ARRAY:
      ((MET_DOUBLE_TYPE *)_data)[_index] = (MET_DOUBLE_TYPE)_value;
      return true;
    case MET_STRING:
      sprintf(&(((MET_CHAR_TYPE *)_data)[_index]), "%f", _value);
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
      (((MET_CHAR_TYPE *)_toData)[_index]) = (MET_CHAR_TYPE)tf;
      return true;
    case MET_UCHAR:
    case MET_UCHAR_ARRAY:
      (((MET_UCHAR_TYPE *)_toData)[_index]) = (MET_UCHAR_TYPE)tf;
      return true;
    case MET_SHORT:
    case MET_SHORT_ARRAY:
      (((MET_SHORT_TYPE *)_toData)[_index]) = (MET_SHORT_TYPE)tf;
      return true;
    case MET_USHORT:
    case MET_USHORT_ARRAY:
      (((MET_USHORT_TYPE *)_toData)[_index]) = (MET_USHORT_TYPE)tf;
      return true;
    case MET_INT:
    case MET_INT_ARRAY:
      (((MET_INT_TYPE *)_toData)[_index]) = (MET_INT_TYPE)tf;
      return true;
    case MET_LONG:
    case MET_LONG_ARRAY:
      (((MET_LONG_TYPE *)_toData)[_index]) = (MET_LONG_TYPE)tf;
      return true;
    case MET_UINT:
    case MET_UINT_ARRAY:
      (((MET_UINT_TYPE *)_toData)[_index]) = (MET_UINT_TYPE)tf;
      return true;
    case MET_ULONG:
    case MET_ULONG_ARRAY:
      (((MET_ULONG_TYPE *)_toData)[_index]) = (MET_ULONG_TYPE)tf;
      return true;
    case MET_LONG_LONG:
    case MET_LONG_LONG_ARRAY:
      (((MET_LONG_LONG_TYPE *)_toData)[_index]) = (MET_LONG_LONG_TYPE)tf;
      return true;
    case MET_ULONG_LONG:
    case MET_ULONG_LONG_ARRAY:
      (((MET_ULONG_LONG_TYPE *)_toData)[_index]) = (MET_ULONG_LONG_TYPE)tf;
      return true;
    case MET_DOUBLE:
    case MET_DOUBLE_ARRAY:
      (((MET_DOUBLE_TYPE *)_toData)[_index]) = (MET_DOUBLE_TYPE)tf;
      return true;
    case MET_FLOAT:
    case MET_FLOAT_ARRAY:
    case MET_FLOAT_MATRIX:
      (((MET_FLOAT_TYPE *)_toData)[_index]) = (MET_FLOAT_TYPE)tf;
      return true;
    case MET_STRING:
      sprintf(&(((MET_CHAR_TYPE *)_toData)[_index]), "%f", tf);
      return true;
    default:
      return false;
    }
  }

//
//
//
bool MET_StringToWordArray(const char *s, int *n, char ***val)
{
  long l = static_cast<long>( strlen(s) );

  int p = 0;
  while(p<l && s[p] == ' ')
    {
    p++;
    }
  
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
  
  long i, j;
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
  long i;
  
  long l = static_cast<long>( strlen(_fName) );
  
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
  *i = static_cast<int>( strlen(_fName) );
  int j = *i - 5;
  if(j<0)
    {
    j = 0;
    }
  while(*i>j)
    {
    if(_fName[(*i)-1] == '.')
      {
      return true;
      }
    else
      {
      (*i)--;
      }
    }
  *i = 0;
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
    {
    strcat(_fName, _suf);
    return true;
    }
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

  while( c != MET_SeperatorChar && c != ':' && !fp.eof() )
    {
    c = fp.get();
    }

  while( ( c == MET_SeperatorChar || c == ':' || isspace(c) ) && !fp.eof() )
    {
    c = fp.get();
    }
  
  if( fp.eof() )
    {
    std::cerr << "Incomplete file record definition" << std::endl;
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
      std::cerr << (*fieldIter)->name << " required and not defined." 
                << std::endl;
      return false;
      }
    }
  return true;
  }

//
bool MET_Read(std::istream &fp, std::vector<MET_FieldRecordType *> * fields,
              char _MET_SeperatorChar, bool oneLine, bool display_warnings)
  {

  char s[1024];
  int i, j;

  std::vector<MET_FieldRecordType *>::iterator fieldIter;
  
  MET_SeperatorChar = _MET_SeperatorChar;
  
  bool found;
  
  unsigned char c;
  while(!fp.eof())
    {
    i = 0;
    c = fp.get();
    while(!fp.eof() && c != MET_SeperatorChar && c != ':'
          && (c == '\n' || isspace(c)))
      {
      c = fp.get();
      }
    while(!fp.eof() && c != MET_SeperatorChar && c != ':' && c != '\n' && i<500)
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
            std::cerr << (*fieldIter)->name << " defined prior to defining ";
            std::cerr << (*fields)[(*fieldIter)->dependsOn]->name << std::endl;
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
            MET_CHAR_TYPE c = fp.get();
            c = fp.get();
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
          case MET_LONG:
          case MET_ULONG:
          case MET_LONG_LONG:
          case MET_ULONG_LONG:
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
            MET_CHAR_TYPE * str = (MET_CHAR_TYPE *)((*fieldIter)->value);
            fp.getline( str, 500 );
            j = static_cast<long>( strlen(str) ) - 1;
            while(!isprint(str[j]) || isspace(str[j]))
              {
              str[j--] = '\0';
              }
            (*fieldIter)->length = static_cast<int>( strlen( str ) );
            break;
            }
          case MET_CHAR_ARRAY:
          case MET_UCHAR_ARRAY:
          case MET_SHORT_ARRAY:
          case MET_USHORT_ARRAY:
          case MET_INT_ARRAY:
          case MET_UINT_ARRAY:
          case MET_LONG_ARRAY:
          case MET_ULONG_ARRAY:
          case MET_LONG_LONG_ARRAY:
          case MET_ULONG_LONG_ARRAY:
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
                std::cerr << 
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
                std::cerr << 
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
      if(display_warnings)
        {
        std::cerr << "Skipping unrecognized field " << s << std::endl;
        }
      fp.getline( s, 500 );
      }
    if(oneLine)
      {
      return MET_IsComplete(fields);
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
        fp << (MET_CHAR_TYPE)(*fieldIter)->value[0] << std::endl;
        break;
        }
      case MET_CHAR:
      case MET_SHORT:
      case MET_LONG:
      case MET_INT:
        {
        fp << (*fieldIter)->name << " " << MET_SeperatorChar << " ";
        fp << (MET_LONG_TYPE)((*fieldIter)->value[0]) << std::endl;
        break;
        }
      case MET_LONG_LONG:
        {
#if defined(_MSC_VER) // NOTE: you cannot use __int64 in an ostream in MSV6
        fp << (double)((MET_LONG_LONG_TYPE)((*fieldIter)->value[0])) 
           << std::endl;
        std::cerr << "Programs compiled using MSV6 cannot write 64 bit ints"
                  << std::endl;
        std::cerr << "  Writing as double instead.  Loss of precision results."
                  << std::endl;
#else
        fp << (MET_LONG_LONG_TYPE)((*fieldIter)->value[0]) << std::endl;
#endif
        break;
        }
      case MET_UCHAR:
      case MET_USHORT:
      case MET_UINT:
      case MET_ULONG:
        {
        fp << (*fieldIter)->name << " " << MET_SeperatorChar << " ";
        fp << (MET_ULONG_TYPE)((*fieldIter)->value[0]) << std::endl;
        break;
        }
      case MET_ULONG_LONG:
        {
#if defined(_MSC_VER) // NOTE: you cannot use __int64 in an ostream in MSV6
        fp << (double)((MET_ULONG_LONG_TYPE)((*fieldIter)->value[0])) 
           << std::endl;
        std::cerr << "Programs compiled using MSV6 cannot write 64 bit ints"
                  << std::endl;
        std::cerr << "  Writing as double instead.  Loss of precision results."
                  << std::endl;
#else
        fp << (MET_ULONG_LONG_TYPE)((*fieldIter)->value[0]) << std::endl;
#endif
        break;
        }
      case MET_FLOAT:
      case MET_DOUBLE:
        {
        fp << (*fieldIter)->name << " " << MET_SeperatorChar << " ";
        fp << (MET_DOUBLE_TYPE)(*fieldIter)->value[0] << std::endl;
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
            std::cerr << "Warning:";
            std::cerr << "length and dependsOn values not equal in write";
            std::cerr << std::endl;
            }
          }
        fp.write( (char *)((*fieldIter)->value), (*fieldIter)->length );
        fp << std::endl;
        break;
        }
      case MET_CHAR_ARRAY:
      case MET_SHORT_ARRAY:
      case MET_INT_ARRAY:
      case MET_LONG_ARRAY:
        {
        fp << (*fieldIter)->name << " " << MET_SeperatorChar;
        if((*fieldIter)->dependsOn >= 0)
          {
          if((*fieldIter)->length != 
             (*fields)[(*fieldIter)->dependsOn]->value[0])
            {
            std::cerr << "Warning: ";
            std::cerr << "Length and dependsOn values not equal in write";
            std::cerr << std::endl;
            }
          }
        for(j=0; j<(*fieldIter)->length; j++)
          {
          fp << " " << (MET_LONG_TYPE)((*fieldIter)->value[j]);
          }
        fp << std::endl;
        break;
        }
      case MET_LONG_LONG_ARRAY:
        {
        fp << (*fieldIter)->name << " " << MET_SeperatorChar;
        if((*fieldIter)->dependsOn >= 0)
          {
          if((*fieldIter)->length != 
             (*fields)[(*fieldIter)->dependsOn]->value[0])
            {
            std::cerr << "Warning: ";
            std::cerr << "Length and dependsOn values not equal in write";
            std::cerr << std::endl;
            }
          }
        for(j=0; j<(*fieldIter)->length; j++)
          {
#if defined(_MSC_VER) // NOTE: you cannot use __int64 in an ostream in MSV6
          fp << " " << (double)((MET_LONG_LONG_TYPE)((*fieldIter)->value[j]));
          std::cerr << "Programs compiled using MSV6 cannot write 64 bit ints"
                    << std::endl;
          std::cerr << "  Writing as double instead. Loss of precision results."
                    << std::endl;
#else
          fp << " " << (MET_LONG_LONG_TYPE)((*fieldIter)->value[j]);
#endif
          }
        fp << std::endl;
        break;
        }

      case MET_UCHAR_ARRAY:
      case MET_USHORT_ARRAY:
      case MET_UINT_ARRAY:
      case MET_ULONG_ARRAY:
        {
        fp << (*fieldIter)->name << " " << MET_SeperatorChar;
        if((*fieldIter)->dependsOn >= 0)
          {
          if((*fieldIter)->length != 
             (*fields)[(*fieldIter)->dependsOn]->value[0])
            {
            std::cerr << "Warning: ";
            std::cerr << "Length and dependsOn values not equal in write";
            std::cerr << std::endl;
            }
          }
        for(j=0; j<(*fieldIter)->length; j++)
          {
          fp << " " << (MET_ULONG_TYPE)((*fieldIter)->value[j]);
          }
        fp << std::endl;
        break;
        }
      case MET_ULONG_LONG_ARRAY:
        {
        fp << (*fieldIter)->name << " " << MET_SeperatorChar;
        if((*fieldIter)->dependsOn >= 0)
          {
          if((*fieldIter)->length != 
             (*fields)[(*fieldIter)->dependsOn]->value[0])
            {
            std::cerr << "Warning: ";
            std::cerr << "Length and dependsOn values not equal in write";
            std::cerr << std::endl;
            }
          }
        for(j=0; j<(*fieldIter)->length; j++)
          {
#if defined(_MSC_VER) // NOTE: you cannot use __int64 in an ostream in MSV6
          fp << " " << (double)((MET_ULONG_LONG_TYPE)((*fieldIter)->value[j]));
          std::cerr << "Programs compiled using MSV6 cannot write 64 bit ints"
                    << std::endl;
          std::cerr << "  Writing as double instead. Loss of precision results."
                    << std::endl;
#else
          fp << " " << (MET_ULONG_LONG_TYPE)((*fieldIter)->value[j]);
#endif
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
            std::cerr << "Warning: ";
            std::cerr << "length and dependsOn values not equal in write";
            std::cerr << std::endl;
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
            std::cerr << "Warning: ";
            std::cerr << "length and dependsOn values not equal in write";
            std::cerr << std::endl;
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
        f.value[i] = (double)(((const MET_CHAR_TYPE *)_v)[i]);
        }
      break;
    case MET_UCHAR:
    case MET_UCHAR_ARRAY:
      for(i = 0; i < _n; i++)
        {
        f.value[i] = (double)(((const MET_UCHAR_TYPE *)_v)[i]);
        }
      break;
    case MET_SHORT:
    case MET_SHORT_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)(((const MET_SHORT_TYPE *)_v)[i]);
        }
      break;
    case MET_USHORT:
    case MET_USHORT_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)(((const MET_USHORT_TYPE *)_v)[i]);
        }
      break;
    case MET_INT:
    case MET_INT_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)(((const MET_INT_TYPE *)_v)[i]);
        }
      break;
    case MET_UINT:
    case MET_UINT_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)(((const MET_UINT_TYPE *)_v)[i]);
        }
      break;
    case MET_LONG:
    case MET_LONG_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)(((const MET_LONG_TYPE *)_v)[i]);
        }
      break;
    case MET_ULONG:
    case MET_ULONG_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)(((const MET_ULONG_TYPE *)_v)[i]);
        }
      break;
    case MET_LONG_LONG:
    case MET_LONG_LONG_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)(((const MET_LONG_LONG_TYPE *)_v)[i]);
        }
      break;
    case MET_ULONG_LONG:
    case MET_ULONG_LONG_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)(((const MET_ULONG_LONG_TYPE *)_v)[i]);
        }
      break;
    case MET_FLOAT:
    case MET_FLOAT_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)((const MET_FLOAT_TYPE *)_v)[i];
        }
      break;
    case MET_DOUBLE:
    case MET_DOUBLE_ARRAY:
      for(i=0; i<_n; i++)
        {
        f.value[i] = (double)(((const MET_DOUBLE_TYPE *)_v)[i]);
        }
      break;
    case MET_STRING:
      strcpy((MET_CHAR_TYPE *)(f.value), (const MET_CHAR_TYPE *)_v);
      break;
    case MET_FLOAT_MATRIX:
      for(i=0; i<_n*_n; i++)
        {
        f.value[i] = (double)((const MET_FLOAT_TYPE *)_v)[i];
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

