/**
 *    MetaUtils (.h and .cpp)
 *
 * Description:
 *    This file provides generic ascii file parsing capabilities.
 *    It assumes that the files consist of a set of fields
 *    Each field is list of variable = value pairs
 *
 * Features:
 *    There can be dependencies between fields, required fields, 
 *       and optional fields.
 *    Undefined fields are skipped.
 *    Values must conform to expected types.   There can be default 
 *       values for fields.
 *
 * Author:
 *    Stephen R. Aylward
 *
 * Date:
 *    February 22, 2002
 *
 **/
#ifndef METAFILEUTILS_H
#define METAFILEUTILS_H

#include <iostream>
#include <vector>
#include <typeinfo>

#include <metaTypes.h>



extern MET_FieldRecordType * MET_GetFieldRecord(const char * _fieldName,
                           std::vector<MET_FieldRecordType *> * _fields);
 
extern int MET_GetFieldRecordNumber(const char * _fieldName,
                           std::vector<MET_FieldRecordType *> * _fields);

extern bool MET_SystemByteOrderMSB(void);

inline unsigned short MET_ByteOrderSwapShort(unsigned short x)
  {
  return (unsigned short)((unsigned short)(x<<8) | (unsigned short)(x>>8));
  }

inline unsigned long MET_ByteOrderSwapLong(unsigned int x)
  {
  return (((x<<24) & 0xf000) |
          ((x<<8) & 0x0f00) |
          ((x>>8) & 0x00f0) |
          ((x>>24) & 0x000f));
  }

inline unsigned long MET_ByteOrderSwap8(unsigned long x)
  {
  return (((x<<24) & 0xf000f000) |
          ((x<<8) & 0x0f000f00) |
          ((x>>8) & 0x00f000f0) |
          ((x>>24) & 0x000f000f));
  }

extern bool MET_StringToType(const char *_str, MET_ValueEnumType *_type);

extern bool MET_TypeToString(MET_ValueEnumType _type, char *_str);

extern bool MET_SizeOfType(MET_ValueEnumType _type, int *_size);

extern bool MET_ValueToDouble(MET_ValueEnumType _pType,
                              const void *_data,
                              int _index,
                              double *_value);

extern bool MET_DoubleToValue(double _value, 
                              MET_ValueEnumType _type, 
                              void *_data, 
                              int _index);

extern bool MET_ValueToValue(MET_ValueEnumType _fromType,
                             const void *_fromData,
                             int _index,
                             MET_ValueEnumType _toType,
                             void  *_toData,
                             double _fromMin=0, double _fromMax=0,
                             double _toMin=0, double _toMax=0);

extern bool MET_StringToWordArray(char *s, int *n, char ***val);

//
//
//
extern bool MET_GetFilePath(const char *_fName, char *_fPath);

extern bool MET_GetFileSuffixPtr(const char *_fName, int *i);

extern bool MET_SetFileSuffix(char *_fName, const char *_suf);

//
//
//
extern bool MET_InitWriteField(MET_FieldRecordType * _mf, 
                                   const char *_name, 
                                   MET_ValueEnumType _type, 
                                   double _v=0);

template <class T>
bool MET_InitWriteField(MET_FieldRecordType * _mf, 
                                   const char *_name, 
                                   MET_ValueEnumType _type, 
                                   int _length, 
                                   T *_v)
  {
  strcpy(_mf->name, _name);
  _mf->type = _type;
  _mf->defined = true;
  _mf->length = _length;
  _mf->dependsOn = -1;
  _mf->required = false;
  _mf->terminateRead = false;
  if(_type == MET_FLOAT_MATRIX)
    {
    int i;
    for(i=0; i<_length*_length; i++)
      {
      _mf->value[i] = (double)(_v[i]);
      }
    }
  else if(_type != MET_STRING)
    {
    int i;
    for(i=0; i<_length; i++)
      {
      _mf->value[i] = (double)(_v[i]);
      }
    }
  else
    {
    strcpy((char *)(_mf->value), (char *)_v);
    }
  return true;
  }


extern bool MET_InitReadField(MET_FieldRecordType * _mf, 
                                  const char *_name, 
                                  MET_ValueEnumType _type, 
                                  bool _required=true,
                                  int _dependsOn=-1, 
                                  int _length=0);

// Given an array of fieldRec that define a metaFile format, parse that file
//    fTerm specifies what field (if any) marks the field (variable name)
//       at which file parsing should stop (e.g., if binary data follows)
//    fromTopOfFile indicates whether the File pointer fp should be reset
//       to topOfFile before parsing begins
extern bool MET_Read(std::istream &fp,
                     std::vector<MET_FieldRecordType *> * fields,
                     char _sepChar='=', bool oneLine=false);

// Given an array of fieldRecs, creates a metaFile.
extern bool MET_Write(std::ostream &fp,
                      std::vector<MET_FieldRecordType *> * fields,
                      char _sepChar='=');

extern bool MET_WriteFieldToFile(std::ostream &_fp, const char *_fieldName,
                                 MET_ValueEnumType _pType, int _n, const void *_v);

extern bool MET_WriteFieldToFile(std::ostream &_fp, const char *_fieldName,
                                 MET_ValueEnumType _pType, double _v);



extern const char* MET_ReadType(std::istream & _fp);

inline MET_ValueEnumType MET_GetPixelType(const std::type_info& ptype)
{
  if( ptype == typeid(unsigned char) )
  {
    return MET_UCHAR;
  }
  else if( ptype == typeid(char) )
  {
    return MET_CHAR;
  }
  else if( ptype == typeid(unsigned short))
  {
    return MET_USHORT;
  }
  else if( ptype == typeid(short) )
  {
    return MET_SHORT;
  }
  else if( ptype == typeid(unsigned int) )
  {
    return MET_UINT;
  }
  else if( ptype == typeid(int) )
  {
    return MET_INT;
  }
  else if( ptype == typeid(float) )
  {
    return MET_FLOAT;
  }
  else if( ptype == typeid(double) )
  {
    return MET_DOUBLE;
  }
  else
  {
    std::cout  <<"Couldn't convert pixel type" << std::endl;
    return MET_NONE;
  }
}

#endif
