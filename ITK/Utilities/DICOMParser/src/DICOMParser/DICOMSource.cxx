/*=========================================================================

  Program:   DICOMParser
  Module:    DICOMSource.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2003 Matt Turek
  All rights reserved.
  See Copyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/

#ifdef _MSC_VER
#pragma warning ( disable : 4514 )
#pragma warning ( disable : 4710 )
#pragma warning ( push, 3 )
#endif 

#include <iostream>
#include <fstream>
#include <iomanip>
#include <stdio.h>
#include <string.h>

#include "DICOMConfig.h"
#include "DICOMSource.h"

namespace DICOMPARSER_NAMESPACE
{
DICOMSource::DICOMSource() 
{
  /* Are we little or big endian?  From Harbison&Steele.  */
  union
  {
    long l;
    char c[sizeof (long)];
  } u;
  u.l = 1;
  PlatformIsBigEndian = (u.c[sizeof (long) - 1] == 1);
  if (PlatformIsBigEndian)
    {
    PlatformEndian = "BigEndian";
    }
  else
    {
    PlatformEndian = "LittleEndian";
    }
}

DICOMSource::~DICOMSource()
{
}

DICOMSource::DICOMSource(const DICOMSource& in)
{
  if (strcmp(in.PlatformEndian, "LittleEndian") == 0)
    {
    PlatformEndian = "LittleEndian";
    }
  else
    {
    PlatformEndian = "BigEndian";
    }
}

void DICOMSource::operator=(const DICOMSource& in)
{
  if (strcmp(in.PlatformEndian, "LittleEndian") == 0)
    {
    PlatformEndian = "LittleEndian";
    }
  else
    {
    PlatformEndian = "BigEndian";
    }
}

doublebyte DICOMSource::ReadDoubleByte() 
{
  doublebyte sh = 0;
  int sz = sizeof(doublebyte);
  this->Read((char*)&(sh),sz); 
  if (PlatformIsBigEndian) 
    {
    sh = swapShort(sh);
    }
  return(sh);
}

doublebyte DICOMSource::ReadDoubleByteAsLittleEndian() 
{
  doublebyte sh = 0;
  int sz = sizeof(doublebyte);
  this->Read((char*)&(sh),sz); 
  if (PlatformIsBigEndian)
    {
    sh = swapShort(sh);
    }
  return(sh);
}

quadbyte DICOMSource::ReadQuadByte() 
{
  quadbyte sh;
  int sz = sizeof(quadbyte);
  this->Read((char*)&(sh),sz);
  if (PlatformIsBigEndian) 
    {
    sh = swapLong(sh);
    }
  return(sh);
}

quadbyte DICOMSource::ReadNBytes(int len) 
{
  quadbyte ret = -1;
  switch (len) 
    {
    case 1:
      char ch;
      this->Read(&ch,1);  //from Image
      ret =(quadbyte) ch;
      break;
    case 2:
      ret =(quadbyte) ReadDoubleByte();
      break;
    case 4:
      ret = ReadQuadByte();
      break;
    default:
      dicom_stream::cerr << "Unable to read " << len << " bytes" << dicom_stream::endl;
      break;
    }
  return (ret);
}

float DICOMSource::ReadAsciiFloat(int len) 
{
  float ret=0.0f;


  char* val = new char[len+1];
  this->Read(val,len);
  val[len] = '\0';

#if 0
  //
  // istrstream destroys the data during formatted input.
  //
  int len2 = static_cast<int> (strlen((char*) val));
  char* val2 = new char[len2];
  strncpy(val2, (char*) val, len2);

  dicom_stream::istrstream data(val2);
  data >> ret;
  delete [] val2;
#else
  sscanf(val,"%e",&ret);
#endif

  // dicom_stream::cout << "Read ASCII float: " << ret << dicom_stream::endl;

  delete [] val;
  return (ret);
}

int DICOMSource::ReadAsciiInt(int len) 
{
  int ret=0;

  char* val = new char[len+1];
  this->Read(val,len);
  val[len] = '\0';

#if 0
  //
  // istrstream destroys the data during formatted input.
  //
  int len2 = static_cast<int> (strlen((char*) val));
  char* val2 = new char[len2];
  strncpy(val2, (char*) val, len2);

  dicom_stream::istrstream data(val2);
  data >> ret;
  delete [] val2;
#else
  sscanf(val,"%d",&ret);
#endif

  // dicom_stream::cout << "Read ASCII int: " << ret << dicom_stream::endl;

  delete [] val;
  return (ret);
}

char* DICOMSource::ReadAsciiCharArray(int len) 
{
  if (len <= 0)
    {
    return NULL;
    }
  char* val = new char[len + 1];
  this->Read(val, len);
  val[len] = 0; // NULL terminate.
  return val;
}
}
#ifdef _MSC_VER
#pragma warning ( pop )
#endif
