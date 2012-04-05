/*=========================================================================

  Program:   DICOMParser
  Module:    DICOMSource.h
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

#ifndef __DICOMSOURCE_H_
#define __DICOMSOURCE_H_

#ifdef _MSC_VER
#pragma warning ( disable : 4514 )
#pragma warning ( push, 3 )
#endif 

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string>
#include <cstring>

#include "DICOMTypes.h"
#include "DICOMConfig.h"

namespace DICOMPARSER_NAMESPACE
{
//
// Abstraction of a DICOM data source used by the DICOMParser.
//
class DICOM_EXPORT DICOMSource 
{
 public:
  DICOMSource();
  virtual ~DICOMSource();
  
  //
  // Return the position in the source
  //
  virtual long Tell()=0;
  
  // 
  // Move to a particular position in the source.
  //
  virtual void SkipToPos(long)=0;
  
  //
  // Return the size of the source.
  //
  virtual long GetSize()=0;
  
  //
  // Skip a number of bytes.
  // 
  virtual void Skip(long)=0;
  
  //
  // Skip to the beginning of the source.
  //
  virtual void SkipToStart()=0;
  
  //
  // Read data of length len.
  //
  virtual void Read(void* data, long len)=0;
  
  //
  // Read a double byte of data.
  //
  virtual doublebyte ReadDoubleByte();
  virtual doublebyte ReadDoubleByteAsLittleEndian();

  //
  // Read a quadbyte of data.
  //
  virtual quadbyte   ReadQuadByte();
  
  //
  // Read nbytes of data up to 4 bytes.
  //
  virtual quadbyte   ReadNBytes(int len);
  
  // 
  // Read a float an ascii.
  //
  virtual float      ReadAsciiFloat(int len);
  
  //
  // Read an int as ascii.
  //
  virtual int        ReadAsciiInt(int len);
  
  //
  // Read an array of ascii characters.
  //
  virtual char*      ReadAsciiCharArray(int len);
  
  //
  // Convert the data to signed long.
  //
  static long ReturnAsSignedLong(unsigned char* data, bool )
  {
    return *(reinterpret_cast< quadbyte* >( data ));
  }
  
  
  //
  // Convert the data to unsigned long.
  //
  static ulong ReturnAsUnsignedLong(unsigned char* data, bool )
  {
    return *(reinterpret_cast< ulong* >( data ));
  }
  
  //
  // Convert data to unsigned short.
  //
  static ushort ReturnAsUnsignedShort(unsigned char* data, bool )
  {
    return *(reinterpret_cast< doublebyte* >( data ));
  }
  
  //
  // Convert data to signed short.
  // 
  static short int ReturnAsSignedShort(unsigned char* data, bool )
  {
    return *(reinterpret_cast< short int* >( data ));
  }

  //
  // Convert data to int.
  //
  static int ReturnAsInteger(unsigned char* data, bool)
  {
    return static_cast<int> (atoi((const char *)data));
  }
  
  static float ReturnAsFloat(unsigned char* data, bool)
    {
    return static_cast<float> (atof((const char *)data));
    }

  bool GetPlatformIsBigEndian()
    {
    return PlatformIsBigEndian;
    }

  void SetPlatformIsBigEndian(bool v)
    {
    this->PlatformIsBigEndian = v;
    }

  //
  // Swap the bytes in an array of unsigned shorts.
  //
  static void swapShorts(ushort *ip, ushort *op, int count)
  {
    while (count)
      {
      *op++ = swapShort(*ip++);
      count--;
      }
  }
  
  //
  // Swap the bytes in an array of unsigned longs.
  //
  static void swapLongs(ulong *ip, ulong *op, int count)
  {
    while (count)
      {
      *op++ = swapLong(*ip++);
      count--;
      }
  }
  
  
  //
  // Swap the bytes in an unsigned short.
  //
  static ushort swapShort(ushort v)
  {
    return ushort((v << 8)
      | (v >> 8));
  }
  
  // 
  // Swap the bytes in an unsigned long.
  //
  static ulong swapLong(ulong v)
    {
    return ulong( (v << 24) 
      | ( (v <<  8) & 0x00ff0000 )
      | ( (v >>  8) & 0x0000ff00 )
      |   (v >> 24) );
    }

  const char* GetPlatformEndian() {return this->PlatformEndian;}

 protected:
  DICOMSource(const DICOMSource&);
  void operator=(const DICOMSource&);  

  //
  // Flag for swaping bytes.
  //
  bool PlatformIsBigEndian;

  //
  // Platform endianness
  //
  const char* PlatformEndian;

 private:

};
}
#ifdef _MSC_VER
#pragma warning ( pop )
#endif

#endif // __DICOMSOURCE_H_


