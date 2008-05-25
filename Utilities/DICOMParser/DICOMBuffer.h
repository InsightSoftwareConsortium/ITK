/*=========================================================================

  Program:   DICOMParser
  Module:    DICOMBuffer.h
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

#ifndef __DICOMBUFFER_H_
#define __DICOMBUFFER_H_

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
#include "DICOMSource.h"

namespace DICOMPARSER_NAMESPACE
{
//
// DICOM data source that is a memory buffer.
//
class DICOM_EXPORT DICOMBuffer : public DICOMSource
{
 public:
  DICOMBuffer(unsigned char *buffer, long length);
  virtual ~DICOMBuffer();
  
  //
  // Return the position in the buffer.
  //
  long Tell();
  
  // 
  // Move to a particular position in the buffer.
  //
  void SkipToPos(long);
  
  //
  // Return the size of the buffer.
  //
  long GetSize();
  
  //
  // Skip a number of bytes.
  // 
  void Skip(long);
  
  //
  // Skip to the beginning of the buffer.
  //
  void SkipToStart();
  
  //
  // Read data of length len.
  //
  void Read(void* data, long len);
  
 protected:
  DICOMBuffer(const DICOMBuffer&);
  void operator=(const DICOMBuffer&);  

  unsigned char *Buffer;
  long Length;
  long Position;
  
private:
  DICOMBuffer();

};
}
#ifdef _MSC_VER
#pragma warning ( pop )
#endif

#endif // __DICOMBUFFER_H_


