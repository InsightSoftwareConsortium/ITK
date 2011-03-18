/*=========================================================================

  Program:   DICOMParser
  Module:    DICOMBuffer.cxx
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
#include "DICOMBuffer.h"

namespace DICOMPARSER_NAMESPACE
{
DICOMBuffer::DICOMBuffer(unsigned char *buffer, long length)
  : DICOMSource(),
    Buffer(buffer),
    Length(length),
    Position(0)
{
}

DICOMBuffer::~DICOMBuffer()
{
}

DICOMBuffer::DICOMBuffer(const DICOMBuffer& in)
  : DICOMSource(in)
{
  Buffer = in.Buffer;
  Length = in.Length;
  Position = in.Position;
}

void DICOMBuffer::operator=(const DICOMBuffer& in)
{
  DICOMSource::operator=(in);

  Buffer = in.Buffer;
  Length = in.Length;
  Position = in.Position;
}


long DICOMBuffer::Tell() 
{
  return Position;
}

void DICOMBuffer::SkipToPos(long increment) 
{
  Position = increment;
}

long DICOMBuffer::GetSize() 
{
  return Length;
}

void DICOMBuffer::Skip(long increment) 
{
  Position += increment;
}

void DICOMBuffer::SkipToStart() 
{
  Position = 0;
}

void DICOMBuffer::Read(void* ptr, long nbytes) 
{
  memcpy(ptr, Buffer+Position, nbytes);
  Position += nbytes;
}

}
#ifdef _MSC_VER
#pragma warning ( pop )
#endif
