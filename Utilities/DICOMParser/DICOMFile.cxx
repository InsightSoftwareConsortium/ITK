/*=========================================================================

  Program:   DICOMParser
  Module:    DICOMFile.cxx
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

#include <stdio.h>
#include <string>

#include "DICOMConfig.h"
#include "DICOMFile.h"

namespace DICOMPARSER_NAMESPACE
{
DICOMFile::DICOMFile() : DICOMSource(), InputStream()
{
}

DICOMFile::~DICOMFile()
{
  this->Close();
}

DICOMFile::DICOMFile(const DICOMFile& in)
  : DICOMSource(in)
{
  //
  // Some compilers can't handle. Comment out for now.
  //
  // InputStream = in.InputStream;
}

void DICOMFile::operator=(const DICOMFile& in)
{
  DICOMSource::operator=(in);

  //
  // Some compilers can't handle. Comment out for now.
  //
  // InputStream = in.InputStream;
}

bool DICOMFile::Open(const dicom_stl::string& filename)
{
#ifdef _WIN32
  InputStream.open(filename.c_str(), dicom_stream::ios::binary | dicom_stream::ios::in);
#else  
  InputStream.open(filename.c_str(), dicom_stream::ios::in);
#endif

  //if (InputStream.is_open())
  if (InputStream.rdbuf()->is_open())
    {
    return true;
    }
  else
    {
    return false;
    }
}

void DICOMFile::Close()
{
  InputStream.close();
}

long DICOMFile::Tell() 
{
  long loc = static_cast<long>( InputStream.tellg() );
  // dicom_stream::cout << "Tell: " << loc << dicom_stream::endl;
  return loc;
}

void DICOMFile::SkipToPos(long increment) 
{
  InputStream.seekg(increment, dicom_stream::ios::beg);
}

long DICOMFile::GetSize() 
{
  long curpos = this->Tell();

  InputStream.seekg(0,dicom_stream::ios::end);

  long size = this->Tell();
  // dicom_stream::cout << "Tell says size is: " << size << dicom_stream::endl;
  this->SkipToPos(curpos);

  return size;
}

void DICOMFile::Skip(long increment) 
{
  InputStream.seekg(increment, dicom_stream::ios::cur);
}

void DICOMFile::SkipToStart() 
{
  InputStream.seekg(0, dicom_stream::ios::beg);
}

void DICOMFile::Read(void* ptr, long nbytes) 
{
  InputStream.read((char*)ptr, nbytes);
  // dicom_stream::cout << "DICOMFile::Read " <<  (char*) ptr << dicom_stream::endl;
}
}
#ifdef _MSC_VER
#pragma warning ( pop )
#endif
