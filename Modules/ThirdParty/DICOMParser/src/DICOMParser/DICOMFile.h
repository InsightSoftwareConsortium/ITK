/*=========================================================================

  Program:   DICOMParser
  Module:    DICOMFile.h
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

#ifndef __DICOMFILE_H_
#define __DICOMFILE_H_

#ifdef _MSC_VER
#pragma warning ( disable : 4514 )
#pragma warning ( push, 3 )
#endif 

#include <stdio.h>
#include <stdlib.h>
#include <string>

#include "DICOMTypes.h"
#include "DICOMConfig.h"
#include "DICOMSource.h"

namespace DICOMPARSER_NAMESPACE
{
//
// DICOM data source that is file.
//
class DICOM_EXPORT DICOMFile : public DICOMSource
{
 public:
  DICOMFile();
  virtual ~DICOMFile();
  
  //
  // Open a file with filename.  Returns a bool
  // that is true if the file is successfully
  // opened.
  //
  bool Open(const dicom_stl::string& filename);
  
  //
  // Close a file.
  //
  void Close();
  
  //
  // Return the position in the file.
  //
  long Tell();
  
  // 
  // Move to a particular position in the file.
  //
  void SkipToPos(long);
  
  //
  // Return the size of the file.
  //
  long GetSize();
  
  //
  // Skip a number of bytes.
  // 
  void Skip(long);
  
  //
  // Skip to the beginning of the file.
  //
  void SkipToStart();
  
  //
  // Read data of length len.
  //
  void Read(void* data, long len);
  
 protected:
  DICOMFile(const DICOMFile&);
  void operator=(const DICOMFile&);  

  //
  // Internal storage for the file pointer.
  //
  dicom_stream::ifstream InputStream;
  

 private:

};
}
#ifdef _MSC_VER
#pragma warning ( pop )
#endif

#endif // __DICOMFILE_H_


