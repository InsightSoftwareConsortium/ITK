/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDirectory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkDirectory provides a portable way of finding the names of the files
 * in a system directory.
 *
 * itkDirectory works with windows and unix only.
 */

#ifndef __itkDirectory_h
#define __itkDirectory_h

#include "itkObject.h"

class ITK_EXPORT itkDirectory : public itkObject
{
public:
  /**
   * Return the class name as a string.
   */
  itkTypeMacro(itkDirectory,itkObject);

  /**
   * Create a new itkDirectory object.
   */
  static itkDirectory *New() 
    {return new itkDirectory;};

  /**
   * Load the specified directory and load the names of the files
   * in that directory. 0 is returned if the directory can not be 
   * opened, 1 if it is opened.   
   */
  bool Load(const char* dir);

  /**
   * Return the number of files in the current directory.
   */
  int GetNumberOfFiles() const
    { itkGetMacro(m_NumberOfFiles;) }

  /**
   * Return the file at the given index, the indexing is 0 based
   */
  const char* GetFile(int index);

protected:
  itkDirectory();
  ~itkDirectory() ;
  itkDirectory(const itkDirectory&) {};
  void operator=(const itkDirectory&) {};
  virtual void PrintSelf(std::ostream& os, itkIndent indent);

private:
  const char* m_Path;           // Path to Open'ed directory
  char** m_Files;               // Array of Files
  int m_NumberOfFiles;          // Number if files in open directory
  
};

#endif
