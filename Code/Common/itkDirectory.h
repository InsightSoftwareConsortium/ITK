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
#ifndef __itkDirectory_h
#define __itkDirectory_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include <iostream>
#include <string>
#include <vector>

ITK_NAMESPACE_BEGIN
/** \class Directory
 * \brief Portable directory/filename traversal.
 * 
 * itkDirectory provides a portable way of finding the names of the files
 * in a system directory.
 *
 * itkDirectory works with windows and unix only.
 */


class ITK_EXPORT Directory : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Directory           Self;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Return the class name as a string.
   */
  itkTypeMacro(Directory,Object);

  /**
   * Load the specified directory and load the names of the files
   * in that directory. 0 is returned if the directory can not be 
   * opened, 1 if it is opened.   
   */
  bool Load(const char* dir);

  /**
   * Return the number of files in the current directory.
   */
  int GetNumberOfFiles() { return m_Files.size();}

  /**
   * Return the file at the given index, the indexing is 0 based
   */
  const char* GetFile(unsigned int index);

protected:
  Directory();
  ~Directory() ;
  Directory(const Self&) {}
  void operator=(const Self&) {}
  virtual void PrintSelf(std::ostream& os, Indent indent);

private:
  std::vector<std::string> m_Files; // Array of Files
  std::string m_Path;               // Path to Open'ed directory
}; // End Class: Directory

ITK_NAMESPACE_END
  
#endif
