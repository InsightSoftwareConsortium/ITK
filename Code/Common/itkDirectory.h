/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDirectory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDirectory_h
#define __itkDirectory_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include <iostream>
#include <string>
#include <vector>

namespace itk
{
/** \class Directory
 * \brief Portable directory/filename traversal.
 * 
 * itkDirectory provides a portable way of finding the names of the files
 * in a system directory.
 *
 * itkDirectory works with windows and unix only.
 * \ingroup OSSystemObjects 
 */


class ITK_EXPORT Directory : public Object
{
public:
  /** Standard class typedefs. */
  typedef Directory           Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** Method for creation through the object factory. */
  static Pointer New()
    { Pointer n = new Self; n->UnRegister(); return n; }
  
  /** Return the class name as a string. */
  itkTypeMacro(Directory,Object);

  /** Load the specified directory and load the names of the files
   * in that directory. 0 is returned if the directory can not be 
   * opened, 1 if it is opened.    */
  bool Load(const char* dir);

  /** Return the number of files in the current directory. */
  std::vector<std::string>::size_type GetNumberOfFiles() { return m_Files.size();}

  /** Return the file at the given index, the indexing is 0 based */
  const char* GetFile(unsigned int index);

protected:
  Directory();
  ~Directory() ;
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

private:
  Directory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  std::vector<std::string> m_Files; // Array of Files
  std::string m_Path;               // Path to Open'ed directory
}; // End Class: Directory

} // end namespace itk
  
#endif
