/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDirectory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDirectory_h
#define __itkDirectory_h

#include "itkObject.h"

namespace itk
{
/** \class Directory
 * \brief Portable directory/filename traversal.
 * 
 * itk::Directory provides a portable way of finding the names of the files
 * in a system directory.
 *
 * itk::Directory works with Windows and Unix (POSIX) operating systems.
 * \ingroup OSSystemObjects 
 */


class Directory;
class ITKCommon_EXPORT Directory : public Object
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
  std::vector<std::string>::size_type GetNumberOfFiles();

  /** Return the file at the given index, the indexing is 0 based */
  const char* GetFile(unsigned int index);

protected:
  Directory();
  ~Directory() ;
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

private:
  Directory(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  Directory* m_Internal;
}; // End Class: Directory

} // end namespace itk
  
#endif
