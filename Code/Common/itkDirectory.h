/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDirectory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 */


class ITK_EXPORT Directory : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Directory           Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Method for creation through the object factory.
   */
  static Pointer New()
    {
      return new Directory;
    }
  

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
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

private:
  std::vector<std::string> m_Files; // Array of Files
  std::string m_Path;               // Path to Open'ed directory
}; // End Class: Directory

} // end namespace itk
  
#endif
