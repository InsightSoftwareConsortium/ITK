/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkDirectory_h
#define itkDirectory_h

#include "itkObject.h"
#include "itksys/Directory.hxx"

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
 *
 * See also itksys::Directory
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT Directory:public Object
{
public:
  /** Standard class typedefs. */
  typedef Directory                  Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  static Pointer New()
  { Pointer n = new Self; n->UnRegister(); return n; }

  /** Return the class name as a string. */
  itkTypeMacro(Directory, Object);

  /** Load the specified directory and load the names of the files
   * in that directory. 0 is returned if the directory can not be
   * opened, 1 if it is opened.    */
  bool Load(const char *dir);

  /** Return the number of files in the current directory. */
  std::vector< std::string >::size_type GetNumberOfFiles();

  /** Return the file at the given index, the indexing is 0 based */
  const char * GetFile(unsigned int index);

protected:
  Directory();
  ~Directory() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Directory);

  ::itksys::Directory *m_Internal;
}; // End Class: Directory
} // end namespace itk

#endif
