/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersion.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVersion_h
#define __itkVersion_h

#include "itkObject.h"
#include "itkObjectFactory.h"

#define ITK_VERSION "0.0.1"
#define ITK_MAJOR_VERSION 0
#define ITK_MINOR_VERSION 0
#define ITK_BUILD_VERSION 2
#define ITK_SOURCE_VERSION "itk version " ITK_VERSION ", itk source $Revision: 1.799 $, $Date: 2003-01-02 06:10:11 $ (GMT)"

namespace itk
{
/** \class Version
 * \brief Track the current version of the software.
 *
 * Holds methods for defining/determining the current itk version
 * (major, minor, build).
 *
 * This file will change frequently to update the ITKSourceVersion which
 * timestamps a particular source release.
 *
 * \ingroup ITKSystemObjects
 */

class ITK_EXPORT Version : public Object 
{
public:
  /** Standard class typedefs. */
  typedef Version             Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Standard part of every itk Object. */
  itkTypeMacro(Version,Object);

  /** Return the version of itk this object is a part of.
   * A variety of methods are included. GetITKSourceVersion returns a string
   * with an identifier which timestamps a particular source tree.  */
  static const char *GetITKVersion() { return ITK_VERSION; };
  static int GetITKMajorVersion() { return ITK_MAJOR_VERSION; };
  static int GetITKMinorVersion() { return ITK_MINOR_VERSION; };
  static int GetITKBuildVersion() { return ITK_BUILD_VERSION; };
  static const char *GetITKSourceVersion() { return ITK_SOURCE_VERSION; };
    
protected:
  Version() {}
  ~Version() {}

private:
  Version(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#endif 
