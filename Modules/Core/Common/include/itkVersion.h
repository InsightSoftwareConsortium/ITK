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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkVersion_h
#define itkVersion_h

#include "itkObject.h"
#include "itkObjectFactory.h"

#define ITK_VERSION_TO_STRING(x) ITK_VERSION_TO_STRING0(x)
#define ITK_VERSION_TO_STRING0(x) #x
#define ITK_VERSION                            \
  ITK_VERSION_TO_STRING(ITK_VERSION_MAJOR) "." \
  ITK_VERSION_TO_STRING(ITK_VERSION_MINOR) "." \
  ITK_VERSION_TO_STRING(ITK_VERSION_PATCH)
#define ITK_SOURCE_VERSION "itk version " ITK_VERSION

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
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT Version:public Object
{
public:
  /** Standard class typedefs. */
  typedef Version                    Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard part of every itk Object. */
  itkTypeMacro(Version, Object);

  /** Return the version of itk this object is a part of. */
  static const char * GetITKVersion();

  /** Get the itk major version. */
  static int GetITKMajorVersion();

  /** Get the itk minor version. */
  static int GetITKMinorVersion();

  /** Get the itk build version. */
  static int GetITKBuildVersion();

  /** Get a string with an identifier which timestamps a particular source tree. */
  static const char * GetITKSourceVersion();

protected:
  Version();
  ~Version() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Version);
};
} // end namespace itk

#endif
