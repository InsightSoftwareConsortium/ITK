/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersion.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkVersion_h
#define __itkVersion_h

#include "itkObject.h"
#include "itkObjectFactory.h"

#define ITK_VERSION "0.0.1"
#define ITK_MAJOR_VERSION 0
#define ITK_MINOR_VERSION 0
#define ITK_BUILD_VERSION 2
#define ITK_SOURCE_VERSION "itk version " ITK_VERSION ", itk source $Revision: 1.181 $, $Date: 2001-02-19 09:35:11 $ (GMT)"

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
 */

class ITK_EXPORT Version : public Object 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Version             Self;

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
  itkNewMacro(Self);  

  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(Version,Object);

  /**
   * Return the version of itk this object is a part of.
   * A variety of methods are included. GetITKSourceVersion returns a string
   * with an identifier which timestamps a particular source tree. 
   */
  static const char *GetITKVersion() { return ITK_VERSION; };
  static int GetITKMajorVersion() { return ITK_MAJOR_VERSION; };
  static int GetITKMinorVersion() { return ITK_MINOR_VERSION; };
  static int GetITKBuildVersion() { return ITK_BUILD_VERSION; };
  static const char *GetITKSourceVersion() { return ITK_SOURCE_VERSION; };
  
protected:
  /**
   * insure constructor/destructor protected
   */
  Version() {}
  ~Version() {}
  Version(const Self&) {}
  void operator=(const Self&) {}

};

} // end namespace itk

#endif 
