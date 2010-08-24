/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformIOFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTransformIOFactory_h
#define __itkTransformIOFactory_h

#include "itkObject.h"
#include "itkTransformIOBase.h"

namespace itk
{
/** \class TransformIOFactory
 * \brief Create instances of TransformIO objects using an object factory.
 */
class ITK_EXPORT TransformIOFactory:public Object
{
public:
  /** Standard class typedefs. */
  typedef TransformIOFactory         Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Class Methods used to interface with the registered factories */

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformIOFactory, Object);

  /** Convenient typedefs. */
  typedef TransformIOBase::Pointer TransformIOBasePointer;

  /** Mode in which the files is intended to be used */
  typedef enum { ReadMode, WriteMode } FileModeType;

  /** Create the appropriate TransformIO depending on
   *  the particulars of the file.
   */
  static TransformIOBasePointer
  CreateTransformIO(const char *path, FileModeType mode);

  /** Register Built-in factories */
  static void RegisterBuiltInFactories();

protected:
  TransformIOFactory();
  ~TransformIOFactory();
private:
  TransformIOFactory(const Self &); //purposely not implemented
  void operator=(const Self &);     //purposely not implemented
};
} // end namespace itk

#endif
