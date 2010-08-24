/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformFileWriterWithFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTransformFileWriterWithFactory_h
#define __itkTransformFileWriterWithFactory_h

#include "itkLightProcessObject.h"
#include "metaTransform.h"
#include "itkTransformBase.h"
#include "itkTransformIOBase.h"
#include <iostream>
#include <fstream>

namespace itk
{
class TransformFileWriter:public LightProcessObject
{
public:

  /** SmartPointer typedef support */
  typedef TransformFileWriter  Self;
  typedef LightProcessObject   Superclass;
  typedef SmartPointer< Self > Pointer;

  typedef TransformBase                           TransformType;
  typedef TransformIOBase::ConstTransformPointer  ConstTransformPointer;
  typedef TransformIOBase::ConstTransformListType ConstTransformListType;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformFileWriter, LightProcessObject);

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Set/Get the write mode (append/overwrite) for the Filter */
  void SetAppendOff();

  void SetAppendOn();

  void SetAppendMode(bool mode);

  bool GetAppendMode();

  /** Set/Get the input transform to write */
  void SetInput(const TransformType *transform);

  ConstTransformPointer GetInput() { return *( m_TransformList.begin() ); }

  /** Add a transform to be written */
  void AddTransform(const TransformType *transform);

  /** Set/Get the precision of the writing */
  itkSetMacro(Precision, unsigned int);
  itkGetConstMacro(Precision, unsigned int);

  /** Write out the transform */
  void Update();

protected:
  TransformFileWriter(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented

  TransformFileWriter();
  virtual ~TransformFileWriter();
private:
  void OpenStream(std::ofstream & out, bool binary);

  std::string            m_FileName;
  ConstTransformListType m_TransformList;
  unsigned int           m_Precision;
  bool                   m_AppendMode;
};
} // namespace itk

#endif // __itkTransformFileWriter_h
