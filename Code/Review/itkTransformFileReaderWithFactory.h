/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformFileReaderWithFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTransformFileReaderWithFactory_h
#define __itkTransformFileReaderWithFactory_h

#include "itkLightProcessObject.h"
#include "metaTransform.h"
#include "itkTransformIOBase.h"

namespace itk
{
class TransformFileReader:public LightProcessObject
{
public:

  /** SmartPointer typedef support */
  typedef TransformFileReader  Self;
  typedef SmartPointer< Self > Pointer;

  typedef TransformBase TransformType;

  typedef TransformType::ParametersType      ParametersType;
  typedef TransformIOBase::TransformPointer  TransformPointer;
  typedef TransformIOBase::TransformListType TransformListType;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  typedef Object Superclass;
  itkTypeMacro(TransformFileReader, LightProcessObject);

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Read the transform */
  void Update();

  /** Get the list of transform */
  TransformListType * GetTransformList() { return &m_TransformList; }
protected:
  TransformIOBase::Pointer m_TransformIO;
  TransformFileReader(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented

  std::string m_FileName;

  TransformFileReader();
  virtual ~TransformFileReader();
  void CreateTransform(TransformPointer & ptr, const std::string & ClassName);

private:
  TransformListType m_TransformList;
};
} // namespace itk

#endif // __itkTransformFileReader_h
