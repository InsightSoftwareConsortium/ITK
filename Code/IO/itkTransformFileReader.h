/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformFileReader.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTransformFileReader_h
#define __itkTransformFileReader_h

#include "itkLightProcessObject.h"
#include "metaTransform.h"
#include "itkTransformBase.h"

namespace itk
{

class TransformFileReader : public LightProcessObject
{
public:

  /** SmartPointer typedef support */
  typedef TransformFileReader Self;
  typedef SmartPointer<Self> Pointer;
  typedef TransformBase TransformType;
  typedef TransformType::ParametersType ParametersType;
  typedef TransformType::Pointer TransformPointer;
  typedef std::list<TransformPointer> TransformListType;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  typedef Object Superclass;
  itkTypeMacro(TransformFileReader, LightProcessObject);

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Write out the transform */
  void Update();

  /** Get the list of transform */
  TransformListType * GetTransformList() {return & m_TransformList;}

protected:
  TransformFileReader(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
   
  std::string m_FileName;

  TransformFileReader();
  virtual ~TransformFileReader();

private:

  TransformListType    m_TransformList;
};

} // namespace itk


#endif // __itkTransformFileReader_h
