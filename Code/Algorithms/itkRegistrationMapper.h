/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMapper.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRegistrationMapper_h
#define __itkRegistrationMapper_h

#include "itkObject.h"

namespace itk
{
  
/** \class RegistrationMapper
 * \brief Maps one object on the coordinate system of other.
 *
 * This Class is templated over the type of the mapped object
 * and over the type of the transformation used to convert the
 * coordinate system.
 *
 * \ingroup Functions
 */
template <class TDomain, class TTransform> 
class ITK_EXPORT RegistrationMapper : public Object 
{
public:
  /** Standard class typedefs. */
  typedef RegistrationMapper  Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**  Type of the domain. */
  typedef TDomain            DomainType;


  /**  Type of the transform. */
  typedef TTransform       TransformType;

  /**  Pointer type for the reference. */
  typedef typename DomainType::Pointer DomainPointer;

  /**  Const Pointer type for the reference.  */
  typedef typename DomainType::ConstPointer DomainConstPointer;

  /**  Pointer type for the transform. */
  typedef typename TransformType::Pointer TransformPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(RegistrationMapper, Object);

  /** Connect the domain. */
  itkSetConstObjectMacro( Domain, DomainType );

  /** Get the domain. */
  itkGetConstObjectMacro( Domain, DomainType);

  /** Connect the transform. */
  itkSetObjectMacro( Transform,TransformType);

  /** Get the transform. */
  itkGetObjectMacro( Transform,TransformType);
  
protected:
  RegistrationMapper();
  virtual ~RegistrationMapper() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  // This is protected as opposed to private because it needs to accessed
  // by subclasses in methods that are called by algorithm inner loops.
  // The overhead of the GetTransform() method is too high in this case
  // since it needs to construct a SmartPointer.
  TransformPointer         m_Transform;

private:
  RegistrationMapper(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  DomainConstPointer       m_Domain;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationMapper.txx"
#endif

#endif



