/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPDEDeformableRegistrationFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPDEDeformableRegistrationFunction_h_
#define _itkPDEDeformableRegistrationFunction_h_

#include "itkFiniteDifferenceFunction.h"

namespace itk {

/** \class PDEDeformableRegistrationFunction
 *
 * This is an abstract base class for all PDE functions which drives a
 * deformable registration algorithm. It is used by 
 * PDEDeformationRegistrationFilter subclasses to compute the
 * output deformation field which will map a reference image onto
 * a target image.
 *
 * This class is templated over the Reference image type, Target image type
 * and the deformation field type.
 *
 * \sa PDEDeformableRegistrationFilter
 * \ingroup FiniteDifferenceFunctions
 */
template<class TReference, class TTarget, class TDeformationField>
class ITK_EXPORT PDEDeformableRegistrationFunction : 
  public FiniteDifferenceFunction<TDeformationField>
{
public:
  /** Standard class typedefs. */
  typedef PDEDeformableRegistrationFunction    Self;
  typedef FiniteDifferenceFunction<TDeformationField>    Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro( PDEDeformableRegistrationFunction, 
    FiniteDifferenceFunction );

  /** Reference image type. */
  typedef TReference   ReferenceType;
  typedef typename ReferenceType::ConstPointer  ReferencePointer;

  /** Target image type. */
  typedef TTarget    TargetType;
  typedef typename TargetType::ConstPointer  TargetPointer;
  
  /** Deformation field type. */
  typedef TDeformationField    DeformationFieldType;
  typedef typename DeformationFieldType::Pointer   
    DeformationFieldTypePointer;

  /** Set the reference image.  */
  void SetReference( const ReferenceType * ptr )
    { m_Reference = ptr; }

  /** Set the reference image. */
  ReferencePointer GetReference()
    { return m_Reference; }

  /** Set the target. */
  void SetTarget( const TargetType * ptr )
    { m_Target = ptr; }

  /** Get the target. */
  TargetPointer GetTarget()
    { return m_Target; }

protected:
  PDEDeformableRegistrationFunction()
    {
      m_Reference = NULL;
      m_Target = NULL;
    }

  ~PDEDeformableRegistrationFunction() {}

  void PrintSelf(std::ostream& os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "Reference: ";
    os << m_Reference.GetPointer() << std::endl;
    os << indent << "Target: ";
    os << m_Target.GetPointer() << std::endl;

  };

  /** The reference (from) image. */
  ReferencePointer                m_Reference;
  
  /** The target (to) image. */
  TargetPointer                   m_Target;

private:
  PDEDeformableRegistrationFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};


} // end namespace itk


#endif
