/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkPDEDeformableRegistrationFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkPDEDeformableRegistrationFunction_h_
#define _itkPDEDeformableRegistrationFunction_h_

#include "itkFiniteDifferenceFunction.h"

namespace itk {

/** \class PDEDeformableRegistrationFunction
 *
 * This is a base class for all PDE functions which drives a
 * deformable registration algorithm. It is used by 
 * PDEDeformationRegistrationFilter subclasses to compute the
 * output deformation field which will map a reference image onto
 * a target image.
 *
 * This class is templated over the Reference image type, Target image type
 * and the deformation field type.
 *
 * \sa PDEDeformableRegistrationFilter
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

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

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
