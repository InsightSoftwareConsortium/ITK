/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMapper.h
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
 */

template <class TDomain, class TTransformation> 
class ITK_EXPORT RegistrationMapper : public Object 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegistrationMapper  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /**
   *  Type of the Domain
   */
  typedef TDomain            DomainType;


  /**
   *  Type of the Transformation
   */
  typedef TTransformation       TransformationType;
  

  /**
   *  Pointer type for the Reference 
   */
  typedef typename DomainType::Pointer DomainPointer;


  /**
   *  Const Pointer type for the Reference 
   */
  typedef typename DomainType::ConstPointer DomainConstPointer;


  /**
   *  Pointer type for the Transformation
   */
  typedef typename TransformationType::Pointer TransformationPointer;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RegistrationMapper, Object);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Connect the Domain
   */
  itkSetConstObjectMacro( Domain, DomainType );


  /**
   * Get the Domain
   */
  itkGetConstObjectMacro( Domain, DomainType);

  /**
   * Connect the Transformation
   */
  itkSetObjectMacro( Transformation,TransformationType);

  /**
   * Get the Transformation
   */
  itkGetObjectMacro( Transformation,TransformationType);
  


protected:
  
  RegistrationMapper();
  virtual ~RegistrationMapper() {};
  RegistrationMapper(const Self&) {}
  void operator=(const Self&) {}


private:

  DomainConstPointer       m_Domain;
  TransformationPointer    m_Transformation;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationMapper.txx"
#endif

#endif



