/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMethod.h
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
#ifndef __itkRegistrationMethod_h
#define __itkRegistrationMethod_h

#include "itkProcessObject.h"

namespace itk
{

/**
 * \class RegistrationMethod
 * \brief Base class for Registration Methods
 *
 * This Class define the generic interface for a registration method.
 * The basic elements of a registration method are defined in a Traits
 * class
 * 
 */

template <class TTraits>
class ITK_EXPORT RegistrationMethod : public ProcessObject 
{
public:
  /**
   * Standard "Self" typedef.
   */
   typedef RegistrationMethod  Self;


  /**
   * Standard "Superclass" typedef.
   */
   typedef ProcessObject  Superclass;


  /** 
   * Smart pointer typedef support 
   */
   typedef SmartPointer<Self>   Pointer;
   typedef SmartPointer<const Self>  ConstPointer;

  /**
   *  Type of the Target
   */
   typedef typename TTraits::TargetType              TargetType;
   typedef typename TargetType::ConstPointer              TargetConstPointer;

  /**
   *  Type of the Metric
   */
   typedef typename TTraits::MetricType              MetricType;

  /**
   *  Type of the Mapper
   */
   typedef typename TTraits::MapperType              MapperType;

  /**
   *  Type of the Transformation
   */
   typedef typename TTraits::TransformationType TransformationType;


  /**
   *  Type of the Reference
   */
   typedef typename TTraits::ReferenceType              ReferenceType;
   typedef typename ReferenceType::ConstPointer         ReferenceConstPointer;


  /**
   *  Type of the Optimizer
   */
   typedef typename TTraits::OptimizerType         OptimizerType;


  /**
   *  Type of the Transformation parameters
   *  This is the same type used to represent the search
   *  space of the optimization algorithm
   */
   typedef typename TTraits::ParametersType         ParametersType;


  /** 
   * Run-time type information (and related methods).
   */
   itkTypeMacro(RegistrationMethod, Object);


  /**
   * Method for creation through the object factory.
   */
   itkNewMacro(Self);
  

  /**
   * Method that initiates the registration.
   */
   void StartRegistration(void);


  /**
   * Set the Target
   */
   void SetTarget( const TargetType * Target );


  /**
   * Set the Reference
   */
   void SetReference( const ReferenceType * Reference );


  /**
   * Set the Optimizer
   */
   itkSetObjectMacro( Optimizer,  OptimizerType );


  /**
   * Set the Metric
   */
   itkSetObjectMacro( Metric, MetricType );


  /**
   * Get the Reference
   */
   ReferenceConstPointer GetReference( void );
   

  /**
   * Get the Target
   */
   TargetConstPointer     GetTarget( void );


  /**
   * Get the Optimizer
   */
   itkGetObjectMacro( Optimizer, OptimizerType );


  /**
   * Get the Metric
   */
   itkGetObjectMacro( Metric, MetricType );


protected:

  RegistrationMethod();
  virtual ~RegistrationMethod();
  RegistrationMethod(const Self&);
  const Self & operator=(const Self&);
 

private:

  typename MetricType::Pointer              m_Metric;
  typename OptimizerType::Pointer           m_Optimizer;

};


} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationMethod.txx"
#endif

#endif



