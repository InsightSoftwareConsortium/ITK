/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationTransform.h
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
#ifndef __itkRegistrationTransform_h
#define __itkRegistrationTransform_h

#include "itkObject.h"

namespace itk
{

/**
 * \class RegistrationTransform
 * \brief Base class for registration methods
 *
 * This Class define the generic interface for a registration method.
 * The basic elements of a registration method are:
 *   - Metric to compare the reference and the target
 *   - Transformation used to register the reference against the target
 *   - Optimization method used to search for the best transformation
 * 
 * Registration is not limited to Images, and for this reason
 * this class is templated over the type of the reference object,
 * the target object and the transformation. This types are obtained
 * from the Metric type, to reduce the number of redundant
 * template parameters
 *
 */

template <class TMetric, class TOptimizer >
class ITK_EXPORT RegistrationTransform : public Object 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegistrationTransform  Self;


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
   *  Type of the Metric
   */
  typedef TMetric               MetricType;


  /**
   *  Type of the Optimizer 
   */
  typedef TOptimizer            OptimizerType;


  /**
   *  Type of the Reference
   */
  typedef typename MetricType::ReferenceType  ReferenceType;


  /**
   *  Type of the Metric
   */
  typedef typename MetricType::TargetType  TargetType;
 

  /**
   *  Type of the Mapper
   */
  typedef typename MetricType::MapperType  MapperType;


  /**
   *  Type of the Transformation
   */
  typedef typename MapperType::TransformationType  TransformationType;
 
  
  /**
   *  Pointer type for the Reference 
   */
  typedef typename ReferenceType::ConstPointer ReferenceConstPointer;

  
  /**
   *  Pointer type for the Target 
   */
  typedef typename TargetType::ConstPointer TargetConstPointer;


  /**
   *  Pointer type for the Transformation
   */
  typedef typename TransformationType::Pointer TransformationPointer;


  /**
   *  Pointer type for the metric
   */
  typedef typename MetricType::Pointer        MetricPointer;


  /**
   *  Pointer type for the mapper
   */
  typedef typename MapperType::Pointer        MapperPointer;


  /**
   *  Pointer type for the optimizer 
   */
  typedef typename OptimizerType::Pointer     OptimizerPointer;


 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RegistrationTransform, Object);


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
   itkSetConstObjectMacro( Target, TargetType );

   
  /**
   * Set the Reference
   */
   itkSetConstObjectMacro( Reference, ReferenceType );


  /**
   * Set the Transformation
   */
   void SetTransformation( TransformationType * );


  /**
   * Get the Reference
   */
   itkGetConstObjectMacro( Reference, ReferenceType );

   
  /**
   * Get the Target
   */
   itkGetConstObjectMacro( Target, TargetType );


  /**
   * Get the Transformation
   */
   itkGetMacro( Transformation, TransformationPointer );


  /**
   * Get the Optimizer
   */
   itkGetMacro( Optimizer, OptimizerPointer );




protected:

  RegistrationTransform();
  virtual ~RegistrationTransform();
  RegistrationTransform(const Self&);
  const Self & operator=(const Self&);


private:

  TargetPointer              m_Target;
  ReferencePointer           m_Reference;
  TransformationPointer      m_Transformation;
  MapperPointer              m_Mapper;  
  MetricPointer              m_Metric;
  OptimizerPointer           m_Optimizer;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationTransform.txx"
#endif

#endif



