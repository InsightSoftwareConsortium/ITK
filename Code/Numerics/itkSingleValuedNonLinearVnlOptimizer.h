/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSingleValuedNonLinearVnlOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSingleValuedNonLinearVnlOptimizer_h
#define __itkSingleValuedNonLinearVnlOptimizer_h

#include "itkSingleValuedNonLinearOptimizer.h"
#include "itkSingleValuedVnlCostFunctionAdaptor.h"


namespace itk
{
  
/** \class SingleValuedNonLinearVnlOptimizer
 * \brief This class is a base for the Optimization methods that 
 * optimize a single valued function. 
 *
 * It is an Adaptor class for optimizers provided by the vnl library
 *
 * \ingroup Numerics Optimizers
 */
class ITK_EXPORT SingleValuedNonLinearVnlOptimizer : 
    public SingleValuedNonLinearOptimizer
{
public:
  /** Standard class typedefs. */
  typedef SingleValuedNonLinearVnlOptimizer     Self;
  typedef SingleValuedNonLinearOptimizer        Superclass;
  typedef SmartPointer<Self>                    Pointer;
  typedef SmartPointer<const Self>              ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( SingleValuedNonLinearVnlOptimizer, 
                SingleValueNonLinearOptimizer );

  /** Set the cost Function. This method has to be overloaded
   *  by derived classes because the CostFunctionAdaptor requires
   *  to know the number of parameters at construction time. This
   *  number of parameters is obtained at run-time from the itkCostFunction.
   *  As a consequence each derived optimizer should construct its own 
   *  CostFunctionAdaptor when overloading this method  */
  virtual void SetCostFunction( SingleValuedCostFunction * costFunction ) = 0;

protected:
  SingleValuedNonLinearVnlOptimizer();
  virtual ~SingleValuedNonLinearVnlOptimizer();

  typedef SingleValuedVnlCostFunctionAdaptor   CostFunctionAdaptorType;

  void SetCostFunctionAdaptor( CostFunctionAdaptorType * adaptor );
  const CostFunctionAdaptorType * GetCostFunctionAdaptor( void ) const;

private:
  SingleValuedNonLinearVnlOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  CostFunctionAdaptorType * m_CostFunctionAdaptor;

};

} // end namespace itk


  


#endif



