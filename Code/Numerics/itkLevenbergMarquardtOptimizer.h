/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevenbergMarquardtOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLevenbergMarquardtOptimizer_h
#define __itkLevenbergMarquardtOptimizer_h

#include "itkMultipleValuedNonLinearVnlOptimizer.h"
#include "vnl/algo/vnl_levenberg_marquardt.h"

namespace itk
{
  
/** \class LevenbergMarquardtOptimizer
 * \brief Wrap of the vnl_levenberg_marquardt algorithm
 *
 * \ingroup Numerics Optimizers
 */
class ITK_EXPORT LevenbergMarquardtOptimizer : 
    public MultipleValuedNonLinearVnlOptimizer
{
public:
  /** Standard "Self" typedef. */
  typedef LevenbergMarquardtOptimizer           Self;
  typedef MultipleValuedNonLinearVnlOptimizer   Superclass;
  typedef SmartPointer<Self>                    Pointer;
  typedef SmartPointer<const Self>              ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( LevenbergMarquardtOptimizer, MultipleValuedNonLinearVnlOptimizer );

  /** InternalParameters typedef. */
  typedef   vnl_vector<double>     InternalParametersType;

  /** Internal optimizer type. */
  typedef   vnl_levenberg_marquardt   InternalOptimizerType;

  /** Method for getting access to the internal optimizer. */
  vnl_levenberg_marquardt * GetOptimizer(void);

  /** Start optimization with an initial value. */
  void StartOptimization( void );

  /** Plug in a Cost Function into the optimizer  */
  virtual void SetCostFunction( MultipleValuedCostFunction * costFunction );

protected:
  LevenbergMarquardtOptimizer();
  virtual ~LevenbergMarquardtOptimizer();

  typedef Superclass::CostFunctionAdaptorType   CostFunctionAdaptorType;

private:
  LevenbergMarquardtOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  bool                          m_OptimizerInitialized;
  InternalOptimizerType       * m_VnlOptimizer;

};

} // end namespace itk


#endif



