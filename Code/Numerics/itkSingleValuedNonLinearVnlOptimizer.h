/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSingleValuedNonLinearVnlOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSingleValuedNonLinearVnlOptimizer_h
#define __itkSingleValuedNonLinearVnlOptimizer_h

#include "itkSingleValuedNonLinearOptimizer.h"
#include "itkSingleValuedVnlCostFunctionAdaptor.h"
#include "itkCommand.h"


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

  /** Command observer that will interact with the ITK-VNL cost-function
   * adaptor in order to generate iteration events. This will allow to overcome
   * the limitation of VNL optimizers not offering callbacks for every
   * iteration */
  typedef SimpleMemberCommand< Self >           CommandType;

  /** Set the cost Function. This method has to be overloaded
   *  by derived classes because the CostFunctionAdaptor requires
   *  to know the number of parameters at construction time. This
   *  number of parameters is obtained at run-time from the itkCostFunction.
   *  As a consequence each derived optimizer should construct its own 
   *  CostFunctionAdaptor when overloading this method  */
  virtual void SetCostFunction( SingleValuedCostFunction * costFunction ) = 0;

  /** Methods to define whether the cost function will be maximized or
   * minimized. By default the VNL amoeba optimizer is only a minimizer.
   * Maximization is implemented here by notifying the CostFunctionAdaptor
   * which in its turn will multiply the function values and its derivative by
   * -1.0. */
  itkGetConstReferenceMacro( Maximize, bool );
  itkSetMacro( Maximize, bool );
  itkBooleanMacro( Maximize );
  bool GetMinimize( ) const
  { return !m_Maximize; }
  void SetMinimize(bool v)
  { this->SetMaximize(!v); }
  void MinimizeOn()
  { this->MaximizeOff(); }
  void MinimizeOff()
  { this->MaximizeOn(); }
 
protected:
  SingleValuedNonLinearVnlOptimizer();
  virtual ~SingleValuedNonLinearVnlOptimizer();

  typedef SingleValuedVnlCostFunctionAdaptor   CostFunctionAdaptorType;

  void SetCostFunctionAdaptor( CostFunctionAdaptorType * adaptor );
  const CostFunctionAdaptorType * GetCostFunctionAdaptor( void ) const;
  CostFunctionAdaptorType * GetCostFunctionAdaptor( void );

  /** The purpose of this method is to get around the lack of
   *  const-correctness in VNL cost-functions and optimizers */
  CostFunctionAdaptorType * GetNonConstCostFunctionAdaptor( void ) const;

  /** Print out internal state */
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  /** Callback function for the Command Observer */
  void IterationReport();
  
  SingleValuedNonLinearVnlOptimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  CostFunctionAdaptorType * m_CostFunctionAdaptor;

  bool                      m_Maximize;

  CommandType::Pointer      m_Command;
};

} // end namespace itk


  


#endif



