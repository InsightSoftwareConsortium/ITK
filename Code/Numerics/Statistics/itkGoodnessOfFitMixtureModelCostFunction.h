/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGoodnessOfFitMixtureModelCostFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGoodnessOfFitMixtureModelCostFunction_h
#define __itkGoodnessOfFitMixtureModelCostFunction_h

#include "itkSingleValuedCostFunction.h"
#include "itkHistogram.h"
#include "itkGoodnessOfFitComponentBase.h"
#include "itkGoodnessOfFitFunctionBase.h"
#include "itkFunctionBase.h"

namespace itk{ 
namespace Statistics{

/** \class GoodnessOfFitMixtureModelCostFunction 
 *  \brief calculates the goodness-of-fit statstics for multivarate 
 *  mixture model 
 *
 * The goodness-of-fit statistics for a single model is discrepancy
 * between the observed frequency and the expected frequency. 
 * To reduce computational load of multivariate case, this class
 * uses projective method. 
 * 
 * The projective multivariate goodness-of-fit statistics calculation follows 
 * the following steps:
 *   
 *   1) creates a subsample that includes the measurement vectors that fall 
 *      in a spherical kernel.
 *   2) finds the base axes determined by the eigen vectors of the covariance 
 *      matrix.
 *   3) project the subsample on to one of the base axes (from step 2)
 *   4) calculates the observed frequencies (in an 1D Histogram object) after 
 *      projection (step 3) and the expected frequencies (in an 1D Histogram 
 *      object)
 *   5) calculates the discrepancy between the observed histogram and 
 *      the expected histogram using a goodness-of-fit statistics
 *   6) repeat step 3) - 5) and sum the goodness-of-fit values
 *    
 * For a mixture model, the above procedure is applied independently for each 
 * model (module). The sum of the goodness-of-fit values of models is the
 * goodness-of-fit statistics for the mixture model.
 *
 * The step 1) - 4) is done by the subclasses of GoodnessOfFitComponentBase, and
 * the step 5) is done by the subclasses of GoodnessOfFitFunctionBase.
 *
 * To see how this class interacts GoodnessOfFitComponentBase objects and 
 * GoodnessOfFitFunctionBase objects, please look at the implementation of
 * the GetValue method of this class.
 *
 * Better fit means smaller goodness-of-fit value in this implementation. 
 * This class is following the SingleValuedCostFunction interfaces so that
 * users can uses this function with any subclasses of 
 * SingleValuedNonLinearOptimizer class as long as they do not use
 * GetDerivative and GetValueAndDerivative methods.
 *
 * \sa GoodnessOfFitFunctionBase, GoodnessOfFitComponentBase, 
 * SingleValuedCostFunction, SingleValuedNonLinearOptimizer
 */

template< class TInputSample >
class ITK_EXPORT GoodnessOfFitMixtureModelCostFunction 
  : public SingleValuedCostFunction 
{
public:
  /** Standard class typedefs */
  typedef GoodnessOfFitMixtureModelCostFunction Self;
  typedef SingleValuedCostFunction Superclass;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(GoodnessOfFitMixtureModelCostFunction, SingleValuedCostFunction) ;

  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  typedef TInputSample InputSampleType ;
  itkStaticConstMacro(MeasurementVectorSize, unsigned int, 
                      TInputSample::MeasurementVectorSize) ;
  typedef typename TInputSample::MeasurementType MeasurementType ;
  typedef typename TInputSample::MeasurementVectorType MeasurementVectorType ;

  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef SingleValuedCostFunction::ParametersType ParamtersType ;

  /**  MeasureType typedef.
   *  It defines a type used to return the cost function value. */
  typedef SingleValuedCostFunction::MeasureType MeasureType ;

  typedef GoodnessOfFitComponentBase< TInputSample > ComponentType ;
  typedef std::vector< ComponentType* > ComponentVectorType ;

  typedef GoodnessOfFitFunctionBase< typename ComponentType::HistogramType > 
  FunctionType ;

  /** aceesing methods for the sample manipulator */ 
  void AddComponent(ComponentType* component) ;
  
  /** aceesing methods for the expected probability histogram */ 
  void SetFunction(FunctionType* core) ;

  FunctionType* GetFunction()
  { return m_Function ; }

  virtual unsigned int GetNumberOfParameters() const ;

  /** This method returns the value of the cost function corresponding
    * to the specified parameters. */ 
  virtual MeasureType GetValue( const ParametersType & parameters ) const ;

  /** This method returns the derivative of the cost function corresponding
    * to the specified parameters.   */ 
  virtual void GetDerivative( const ParametersType & parameters,
                                    DerivativeType & derivative ) const 
  { /* not implemented */ }

protected:
  GoodnessOfFitMixtureModelCostFunction() ;
  virtual ~GoodnessOfFitMixtureModelCostFunction() ;
  virtual void PrintSelf(std::ostream& os, Indent indent) const ;

private:
  /** helper classes */
  ComponentVectorType m_Components ;
  FunctionType* m_Function ;
} ; // end of class

} // end of namespace Statistics 
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGoodnessOfFitMixtureModelCostFunction.txx"
#endif

#endif

