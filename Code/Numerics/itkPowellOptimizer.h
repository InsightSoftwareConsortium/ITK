/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPowellOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPowellOptimizer_h
#define __itkPowellOptimizer_h

#include <itkVector.h>
#include <itkMatrix.h>
#include <itkSingleValuedNonLinearOptimizer.h>

namespace itk
{

/** \class PowellOptimizer
 * \brief Implements Powell optimization using Brent line search - adapted from
 * Numerical Recipes in C (first edition).
 *
 * This optimizer needs a cost function.
 * Partial derivatives of that function are not required.
 *
 * For an N-dimensional parameter space, each iteration minimizes(maximizes)
 * the function in N (initially orthogonal) directions.  Typically only 2-5
 * iterations are required.   If gradients are available, consider a conjugate
 * gradient line search strategy.
 *
 * The SetStepLength determines the initial distance to step in a line direction
 * when bounding the minimum (using bracketing triple spaced using a golden
 * search strategy).
 *
 * The StepTolerance terminates optimization when the parameter values are
 * known to be within this (scaled) distance of the local extreme.
 *
 * The ValueTolerance terminates optimization when the cost function values at
 * the current parameters and at the local extreme are likely (within a second
 * order approximation) to be within this is tolerance.
 *
 * \ingroup Numerics Optimizers
 *
 */

class ITK_EXPORT PowellOptimizer: 
    public SingleValuedNonLinearOptimizer
{
public:
  /** Standard "Self" typedef. */
  typedef PowellOptimizer             Self ;
  typedef SingleValuedNonLinearOptimizer      Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;

  typedef SingleValuedNonLinearOptimizer::ParametersType
                                              ParametersType;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
 
  /** Run-time type information (and related methods). */
  itkTypeMacro(PowellOptimizer, SingleValuedNonLinearOptimizer );
  
  /** Type of the Cost Function   */
  typedef  SingleValuedCostFunction         CostFunctionType;
  typedef  CostFunctionType::Pointer        CostFunctionPointer;

  /** Set if the Optimizer should Maximize the metric */
  itkSetMacro( Maximize, bool );
  itkGetConstReferenceMacro( Maximize, bool );

  /** Set/Get maximum iteration limit. */
  itkSetMacro( MaximumIteration, unsigned int );
  itkGetConstReferenceMacro( MaximumIteration, unsigned int );

  /** Set/Get the maximum number of line search iterations */
  itkSetMacro(MaximumLineIteration, unsigned int);
  itkGetConstMacro(MaximumLineIteration, unsigned int);

  /** Set/Get StepLength for the (scaled) spacing of the sampling of
   * parameter space while bracketing the extremum */
  itkSetMacro( StepLength, double ) ;
  itkGetConstReferenceMacro( StepLength, double );

  /** Set/Get StepTolerance.  Once the local extreme is known to be within this
   * distance of the current parameter values, optimization terminates */
  itkSetMacro( StepTolerance, double ) ;
  itkGetConstReferenceMacro( StepTolerance, double );

  /** Set/Get ValueTolerance.  Once this current cost function value is known
   * to be within this tolerance of the cost function value at the local
   * extreme, optimization terminates */
  itkSetMacro( ValueTolerance, double ) ;
  itkGetConstReferenceMacro( ValueTolerance, double );

  /** Return Current Value */
  itkGetConstReferenceMacro( CurrentCost, MeasureType );
  MeasureType GetValue() const { return this->GetCurrentCost(); }

  /** Return Current Iteration */
  itkGetConstReferenceMacro( CurrentIteration, unsigned int);

  /** Get the current line search iteration */
  itkGetConstReferenceMacro( CurrentLineIteration, unsigned int);

  /** Start optimization. */
  void StartOptimization() ;

  /** When users call StartOptimization, this value will be set false.
   * By calling StopOptimization, this flag will be set true, and 
   * optimization will stop at the next iteration. */
  void StopOptimization() 
    { m_Stop = true ; }

protected:
  PowellOptimizer() ;
  PowellOptimizer(const PowellOptimizer&) ;
  virtual ~PowellOptimizer() ;
  void PrintSelf(std::ostream& os, Indent indent) const;

  itkSetMacro(CurrentCost, double);

  /** Used to specify the line direction through the n-dimensional parameter
   * space the is currently being bracketed and optimized. */
  void SetLine(const ParametersType & origin,
               const vnl_vector<double> & direction);

  /** Get the value of the n-dimensional cost function at this scalar step
   * distance along the current line direction from the current line origin.
   * Line origin and distances are set via SetLine */
  double GetLineValue(double x) const;

  /** Set the given scalar step distance (x) and function value (fx) as the
   * "best-so-far" optimizer values. */
  void   SetCurrentLinePoint(double x, double fx);

  /** Used in bracketing the extreme along the current line.
   * Adapted from NRC */
  void   Swap(double *a, double *b) const;

  /** Used in bracketing the extreme along the current line.
   * Adapted from NRC */
  void   Shift(double *a, double *b, double *c, double d) const;

  /** The LineBracket routine from NRC. Uses current origin and line direction
   * (from SetLine) to find a triple of points (ax, bx, cx) that bracket the
   * extreme "near" the origin.  Search first considers the point StepLength 
   * distance from ax.
   * IMPORTANT: The value of ax and the value of the function at ax (i.e., fa),
   * must both be provided to this function. */
  virtual void   LineBracket(double *ax, double *bx, double *cx,
                             double *fa, double *fb, double *fc);

  /** Given a bracketing triple of points and their function values, returns
   * a bounded extreme.  These values are in parameter space, along the 
   * current line and wrt the current origin set via SetLine.   Optimization
   * terminates based on MaximumIteration, StepTolerance, or ValueTolerance. 
   * Implemented as Brent line optimers from NRC.  */
  virtual void   BracketedLineOptimize(double ax, double bx, double cx,
                                       double fa, double fb, double fc,
                                       double * extX, double * extVal);

  itkGetMacro(SpaceDimension, unsigned int);
  itkSetMacro(SpaceDimension, unsigned int);

  itkSetMacro(CurrentIteration, unsigned int);

  itkGetMacro(Stop, bool);
  itkSetMacro(Stop, bool);



  unsigned int       m_SpaceDimension;

  /** Current iteration */
  unsigned int       m_CurrentIteration ;
  unsigned int       m_CurrentLineIteration ;

  /** Maximum iteration limit. */
  unsigned int       m_MaximumIteration ;
  unsigned int       m_MaximumLineIteration ;

  /** Set if the Metric should be maximized: Default = False */
  bool               m_Maximize;

  /** The minimal size of search */
  double             m_StepLength ;
  double             m_StepTolerance ;

  ParametersType     m_LineOrigin;
  vnl_vector<double> m_LineDirection;

  double             m_ValueTolerance;

  /** Internal storage for the value type / used as a cache  */
  MeasureType        m_CurrentCost;

  /** this is user-settable flag to stop optimization.
   * when users call StartOptimization, this value will be set false.
   * By calling StopOptimization, this flag will be set true, and 
   * optimization will stop at the next iteration. */
  bool               m_Stop ;

} ; // end of class

} // end of namespace itk

#endif
