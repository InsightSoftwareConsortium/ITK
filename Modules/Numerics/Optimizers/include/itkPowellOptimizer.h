/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkPowellOptimizer_h
#define itkPowellOptimizer_h

#include "itkVector.h"
#include "itkMatrix.h"
#include "itkSingleValuedNonLinearOptimizer.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class PowellOptimizer
 * \brief Implements Powell optimization using Brent line search.
 *
 * The code in this class was adapted from the Wikipedia and the
 * netlib.org zeroin function.
 *
 * http://www.netlib.org/go/zeroin.f
 * http://en.wikipedia.org/wiki/Brent_method
 * http://en.wikipedia.org/wiki/Golden_section_search
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
 * \ingroup ITKOptimizers
 */

class ITKOptimizers_EXPORT PowellOptimizer:
  public SingleValuedNonLinearOptimizer
{
public:
  /** Standard "Self" typedef. */
  typedef PowellOptimizer                Self;
  typedef SingleValuedNonLinearOptimizer Superclass;
  typedef SmartPointer< Self >           Pointer;
  typedef SmartPointer< const Self >     ConstPointer;

  typedef SingleValuedNonLinearOptimizer::ParametersType
  ParametersType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PowellOptimizer, SingleValuedNonLinearOptimizer);

  /** Type of the Cost Function   */
  typedef  SingleValuedCostFunction  CostFunctionType;
  typedef  CostFunctionType::Pointer CostFunctionPointer;

  /** Set if the Optimizer should Maximize the metric */
  itkSetMacro(Maximize, bool);
  itkBooleanMacro(Maximize);
  itkGetConstReferenceMacro(Maximize, bool);

  /** Set/Get maximum iteration limit. */
  itkSetMacro(MaximumIteration, unsigned int);
  itkGetConstReferenceMacro(MaximumIteration, unsigned int);

  /** Set/Get the maximum number of line search iterations */
  itkSetMacro(MaximumLineIteration, unsigned int);
  itkGetConstMacro(MaximumLineIteration, unsigned int);

  /** Set/Get StepLength for the (scaled) spacing of the sampling of
   * parameter space while bracketing the extremum */
  itkSetMacro(StepLength, double);
  itkGetConstReferenceMacro(StepLength, double);

  /** Set/Get StepTolerance.  Once the local extreme is known to be within this
   * distance of the current parameter values, optimization terminates */
  itkSetMacro(StepTolerance, double);
  itkGetConstReferenceMacro(StepTolerance, double);

  /** Set/Get ValueTolerance.  Once this current cost function value is known
   * to be within this tolerance of the cost function value at the local
   * extreme, optimization terminates */
  itkSetMacro(ValueTolerance, double);
  itkGetConstReferenceMacro(ValueTolerance, double);

  /** Return Current Value */
  itkGetConstReferenceMacro(CurrentCost, MeasureType);
  MeasureType GetValue() const { return this->GetCurrentCost(); }

  /** Return Current Iteration */
  itkGetConstReferenceMacro(CurrentIteration, unsigned int);

  /** Get the current line search iteration */
  itkGetConstReferenceMacro(CurrentLineIteration, unsigned int);

  /** Start optimization. */
  virtual void StartOptimization() ITK_OVERRIDE;

  /** When users call StartOptimization, this value will be set false.
   * By calling StopOptimization, this flag will be set true, and
   * optimization will stop at the next iteration. */
  void StopOptimization()
  { m_Stop = true; }

  itkGetConstReferenceMacro(CatchGetValueException, bool);
  itkSetMacro(CatchGetValueException, bool);

  itkGetConstReferenceMacro(MetricWorstPossibleValue, double);
  itkSetMacro(MetricWorstPossibleValue, double);

  virtual const std::string GetStopConditionDescription() const ITK_OVERRIDE;

protected:
  PowellOptimizer();
  PowellOptimizer(const PowellOptimizer &);
  virtual ~PowellOptimizer() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  itkSetMacro(CurrentCost, double);

  /** Used to specify the line direction through the n-dimensional parameter
   * space the is currently being bracketed and optimized. */
  void SetLine(const ParametersType & origin,
               const vnl_vector< double > & direction);

  /** Get the value of the n-dimensional cost function at this scalar step
   * distance along the current line direction from the current line origin.
   * Line origin and distances are set via SetLine */
  double GetLineValue(double x) const;

  double GetLineValue(double x, ParametersType & tempCoord) const;

  /** Set the given scalar step distance (x) and function value (fx) as the
   * "best-so-far" optimizer values. */
  void   SetCurrentLinePoint(double x, double fx);

  /** Used in bracketing the extreme along the current line.
   * Adapted from NRC */
  void   Swap(double *a, double *b) const;

  /** Used in bracketing the extreme along the current line.
   * Adapted from NRC */
  void   Shift(double *a, double *b, double *c, double d) const;

  /** The LineBracket routine from NRC. Later reimplemented from the description
   * of the method available in the Wikipedia.
   *
   * Uses current origin and line direction (from SetLine) to find a triple of
   * points (ax, bx, cx) that bracket the extreme "near" the origin.  Search
   * first considers the point StepLength distance from ax.
   *
   * IMPORTANT: The value of ax and the value of the function at ax (i.e., fa),
   * must both be provided to this function. */
  virtual void   LineBracket(double *ax, double *bx, double *cx,
                             double *fa, double *fb, double *fc);

  virtual void   LineBracket(double *ax, double *bx, double *cx,
                             double *fa, double *fb, double *fc,
                             ParametersType & tempCoord);

  /** Given a bracketing triple of points and their function values, returns
   * a bounded extreme.  These values are in parameter space, along the
   * current line and wrt the current origin set via SetLine.   Optimization
   * terminates based on MaximumIteration, StepTolerance, or ValueTolerance.
   * Implemented as Brent line optimers from NRC.  */
  virtual void   BracketedLineOptimize(double ax, double bx, double cx,
                                       double fa, double fb, double fc,
                                       double *extX, double *extVal);

  virtual void   BracketedLineOptimize(double ax, double bx, double cx,
                                       double fa, double fb, double fc,
                                       double *extX, double *extVal,
                                       ParametersType & tempCoord);

  itkGetMacro(SpaceDimension, unsigned int);
  void SetSpaceDimension(unsigned int dim)
  {
    this->m_SpaceDimension = dim;
    this->m_LineDirection.set_size(dim);
    this->m_LineOrigin.set_size(dim);
    this->m_CurrentPosition.set_size(dim);
    this->Modified();
  }

  itkSetMacro(CurrentIteration, unsigned int);

  itkGetMacro(Stop, bool);
  itkSetMacro(Stop, bool);

private:
  unsigned int m_SpaceDimension;

  /** Current iteration */
  unsigned int m_CurrentIteration;
  unsigned int m_CurrentLineIteration;

  /** Maximum iteration limit. */
  unsigned int m_MaximumIteration;
  unsigned int m_MaximumLineIteration;

  bool   m_CatchGetValueException;
  double m_MetricWorstPossibleValue;

  /** Set if the Metric should be maximized: Default = False */
  bool m_Maximize;

  /** The minimal size of search */
  double m_StepLength;
  double m_StepTolerance;

  ParametersType       m_LineOrigin;
  vnl_vector< double > m_LineDirection;

  double m_ValueTolerance;

  /** Internal storage for the value type / used as a cache  */
  MeasureType m_CurrentCost;

  /** this is user-settable flag to stop optimization.
   * when users call StartOptimization, this value will be set false.
   * By calling StopOptimization, this flag will be set true, and
   * optimization will stop at the next iteration. */
  bool m_Stop;

  std::ostringstream m_StopConditionDescription;
}; // end of class
} // end of namespace itk

#endif
