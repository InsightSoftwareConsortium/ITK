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
#ifndef __itkMultiStartOptimizerv4_h
#define __itkMultiStartOptimizerv4_h
#include "itkObjectToObjectOptimizerBase.h"
#include "itkGradientDescentOptimizerv4.h"

namespace itk
{
/** \class MultiStartOptimizerv4
 *  \brief Multi-start searches over input parameters and returns the best metric value
 *
 *   The multi-start algorithm performs gradient descent from N (large) number of starting points and
 *   returns the best solution. Ideal start points would sample the solution space almost uniformly, thus,
 *   in theory, this is a global optimizer.  In this implementation, the quality of the optimization
 *   depends on the parameter space samples that the user inputs to the optimizer.  Multi-start can be
 *   modified in numerous ways to improve robustness of standard approaches.  These improvements usually
 *   focus modifying the parameter sample space.  This is why we place the burden on the user to provide
 *   the parameter samples over which to optimize.
 *
 * \ingroup ITKOptimizersv4
 */

class ITK_EXPORT MultiStartOptimizerv4
  : public ObjectToObjectOptimizerBase
{
public:
  /** Standard class typedefs. */
  typedef MultiStartOptimizerv4          Self;
  typedef ObjectToObjectOptimizerBase    Superclass;
  typedef SmartPointer< Self >           Pointer;
  typedef SmartPointer< const Self >     ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiStartOptimizerv4, ObjectToObjectOptimizerBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef Superclass::ParametersType                 ParametersType;
  typedef std::vector< ParametersType >              ParametersListType;
  typedef ParametersListType::size_type              ParameterListSizeType;
  typedef ObjectToObjectOptimizerBase                OptimizerType;
  typedef OptimizerType::Pointer                     OptimizerPointer;
  typedef itk::GradientDescentOptimizerv4            LocalOptimizerType;
  typedef itk::GradientDescentOptimizerv4::Pointer   LocalOptimizerPointer;

  /** Codes of stopping conditions. */
  typedef enum {
    MAXIMUM_NUMBER_OF_ITERATIONS,
    COSTFUNCTION_ERROR,
    UPDATE_PARAMETERS_ERROR,
    STEP_TOO_SMALL,
    CONVERGENCE_CHECKER_PASSED,
    OTHER_ERROR
    } StopConditionType;

  /** Stop condition return string type */
  typedef std::string                            StopConditionReturnStringType;

  /** Stop condition internal string type */
  typedef std::ostringstream                     StopConditionDescriptionType;

  /** Metric type over which this class is templated */
  typedef Superclass::MetricType                    MetricType;
  typedef MetricType::Pointer                       MetricTypePointer;

  /** Derivative type */
  typedef MetricType::DerivativeType                DerivativeType;

  /** Measure type */
  typedef Superclass::MeasureType                   MeasureType;
  typedef std::vector< MeasureType >                MetricValuesListType;

  /** Internal computation type, for maintaining a desired precision */
  typedef Superclass::InternalComputationValueType InternalComputationValueType;

  /** Get stop condition enum */
  itkGetConstReferenceMacro(StopCondition, StopConditionType);

  /** Set the number of iterations. */
  itkSetMacro(NumberOfIterations, SizeValueType);

  /** Get the number of iterations. */
  itkGetConstReferenceMacro(NumberOfIterations, SizeValueType);

  /** Get the current iteration number. */
  itkGetConstMacro(CurrentIteration, SizeValueType);

  /** Create an instance of the local optimizer */
  void InstantiateLocalOptimizer(void);

  /** Begin the optimization */
  virtual void StartOptimization( bool doOnlyInitialization = false );

  /** Stop optimization. The object is left in a state so the
   * optimization can be resumed by calling ResumeOptimization. */
  virtual void StopOptimization(void);

  /** Resume the optimization. Can be called after StopOptimization to
   * resume. The bulk of the optimization work loop is here. */
  virtual void ResumeOptimization();

  /** Get the reason for termination */
  virtual const StopConditionReturnStringType GetStopConditionDescription() const;

  /** Get the list of parameters over which to search.  */
  ParametersListType & GetParametersList();

  /** Set the list of parameters over which to search */
  void SetParametersList(ParametersListType & p);

  /** Get the list of metric values that we produced after the multi-start search.  */
  const MetricValuesListType & GetMetricValuesList() const;

  /** Return the parameters from the best visited position */
  ParametersType GetBestParameters( );

  /** Set/Get the optimizer. */
  itkSetObjectMacro( LocalOptimizer, OptimizerType );
  itkGetModifiableObjectMacro(LocalOptimizer, OptimizerType );

  inline ParameterListSizeType GetBestParametersIndex( ) { return this->m_BestParametersIndex; }

protected:

  /** Default constructor */
  MultiStartOptimizerv4();
  virtual ~MultiStartOptimizerv4();

  virtual void PrintSelf(std::ostream & os, Indent indent) const;

  /* Common variables for optimization control and reporting */
  bool                          m_Stop;
  StopConditionType             m_StopCondition;
  StopConditionDescriptionType  m_StopConditionDescription;
  SizeValueType                 m_NumberOfIterations;
  SizeValueType                 m_CurrentIteration;
  ParametersListType            m_ParametersList;
  MetricValuesListType          m_MetricValuesList;
  MeasureType                   m_MinimumMetricValue;
  MeasureType                   m_MaximumMetricValue;
  ParameterListSizeType         m_BestParametersIndex;
  OptimizerPointer              m_LocalOptimizer;

private:
  MultiStartOptimizerv4( const Self & ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

};

} // end namespace itk

#endif
