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
#ifndef itkMultiStartOptimizerv4_h
#define itkMultiStartOptimizerv4_h

#include "itkObjectToObjectOptimizerBase.h"
#include "itkGradientDescentOptimizerv4.h"

namespace itk
{
  /** \class MultiStartOptimizerv4Template
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
template<typename TInternalComputationValueType>
class ITK_TEMPLATE_EXPORT MultiStartOptimizerv4Template
: public ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>
{
public:
  /** Standard class typedefs. */
  typedef MultiStartOptimizerv4Template                                Self;
  typedef ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType> Superclass;
  typedef SmartPointer< Self >                                         Pointer;
  typedef SmartPointer< const Self >                                   ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiStartOptimizerv4Template, Superclass);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef typename Superclass::ParametersType                                 ParametersType;
  typedef std::vector< ParametersType >                                       ParametersListType;
  typedef typename ParametersListType::size_type                              ParameterListSizeType;

  typedef ObjectToObjectOptimizerBaseTemplate<TInternalComputationValueType>              OptimizerType;
  typedef typename OptimizerType::Pointer                                           OptimizerPointer;
  typedef typename itk::GradientDescentOptimizerv4Template<TInternalComputationValueType> LocalOptimizerType;
  typedef typename LocalOptimizerType::Pointer                                      LocalOptimizerPointer;

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
  typedef typename Superclass::StopConditionReturnStringType StopConditionReturnStringType;

  /** Stop condition internal string type */
  typedef typename Superclass::StopConditionDescriptionType  StopConditionDescriptionType;
  /** Stop condition return string type */

  /** It should be possible to derive the internal computation type from the class object. */
  typedef TInternalComputationValueType             InternalComputationValueType;

  /** Metric type over which this class is templated */
  typedef typename Superclass::MetricType           MetricType;
  typedef typename MetricType::Pointer              MetricTypePointer;

  /** Derivative type */
  typedef typename MetricType::DerivativeType       DerivativeType;

  /** Measure type */
  typedef typename Superclass::MeasureType          MeasureType;
  typedef std::vector< MeasureType >                MetricValuesListType;

  /** Get stop condition enum */
  itkGetConstReferenceMacro(StopCondition, StopConditionType);

  /** Create an instance of the local optimizer */
  void InstantiateLocalOptimizer();

  /** Begin the optimization */
  virtual void StartOptimization( bool doOnlyInitialization = false ) ITK_OVERRIDE;

  /** Stop optimization. The object is left in a state so the
   * optimization can be resumed by calling ResumeOptimization. */
  virtual void StopOptimization();

  /** Resume the optimization. Can be called after StopOptimization to
   * resume. The bulk of the optimization work loop is here. */
  virtual void ResumeOptimization();

  /** Get the reason for termination */
  virtual const StopConditionReturnStringType GetStopConditionDescription() const ITK_OVERRIDE;

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
  MultiStartOptimizerv4Template();
  virtual ~MultiStartOptimizerv4Template() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /* Common variables for optimization control and reporting */
  bool                          m_Stop;
  StopConditionType             m_StopCondition;
  StopConditionDescriptionType  m_StopConditionDescription;
  ParametersListType            m_ParametersList;
  MetricValuesListType          m_MetricValuesList;
  MeasureType                   m_MinimumMetricValue;
  MeasureType                   m_MaximumMetricValue;
  ParameterListSizeType         m_BestParametersIndex;
  OptimizerPointer              m_LocalOptimizer;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MultiStartOptimizerv4Template);

};

/** This helps to meet backward compatibility */
typedef MultiStartOptimizerv4Template<double> MultiStartOptimizerv4;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultiStartOptimizerv4.hxx"
#endif

#endif
