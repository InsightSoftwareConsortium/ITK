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
#ifndef itkCumulativeGaussianOptimizer_h
#define itkCumulativeGaussianOptimizer_h

#include "itkMultipleValuedNonLinearOptimizer.h"
#include "itkCumulativeGaussianCostFunction.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class CumulativeGaussianOptimizer
 * \brief This is an optimizer specific to estimating
 * the parameters of Cumulative Gaussian sampled data.
 *
 * This optimizer will only work if the data array is
 * sampled from a Cumulative Gaussian curve. It's more
 * of a curve fitter than an optimizer, with the
 * advantage of being fast and specific. It works by
 * taking the derivative of the Cumulative Gaussian sample
 * then repeatedly extending the tails of the Gaussian
 * and recalculating the Gaussian parameters until
 * the change in iterations is within tolerance or very small.
 * The Gaussian is then integrated to reproduce the
 * Cumulative Gaussian and the asymptotes are estimated
 * by using least squares fit to estimate the constant
 * from integration.
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */

class ITKOptimizers_EXPORT CumulativeGaussianOptimizer:
  public MultipleValuedNonLinearOptimizer
{
public:

  /** Standard typedefs. */
  typedef CumulativeGaussianOptimizer      Self;
  typedef MultipleValuedNonLinearOptimizer Superclass;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer< const Self >       ConstPointer;

  /** Cost function typedef. NOTE: This optimizer is specific to fitting a
    Cumulative Gaussian. */
  typedef CumulativeGaussianCostFunction CostFunctionType;

  /** Data array typedef. */
  typedef CostFunctionType::MeasureType MeasureType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CumulativeGaussianOptimizer, MultipleValuedNonLinearOptimizer);

  /** Set and get macros. */
  itkSetMacro(DifferenceTolerance, double);
  itkGetMacro(DifferenceTolerance, double);
  itkSetMacro(Verbose, bool);
  itkGetMacro(Verbose, bool);
  itkGetMacro(ComputedMean, double);
  itkGetMacro(ComputedStandardDeviation, double);
  itkGetMacro(UpperAsymptote, double);
  itkGetMacro(LowerAsymptote, double);
  itkGetMacro(FinalSampledArray, MeasureType *);
  itkGetMacro(FitError, double);

  void SetDataArray(MeasureType *dataArray);

  /** Start the optimizer. */
  virtual void StartOptimization() ITK_OVERRIDE;

  /** Print an array. */
  void PrintArray(MeasureType *array);

  /** Report the reason for stopping. */
  virtual const std::string GetStopConditionDescription() const ITK_OVERRIDE;

protected:
  CumulativeGaussianOptimizer();
  virtual ~CumulativeGaussianOptimizer() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  /** When to stop the iteration for the Gaussian extension loop. */
  double m_DifferenceTolerance;

  /** The final mean of the Cumulative Gaussian. */
  double m_ComputedMean;

  /** The final standard deviation of the Cumulative Gaussian. */
  double m_ComputedStandardDeviation;

  /** The final amplitude of the Gaussian. */
  double m_ComputedAmplitude;

  /** The transition height (distance between upper and lower
   * asymptotes) of the Cumulative Gaussian. */
  double m_ComputedTransitionHeight;

  /** The final upper asymptote of the Cumulative Gaussian. */
  double m_UpperAsymptote;

  /** The final lower asymptote of the Cumulative Gaussian. */
  double m_LowerAsymptote;

  /** Offset for the mean calculation. */
  double m_OffsetForMean;

  /** Flag to print iteration results. */
  bool m_Verbose;

  /** Least squares fit error as a measure of goodness. */
  double m_FitError;

  /** Array of values computed from the final parameters of the
   * Cumulative Gaussian. */
  MeasureType *m_FinalSampledArray;

  /** Original data array. */
  MeasureType *m_CumulativeGaussianArray;

  /** Extend the tails of the Gaussian. */
  MeasureType * ExtendGaussian(MeasureType *originalArray, MeasureType *extendedArray, int startingPointForInsertion);

  /** Recalulate the parameters of the extended Gaussian array. */
  MeasureType * RecalculateExtendedArrayFromGaussianParameters(MeasureType *originalArray,
                                                               MeasureType *extendedArray,
                                                               int startingPointForInsertion);

  /** Calculates the squared difference error between each Gaussian
   * iteration loop. */
  double FindAverageSumOfSquaredDifferences(MeasureType *array1, MeasureType *array2);

  /** Given an array sampled from a Gaussin, compute the final parameters. */
  void FindParametersOfGaussian(MeasureType *sampledGaussianArray);

  /** Measure the parameters of a Gaussian sampled array. */
  void MeasureGaussianParameters(MeasureType *array);

  /** Print the header for output table. */
  void PrintComputedParameterHeader();

  /** Print the computed parameters. */
  void PrintComputedParameters();

  /** Find the constant of the integrated sample. */
  double VerticalBestShift(MeasureType *originalArray, MeasureType *newArray);

  /** Describe the stop condition */
  std::ostringstream m_StopConditionDescription;
};
} // end namespace itk

#endif
