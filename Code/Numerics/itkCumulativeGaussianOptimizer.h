/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCumulativeGaussianOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkCumulativeGaussianOptimizer_h
#define _itkCumulativeGaussianOptimizer_h

#include "itkMultipleValuedNonLinearOptimizer.h"
#include "itkCumulativeGaussianCostFunction.h"

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
*/

class ITK_EXPORT CumulativeGaussianOptimizer : 
    public MultipleValuedNonLinearOptimizer
{

public:

   /** Standard typedefs. */
  typedef CumulativeGaussianOptimizer           Self;
  typedef MultipleValuedNonLinearOptimizer      Superclass;
  typedef SmartPointer<Self>                    Pointer;
  typedef SmartPointer<const Self>              ConstPointer;
  
  /** Cost function typedef. NOTE: This optimizer is specific to fitting a Cumulative Gaussian. */
  typedef CumulativeGaussianCostFunction CostFunctionType;
  
  /** Data array typedef. */
  typedef CostFunctionType::MeasureType MeasureType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(CumulativeGaussianOptimizer, MultipleValuedNonLinearOptimizer);

  /** Set and get macros. */
  itkSetMacro(DifferenceTolerance, double);
  itkSetMacro(Verbose, bool);
  itkGetMacro(ComputedMean, double);
  itkGetMacro(ComputedStandardDeviation, double);
  itkGetMacro(UpperAsymptote, double);
  itkGetMacro(LowerAsymptote, double);
  itkGetMacro(FinalSampledArray, MeasureType*);
  itkGetMacro(FitError, double);

  void SetDataArray(MeasureType * dataArray);

  /** Start the optimizer. */
  void StartOptimization();
  
  /** Set the cost function. */
  void SetCostFunction(CostFunctionType::Pointer costFunction);

  /** Print an array. */
  void PrintArray(MeasureType * array);

protected:
  CumulativeGaussianOptimizer();
  virtual ~CumulativeGaussianOptimizer();
  void PrintSelf(std::ostream &os, Indent indent) const;

private:

  /** When to stop the iteration for the Gaussian extension loop. */
  double  m_DifferenceTolerance;
  
  /** The final mean of the Cumulative Gaussian. */
  double  m_ComputedMean; 
  
  /** The final standard deviation of the Cumulative Gaussian. */
  double  m_ComputedStandardDeviation; 
  
  /** The final amplitude of the Gaussian. */
  double  m_ComputedAmplitude;  
  
  /** The transition height (distance between upper and lower asymptotes) of the Cumulative Gaussian. */
  double  m_ComputedTransitionHeight;
  
  /** The final upper asymptote of the Cumulative Gaussian. */
  double  m_UpperAsymptote;
  
  /** The final lower asymptote of the Cumulative Gaussian. */
  double  m_LowerAsymptote;
  
  /** Offset for the mean calculation. */
  double  m_OffsetForMean;

  /** Flag to print iteration results. */
  bool m_Verbose;

  /** Least squares fit error as a measure of goodness. */
  double m_FitError;

  /** Array of values computed from the final parameters of the Cumulative Gaussian. */
  MeasureType * m_FinalSampledArray;

  /** Original data array. */
  MeasureType * m_CumulativeGaussianArray;

  /** Pointer to the cost function. */
  CostFunctionType::Pointer m_CostFunction;

  /** Extend the tails of the Gaussian. */
  MeasureType * ExtendGaussian(MeasureType * originalArray, MeasureType * extendedArray, int startingPointForInsertion);
  
  /** Recalulate the parameters of the extended Gaussian array. */
  MeasureType * RecalculateExtendedArrayFromGaussianParameters(MeasureType * originalArray,
                                                               MeasureType * extendedArray,
                                                               int startingPointForInsertion);

  /** Calculates the squared difference error between each Gaussian iteration loop. */
  double FindAverageSumOfSquaredDifferences(MeasureType * array1, MeasureType * array2);

  /** Given an array sampled from a Gaussin, compute the final parameters. */
  void FindParametersOfGaussian(MeasureType * sampledGaussianArray);

  /** Measure the parameters of a Gaussian sampled array. */
  void MeasureGaussianParameters(MeasureType * array);
  
  /** Print the header for output table. */
  void PrintComputedParameterHeader();
  
  /** Print the computed parameters. */
  void PrintComputedParameters();
  
  /** Find the constant of the integrated sample. */
  double VerticalBestShift(MeasureType * originalArray, MeasureType * newArray);
};

} // end namespace itk

#endif



