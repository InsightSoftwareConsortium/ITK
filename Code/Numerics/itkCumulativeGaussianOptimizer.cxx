/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCumulativeGaussianOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkCumulativeGaussianOptimizer_cxx
#define _itkCumulativeGaussianOptimizer_cxx

#include "itkCumulativeGaussianOptimizer.h"
#include "assert.h"

namespace itk
{

CumulativeGaussianOptimizer::CumulativeGaussianOptimizer()
{
  // Set some initial values for the variables.
  m_ComputedMean = 0; 
  m_ComputedStandardDeviation = 0; 
  m_ComputedAmplitude = 0;
  m_ComputedTransitionHeight = 0;
  m_UpperAsymptote = 0;
  m_LowerAsymptote = 0;
  m_OffsetForMean = 0;
  m_DifferenceTolerance = 1e-10;
  m_Verbose = 0;
  m_FitError = 0;
}

CumulativeGaussianOptimizer::~CumulativeGaussianOptimizer()
{

}

CumulativeGaussianOptimizer::MeasureType *
CumulativeGaussianOptimizer
::ExtendGaussian(MeasureType * originalArray, MeasureType * extendedArray, int startingPointForInsertion)
{
  // Use the parameters from originalArray to construct a Gaussian in extendedArray
  // shifting the mean to the right by startingPointForInsertion.
  double  mean = startingPointForInsertion + m_ComputedMean;
  double  sd = m_ComputedStandardDeviation; 
  double  amplitude = m_ComputedAmplitude; 

  m_OffsetForMean = startingPointForInsertion;

  for(int i=0; i<extendedArray->GetNumberOfElements(); i++)
    extendedArray->put(i, amplitude * exp( - ( pow((i-mean),2) / (2*pow(sd,2)) ) ));

  // Then insert the originalArray over the middle section of extendedArray.
  for(int i=0; i<originalArray->GetNumberOfElements(); i++)
    extendedArray->put(i+startingPointForInsertion, originalArray->get(i));    

  return extendedArray;
}

double 
CumulativeGaussianOptimizer
::FindAverageSumOfSquaredDifferences(MeasureType * array1, MeasureType * array2)
{
  // Given two arrays array1 and array2 of equal length, calculate the average sum of squared
  // differences between them.
  int size = array1->GetNumberOfElements();
  double sum = 0;
  for (int i=0; i<size; i++)
    sum = sum + (array1->get(i) - array2->get(i)) * (array1->get(i) - array2->get(i));
  return (sum/size);
}

void 
CumulativeGaussianOptimizer
::FindParametersOfGaussian(MeasureType * sampledGaussianArray)
{
  // Measure the parameters of the sampled Gaussian curve and use these parameters to 
  // construct an extended curve.  Then measure the parameters of the extended curve, which
  // should be closer to the original Gaussian parameters and recalculate the extended portion
  // of the curve. Iterate these last two steps until the average sum of squared differences
  // between 2 iterations converge within differenceTolerance.
  MeasureGaussianParameters(sampledGaussianArray);

  if(m_Verbose)
    {
    PrintComputedParameterHeader();
    PrintComputedParameters();
    }

  int sampledGaussianArraySize = sampledGaussianArray->GetNumberOfElements();
  int extendedArraySize = 3 * sampledGaussianArraySize;
  MeasureType * extendedArray = new MeasureType();  
  extendedArray->resize(extendedArraySize);
  MeasureType * extendedArrayCopy = new MeasureType(); 
  extendedArrayCopy->resize(extendedArraySize);

  double averageSumOfSquaredDifferences = m_DifferenceTolerance;
  double temp = 0;
  
  extendedArray = ExtendGaussian(sampledGaussianArray, extendedArray, sampledGaussianArraySize);
  
  MeasureGaussianParameters(extendedArray);
  while (averageSumOfSquaredDifferences >= m_DifferenceTolerance)
    {
    for(int j = 0; j < extendedArraySize; j++) 
      extendedArrayCopy->put(j, extendedArray->get(j));

    extendedArray = RecalculateExtendedArrayFromGaussianParameters(sampledGaussianArray,
                                                                   extendedArray,
                                                                   sampledGaussianArraySize);
    
    MeasureGaussianParameters(extendedArray);
    if(m_Verbose) 
      PrintComputedParameters();
    
    temp = averageSumOfSquaredDifferences;
    averageSumOfSquaredDifferences = FindAverageSumOfSquaredDifferences(extendedArray, extendedArrayCopy);

    // Stop if there is a very very very small change between iterations.
    if(fabs(temp - averageSumOfSquaredDifferences) <= m_DifferenceTolerance)
      break;
    }
  // Update the mean calculation.
  m_ComputedMean = m_ComputedMean - m_OffsetForMean;
}

void CumulativeGaussianOptimizer
::MeasureGaussianParameters(MeasureType * array)
{
  // Assuming the input array is Gaussian, compute the mean, SD, amplitude, and change in intensity.
  m_ComputedMean               = 0;
  m_ComputedStandardDeviation  = 0;
  m_ComputedAmplitude          = 0;
  m_ComputedTransitionHeight   = 0;

  double sum   = 0;

  // Calculate the mean.
  for(int i = 0; i < array->GetNumberOfElements(); i++)
    {
    m_ComputedMean += i * array->get(i);
    sum  +=  array->get(i);
    }
  // Assertion fails if number of samples <=2 or UpperAsymptote==LowerAsymptote
  // improper behavior if number of samples == 3.
  assert(sum != 0);
  m_ComputedMean /= sum;

  // Calculate the standard deviation
  for(int i = 0; i < array->GetNumberOfElements(); i++)
    m_ComputedStandardDeviation += array->get(i) * pow( (i - m_ComputedMean), 2);

  m_ComputedStandardDeviation = sqrt( m_ComputedStandardDeviation/sum );

  // For the ERF, sum is the difference between the lower and upper intensities.
  m_ComputedTransitionHeight = sum;

  // Calculate the amplitude.
  m_ComputedAmplitude =  sum / (m_ComputedStandardDeviation * sqrt(2*3.14159265));
}
  
void 
CumulativeGaussianOptimizer
::PrintComputedParameterHeader()
{
  std::cerr << "Mean\t" << "SD\t" << "Amp\t" << "Transition" <<std::endl;
}

void CumulativeGaussianOptimizer
::PrintComputedParameters()
{
  std::cerr << m_ComputedMean - m_OffsetForMean << "\t"  // Printed mean is shifted.
       << m_ComputedStandardDeviation       << "\t" 
       << m_ComputedAmplitude              << "\t" 
       << m_ComputedTransitionHeight       << std::endl;
}

CumulativeGaussianOptimizer::MeasureType * 
CumulativeGaussianOptimizer
::RecalculateExtendedArrayFromGaussianParameters(MeasureType * originalArray,
                                                 MeasureType * extendedArray,
                                                 int startingPointForInsertion)
{
  // From the Gaussian parameters stored with the extendedArray,
  // recalculate the extended portion of the extendedArray,
  // leaving the inserted original array unchaged.
  double  mean      = m_ComputedMean;
  double  sd        = m_ComputedStandardDeviation; 
  double  amplitude = m_ComputedAmplitude; 

  for(int i = 0; i < extendedArray->GetNumberOfElements(); i++)
    {
    // Leave the original inserted array unchanged.
    if( i < startingPointForInsertion ||            
        i >= startingPointForInsertion + originalArray->GetNumberOfElements() )
      extendedArray->put(i, amplitude * exp(-(pow((i - mean),2) / (2 * pow(sd,2))))); 
    }
  return extendedArray;
}

void CumulativeGaussianOptimizer::SetCostFunction(CostFunctionType::Pointer costFunction)
{
  m_CostFunction = CostFunctionType::New();
  m_CostFunction = costFunction;
}

void 
CumulativeGaussianOptimizer
::StartOptimization(MeasureType * cumGaussianArray)
{
  // Declare arrays.
  int cumGaussianArraySize = cumGaussianArray->GetNumberOfElements();
  int sampledGaussianArraySize = cumGaussianArraySize;
  int cumGaussianArrayCopySize = cumGaussianArraySize;
  
  MeasureType * sampledGaussianArray = new MeasureType();
  sampledGaussianArray->resize(sampledGaussianArraySize);

  MeasureType * cumGaussianArrayCopy = new MeasureType();
  cumGaussianArrayCopy->resize(cumGaussianArraySize);

  // Make a copy of the Cumulative Gaussian sampled data array.
  for(int j = 0; j < cumGaussianArraySize; j++)
    cumGaussianArrayCopy->put(j, cumGaussianArray->get(j));
  
  // Take the derivative of the data array resulting in a Gaussian array.
  MeasureType * derivative = new MeasureType();
  derivative->resize(cumGaussianArraySize - 1);

  for(int i=1; i < derivative->GetNumberOfElements()+1; i++)
    derivative->put(i-1, cumGaussianArray->get(i) - cumGaussianArray->get(i-1) );

  cumGaussianArray = derivative;
  
  // Iteratively recalculate and resample the Gaussian array.
  FindParametersOfGaussian(cumGaussianArray);
  
  // Generate new Gaussian array with final parameters.
  for(int i = 0; i < sampledGaussianArraySize; i++)
    sampledGaussianArray->put(i, m_ComputedAmplitude * exp( - ( pow((i-m_ComputedMean),2) / (2*pow(m_ComputedStandardDeviation,2)) ) ));

  // Add 0.5 to the mean of the sampled Gaussian curve to make up for the 0.5 
  // shift during derivation, then take the integral of the Gaussian sample
  // to produce a Cumulative Gaussian.
  MeasureType * integral = new MeasureType();
  integral->resize(sampledGaussianArraySize);

  for(int i = sampledGaussianArraySize; i > 0; i--)
    sampledGaussianArray->put(i-1, sampledGaussianArray->get(i) - sampledGaussianArray->get(i-1));

  m_ComputedMean += 0.5;

  // Find the best vertical shift that minimizes the least square error.
  double c = VerticalBestShift(cumGaussianArrayCopy, sampledGaussianArray);
  
  // Add constant c to array.
  for(int i = 0; i < sampledGaussianArray->GetNumberOfElements(); i++)
    sampledGaussianArray->put(i, sampledGaussianArray->get(i) + c);

  // Calculate the mean, standard deviation, lower and upper asymptotes of the
  // sampled Cumulative Gaussian.
  int floorOfMean = (int)(m_ComputedMean);
  double yFloorOfMean = sampledGaussianArray->get(floorOfMean);
  double yCeilingOfMean = sampledGaussianArray->get(floorOfMean + 1);
  double y = (m_ComputedMean-floorOfMean)*(yCeilingOfMean-yFloorOfMean)+yFloorOfMean;
  m_UpperAsymptote = y + m_ComputedTransitionHeight/2;
  m_LowerAsymptote = y - m_ComputedTransitionHeight/2;

  m_FinalSampledArray = new MeasureType();
  m_FinalSampledArray->resize(sampledGaussianArray->GetNumberOfElements());
  for(int i = 0; i < m_FinalSampledArray->GetNumberOfElements(); i++)
    m_FinalSampledArray->put(i, sampledGaussianArray->get(i));

  // Calculate the least square error as a measure of goodness of fit.
  m_FitError = m_CostFunction->CalculateFitError(sampledGaussianArray);
}

void CumulativeGaussianOptimizer::PrintArray(MeasureType * array)
{
  for(int i = 0; i < array->GetNumberOfElements(); i++)
    std::cerr << i << " " << array->get(i) << std::endl;
}

double 
CumulativeGaussianOptimizer
::VerticalBestShift(MeasureType * originalArray, MeasureType * newArray)
{

  // Find the constant to minimize the sum of squares of the difference between original Array and newArray+c
  // Proof of algorithm:
  //     Let A = the original array.
  //     Let B = the new array.
  //     Let n = the number of elements in each array (note they must be the same).
  //     We want to mimimize sum(((Bi+c) - (Ai))^2).
  //     So we take the derivative with respect to c and equate this derivative to 0.
  //     d/dc sum(((Bi+c) - (Ai))^2) dc = 0
  //     => sum (2(Bi+c - Ai)) = 0
  //     => sum (Bi + c - Ai) = 0
  //     => (sum(Bi)) + (sum(c)) - (sum(Ai)) = 0
  //     => nC = sum(Ai) - sum(Bi)
  //     => C = (sum(Ai) - sum(Bi)) / n

  double c = 0;
  int size = originalArray->GetNumberOfElements();
  for(int i=0; i<size; i++)
    c += originalArray->get(i);
  for(int i=0; i<size; i++)
    c -= newArray->get(i);
  return (c/size);
}

void
CumulativeGaussianOptimizer
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Difference Tolerance = " << m_DifferenceTolerance
    << "Computed Mean = " << m_ComputedMean
    << "Computed Standard Deviation = " << m_ComputedStandardDeviation
    << "Computed Amplitude = " << m_ComputedAmplitude
    << "Computed Transition Height = " << m_ComputedTransitionHeight
    << std::endl;

  os << indent << "Upper Asymptote = " << m_UpperAsymptote
    << "Lower Asymptote = " << m_LowerAsymptote
    << "Offset For Mean = " << m_OffsetForMean
    << "Verbose = " << m_Verbose
    << "Fit Error = " << m_FitError
    << std::endl;
}


} // end namespace itk

#endif
