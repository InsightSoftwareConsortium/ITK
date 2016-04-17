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
#ifndef _itkCumulativeGaussianOptimizer_cxx
#define _itkCumulativeGaussianOptimizer_cxx

#include "itkCumulativeGaussianOptimizer.h"
#include "itkMath.h"

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
  m_FinalSampledArray = ITK_NULLPTR;
  m_CumulativeGaussianArray = ITK_NULLPTR;
  m_StopConditionDescription << this->GetNameOfClass() << ": Constructed";
}

CumulativeGaussianOptimizer::~CumulativeGaussianOptimizer()
{
  delete m_FinalSampledArray;
}

CumulativeGaussianOptimizer::MeasureType *
CumulativeGaussianOptimizer
::ExtendGaussian(MeasureType *originalArray, MeasureType *extendedArray, int startingPointForInsertion)
{
  // Use the parameters from originalArray to construct a Gaussian in
  // extendedArray
  // shifting the mean to the right by startingPointForInsertion.
  double mean = startingPointForInsertion + m_ComputedMean;
  double sd = m_ComputedStandardDeviation;
  double amplitude = m_ComputedAmplitude;

  m_OffsetForMean = startingPointForInsertion;

  for ( int i = 0; i < (int)( extendedArray->GetNumberOfElements() ); i++ )
    {
    extendedArray->put( i, amplitude * std::exp( -( std::pow( ( i - mean ), 2 ) / ( 2 * std::pow(sd, 2) ) ) ) );
    }
  // Then insert the originalArray over the middle section of extendedArray.
  for ( int i = 0; i < (int)( originalArray->GetNumberOfElements() ); i++ )
    {
    extendedArray->put( i + startingPointForInsertion, originalArray->get(i) );
    }
  return extendedArray;
}

double
CumulativeGaussianOptimizer
::FindAverageSumOfSquaredDifferences(MeasureType *array1, MeasureType *array2)
{
  // Given two arrays array1 and array2 of equal length, calculate the average
  // sum of squared
  // differences between them.
  int    size = array1->GetNumberOfElements();
  double sum = 0;

  for ( int i = 0; i < size; i++ )
    {
    sum = sum + ( array1->get(i) - array2->get(i) ) * ( array1->get(i) - array2->get(i) );
    }
  return ( sum / size );
}

void
CumulativeGaussianOptimizer
::FindParametersOfGaussian(MeasureType *sampledGaussianArray)
{
  // Measure the parameters of the sampled Gaussian curve and use these
  // parameters to
  // construct an extended curve.  Then measure the parameters of the extended
  // curve, which
  // should be closer to the original Gaussian parameters and recalculate the
  // extended portion
  // of the curve. Iterate these last two steps until the average sum of squared
  // differences
  // between 2 iterations converge within differenceTolerance.
  MeasureGaussianParameters(sampledGaussianArray);

  if ( m_Verbose )
    {
    PrintComputedParameterHeader();
    PrintComputedParameters();
    }

  int          sampledGaussianArraySize = sampledGaussianArray->GetNumberOfElements();
  int          extendedArraySize = 3 * sampledGaussianArraySize;
  MeasureType *extendedArray = new MeasureType();
  extendedArray->SetSize(extendedArraySize);
  MeasureType *extendedArrayCopy = new MeasureType();
  extendedArrayCopy->SetSize(extendedArraySize);

  double averageSumOfSquaredDifferences = m_DifferenceTolerance;

  extendedArray = ExtendGaussian(sampledGaussianArray, extendedArray, sampledGaussianArraySize);

  MeasureGaussianParameters(extendedArray);
  bool smallChangeBetweenIterations = false;
  while ( averageSumOfSquaredDifferences >= m_DifferenceTolerance )
    {
    for ( int j = 0; j < extendedArraySize; j++ )
      {
      extendedArrayCopy->put( j, extendedArray->get(j) );
      }
    extendedArray = RecalculateExtendedArrayFromGaussianParameters(sampledGaussianArray,
                                                                   extendedArray,
                                                                   sampledGaussianArraySize);

    MeasureGaussianParameters(extendedArray);
    if ( m_Verbose )
      {
      PrintComputedParameters();
      }
    double temp = averageSumOfSquaredDifferences;
    averageSumOfSquaredDifferences = FindAverageSumOfSquaredDifferences(extendedArray, extendedArrayCopy);

    // Stop if there is a very very very small change between iterations.
    if ( std::fabs(temp - averageSumOfSquaredDifferences) <= m_DifferenceTolerance )
      {
      m_StopConditionDescription.str("");
      m_StopConditionDescription << this->GetNameOfClass() << ": "
                                 << "Change between iterations ("
                                 << std::fabs(temp - averageSumOfSquaredDifferences)
                                 << ") is less than DifferenceTolerance ("
                                 << m_DifferenceTolerance
                                 << ").";
      break;
      }
    }
  if ( !smallChangeBetweenIterations )
    {
    m_StopConditionDescription.str("");
    m_StopConditionDescription << this->GetNameOfClass() << ": "
                               << "Average sum of squared differences ("
                               << averageSumOfSquaredDifferences
                               << ") is less than DifferenceTolerance ("
                               << m_DifferenceTolerance
                               << ").";
    }

  // Update the mean calculation.
  m_ComputedMean = m_ComputedMean - m_OffsetForMean;

  delete extendedArray;
  delete extendedArrayCopy;
}

void CumulativeGaussianOptimizer
::MeasureGaussianParameters(MeasureType *array)
{
  // Assuming the input array is Gaussian, compute the mean, SD, amplitude, and
  // change in intensity.
  m_ComputedMean               = 0;
  m_ComputedStandardDeviation  = 0;
  m_ComputedAmplitude          = 0;
  m_ComputedTransitionHeight   = 0;

  double sum   = 0;

  // Calculate the mean.
  for ( int i = 0; i < (int)( array->GetNumberOfElements() ); i++ )
    {
    m_ComputedMean += i * array->get(i);
    sum += array->get(i);
    }
  // Assertion fails if number of samples <=2 or UpperAsymptote==LowerAsymptote
  // improper behavior if number of samples == 3.
  itkAssertInDebugAndIgnoreInReleaseMacro(sum != 0);
  m_ComputedMean /= sum;

  // Calculate the standard deviation
  for ( int i = 0; i < (int)( array->GetNumberOfElements() ); i++ )
    {
    m_ComputedStandardDeviation += array->get(i) * std::pow( ( i - m_ComputedMean ), 2 );
    }
  m_ComputedStandardDeviation = std::sqrt(m_ComputedStandardDeviation / sum);

  // For the ERF, sum is the difference between the lower and upper intensities.
  m_ComputedTransitionHeight = sum;

  // Calculate the amplitude.
  m_ComputedAmplitude =  sum / ( m_ComputedStandardDeviation * std::sqrt(2 * itk::Math::pi) );
}

void
CumulativeGaussianOptimizer
::PrintComputedParameterHeader()
{
  std::cerr << "Mean\t" << "SD\t" << "Amp\t" << "Transition" << std::endl;
}

void CumulativeGaussianOptimizer
::PrintComputedParameters()
{
  std::cerr << m_ComputedMean - m_OffsetForMean << "\t"  // Printed mean is
                                                         // shifted.
            << m_ComputedStandardDeviation       << "\t"
            << m_ComputedAmplitude              << "\t"
            << m_ComputedTransitionHeight       << std::endl;
}

CumulativeGaussianOptimizer::MeasureType *
CumulativeGaussianOptimizer
::RecalculateExtendedArrayFromGaussianParameters(MeasureType *originalArray,
                                                 MeasureType *extendedArray,
                                                 int startingPointForInsertion)
{
  // From the Gaussian parameters stored with the extendedArray,
  // recalculate the extended portion of the extendedArray,
  // leaving the inserted original array unchaged.
  double mean      = m_ComputedMean;
  double sd        = m_ComputedStandardDeviation;
  double amplitude = m_ComputedAmplitude;

  for ( int i = 0; i < (int)( extendedArray->GetNumberOfElements() ); i++ )
    {
    // Leave the original inserted array unchanged.
    if ( i < startingPointForInsertion
         || i >= startingPointForInsertion + (int)( originalArray->GetNumberOfElements() ) )
      {
      extendedArray->put( i, amplitude * std::exp( -( std::pow( ( i - mean ), 2 ) / ( 2 * std::pow(sd, 2) ) ) ) );
      }
    }
  return extendedArray;
}

void
CumulativeGaussianOptimizer
::SetDataArray(MeasureType *cumGaussianArray)
{
  m_CumulativeGaussianArray = cumGaussianArray;
}

void
CumulativeGaussianOptimizer
::StartOptimization()
{
  this->InvokeEvent( StartEvent() );
  m_StopConditionDescription.str("");
  m_StopConditionDescription << this->GetNameOfClass() << ": Running";

  // Declare arrays.
  int cumGaussianArraySize = m_CumulativeGaussianArray->GetNumberOfElements();
  int sampledGaussianArraySize = cumGaussianArraySize;
  //  int cumGaussianArrayCopySize = cumGaussianArraySize;

  MeasureType *sampledGaussianArray = new MeasureType();
  sampledGaussianArray->SetSize(sampledGaussianArraySize);

  MeasureType *cumGaussianArrayCopy = new MeasureType();
  cumGaussianArrayCopy->SetSize(cumGaussianArraySize);

  // Make a copy of the Cumulative Gaussian sampled data array.
  for ( int j = 0; j < cumGaussianArraySize; j++ )
    {
    cumGaussianArrayCopy->put( j, m_CumulativeGaussianArray->get(j) );
    }
  // Take the derivative of the data array resulting in a Gaussian array.
  MeasureType *derivative = new MeasureType();
  derivative->SetSize(cumGaussianArraySize - 1);

  for ( int i = 1; i < (int)( derivative->GetNumberOfElements() + 1 ); i++ )
    {
    derivative->put( i - 1, m_CumulativeGaussianArray->get(i) - m_CumulativeGaussianArray->get(i - 1) );
    }
  m_CumulativeGaussianArray = derivative;

  // Iteratively recalculate and resample the Gaussian array.
  FindParametersOfGaussian(m_CumulativeGaussianArray);

  // Generate new Gaussian array with final parameters.
  for ( int i = 0; i < sampledGaussianArraySize; i++ )
    {
    sampledGaussianArray->put( i, m_ComputedAmplitude
                               * std::exp( -( std::pow( ( i - m_ComputedMean ),
                                                      2 ) / ( 2 * std::pow(m_ComputedStandardDeviation, 2) ) ) ) );
    }
  // Add 0.5 to the mean of the sampled Gaussian curve to make up for the 0.5
  // shift during derivation, then take the integral of the Gaussian sample
  // to produce a Cumulative Gaussian.

  for ( int i = sampledGaussianArraySize - 1; i > 0; i-- )
    {
    sampledGaussianArray->put( i - 1, sampledGaussianArray->get(i) - sampledGaussianArray->get(i - 1) );
    }
  m_ComputedMean += 0.5;

  // Find the best vertical shift that minimizes the least square error.
  double c = VerticalBestShift(cumGaussianArrayCopy, sampledGaussianArray);

  // Add constant c to array.
  for ( int i = 0; i < (int)( sampledGaussianArray->GetNumberOfElements() ); i++ )
    {
    sampledGaussianArray->put(i, sampledGaussianArray->get(i) + c);
    }
  // Calculate the mean, standard deviation, lower and upper asymptotes of the
  // sampled Cumulative Gaussian.
  int    floorOfMean = (int)( m_ComputedMean );
  double yFloorOfMean = sampledGaussianArray->get(floorOfMean);
  double yCeilingOfMean = sampledGaussianArray->get(floorOfMean + 1);
  double y = ( m_ComputedMean - floorOfMean ) * ( yCeilingOfMean - yFloorOfMean ) + yFloorOfMean;
  m_UpperAsymptote = y + m_ComputedTransitionHeight / 2;
  m_LowerAsymptote = y - m_ComputedTransitionHeight / 2;

  m_FinalSampledArray = new MeasureType();
  m_FinalSampledArray->SetSize( sampledGaussianArray->GetNumberOfElements() );
  for ( int i = 0; i < (int)( m_FinalSampledArray->GetNumberOfElements() ); i++ )
    {
    m_FinalSampledArray->put( i, sampledGaussianArray->get(i) );
    }
  // Calculate the least square error as a measure of goodness of fit.
  m_FitError = static_cast< CostFunctionType * >( m_CostFunction.GetPointer() )->CalculateFitError(sampledGaussianArray);

  delete sampledGaussianArray;
  delete cumGaussianArrayCopy;
  delete derivative;
}

void CumulativeGaussianOptimizer::PrintArray(MeasureType *array)
{
  for ( int i = 0; i < (int)( array->GetNumberOfElements() ); i++ )
    {
    std::cerr << i << " " << array->get(i) << std::endl;
    }
}

double
CumulativeGaussianOptimizer
::VerticalBestShift(MeasureType *originalArray, MeasureType *newArray)
{
  // Find the constant to minimize the sum of squares of the difference between
  // original Array and newArray+c
  // Proof of algorithm:
  //     Let A = the original array.
  //     Let B = the new array.
  //     Let n = the number of elements in each array (note they must be the
  // same).
  //     We want to mimimize sum(((Bi+c) - (Ai))^2).
  //     So we take the derivative with respect to c and equate this derivative
  // to 0.
  //     d/dc sum(((Bi+c) - (Ai))^2) dc = 0
  //     => sum (2(Bi+c - Ai)) = 0
  //     => sum (Bi + c - Ai) = 0
  //     => (sum(Bi)) + (sum(c)) - (sum(Ai)) = 0
  //     => nC = sum(Ai) - sum(Bi)
  //     => C = (sum(Ai) - sum(Bi)) / n

  double c = 0;
  int    size = originalArray->GetNumberOfElements();

  for ( int i = 0; i < size; i++ )
    {
    c += originalArray->get(i);
    }
  for ( int i = 0; i < size; i++ )
    {
    c -= newArray->get(i);
    }
  return ( c / size );
}

const std::string
CumulativeGaussianOptimizer
::GetStopConditionDescription() const
{
  return m_StopConditionDescription.str();
}

void
CumulativeGaussianOptimizer
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Difference Tolerance = " << m_DifferenceTolerance << std::endl;
  os << indent << "Computed Mean = " << m_ComputedMean << std::endl;
  os << indent << "Computed Standard Deviation = " << m_ComputedStandardDeviation << std::endl;
  os << indent << "Computed Amplitude = " << m_ComputedAmplitude << std::endl;
  os << indent << "Computed Transition Height = " << m_ComputedTransitionHeight << std::endl;

  os << indent << "Upper Asymptote = " << m_UpperAsymptote << std::endl;
  os << indent << "Lower Asymptote = " << m_LowerAsymptote << std::endl;
  os << indent << "Offset For Mean = " << m_OffsetForMean << std::endl;
  os << indent << "Verbose = " << m_Verbose << std::endl;
  os << indent << "Fit Error = " << m_FitError << std::endl;

  os << indent << "StopConditionDescription: " << m_StopConditionDescription.str() << std::endl;
  if ( m_FinalSampledArray )
    {
    os << indent << "Final Sampled Array = " << m_FinalSampledArray << std::endl;
    }
  else
    {
    os << indent << "Final Sampled Array = [not defined] " << std::endl;
    }
}
} // end namespace itk

#endif
