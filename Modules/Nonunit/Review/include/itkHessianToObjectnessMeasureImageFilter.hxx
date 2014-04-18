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
#ifndef __itkHessianToObjectnessMeasureImageFilter_hxx
#define __itkHessianToObjectnessMeasureImageFilter_hxx

#include "itkHessianToObjectnessMeasureImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkSymmetricEigenAnalysis.h"
#include "itkProgressReporter.h"

#include "vnl/vnl_math.h"

#include <algorithm>

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
HessianToObjectnessMeasureImageFilter< TInputImage, TOutputImage >
::HessianToObjectnessMeasureImageFilter()
{
  m_Alpha = 0.5;
  m_Beta = 0.5;
  m_Gamma = 5.0;

  m_ScaleObjectnessMeasure = true;

  // by default extract bright lines (equivalent to vesselness)
  m_ObjectDimension = 1;
  m_BrightObject = true;
}

template< typename TInputImage, typename TOutputImage >
void
HessianToObjectnessMeasureImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData(void)
{
  if ( m_ObjectDimension >= ImageDimension )
    {
    itkExceptionMacro("ObjectDimension must be lower than ImageDimension.");
    }
}

template< typename TInputImage, typename TOutputImage >
void
HessianToObjectnessMeasureImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  typename OutputImageType::Pointer output = this->GetOutput();
  typename InputImageType::ConstPointer input = this->GetInput();

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels(), 1000 / this->GetNumberOfThreads() );

  // calculator for computation of the eigen values
  typedef SymmetricEigenAnalysis< InputPixelType, EigenValueArrayType > CalculatorType;
  CalculatorType eigenCalculator(ImageDimension);

  // walk the region of eigen values and get the objectness measure
  ImageRegionConstIterator< InputImageType > it(input, outputRegionForThread);
  ImageRegionIterator< OutputImageType >     oit(output, outputRegionForThread);

  oit.GoToBegin();
  it.GoToBegin();

  while ( !it.IsAtEnd() )
    {
    // compute eigen values
    EigenValueArrayType eigenValues;
    eigenCalculator.ComputeEigenValues(it.Get(), eigenValues);

    // Sort the eigenvalues by magnitude but retain their sign.
    // The eigenvalues are to be sorted |e1|<=|e2|<=...<=|eN|
    EigenValueArrayType sortedEigenValues = eigenValues;
    std::sort( sortedEigenValues.Begin(), sortedEigenValues.End(), AbsLessEqualCompare() );

    // check whether eigenvalues have the right sign
    bool signConstraintsSatisfied = true;
    for ( unsigned int i = m_ObjectDimension; i < ImageDimension; i++ )
      {
      if ( ( m_BrightObject && sortedEigenValues[i] > 0.0 )
           || ( !m_BrightObject && sortedEigenValues[i] < 0.0 ) )
        {
        signConstraintsSatisfied = false;
        break;
        }
      }

    if ( !signConstraintsSatisfied )
      {
      oit.Set(NumericTraits< OutputPixelType >::Zero);
      ++it;
      ++oit;
      progress.CompletedPixel();
      continue;
      }

    EigenValueArrayType sortedAbsEigenValues;
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      sortedAbsEigenValues[i] = vnl_math_abs(sortedEigenValues[i]);
      }

    // initialize the objectness measure
    double objectnessMeasure = 1.0;

    // compute objectness from eigenvalue ratios and second-order structureness
    if ( m_ObjectDimension < ImageDimension - 1 )
      {
      double rA = sortedAbsEigenValues[m_ObjectDimension];
      double rADenominatorBase = 1.0;
      for ( unsigned int j = m_ObjectDimension + 1; j < ImageDimension; j++ )
        {
        rADenominatorBase *= sortedAbsEigenValues[j];
        }
      if ( std::fabs(rADenominatorBase) > 0.0 )
        {
        if ( std::fabs(m_Alpha) > 0.0 )
          {
          rA /= std::pow( rADenominatorBase, 1.0 / ( ImageDimension - m_ObjectDimension - 1 ) );
          objectnessMeasure *= 1.0 - std::exp( -0.5 * vnl_math_sqr(rA) / vnl_math_sqr(m_Alpha) );
          }
        }
      else
        {
        objectnessMeasure = 0.0;
        }
      }

    if ( m_ObjectDimension > 0 )
      {
      double rB = sortedAbsEigenValues[m_ObjectDimension - 1];
      double rBDenominatorBase = 1.0;
      for ( unsigned int j = m_ObjectDimension; j < ImageDimension; j++ )
        {
        rBDenominatorBase *= sortedAbsEigenValues[j];
        }
      if ( std::fabs(rBDenominatorBase) > 0.0 && std::fabs(m_Beta) > 0.0 )
        {
        rB /= std::pow( rBDenominatorBase, 1.0 / ( ImageDimension - m_ObjectDimension ) );

        objectnessMeasure *= std::exp( -0.5 * vnl_math_sqr(rB) / vnl_math_sqr(m_Beta) );
        }
      else
        {
        objectnessMeasure = 0.0;
        }
      }

    if ( std::fabs(m_Gamma) > 0.0 )
      {
      double frobeniusNormSquared = 0.0;
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        frobeniusNormSquared += vnl_math_sqr(sortedAbsEigenValues[i]);
        }
      objectnessMeasure *= 1.0 - std::exp( -0.5 * frobeniusNormSquared / vnl_math_sqr(m_Gamma) );
      }

    // in case, scale by largest absolute eigenvalue
    if ( m_ScaleObjectnessMeasure )
      {
      objectnessMeasure *= sortedAbsEigenValues[ImageDimension - 1];
      }

    oit.Set( static_cast< OutputPixelType >( objectnessMeasure ) );

    ++it;
    ++oit;
    progress.CompletedPixel();
    }
}

template< typename TInputImage, typename TOutputImage >
void
HessianToObjectnessMeasureImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Alpha: " << m_Alpha << std::endl;
  os << indent << "Beta: " << m_Beta << std::endl;
  os << indent << "Gamma: " << m_Gamma << std::endl;
  os << indent << "ScaleObjectnessMeasure: " << m_ScaleObjectnessMeasure << std::endl;
  os << indent << "ObjectDimension: " << m_ObjectDimension << std::endl;
  os << indent << "BrightObject: " << m_BrightObject << std::endl;
}
} // end namespace itk

#endif
