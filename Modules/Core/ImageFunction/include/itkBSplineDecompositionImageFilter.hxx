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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkBSplineDecompositionImageFilter_hxx
#define itkBSplineDecompositionImageFilter_hxx
#include "itkBSplineDecompositionImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"
#include "itkVector.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
BSplineDecompositionImageFilter< TInputImage, TOutputImage >
::BSplineDecompositionImageFilter() :
  m_SplineOrder( 0 ),
  m_Tolerance( 1e-10 ),   // Need some guidance on this one...what is reasonable?
  m_IteratorDirection( 0 )
{
  this->SetSplineOrder( 3 );

  for( unsigned int i = 0; i < m_Scratch.size(); ++i )
    {
    m_Scratch[i] = 0;
    }

  m_DataLength.Fill( itk::NumericTraits< typename TInputImage::SizeType::SizeValueType >::ZeroValue() );
}

template< typename TInputImage, typename TOutputImage >
void
BSplineDecompositionImageFilter< TInputImage, TOutputImage >
::PrintSelf( std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Scratch: " << std::endl;
  for( unsigned int i = 0; i < m_Scratch.size(); ++i )
    {
    os << indent << "[" << i << "]: " << m_Scratch[i] << std::endl;
    }
  os << indent << "Data Length: " << m_DataLength << std::endl;
  os << indent << "Spline Order: " << m_SplineOrder << std::endl;
  os << indent << "SplinePoles: " << std::endl;
  for( unsigned int i = 0; i < m_SplinePoles.size(); ++i )
    {
    os << indent << "[" << i << "]" << m_SplinePoles[i] << std::endl;
    }
  os << indent << "Number Of Poles: " << m_NumberOfPoles << std::endl;
  os << indent << "Tolerance: " << m_Tolerance << std::endl;
  os << indent << "Iterator Direction: " << m_IteratorDirection << std::endl;
}

template< typename TInputImage, typename TOutputImage >
bool
BSplineDecompositionImageFilter< TInputImage, TOutputImage >
::DataToCoefficients1D()
{
  // See Unser, 1993, Part II, Equation 2.5,
  // or Unser, 1999, Box 2. for an explanation.

  double c0 = 1.0;

  if ( m_DataLength[m_IteratorDirection] == 1 ) // Required by mirror boundaries
    {
    return false;
    }

  // Compute over all gain
  for ( int k = 0; k < m_NumberOfPoles; k++ )
    {
    // Note for cubic splines lambda = 6
    c0 = c0 * ( 1.0 - m_SplinePoles[k] ) * ( 1.0 - 1.0 / m_SplinePoles[k] );
    }

  // Apply the gain
  for ( unsigned int n = 0; n < m_DataLength[m_IteratorDirection]; n++ )
    {
    m_Scratch[n] *= c0;
    }

  // Loop over all poles
  for ( int k = 0; k < m_NumberOfPoles; k++ )
    {
    // Causal initialization
    this->SetInitialCausalCoefficient(m_SplinePoles[k]);
    // Causal recursion
    for ( unsigned int n = 1; n < m_DataLength[m_IteratorDirection]; n++ )
      {
      m_Scratch[n] += m_SplinePoles[k] * m_Scratch[n - 1];
      }

    // anticausal initialization
    this->SetInitialAntiCausalCoefficient(m_SplinePoles[k]);
    // anticausal recursion
    for ( int n = m_DataLength[m_IteratorDirection] - 2; 0 <= n; n-- )
      {
      m_Scratch[n] = m_SplinePoles[k] * ( m_Scratch[n + 1] - m_Scratch[n] );
      }
    }
  return true;
}

template< typename TInputImage, typename TOutputImage >
void
BSplineDecompositionImageFilter< TInputImage, TOutputImage >
::SetSplineOrder(unsigned int SplineOrder)
{
  if ( SplineOrder == m_SplineOrder )
    {
    return;
    }
  m_SplinePoles.clear();
  m_SplineOrder = SplineOrder;
  this->SetPoles();
  this->Modified();
}

template< typename TInputImage, typename TOutputImage >
void
BSplineDecompositionImageFilter< TInputImage, TOutputImage >
::SetPoles()
{
  // See Unser, 1997. Part II, Table I for Pole values.
  // See also, Handbook of Medical Imaging, Processing and Analysis, Ed. Isaac
  // N. Bankman, 2000, pg. 416.
  switch ( m_SplineOrder )
    {
    case 3:
      m_NumberOfPoles = 1;
      m_SplinePoles.resize( m_NumberOfPoles );
      m_SplinePoles.at( 0 ) = std::sqrt(3.0) - 2.0;
      break;
    case 0:
      m_NumberOfPoles = 0;
      break;
    case 1:
      m_NumberOfPoles = 0;
      break;
    case 2:
      m_NumberOfPoles = 1;
      m_SplinePoles.resize( m_NumberOfPoles );
      m_SplinePoles.at( 0 ) = std::sqrt(8.0) - 3.0;
      break;
    case 4:
      m_NumberOfPoles = 2;
      m_SplinePoles.resize( m_NumberOfPoles );
      m_SplinePoles.at( 0 ) = std::sqrt( 664.0 - std::sqrt(438976.0) ) + std::sqrt(304.0) - 19.0;
      m_SplinePoles.at( 1 ) = std::sqrt( 664.0 + std::sqrt(438976.0) ) - std::sqrt(304.0) - 19.0;
      break;
    case 5:
      m_NumberOfPoles = 2;
      m_SplinePoles.resize( m_NumberOfPoles );
      m_SplinePoles.at( 0 ) = std::sqrt( 135.0 / 2.0 - std::sqrt(17745.0 / 4.0) ) + std::sqrt(105.0 / 4.0)
                         - 13.0 / 2.0;
      m_SplinePoles.at( 1 ) = std::sqrt( 135.0 / 2.0 + std::sqrt(17745.0 / 4.0) ) - std::sqrt(105.0 / 4.0)
                         - 13.0 / 2.0;
      break;
    default:
      // SplineOrder not implemented yet.
      itkExceptionMacro(<< "SplineOrder must be between 0 and 5. Requested spline order has not been implemented yet.");
      break;
    }
}

template< typename TInputImage, typename TOutputImage >
void
BSplineDecompositionImageFilter< TInputImage, TOutputImage >
::SetInitialCausalCoefficient(double z)
{
  // See Unser, 1999, Box 2 for explanation
  CoeffType     sum;
  double        zn, z2n, iz;
  typename TInputImage::SizeValueType horizon;

  // Yhis initialization corresponds to mirror boundaries
  horizon = m_DataLength[m_IteratorDirection];
  zn = z;
  if ( m_Tolerance > 0.0 )
    {
    horizon = (typename TInputImage::SizeValueType)
      std::ceil( std::log(m_Tolerance) / std::log( std::fabs(z) ) );
    }
  if ( horizon < m_DataLength[m_IteratorDirection] )
    {
    // Accelerated loop
    sum = m_Scratch[0];   // verify this
    for ( unsigned int n = 1; n < horizon; n++ )
      {
      sum += zn * m_Scratch[n];
      zn *= z;
      }
    m_Scratch[0] = sum;
    }
  else
    {
    // Full loop
    iz = 1.0 / z;
    z2n = std::pow( z, (double)( m_DataLength[m_IteratorDirection] - 1L ) );
    sum = m_Scratch[0] + z2n * m_Scratch[m_DataLength[m_IteratorDirection] - 1L];
    z2n *= z2n * iz;
    for ( unsigned int n = 1; n <= ( m_DataLength[m_IteratorDirection] - 2 ); n++ )
      {
      sum += ( zn + z2n ) * m_Scratch[n];
      zn *= z;
      z2n *= iz;
      }
    m_Scratch[0] = sum / ( 1.0 - zn * zn );
    }
}

template< typename TInputImage, typename TOutputImage >
void
BSplineDecompositionImageFilter< TInputImage, TOutputImage >
::SetInitialAntiCausalCoefficient(double z)
{
  // This initialization corresponds to mirror boundaries.
  // See Unser, 1999, Box 2 for explanation.
  // Also see erratum at http://bigwww.epfl.ch/publications/unser9902.html
  m_Scratch[m_DataLength[m_IteratorDirection] - 1] =
    ( z / ( z * z - 1.0 ) )
    * ( z * m_Scratch[m_DataLength[m_IteratorDirection] - 2] + m_Scratch[m_DataLength[m_IteratorDirection] - 1] );
}

template< typename TInputImage, typename TOutputImage >
void
BSplineDecompositionImageFilter< TInputImage, TOutputImage >
::DataToCoefficientsND()
{
  OutputImagePointer output = this->GetOutput();

  Size< ImageDimension > size = output->GetBufferedRegion().GetSize();

  unsigned int count = output->GetBufferedRegion().GetNumberOfPixels() / size[0] * ImageDimension;

  ProgressReporter progress(this, 0, count, 10);

  // Initialize coeffient array
  this->CopyImageToImage();   // Coefficients are initialized to the input data

  // Loop through each dimension
  for ( unsigned int n = 0; n < ImageDimension; n++ )
    {
    m_IteratorDirection = n;

    // Initialize iterators
    OutputLinearIterator CIterator( output, output->GetBufferedRegion() );
    CIterator.SetDirection(m_IteratorDirection);
    // For each data vector
    while ( !CIterator.IsAtEnd() )
      {
      // Copy coefficients to scratch
      this->CopyCoefficientsToScratch(CIterator);

      // Perform 1D BSpline calculations
      this->DataToCoefficients1D();

      // Copy scratch back to coefficients.
      // Brings us back to the end of the line we were working on.
      CIterator.GoToBeginOfLine();
      this->CopyScratchToCoefficients(CIterator);   // m_Scratch = m_Image;
      CIterator.NextLine();
      progress.CompletedPixel();
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
BSplineDecompositionImageFilter< TInputImage, TOutputImage >
::CopyImageToImage()
{
  typedef ImageRegionConstIteratorWithIndex< TInputImage > InputIterator;
  typedef ImageRegionIterator< TOutputImage >              OutputIterator;
  typedef typename TOutputImage::PixelType                 OutputPixelType;

  InputIterator  inIt( this->GetInput(), this->GetInput()->GetBufferedRegion() );
  OutputIterator outIt( this->GetOutput(), this->GetOutput()->GetBufferedRegion() );

  inIt.GoToBegin();
  outIt.GoToBegin();

  while ( !outIt.IsAtEnd() )
    {
    outIt.Set( static_cast< OutputPixelType >( inIt.Get() ) );
    ++inIt;
    ++outIt;
    }
}

template< typename TInputImage, typename TOutputImage >
void
BSplineDecompositionImageFilter< TInputImage, TOutputImage >
::CopyScratchToCoefficients(OutputLinearIterator & Iter)
{
  typedef typename TOutputImage::PixelType OutputPixelType;
  typename TOutputImage::SizeValueType j = 0;
  while ( !Iter.IsAtEndOfLine() )
    {
    Iter.Set( static_cast< OutputPixelType >( m_Scratch[j] ) );
    ++Iter;
    ++j;
    }
}

/**
 */
template< typename TInputImage, typename TOutputImage >
void
BSplineDecompositionImageFilter< TInputImage, TOutputImage >
::CopyCoefficientsToScratch(OutputLinearIterator & Iter)
{
  typename TOutputImage::SizeValueType j = 0;

  while ( !Iter.IsAtEndOfLine() )
    {
    m_Scratch[j] = static_cast< CoeffType >( Iter.Get() );
    ++Iter;
    ++j;
    }
}

template< typename TInputImage, typename TOutputImage >
void
BSplineDecompositionImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // This filter requires all of the input image to be in the buffer
  InputImagePointer inputPtr = const_cast< TInputImage * >( this->GetInput() );

  if ( inputPtr )
    {
    inputPtr->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< typename TInputImage, typename TOutputImage >
void
BSplineDecompositionImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  // This filter requires all of the input image to be in the buffer
  TOutputImage *imgData;

  imgData = dynamic_cast< TOutputImage * >( output );
  if ( imgData )
    {
    imgData->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< typename TInputImage, typename TOutputImage >
void
BSplineDecompositionImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Allocate scratch memory
  InputImageConstPointer inputPtr = this->GetInput();

  m_DataLength = inputPtr->GetBufferedRegion().GetSize();

  typename TOutputImage::SizeValueType maxLength = 0;
  for ( unsigned int n = 0; n < ImageDimension; n++ )
    {
    if ( m_DataLength[n] > maxLength )
      {
      maxLength = m_DataLength[n];
      }
    }
  m_Scratch.resize(maxLength);

  // Allocate memory for output image
  OutputImagePointer outputPtr = this->GetOutput();
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Calculate actual output
  this->DataToCoefficientsND();

  // Clean up
  m_Scratch.clear();
}
} // namespace itk

#endif
