/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineDecompositionImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBSplineDecompositionImageFilter_txx
#define _itkBSplineDecompositionImageFilter_txx
#include "itkBSplineDecompositionImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"

#include "itkVector.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage>
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::BSplineDecompositionImageFilter()
{
  m_SplineOrder = 0;
  int SplineOrder = 3;
  m_Tolerance = 1e-10;   // Need some guidance on this one...what is reasonable?
  m_IteratorDirection = 0;
  this->SetSplineOrder(SplineOrder);
}

/**
 * Standard "PrintSelf" method
 */
template <class TInputImage, class TOutputImage>
void
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::PrintSelf(
std::ostream& os, 
Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Spline Order: " << m_SplineOrder << std::endl;

}


template <class TInputImage, class TOutputImage>
void
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::SetInput(const TInputImage * inputData)
{
  Superclass::SetInput(inputData);

  m_DataLength = inputData->GetLargestPossibleRegion().GetSize();
  unsigned long maxLength = 0;
  for ( int n = 0; n < ImageDimension; n++ )
    {
    if ( m_DataLength[n] > maxLength )
      {
      maxLength = m_DataLength[n];
      }
    }
  m_Scratch.resize( maxLength );

  // DataToCoefficientsND requires that the spline order and the input data be set.
  // TODO:  We need to ensure that this is only run once and only after both input and
  //        spline order have been set.  Should the user be required to explicitly run
  //        this routine?  Or we need to figure out the "update" format.


}

template <class TInputImage, class TOutputImage>
void
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::SetInput( unsigned int index, const TInputImage * inputData)
{
  Superclass::SetInput(index, inputData);

  m_DataLength = inputData->GetLargestPossibleRegion().GetSize();
  unsigned long maxLength = 0;
  for ( int n = 0; n < ImageDimension; n++ )
    {
    if ( m_DataLength[n] > maxLength )
      {
      maxLength = m_DataLength[n];
      }
    }
  m_Scratch.resize( maxLength );

}


template <class TInputImage, class TOutputImage>
bool
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::DataToCoefficients1D()
  { 

  // See Unser, 1993, Part II, Equation 2.5, 
  //   or Unser, 1999, Box 2. for an explaination. 

  double c0 = 1.0;  
  
  if (m_DataLength[m_IteratorDirection] == 1) //Required by mirror boundaries
    {
    return false;
    }

  // Compute overall gain
  for (int k = 0; k < m_NumberOfPoles; k++)
    {
    // Note for cubic splines lambda = 6 
    c0 = c0 * (1.0 - m_SplinePoles[k]) * (1.0 - 1.0 / m_SplinePoles[k]);
    }

  // apply the gain 
  for (unsigned int n = 0; n < m_DataLength[m_IteratorDirection]; n++)
    {
    m_Scratch[n] *= c0;
    }

  // loop over all poles 
  for (int k = 0; k < m_NumberOfPoles; k++) 
    {
    // causal initialization 
    this->SetInitialCausalCoefficient(m_SplinePoles[k]);
    // causal recursion 
    for (unsigned int n = 1; n < m_DataLength[m_IteratorDirection]; n++)
      {
      m_Scratch[n] += m_SplinePoles[k] * m_Scratch[n - 1];
      }

    // anticausal initialization 
    this->SetInitialAntiCausalCoefficient(m_SplinePoles[k]);
    // anticausal recursion 
    for ( int n = m_DataLength[m_IteratorDirection] - 2; 0 <= n; n--)
      {
      m_Scratch[n] = m_SplinePoles[k] * (m_Scratch[n + 1] - m_Scratch[n]);
      }
    }
  return true;
}

template <class TInputImage, class TOutputImage>
void
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::SetSplineOrder(unsigned int SplineOrder)
{
  if (SplineOrder == m_SplineOrder)
    {
    return;
    }
  m_SplineOrder = SplineOrder;
  this->SetPoles();

}

template <class TInputImage, class TOutputImage>
void
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::SetPoles()
{
  /* See Unser, 1997. Part II, Table I for Pole values */
  // See also, Handbook of Medical Imaging, Processing and Analysis, Ed. Isaac N. Bankman, 
  //  2000, pg. 416.
  switch (m_SplineOrder)
    {
    case 3:
      m_NumberOfPoles = 1;
      m_SplinePoles[0] = sqrt(3.0) - 2.0;
      break;
    case 0:
      m_NumberOfPoles = 0;
      break;
    case 1:
      m_NumberOfPoles = 0;
      break;
    case 2:
      m_NumberOfPoles = 1;
      m_SplinePoles[0] = sqrt(8.0) - 3.0;
      break;
    case 4:
      m_NumberOfPoles = 2;
      m_SplinePoles[0] = sqrt(664.0 - sqrt(438976.0)) + sqrt(304.0) - 19.0;
      m_SplinePoles[1] = sqrt(664.0 + sqrt(438976.0)) - sqrt(304.0) - 19.0;
      break;
    case 5:
      m_NumberOfPoles = 2;
      m_SplinePoles[0] = sqrt(135.0 / 2.0 - sqrt(17745.0 / 4.0)) + sqrt(105.0 / 4.0)
        - 13.0 / 2.0;
      m_SplinePoles[1] = sqrt(135.0 / 2.0 + sqrt(17745.0 / 4.0)) - sqrt(105.0 / 4.0)
        - 13.0 / 2.0;
      break;
    default:
      // SplineOrder not implemented yet.
      ExceptionObject err(__FILE__, __LINE__);
      err.SetLocation( "BSplineInterpolateImageFunction" );
      err.SetDescription( "SplineOrder must be between 0 and 5. Requested spline order has not been implemented yet." );
      throw err;
      break;
    }
}

template <class TInputImage, class TOutputImage>
void
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::SetInitialCausalCoefficient(double z)
{
  /* begining InitialCausalCoefficient */
  /* See Unser, 1999, Box 2 for explaination */

  double  sum, zn, z2n, iz;
  unsigned long  horizon;

  /* this initialization corresponds to mirror boundaries */
  horizon = m_DataLength[m_IteratorDirection];
  zn = z;
  if (m_Tolerance > 0.0)
    {
    horizon = (long)ceil(log(m_Tolerance) / log(fabs(z)));
    }
  if (horizon < m_DataLength[m_IteratorDirection])
    {
    /* accelerated loop */
    sum = m_Scratch[0];   // verify this
    for (unsigned int n = 1; n < horizon; n++) 
      {
      sum += zn * m_Scratch[n];
      zn *= z;
      }
    m_Scratch[0] = sum;
    }
  else {
    /* full loop */
    iz = 1.0 / z;
    z2n = pow(z, (double)(m_DataLength[m_IteratorDirection] - 1L));
    sum = m_Scratch[0] + z2n * m_Scratch[m_DataLength[m_IteratorDirection] - 1L];
    z2n *= z2n * iz;
    for (unsigned int n = 1; n <= (m_DataLength[m_IteratorDirection] - 2); n++) {
      sum += (zn + z2n) * m_Scratch[n];
      zn *= z;
      z2n *= iz;
    }
    m_Scratch[0] = sum / (1.0 - zn * zn);
  }
}

template <class TInputImage, class TOutputImage>
void
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::SetInitialAntiCausalCoefficient(double z)
{
  // this initialization corresponds to mirror boundaries 
  /* See Unser, 1999, Box 2 for explaination */
  //  Also see erratum at http://bigwww.epfl.ch/publications/unser9902.html
  m_Scratch[m_DataLength[m_IteratorDirection] - 1] =
    (z / (z * z - 1.0)) * 
    (z * m_Scratch[m_DataLength[m_IteratorDirection] - 2] + m_Scratch[m_DataLength[m_IteratorDirection] - 1]);
}

template <class TInputImage, class TOutputImage>
void
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::DataToCoefficientsND( InputImagePointer inputPtr, OutputImagePointer outputPtr )
  {
  // Initilize coeffient array
  this->CopyImageToImage( inputPtr, outputPtr );   // Coefficients are initialized to the input data
  for (int n=0; n < ImageDimension; n++)
    {
    m_IteratorDirection = n;
    // Loop through each dimension

    // Inititilize iterators
    OutputLinearIterator CIterator( outputPtr, outputPtr->GetBufferedRegion() );
    CIterator.SetDirection( m_IteratorDirection );
    // For each data vector
    while ( !CIterator.IsAtEnd() )
      {
      // Copy coefficients to scratch
      this->CopyCoefficientsToScratch( CIterator );


      // Perform 1D BSpline calculations
      this->DataToCoefficients1D();
    
      // Copy scratch back to coefficients.
      // Brings us back to the end of the line we were working on.
      CIterator.GoToBeginOfLine();
      this->CopyScratchToCoefficients( CIterator ); // m_Scratch = m_Image;
      CIterator.NextLine();
      }
    }


}

template <class TInputImage, class TOutputImage>
void
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::CopyImageToImage(const TInputImage * input, TOutputImage * output )
{

  // setup the output
  // TODO:  Remove this when a system copy image to image function is available.
  output->CopyInformation( input );
  output->SetBufferedRegion( 
    input->GetBufferedRegion() );
  output->Allocate();

  // setup the iterators
  typedef ImageRegionConstIteratorWithIndex< TInputImage > InputIterator;
  typedef ImageRegionIterator< TOutputImage > OutputIterator;

  InputIterator inIt( input, input->GetBufferedRegion() );

  OutputIterator outIt( output, output->GetBufferedRegion() );

  inIt = inIt.Begin();
  outIt = outIt.Begin();

  while ( !outIt.IsAtEnd() )
  {
    outIt.Set( inIt.Get() );
    ++inIt;
    ++outIt;
  }
 
}

template <class TInputImage, class TOutputImage>
void
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::CopyScratchToCoefficients(OutputLinearIterator & Iter)
{
  unsigned long j = 0;
  while ( !Iter.IsAtEndOfLine() )
    {
    Iter.Set(m_Scratch[j]);
    ++Iter;
    ++j;
    }

}

template <class TInputImage, class TOutputImage>
void
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::CopyCoefficientsToScratch(OutputLinearIterator & Iter)
{
  unsigned long j = 0;
  while ( !Iter.IsAtEndOfLine() )
    {
    m_Scratch[j] = Iter.Get() ;
    ++Iter;
    ++j;
    }
}


/*
 * GenerateInputRequestedRegion method.
 */
template <class TInputImage, class TOutputImage>
void
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion()
{
  // this filter requires the all of the input image to be in
  // the buffer
  InputImagePointer  inputPtr = const_cast< TInputImage * > ( this->GetInput() );
  inputPtr->SetRequestedRegionToLargestPossibleRegion();
}


/*
 * EnlargeOutputRequestedRegion method.
 */
template <class TInputImage, class TOutputImage>
void
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::EnlargeOutputRequestedRegion(
DataObject *output )
{

  // this filter requires the all of the output image to be in
  // the buffer
  TOutputImage *imgData;
  imgData = dynamic_cast<TOutputImage*>( output );
  imgData->SetRequestedRegionToLargestPossibleRegion();

}

/*
 * GenerateOutputInformation method.
 */
template <class TInputImage, class TOutputImage>
void
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::GenerateOutputInformation()
{
  //***TODO:  Do I need this function at all?
  Superclass::GenerateOutputInformation();
  InputImagePointer  inputPtr = const_cast< TInputImage * > ( this->GetInput() );
  typename TOutputImage::Pointer outputPtr = this->GetOutput();
  outputPtr->SetLargestPossibleRegion( inputPtr->GetLargestPossibleRegion() );

}


template <class TInputImage, class TOutputImage>
void
BSplineDecompositionImageFilter<TInputImage, TOutputImage>
::GenerateData()
{

  // Get the input and output pointers
  InputImagePointer inputPtr = const_cast< TInputImage * > ( this->GetInput() );

  OutputImagePointer outputPtr = this->GetOutput();

  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Set Coordinates

  // Calculate actual output
  this->DataToCoefficientsND(inputPtr, outputPtr);

}

} // namespace itk

#endif
