/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBSplineInterpolateImageFunction.txx
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
#include "itkImageLinearIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"

namespace itk
{

/**
 * Constructor
 */
template <class TImageType>
BSplineInterpolateImageFunction<TImageType>
::BSplineInterpolateImageFunction()
{
  m_SplineOrder = 0;
  int SplineOrder = 3;
  m_Tolerance = 1e-10;   // Need some guidance on this one...what is reasonable?
  m_IteratorDirection = 0;
  m_Coefficients = TImageType::New();
  this->SetSplineOrder(SplineOrder);
}

/**
 * Standard "PrintSelf" method
 */
template <class TImageType>
void
BSplineInterpolateImageFunction<TImageType>
::PrintSelf(
std::ostream& os, 
Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Spline Order: " << m_SplineOrder << std::endl;

}


template <class TImageType>
void BSplineInterpolateImageFunction<TImageType>
::SetInputImage(const TImageType * inputData)
{
  Superclass::SetInputImage(inputData);

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
  this->DataToCoefficientsND();   // This seems like a reasonable time to set this...


}



template <class TImageType>
bool BSplineInterpolateImageFunction< TImageType>
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
  for (int n = 0; n < m_DataLength[m_IteratorDirection]; n++)
    {
    m_Scratch[n] *= c0;
    }

  // loop over all poles 
  for (int k = 0; k < m_NumberOfPoles; k++) 
    {
    // causal initialization 
    this->SetInitialCausalCoefficient(m_SplinePoles[k]);
    // causal recursion 
    for (int n = 1; n < m_DataLength[m_IteratorDirection]; n++)
      {
      m_Scratch[n] += m_SplinePoles[k] * m_Scratch[n - 1];
      }

    // anticausal initialization 
    this->SetInitialAntiCausalCoefficient(m_SplinePoles[k]);
    // anticausal recursion 
    for (int n = m_DataLength[m_IteratorDirection] - 2; 0 <= n; n--)
      {
      m_Scratch[n] = m_SplinePoles[k] * (m_Scratch[n + 1] - m_Scratch[n]);
      }
    }
  return true;
}

template < class TImageType>
void BSplineInterpolateImageFunction<TImageType>
::SetSplineOrder(int SplineOrder)
{
  if (SplineOrder == m_SplineOrder)
    {
    return;
    }
  m_SplineOrder = SplineOrder;
  this->SetPoles();
  m_MaxNumberInterpolationPoints = 1;
  for (int n=0; n < ImageDimension; n++)
    {
    m_MaxNumberInterpolationPoints *= ( m_SplineOrder + 1);
    } 
  this->GeneratePointsToIndex( );
}

template < class TImageType>
void BSplineInterpolateImageFunction< TImageType>
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

template < class TImageType>
void BSplineInterpolateImageFunction< TImageType>
::SetInitialCausalCoefficient(double z)
{
  /* begining InitialCausalCoefficient */
  /* See Unser, 1999, Box 2 for explaination */

  double  sum, zn, z2n, iz;
  long  horizon;

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
    for (int n = 1; n < horizon; n++) 
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
    for (int n = 1; n <= (m_DataLength[m_IteratorDirection] - 2); n++) {
      sum += (zn + z2n) * m_Scratch[n];
      zn *= z;
      z2n *= iz;
    }
    m_Scratch[0] = sum / (1.0 - zn * zn);
  }
}

template <class TImageType>
void BSplineInterpolateImageFunction<TImageType>
::SetInitialAntiCausalCoefficient(double z)
{
  // this initialization corresponds to mirror boundaries 
  /* See Unser, 1999, Box 2 for explaination */
  //  Also see erratum at http://bigwww.epfl.ch/publications/unser9902.html
  m_Scratch[m_DataLength[m_IteratorDirection] - 1] =
    (z / (z * z - 1.0)) * 
    (z * m_Scratch[m_DataLength[m_IteratorDirection] - 2] + m_Scratch[m_DataLength[m_IteratorDirection] - 1]);
}

template <class TImageType>
void BSplineInterpolateImageFunction< TImageType>
::DataToCoefficientsND()
  {
  // Initilize coeffient array
  this->CopyImageToImage( m_Image, m_Coefficients );   // Coefficients are initialized to the input data
  for (int n=0; n < ImageDimension; n++)
    {
    m_IteratorDirection = n;
    // Loop through each dimension

    // Inititilize iterators
    Iterator CIterator( m_Coefficients, m_Coefficients->GetBufferedRegion() );
    CIterator.SetDirection( m_IteratorDirection );
    // For each data vector
    while ( !CIterator.IsAtEnd() )
      {
      // Copy coefficients to scratch
      this->CopyCoefficientsToScratch( CIterator );


      // Perform 1D BSpline calculations
      this->DataToCoefficients1D();
    
      // Copy scratch back to coefficients.
//      CIterator.PreviousLine(); // Brings us back to the end of the line we were working on.
      CIterator.GoToBeginOfLine();
      this->CopyScratchToCoefficients( CIterator ); // m_Scratch = m_Image;
      CIterator.NextLine();
      }
    }


  }

template < class TImageType>
void BSplineInterpolateImageFunction< TImageType>
::CopyImageToImage(const TImageType * input, TImageType * output )
{

  // setup the output
  // TODO:  Remove this when a system copy image to image function is available.
  output->CopyInformation( input );
  output->SetBufferedRegion( 
    input->GetBufferedRegion() );
  output->Allocate();

  // setup the iterators
  typedef ImageRegionConstIteratorWithIndex< TImageType > InputIterator;
  typedef ImageRegionIterator< TImageType > OutputIterator;

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

template < class TImageType>
void BSplineInterpolateImageFunction<TImageType>
::CopyScratchToCoefficients(Iterator & Iter)
  {
  unsigned long j = 0;
  while ( !Iter.IsAtEndOfLine() )
    {
    Iter.Set(m_Scratch[j]);
    ++Iter;
    ++j;
    }

  }

template <class TImageType>
void BSplineInterpolateImageFunction<TImageType>
::CopyCoefficientsToScratch(Iterator & Iter)
  {
  unsigned long j = 0;
  while ( !Iter.IsAtEndOfLine() )
    {
    m_Scratch[j] = Iter.Get() ;
    ++Iter;
    ++j;
    }
  }


template <class TImageType>
double BSplineInterpolateImageFunction<TImageType>
::EvaluateAtContinuousIndex( const ContinuousIndexType & x ) const
{
  long indx;
  vnl_matrix<long>        EvaluateIndex(ImageDimension, ( m_SplineOrder + 1 ));

// compute the interpolation indexes 
  for (int n = 0; n< ImageDimension; n++)
    {
    if (m_SplineOrder & 1)     // Use this index calculation for odd splineOrder
      {
      indx = (long)floor(x[n]) - m_SplineOrder / 2;
      for (int k = 0; k <= m_SplineOrder; k++)
        {
        EvaluateIndex[n][k] = indx++;
        }
      }
    else                       // Use this index calculation for even splineOrder
      { 
      indx = (long)floor(x[n] + 0.5) - m_SplineOrder / 2;
      for (int k = 0; k <= m_SplineOrder; k++)
        {
        EvaluateIndex[n][k] = indx++;
        }
      }
    }
  
  // Determine weights
  vnl_matrix<double>        weights(ImageDimension, ( m_SplineOrder + 1 ));
  SetInterpolationWeights( x, EvaluateIndex, weights );

  for (int n = 0; n < ImageDimension; n++)
    {
    int dataLength2 = 2 * m_DataLength[n] - 2;

    /* apply the mirror boundary conditions */
    // TODO:  We could implement other boundary options beside mirror
    if (m_DataLength[n] == 1)
      {
      for (int k = 0; k <= m_SplineOrder; k++)
        {
        EvaluateIndex[n][k] = 0;
        }
      }
    else
      {
        for (int k = 0; k <= m_SplineOrder; k++)
        {
        // btw - Think about this couldn't this be replaced with a more elagent modulus method?
        EvaluateIndex[n][k] = (EvaluateIndex[n][k] < 0L) ? (-EvaluateIndex[n][k] - dataLength2 * ((-EvaluateIndex[n][k]) / dataLength2))
          : (EvaluateIndex[n][k] - dataLength2 * (EvaluateIndex[n][k] / dataLength2));
        if (m_DataLength[n] <= EvaluateIndex[n][k])
          {
          EvaluateIndex[n][k] = dataLength2 - EvaluateIndex[n][k];
          }
        }

      }
  
    }
  
    /* perform interpolation */
    double interpolated = 0.0;
    IndexType coefficientIndex;
    // Step through eachpoint in the N-dimensional interpolation cube.
    for (int p = 0; p < m_MaxNumberInterpolationPoints; p++)
      {
      // translate each step into the N-dimensional index.
//      IndexType pointIndex = PointToIndex( p );

      double w = 1.0;
      for (int n = 0; n < ImageDimension; n++ )
        {
        w *= weights[n][ m_PointsToIndex[p][n] ];
        coefficientIndex[n] = EvaluateIndex[n][m_PointsToIndex[p][n]];  // Build up ND index for coefficients.
        }
        // Convert our step p to the appropriate point in ND space in the
        // m_Coefficients cube.
        interpolated += w * m_Coefficients->GetPixel(coefficientIndex);
      }
    
   int jj = 0;   
  return(interpolated);
    
}


template < class TImageType>
void BSplineInterpolateImageFunction< TImageType>
::SetInterpolationWeights( const ContinuousIndexType & x, const vnl_matrix<long> & EvaluateIndex, vnl_matrix<double> & weights ) const
{
  // For speed improvements we could make each case a separate function and use
  // function pointers to reference the correct weight order.
  // Another possiblity would be to loop inside the case statement (reducing the number
  // of switch statement executions to one per routine call.
  // Left as is for now for readability.
  double w, w2, w4, t, t0, t1;
  for (int n = 0; n < ImageDimension; n++)
    {
    switch (m_SplineOrder)
      {
      case 3:
        w = x[n] - (double) EvaluateIndex[n][1];
        weights[n][3] = (1.0 / 6.0) * w * w * w;
        weights[n][0] = (1.0 / 6.0) + (1.0 / 2.0) * w * (w - 1.0) - weights[n][3];
        weights[n][2] = w + weights[n][0] - 2.0 * weights[n][3];
        weights[n][1] = 1.0 - weights[n][0] - weights[n][2] - weights[n][3];
        break;
      case 0:
        weights[n][0] = 1; // implements nearest neighbor
        break;
      case 1:
        w = x[n] - (double) EvaluateIndex[n][0];
        weights[n][1] = w;
        weights[n][0] = 1.0 - w;
        break;
      case 2:
      /* x */
      w = x[n] - (double)EvaluateIndex[n][1];
      weights[n][1] = 3.0 / 4.0 - w * w;
      weights[n][2] = (1.0 / 2.0) * (w - weights[n][1] + 1.0);
      weights[n][0] = 1.0 - weights[n][1] - weights[n][2];
      break;
    case 4:
      /* x */
      w = x[n] - (double)EvaluateIndex[n][2];
      w2 = w * w;
      t = (1.0 / 6.0) * w2;
      weights[n][0] = 1.0 / 2.0 - w;
      weights[n][0] *= weights[n][0];
      weights[n][0] *= (1.0 / 24.0) * weights[n][0];
      t0 = w * (t - 11.0 / 24.0);
      t1 = 19.0 / 96.0 + w2 * (1.0 / 4.0 - t);
      weights[n][1] = t1 + t0;
      weights[n][3] = t1 - t0;
      weights[n][4] = weights[n][0] + t0 + (1.0 / 2.0) * w;
      weights[n][2] = 1.0 - weights[n][0] - weights[n][1] - weights[n][3] - weights[n][4];
      break;
    case 5:
      /* x */
      w = x[n] - (double)EvaluateIndex[n][2];
      w2 = w * w;
      weights[n][5] = (1.0 / 120.0) * w * w2 * w2;
      w2 -= w;
      w4 = w2 * w2;
      w -= 1.0 / 2.0;
      t = w2 * (w2 - 3.0);
      weights[n][0] = (1.0 / 24.0) * (1.0 / 5.0 + w2 + w4) - weights[n][5];
      t0 = (1.0 / 24.0) * (w2 * (w2 - 5.0) + 46.0 / 5.0);
      t1 = (-1.0 / 12.0) * w * (t + 4.0);
      weights[n][2] = t0 + t1;
      weights[n][3] = t0 - t1;
      t0 = (1.0 / 16.0) * (9.0 / 5.0 - t);
      t1 = (1.0 / 24.0) * w * (w4 - w2 - 5.0);
      weights[n][1] = t0 + t1;
      weights[n][4] = t0 - t1;
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
}


// Generates m_PointsToIndex;
template <class TImageType>
void
BSplineInterpolateImageFunction <TImageType>
::GeneratePointsToIndex( )
  {
  // m_PointsToIndex is used to convert a sequential location to an N-dimension
  // index vector.  This is precomputed to save time during the interpolation routine.
  m_PointsToIndex.resize(m_MaxNumberInterpolationPoints);
    for (int p = 0; p < m_MaxNumberInterpolationPoints; p++)
      {
      int pp = p;
    unsigned long indexFactor[ImageDimension];
    indexFactor[0] = 1;
    for (int j=1; j< ImageDimension; j++)
      {
      indexFactor[j] = indexFactor[j-1] * ( m_SplineOrder + 1 );
      }
    for (int j = (ImageDimension - 1); j >= 0; j--)
      {
      m_PointsToIndex[p][j] = pp / indexFactor[j];
      pp = pp % indexFactor[j];
      }
    }
  }

} // namespace itk

