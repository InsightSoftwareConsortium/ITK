/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkParzenWindowAffineMutualInformationMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#include "itkNumericTraits.h"
#include "vnl/vnl_math.h"
#include "vnl/vnl_sample.h"

namespace itk
{

/**
 * Default constructor
 */
template <class TRefImage, class TTestImage, class TDerivImage>
ParzenWindowAffineMutualInformationImageMetricMetricMetric<TRefImage,TTestImage,TDerivImage>
::ParzenWindowAffineMutualInformationImageMetricMetricMetric()
{
  m_NumberOfSamples = 0;
  m_SamplesValid    = false;
  m_ReferenceStdDev = 0.1;
  m_TestStdDev      = 0.1;

  m_KernelFunction  = dynamic_cast<KernelFunction*>( 
    GaussianKernelFunction::New().GetPointer() );

  this->Resize(50);

}


/**
 * PrintSelf
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
ParzenWindowAffineMutualInformationImageMetricMetricMetric<TRefImage,TTestImage,TDerivImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Mutual information using Viola and Wells" << std::endl;

}



/**
 * Define the input test image.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
ParzenWindowAffineMutualInformationImageMetricMetricMetric<TRefImage,TTestImage,TDerivImage>
::SetTestImage(
TestImageType * ptr )
{
  this->MutualInformationImageMetric<TRefImage,TTestImage>::SetTestImage(ptr);

  // connect the image to a interpolator function
  m_Interpolator = InterpolatorType::New();
  m_Interpolator->SetInputImage( this->GetTestImage() );
}



/**
 * Resize the containers holding the current spatial 
 * samples.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
ParzenWindowAffineMutualInformationImageMetricMetricMetric<TRefImage,TTestImage,TDerivImage>
::Resize(
unsigned int num )
{
  if( num == m_NumberOfSamples ) return;

  m_SpatialSamplesA.resize( num );
  m_RefIntensitiesA.resize( num );
  m_TestIntensitiesA.resize( num );
  m_TestDerivativesA.resize( num );

  m_TestMatrixDerivA.resize( num );

  m_SpatialSamplesB.resize( num );
  m_RefIntensitiesB.resize( num );
  m_TestIntensitiesB.resize( num );
  m_TestDerivativesB.resize( num );

  m_NumberOfSamples = num;
  m_SamplesValid = false;

}


/**
 * Uniformly sample the reference image.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
ParzenWindowAffineMutualInformationImageMetricMetricMetric<TRefImage,TTestImage,TDerivImage>
::SpatialSample()
{

  if( !m_RefImage ) return;

  this->SpatialSample( m_SpatialSamplesA, m_RefIntensitiesA );
  this->SpatialSample( m_SpatialSamplesB, m_RefIntensitiesB );

  m_SamplesValid = true;

}


/**
 * Uniformly sample the reference image.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
ParzenWindowAffineMutualInformationImageMetricMetricMetric<TRefImage,TTestImage,TDerivImage>
::SpatialSample(
IndexContainer& indices,
IntensityContainer& intensities )
{

  RefImagePointer refImage = this->GetReferenceImage();

  double range = (double) refImage->GetOffsetTable()[ImageDimension];

  for( unsigned int s = 0; s < m_NumberOfSamples; s++ )
    {

    double uRand = vnl_sample_uniform( 0.0, 1.0 ) * range;

    unsigned long offset;
    if( uRand == range )
      {
      offset = (unsigned long) floor(range) - 1;
      }
    else 
      {
      offset = (unsigned long) floor( uRand );
      }

    IndexType index = refImage->ComputeIndex( offset );

    indices[s] = index;
    intensities[s] = refImage->GetPixel( index );

    }

}


/**
 * Calculate the test image intensities corresponding to the
 * reference image spatial samples.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
ParzenWindowAffineMutualInformationImageMetricMetricMetric<TRefImage,TTestImage,TDerivImage>
::CalculateTestIntensities()
{
  if( !m_TestImage ) return;

  this->CalculateTestIntensities( m_SpatialSamplesA, m_TestIntensitiesA );
  this->CalculateTestIntensities( m_SpatialSamplesB, m_TestIntensitiesB );
}


/**
 * Calculate the test image intensities corresponding to the
 * reference image spatial samples.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
ParzenWindowAffineMutualInformationImageMetricMetricMetric<TRefImage,TTestImage,TDerivImage>
::CalculateTestIntensities(
IndexContainer& indices,
IntensityContainer& intensities )
{
  if( !m_TestImage ) return;

  VectorType refVec;

  for( unsigned int s = 0; s < m_NumberOfSamples; s++ )
    {
    IndexType index = indices[s];

    for( int j = 0; j < ImageDimension; j++ )
      {
      refVec[j] = (double) index[j];
      }

    refVec = m_AffineMatrix * refVec;
    refVec += m_AffineVector;

    intensities[s] = m_Interpolator->Evaluate( refVec.data_block() );

    }

}


/**
 * Calculate the test image derivatives corresponding to the
 * reference image spatial sample positions.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
ParzenWindowAffineMutualInformationImageMetricMetricMetric<TRefImage,TTestImage,TDerivImage>
::CalculateTestDerivatives()
{
  if( !m_TestImage ) return;

  for( int j = 0; j < ImageDimension; j++ )
    {
    if( !m_DerivImages[j] ) return;
    }

  this->CalculateTestDerivatives( m_SpatialSamplesA, m_TestDerivativesA );
  this->CalculateTestDerivatives( m_SpatialSamplesB, m_TestDerivativesB );

}


/**
 * Calculate the test image derivatives corresponding to the
 * reference image spatial sample positions.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
ParzenWindowAffineMutualInformationImageMetricMetricMetric<TRefImage,TTestImage,TDerivImage>
::CalculateTestDerivatives(
IndexContainer& indices,
VectorContainer& derivatives )
{
  VectorType refVec;
  VectorType derivVec;
  typename TTestImage::SizeType size = 
    m_TestImage->GetLargestPossibleRegion().GetSize();

  for( unsigned int s = 0; s < m_NumberOfSamples; s++ )
  {
    IndexType index = indices[s];
    bool inRange = true;

    for( int j = 0; j < ImageDimension; j++ )
      {
      refVec[j] = (double) index[j];
      }
    refVec = m_AffineMatrix * refVec;
    refVec += m_AffineVector;

    for( int j = 0; j < ImageDimension; j++ )
      {
      index[j] = vnl_math_rnd( refVec[j] );
      if( index[j] < 0 || index[j] >= size[j] )
        {
        inRange = false;
        break;
        }
      }

    if( inRange )
      {
      for( int j = 0; j < ImageDimension; j++ )
        {
        typedef typename TDerivImage::PixelType DerivPixelType;
        derivVec[j] = ScalarTraits<DerivPixelType>::GetScalar(
          m_DerivImages[j]->GetPixel( index ) );
        }
      derivatives[s] = derivVec;

      }
    else 
      {
      derivVec.fill( 0.0 );
      derivatives[s] = derivVec;
      }

    }

}



/**
 * Calculate the mutual information from the spatial samples.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
ParzenWindowAffineMutualInformationImageMetricMetricMetric<TRefImage,TTestImage,TDerivImage>
::CalculateMutualInformationImageMetric(
bool resample)
{
  RefImagePointer refImage = this->GetReferenceImage();
  TestImagePointer testImage = this->GetTestImage();

  if( !refImage || !testImage ) return;

  if( !m_SamplesValid || resample )
    {
    this->SpatialSample();
    }

  this->CalculateTestIntensities();

  double dLogSumRef = 0.0;
  double dLogSumTest = 0.0;
  double dLogSumJoint = 0.0;

  for( unsigned int nB = 0; nB < m_NumberOfSamples; nB++ )
    {
    double dSumRef  = 0.0;
    double dSumTest = 0.0;
    double dSumJoint = 0.0;

    for( unsigned int nA = 0; nA < m_NumberOfSamples; nA++ )
      {
      double valueRef;
      double valueTest;

      valueRef = ( m_RefIntensitiesB[nB] - m_RefIntensitiesA[nA] ) / m_ReferenceStdDev;
      valueRef = m_KernelFunction->Evaluate( valueRef );

      valueTest = ( m_TestIntensitiesB[nB] - m_TestIntensitiesA[nA] ) / m_TestStdDev;
      valueTest = m_KernelFunction->Evaluate( valueTest );

      dSumRef += valueRef;
      dSumTest += valueTest;
      dSumJoint += valueRef * valueTest;

      }


    dSumRef /= (double) m_NumberOfSamples * m_ReferenceStdDev;
    dSumTest /= (double) m_NumberOfSamples * m_TestStdDev;
    dSumJoint /= (double) m_NumberOfSamples * m_ReferenceStdDev * m_TestStdDev;

    dLogSumRef -= log( dSumRef );
    dLogSumTest -= log( dSumTest );
    dLogSumJoint -= log( dSumJoint );

    }

  dLogSumRef /= (double) m_NumberOfSamples;
  dLogSumTest /= (double) m_NumberOfSamples;
  dLogSumJoint /= (double) m_NumberOfSamples;

  m_MutualInformationImageMetric = dLogSumRef + dLogSumTest - dLogSumJoint;

}



/**
 * Calculate the mutual information and
 * gradient with repsect to the affine parameters.
 */
template <class TRefImage, class TTestImage, class TDerivImage>
void
ParzenWindowAffineMutualInformationImageMetricMetricMetric<TRefImage,TTestImage,TDerivImage>
::CalculateMutualInformationImageMetricAndGradient(
bool resample)
{

  RefImagePointer refImage = this->GetReferenceImage();
  TestImagePointer testImage = this->GetTestImage();

  if( !testImage || !refImage ) return;

  if( !m_SamplesValid || resample )
    {
    this->SpatialSample();
    }

  this->CalculateTestIntensities();
  this->CalculateTestDerivatives();

  // calculate the derivatives

  for( unsigned int nA = 0; nA < m_NumberOfSamples; nA++ )
    {  
    for( unsigned int idim = 0; idim < ImageDimension; idim++ )
      {
      for( unsigned int jdim = 0; jdim < ImageDimension; jdim++ )
        {
        (m_TestMatrixDerivA[nA])[idim][jdim] = 
          (m_TestDerivativesA[nA])[idim] * (m_SpatialSamplesA[nA])[jdim];
        }
      }
    } // end A sample loop

  m_AffineMatrixDerivative.fill( 0.0 );
  m_AffineVectorDerivative.fill( 0.0 );

  double dLogSumRef = 0.0;
  double dLogSumTest = 0.0;
  double dLogSumJoint = 0.0;

  for( unsigned int nB = 0; nB < m_NumberOfSamples; nB++ )
    {
    double dDenominatorTest = 0.0;
    double dDenominatorJoint = 0.0;

    double dSumRef = 0.0;

    for( unsigned int nA = 0; nA < m_NumberOfSamples; nA++ )
      {
      double valueRef;
      double valueTest;

      valueRef = ( m_RefIntensitiesB[nB] - m_RefIntensitiesA[nA] ) 
        / m_ReferenceStdDev;
      valueRef = m_KernelFunction->Evaluate( valueRef );

      valueTest = ( m_TestIntensitiesB[nB] - m_TestIntensitiesA[nA] ) 
        / m_TestStdDev;
      valueTest = m_KernelFunction->Evaluate( valueTest );

      dDenominatorTest += valueTest;
      dDenominatorJoint += valueTest * valueRef;

      dSumRef += valueRef;
      
      } // end A sample loop

    dLogSumRef -= log( dSumRef / 
      ( (double) m_NumberOfSamples * m_ReferenceStdDev ) );
    dLogSumTest -= log( dDenominatorTest / 
      ( (double) m_NumberOfSamples * m_TestStdDev ) );
    dLogSumJoint -= log( dDenominatorJoint / 
      ( (double) m_NumberOfSamples * m_ReferenceStdDev *
      m_TestStdDev ) );

    MatrixType testMatrixDerivB;
    for( int idim = 0; idim < ImageDimension; idim++ )
      {
      for( int jdim = 0; jdim < ImageDimension; jdim++ )
        {
        testMatrixDerivB[idim][jdim] =
          (m_TestDerivativesB[nB])[idim] * (m_SpatialSamplesB[nB])[jdim];
        }
      }

    for( unsigned int nA = 0; nA < m_NumberOfSamples; nA++ )
      {
      double valueRef;
      double valueTest;
      double weightTest;
      double weightJoint;
      double weight;

      valueRef = ( m_RefIntensitiesB[nB] - m_RefIntensitiesA[nA] ) / 
        m_ReferenceStdDev;
      valueRef = m_KernelFunction->Evaluate( valueRef );

      valueTest = ( m_TestIntensitiesB[nB] - m_TestIntensitiesA[nA] ) / 
        m_TestStdDev;
      valueTest = m_KernelFunction->Evaluate( valueTest );

      weightTest = valueTest / dDenominatorTest;
      weightJoint = valueTest * valueRef / dDenominatorJoint;

      weight = ( weightTest - weightJoint ) / m_TestStdDev;
      weight *= m_TestIntensitiesB[nB] - m_TestIntensitiesA[nA];

      m_AffineMatrixDerivative += ( weight * 
        ( testMatrixDerivB - m_TestMatrixDerivA[nA] ) );
      m_AffineVectorDerivative += ( weight * 
        ( m_TestDerivativesB[nB] - m_TestDerivativesA[nA] ) );
      
      } // end A sample loop

    }  // end B sample loop

  dLogSumRef /= (double) m_NumberOfSamples;
  dLogSumTest /= (double) m_NumberOfSamples;
  dLogSumJoint /= (double) m_NumberOfSamples;

  m_MutualInformationImageMetric = dLogSumRef + dLogSumTest - dLogSumJoint;

  m_AffineMatrixDerivative /= (double) m_NumberOfSamples;
  m_AffineVectorDerivative /= (double) m_NumberOfSamples;

}


} // namespace itk
