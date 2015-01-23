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
#ifndef itkSpatialObjectToImageStatisticsCalculator_hxx
#define itkSpatialObjectToImageStatisticsCalculator_hxx

#include "itkSpatialObjectToImageStatisticsCalculator.h"
#include "itkImageRegionConstIteratorWithIndex.h"

#include "itkMeanSampleFilter.h"
#include "itkCovarianceSampleFilter.h"
#include "itkImageMaskSpatialObject.h"

namespace itk
{
/** Constructor */
template< typename TInputImage, typename TInputSpatialObject, unsigned int TSampleDimension >
SpatialObjectToImageStatisticsCalculator< TInputImage, TInputSpatialObject, TSampleDimension >
::SpatialObjectToImageStatisticsCalculator()
{
  m_Image = ITK_NULLPTR;
  m_SpatialObject = ITK_NULLPTR;
  m_Mean.Fill(0);
  m_CovarianceMatrix.SetIdentity();
  m_SampleDirection = TSampleDimension - 1;
  m_InternalImageTime = 0;
  m_InternalSpatialObjectTime = 0;
  m_Sum = 0;
  m_NumberOfPixels = 0;
  m_Sample = SampleType::New();
}

/** Compute Statistics from the Sample vector */
template< typename TInputImage, typename TInputSpatialObject, unsigned int TSampleDimension >
bool
SpatialObjectToImageStatisticsCalculator< TInputImage, TInputSpatialObject, TSampleDimension >
::ComputeStatistics()
{
  typedef itk::Statistics::MeanSampleFilter< SampleType > MeanAlgorithmType;
  typename MeanAlgorithmType::Pointer meanAlgorithm = MeanAlgorithmType::New();
  meanAlgorithm->SetInput(m_Sample);
  meanAlgorithm->Update();

  typename MeanAlgorithmType::MeasurementVectorType mean = meanAlgorithm->GetMean();

  for ( unsigned int i = 0; i < SampleDimension; i++ )
    {
    m_Mean[i] = mean[i];
    }

  typedef itk::Statistics::CovarianceSampleFilter< SampleType > CovarianceAlgorithmType;
  typename CovarianceAlgorithmType::Pointer covarianceAlgorithm =
    CovarianceAlgorithmType::New();

  covarianceAlgorithm->SetInput(m_Sample);
  covarianceAlgorithm->Update();

  typename CovarianceAlgorithmType::MatrixType covarianceMatrix = covarianceAlgorithm->GetCovarianceMatrix();
  for ( unsigned int i = 0; i < covarianceMatrix.Rows(); i++ )
    {
    for ( unsigned int j = 0; j < covarianceMatrix.Rows(); j++ )
      {
      m_CovarianceMatrix(i, j) = covarianceMatrix(i, j);
      }
    }

  return true;
}

/** */
template< typename TInputImage, typename TInputSpatialObject, unsigned int TSampleDimension >
void
SpatialObjectToImageStatisticsCalculator< TInputImage, TInputSpatialObject, TSampleDimension >
::Update(void)
{
  if ( !m_Image || !m_SpatialObject )
    {
    itkExceptionMacro("SpatialObjectToImageStatisticsCalculator: Please set the image AND the spatial object first.");
    }

  // Update only if the image or the spatial object has been modified
  if ( ( m_Image->GetMTime() == m_InternalImageTime )
       &&
       ( m_SpatialObject->GetMTime() == m_InternalSpatialObjectTime )
        )
    {
    return; // No need to update
    }

  m_InternalImageTime = m_Image->GetMTime();
  m_InternalSpatialObjectTime = m_SpatialObject->GetMTime();

  m_Sample = SampleType::New();
  m_Sample->SetMeasurementVectorSize(SampleDimension);

  m_NumberOfPixels = 0;
  m_Sum = 0;

  // If this is an ImageMaskSpatialObject we cannot use the flood filled
  // iterator
  if ( !strcmp(m_SpatialObject->GetTypeName(), "ImageMaskSpatialObject") )
    {
    typedef Image< unsigned char, itkGetStaticConstMacro(ObjectDimension) >   MaskImageType;
    typedef ImageMaskSpatialObject< itkGetStaticConstMacro(ObjectDimension) > MaskSOType;

    typename MaskSOType::Pointer maskSpatialObject = dynamic_cast< MaskSOType * >( m_SpatialObject.GetPointer() );
    if( maskSpatialObject.IsNull() )
      {
      itkExceptionMacro("Invalid dynamic cast.");
      }

    typename MaskImageType::ConstPointer maskImage =  maskSpatialObject->GetImage();

    typedef ImageRegionConstIterator< MaskImageType > MaskIteratorType;
    MaskIteratorType it( maskImage, maskImage->GetLargestPossibleRegion() );
    it.GoToBegin();
    while ( !it.IsAtEnd() )
      {
      if ( it.Get() > 0 ) // if inside the mask
        {
        IndexType  ind = it.GetIndex();
        VectorType mv;
        mv[0] = m_Image->GetPixel(ind);
        m_Sum += static_cast< AccumulateType >( mv[0] );
        for ( unsigned int i = 1; i < itkGetStaticConstMacro(SampleDimension); i++ )
          {
          ind[m_SampleDirection] += 1;
          mv[i] = m_Image->GetPixel(ind);
          m_Sum += static_cast< AccumulateType >( mv[i] );
          }
        m_Sample->PushBack(mv);
        m_NumberOfPixels++;
        }
      ++it;
      }
    }
  else
    {
    // Get the bounding box
    typename SpatialObjectType::BoundingBoxType::Pointer boundingBox;
    m_SpatialObject->ComputeBoundingBox();
    boundingBox = m_SpatialObject->GetBoundingBox();

    Point< double, itkGetStaticConstMacro(ObjectDimension) > pt;
    for ( unsigned int i = 0; i < itkGetStaticConstMacro(ObjectDimension); i++ )
      {
      pt[i] =
        boundingBox->GetBounds()[i * 2] + ( boundingBox->GetBounds()[i * 2 + 1] - boundingBox->GetBounds()[i * 2] ) / 2;
      }

    IndexType index;

    // We should remove the spacing and the origin of the image since the
    // FloodFill iterator is
    // considering them.
    for ( unsigned int i = 0; i < itkGetStaticConstMacro(ObjectDimension); i++ )
      {
      index[i] = (IndexValueType)( ( pt[i] - m_Image->GetOrigin()[i] ) / m_Image->GetSpacing()[i] );
      }

    IteratorType it = IteratorType(m_Image, m_SpatialObject, index);
    it.SetOriginInclusionStrategy();
    it.GoToBegin();

    while ( !it.IsAtEnd() )
      {
      IndexType  ind = it.GetIndex();
      VectorType mv;
      mv[0] = it.Get();
      m_Sum += static_cast< AccumulateType >( mv[0] );
      for ( unsigned int i = 1; i < itkGetStaticConstMacro(SampleDimension); i++ )
        {
        ind[m_SampleDirection] += 1;
        mv[i] = m_Image->GetPixel(ind);
        m_Sum += static_cast< AccumulateType >( mv[i] );
        }
      m_Sample->PushBack(mv);
      m_NumberOfPixels++;
      ++it;
      }
    }

  this->ComputeStatistics();
}

template< typename TInputImage, typename TInputSpatialObject, unsigned int TSampleDimension >
void
SpatialObjectToImageStatisticsCalculator< TInputImage, TInputSpatialObject, TSampleDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Image: " << m_Image << std::endl;
  os << indent << "SpatialObject: " << m_SpatialObject << std::endl;
  os << indent << "Mean: " << m_Mean << std::endl;
  os << indent << "Covariance Matrix: " << m_CovarianceMatrix << std::endl;
  os << indent << "Sum: " << m_Sum << std::endl;
  os << indent << "m_NumberOfPixels: " << m_NumberOfPixels << std::endl;
  os << indent << "Internal Image Time: " << m_InternalImageTime << std::endl;
  os << indent << "Internal Spatial Object Time: " << m_InternalSpatialObjectTime << std::endl;
  os << indent << "SampleDirection: " << m_SampleDirection << std::endl;
  os << indent << "Sample: " << m_Sample << std::endl;
}
} // end namespace itk

#endif
