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
#ifndef __itkRegistrationParameterScalesEstimator_hxx
#define __itkRegistrationParameterScalesEstimator_hxx

#include "itkRegistrationParameterScalesEstimator.h"

namespace itk
{

template< class TMetric >
RegistrationParameterScalesEstimator< TMetric >
::RegistrationParameterScalesEstimator()
{
  // estimate paramter scales of the moving transform
  m_TransformForward = true;

  // number for random sampling
  m_NumberOfRandomSamples = 0;

  // default sampling strategy
  m_SamplingStrategy = FullDomainSampling;

  // the default radius of the central region for sampling
  m_CentralRegionRadius = 5;

  // the metric object must be set before EstimateScales()
}

/** Estimate the trusted scale for steps. It returns the voxel spacing. */
template< class TMetric >
typename RegistrationParameterScalesEstimator< TMetric >::FloatType
RegistrationParameterScalesEstimator< TMetric >
::EstimateMaximumStepSize()
{
  this->CheckAndSetInputs();

  const typename VirtualImageType::SpacingType & spacing
    = this->m_VirtualImage->GetSpacing();

  const SizeValueType dim = this->GetImageDimension();

  FloatType minSpacing = NumericTraits<FloatType>::max();

  for (SizeValueType d=0; d<dim; d++)
    {
    if (minSpacing > spacing[d])
      {
      minSpacing = spacing[d];
      }
    }

  return minSpacing;
}

/** Validate and set metric and its transforms and images. */
template< class TMetric >
bool
RegistrationParameterScalesEstimator< TMetric >
::CheckAndSetInputs()
{
if (m_Metric.IsNull())
    {
    itkExceptionMacro("RegistrationParameterScalesEstimator: the metric is NULL");
    }

  this->m_FixedImage = m_Metric->GetFixedImage();
  this->m_MovingImage = m_Metric->GetMovingImage();
  this->m_VirtualImage = m_Metric->GetVirtualDomainImage();

  if (this->m_FixedImage.GetPointer() == NULL)
    {
    itkExceptionMacro("RegistrationParameterScalesEstimator: m_FixedImage in the metric is NULL");
    }
  if (this->m_MovingImage.GetPointer() == NULL)
    {
    itkExceptionMacro("RegistrationParameterScalesEstimator: m_MovingImage in the metric is NULL");
    }
  if (this->m_VirtualImage.GetPointer() == NULL)
    {
    itkExceptionMacro("RegistrationParameterScalesEstimator: m_VirtualDomainImage in the metric is NULL");
    }

  this->m_MovingTransform = m_Metric->GetMovingTransform();
  this->m_FixedTransform = m_Metric->GetFixedTransform();

  if (this->m_MovingTransform.GetPointer() == NULL)
    {
    itkExceptionMacro("RegistrationParameterScalesEstimator: m_MovingTransform in the metric is NULL.");
    }
  if (this->m_FixedTransform.GetPointer() == NULL)
    {
    itkExceptionMacro("RegistrationParameterScalesEstimator: m_FixedTransform in the metric is NULL.");
    }

  return true;
}

/** Get the transform being estimated scales for. */
template< class TMetric >
const TransformBase *
RegistrationParameterScalesEstimator< TMetric >
::GetTransform()
{
  if (m_TransformForward)
    {
    return m_MovingTransform.GetPointer();
    }
  else
    {
    return m_FixedTransform.GetPointer();
    }
}

/** Get the dimension of the target image transformed to. */
template< class TMetric >
itk::SizeValueType
RegistrationParameterScalesEstimator< TMetric >
::GetImageDimension()
{
  if (m_TransformForward)
    {
    return MovingImageDimension;
    }
  else
    {
    return FixedImageDimension;
    }
}

/** Check if the transform being optimized has local support. */
template< class TMetric >
bool
RegistrationParameterScalesEstimator< TMetric >
::HasLocalSupport()
{
  if (this->m_TransformForward)
    {
    return this->m_MovingTransform->HasLocalSupport();
    }
  else
    {
    return this->m_FixedTransform->HasLocalSupport();
    }
}

/** Get the number of scales. */
template< class TMetric >
SizeValueType
RegistrationParameterScalesEstimator< TMetric >
::GetNumberOfLocalParameters()
{
  if (this->GetTransformForward())
    {
    return this->GetMovingTransform()->GetNumberOfLocalParameters();
    }
  else
    {
    return this->GetFixedTransform()->GetNumberOfLocalParameters();
    }
}

/** Update the transform with a change in parameters. */
template< class TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::UpdateTransformParameters(const ParametersType &deltaParameters)
{
  // Apply the delta parameters to the transform
  if (this->m_TransformForward)
    {
    typename MovingTransformType::Pointer movingTransform =
      const_cast<MovingTransformType *>(this->GetMovingTransform());
    ParametersType &step = const_cast<ParametersType &>(deltaParameters);
    movingTransform->UpdateTransformParameters(step);
    }
  else
    {
    typename FixedTransformType::Pointer fixedTransform =
      const_cast<FixedTransformType *>(this->GetFixedTransform());
    ParametersType &step = const_cast<ParametersType &>(deltaParameters);
    fixedTransform->UpdateTransformParameters(step);
    }
}

/** Transform a physical point to a new physical point.
 *  We want to compute shift in physical space so that the scales is not
 *  sensitive to spacings and directions of image voxel sampling.
 */
template< class TMetric >
template< class TTargetPointType >
void
RegistrationParameterScalesEstimator< TMetric >
::TransformPoint(const VirtualPointType &point,
                 TTargetPointType &mappedPoint)
{
  if (this->GetTransformForward())
    {
    mappedPoint = this->GetMovingTransform()->TransformPoint(point);
    }
  else
    {
    mappedPoint = this->GetFixedTransform()->TransformPoint(point);
    }
}

/** Transform a physical point to its continuous index */
template< class TMetric >
template< class TContinuousIndexType >
void
RegistrationParameterScalesEstimator< TMetric >
::TransformPointToContinuousIndex(const VirtualPointType &point,
                                  TContinuousIndexType &mappedIndex)
{
  if (this->GetTransformForward())
    {
    MovingPointType mappedPoint;
    mappedPoint = this->GetMovingTransform()->TransformPoint(point);
    this->GetMovingImage()->TransformPhysicalPointToContinuousIndex(mappedPoint, mappedIndex);
    }
  else
    {
    FixedPointType mappedPoint;
    mappedPoint = this->GetFixedTransform()->TransformPoint(point);
    this->GetFixedImage()->TransformPhysicalPointToContinuousIndex(mappedPoint, mappedIndex);
    }
}

/** Get the squared norms of the transform Jacobians w.r.t parameters at a point */
template< class TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::ComputeSquaredJacobianNorms( const VirtualPointType  & point,
                                     ParametersType & squareNorms )
{
  JacobianType jacobian;
  const SizeValueType numPara = this->GetNumberOfLocalParameters();
  const SizeValueType dim = this->GetImageDimension();

  if (this->GetTransformForward())
    {
    this->GetMovingTransform()->ComputeJacobianWithRespectToParameters(point, jacobian);

    for (SizeValueType p=0; p<numPara; p++)
      {
      squareNorms[p] = NumericTraits< typename ParametersType::ValueType >::Zero;
      for (SizeValueType d=0; d<dim; d++)
        {
        squareNorms[p] += jacobian[d][p] * jacobian[d][p];
        }
      }
    }
  else
    {
    this->GetFixedTransform()->ComputeJacobianWithRespectToParameters(point, jacobian);

    for (SizeValueType p=0; p<numPara; p++)
      {
      squareNorms[p] = NumericTraits< typename ParametersType::ValueType >::Zero;
      for (SizeValueType d=0; d<dim; d++)
        {
        squareNorms[p] += jacobian[d][p] * jacobian[d][p];
        }
      }
    }
}

/** Sample the virtual image domain with phyical points
 *  and store the results into m_ImageSamples.
 */
template< class TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SampleImageDomain()
{
  if ( !(this->m_SamplingTime < this->GetTimeStamp())
    && !(this->m_SamplingTime < this->m_VirtualImage->GetTimeStamp()) )
    {
    // No modification since last sampling
    return;
    }

  if (m_SamplingStrategy == CornerSampling)
    {
    this->SampleImageDomainWithCorners();
    }
  else if (m_SamplingStrategy == RandomSampling)
    {
    this->SampleImageDomainRandomly();
    }
  else if (m_SamplingStrategy == CentralRegionSampling)
    {
    this->SampleImageDomainWithCentralRegion();
    }
  else
    {
    this->SampleImageDomainFully();
    }

  this->Modified();
  this->m_SamplingTime = this->GetTimeStamp();
}

/**
 * Set the sampling strategy automatically for scales estimation.
 */
template< class TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SetScalesSamplingStrategy()
{
  if( this->HasLocalSupport() )
    {
    this->SetSamplingStrategy(CentralRegionSampling);
    }
  else if (this->CheckGeneralAffineTransform())
    {
    this->SetSamplingStrategy(CornerSampling);
    }
  else
    {
    this->SetSamplingStrategy(RandomSampling);
    this->SetNumberOfRandomSamples( SizeOfSmallDomain );
    }
}

/**
 * Set the sampling strategy automatically for step scale estimation.
 */
template< class TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SetStepScaleSamplingStrategy()
{
  if (this->HasLocalSupport())
    {
    // Have to use FullDomainSampling for a transform with local support
    this->SetSamplingStrategy(FullDomainSampling);
    }
  else if (this->CheckGeneralAffineTransform())
    {
    this->SetSamplingStrategy(CornerSampling);
    }
  else
    {
    this->SetSamplingStrategy(RandomSampling);
    this->SetNumberOfRandomSamples( SizeOfSmallDomain );
    }
}

/**
 * Check if the transform is a general affine transform that maps a line
 * segment to a line segment.
 */
template< class TMetric >
bool
RegistrationParameterScalesEstimator< TMetric >
::CheckGeneralAffineTransform()
{
  if (this->GetTransformForward())
    {
    return this->template CheckGeneralAffineTransformTemplated<MovingTransformType>();
    }
  else
    {
    return this->template CheckGeneralAffineTransformTemplated<FixedTransformType>();
    }
}

/**
 * The templated version of CheckGeneralAffineTransform to check if the
 * transform is a general affine transform that maps a line segment to
 * a line segment.
 *
 * Examples are subclasses of MatrixOffsetTransformBaseType, TranslationTransform,
 * Rigid3DPerspectiveTransform, IdentityTransform, etc.
 */
template< class TMetric >
template< class TTransform >
bool
RegistrationParameterScalesEstimator< TMetric >
::CheckGeneralAffineTransformTemplated()
{
  typedef typename TTransform::ScalarType ScalarType;
  const SizeValueType InputSpaceDimension = TTransform::InputSpaceDimension;
  const SizeValueType OutputSpaceDimension = TTransform::OutputSpaceDimension;

  typedef MatrixOffsetTransformBase<ScalarType, InputSpaceDimension, OutputSpaceDimension>
          MatrixOffsetTransformBaseType;
  typedef TranslationTransform<ScalarType, InputSpaceDimension>
          TranslationTransformType;
  typedef IdentityTransform<ScalarType, InputSpaceDimension>
          IdentityTransformType;
  typedef Rigid3DPerspectiveTransform<ScalarType>
          Rigid3DPerspectiveTransformType;

  const TransformBase *transform = this->GetTransform();

  if ( dynamic_cast< const MatrixOffsetTransformBaseType * >( transform ) != NULL
    || dynamic_cast< const TranslationTransformType * >( transform ) != NULL
    || dynamic_cast< const IdentityTransformType * >( transform ) != NULL
    || dynamic_cast< const Rigid3DPerspectiveTransformType * >( transform ) != NULL
    )
    {
    return true;
    }

  return false;
}

/**
 *  Get the index of the virtual image center.
 */
template< class TMetric >
typename RegistrationParameterScalesEstimator< TMetric >::VirtualIndexType
RegistrationParameterScalesEstimator< TMetric >
::GetVirtualImageCentralIndex()
{
  VirtualImageConstPointer image = this->GetVirtualImage();
  VirtualRegionType region = this->m_Metric->GetVirtualDomainRegion();
  const SizeValueType dim = this->GetImageDimension();

  VirtualIndexType lowerIndex, upperIndex, centralIndex;
  lowerIndex = region.GetIndex();
  upperIndex = region.GetUpperIndex();

  for (SizeValueType d=0; d<dim; d++)
    {
    centralIndex[d] = (IndexValueType)((lowerIndex[d] + upperIndex[d])/2.0);
    }

  return centralIndex;
}

/**
 *  Get the region around the virtual image center.
 */
template< class TMetric >
typename RegistrationParameterScalesEstimator< TMetric >::VirtualRegionType
RegistrationParameterScalesEstimator< TMetric >
::GetVirtualImageCentralRegion()
{
  VirtualIndexType centralIndex = this->GetVirtualImageCentralIndex();

  VirtualImageConstPointer image = this->GetVirtualImage();
  VirtualRegionType region = this->m_Metric->GetVirtualDomainRegion();
  const SizeValueType dim = this->GetImageDimension();

  VirtualIndexType lowerIndex, upperIndex;
  lowerIndex = region.GetIndex();
  upperIndex = region.GetUpperIndex();

  for (SizeValueType d=0; d<dim; d++)
    {
    if (lowerIndex[d] < centralIndex[d] - this->m_CentralRegionRadius)
      {
      lowerIndex[d] = centralIndex[d] - this->m_CentralRegionRadius;
      }
    if (upperIndex[d] > centralIndex[d] + this->m_CentralRegionRadius)
      {
      upperIndex[d] = centralIndex[d] + this->m_CentralRegionRadius;
      }
    }

  VirtualRegionType centralRegion;
  centralRegion.SetIndex(lowerIndex);
  centralRegion.SetUpperIndex(upperIndex);

  return centralRegion;
}

/**
 *  Sample the virtual domain with the voxels around the center.
 */
template< class TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SampleImageDomainWithCentralRegion()
{
  VirtualRegionType centralRegion = this->GetVirtualImageCentralRegion();
  SampleImageDomainWithRegion(centralRegion);
}

/**
 *  Sample the virtual domain with all voxels inside a region.
 */
template< class TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SampleImageDomainWithRegion(VirtualRegionType region)
{
  VirtualImageConstPointer image = this->m_VirtualImage;
  const SizeValueType total = region.GetNumberOfPixels();
  m_ImageSamples.resize(total);

  /* Set up an iterator within the user specified virtual image region. */
  typedef ImageRegionConstIteratorWithIndex<VirtualImageType> RegionIterator;
  RegionIterator regionIter( image, region );

  VirtualPointType point;

  /* Iterate over the image */
  SizeValueType count = 0;
  regionIter.GoToBegin();
  while( !regionIter.IsAtEnd() )
    {
    image->TransformIndexToPhysicalPoint( regionIter.GetIndex(), point );
    m_ImageSamples[count] = point;
    ++regionIter;
    ++count;
    }
}

/**
 *  Sample the virtual domain with the points at image corners.
 *  And store the results into m_ImageSamples.
 */
template< class TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SampleImageDomainWithCorners()
{
  VirtualImageConstPointer image = this->m_VirtualImage;

  VirtualRegionType region = this->m_Metric->GetVirtualDomainRegion();
  VirtualIndexType firstCorner = region.GetIndex();
  VirtualIndexType corner;
  VirtualPointType point;

  VirtualSizeType size = region.GetSize();
  const int cornerNumber = 1 << VirtualImageDimension; // 2^ImageDimension

  m_ImageSamples.resize(cornerNumber);

  for(int i=0; i<cornerNumber; i++)
    {
    int bit;
    for (int d=0; d<VirtualImageDimension; d++)
      {
      bit = (int) (( i & (1 << d) ) != 0); // 0 or 1
      corner[d] = firstCorner[d] + bit * (size[d] - 1);
      }

    image->TransformIndexToPhysicalPoint(corner, point);
    m_ImageSamples[i] = point;
    }
}

/**
 * Sample the physical points of the virtual image in a uniform random distribution.
 */
template< class TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SampleImageDomainRandomly()
{
  VirtualImageConstPointer image = this->GetVirtualImage();

  if (m_NumberOfRandomSamples == 0)
    {
    const SizeValueType total = this->m_Metric->GetVirtualDomainRegion().GetNumberOfPixels();
    if (total <= SizeOfSmallDomain)
      {
      m_NumberOfRandomSamples = total;
      }
    else
      {
      FloatType ratio = 1 + vcl_log((FloatType)total/SizeOfSmallDomain);
      //ratio >= 1 since total/SizeOfSmallDomain > 1

      m_NumberOfRandomSamples = static_cast<int>(SizeOfSmallDomain * ratio);
      if (m_NumberOfRandomSamples > total)
        {
        m_NumberOfRandomSamples = total;
        }
      }
    }

  m_ImageSamples.resize(m_NumberOfRandomSamples);

  // Set up a random iterator within the user specified virtual image region.
  typedef ImageRandomConstIteratorWithIndex<VirtualImageType> RandomIterator;
  RandomIterator randIter( image, this->m_Metric->GetVirtualDomainRegion() );

  VirtualPointType point;

  randIter.SetNumberOfSamples( m_NumberOfRandomSamples );
  randIter.GoToBegin();
  for (SizeValueType i=0; i<m_NumberOfRandomSamples; i++)
    {
    image->TransformIndexToPhysicalPoint( randIter.GetIndex(), point );
    m_ImageSamples[i] = point;
    ++randIter;
    }
}

/**
 * Sample the virtual image domain fully with all pixels.
 */
template< class TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SampleImageDomainFully()
{
  VirtualRegionType region = this->m_Metric->GetVirtualDomainRegion();
  this->SampleImageDomainWithRegion(region);
}

/**
 * Print the information about this class.
 */
template< class TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "MetricType   = " << std::endl;
  os << indent << typeid(MetricType).name()  << std::endl;

  os << indent << "FixedTransformType   = " << std::endl;
  os << indent << typeid(FixedTransformType).name()  << std::endl;

  os << indent << "MovingTransformType  = " << std::endl;
  os << indent << typeid(MovingTransformType).name()  << std::endl;

  os << indent << "FixedImageType    = " << std::endl;
  os << indent << typeid(FixedImageType).name()  << std::endl;

  os << indent << "MovingImageType   = " << std::endl;
  os << indent << typeid(MovingImageType).name()  << std::endl;

  os << indent << "VirtualImageType  = " << std::endl;
  os << indent << typeid(VirtualImageType).name()  << std::endl;

  os << indent << "m_ImageSamples.size = " << std::endl;
  os << indent << this->m_ImageSamples.size()  << std::endl;

  os << indent << "m_TransformForward = " << this->m_TransformForward << std::endl;
  os << indent << "m_SamplingStrategy = " << this->m_SamplingStrategy << std::endl;

}

}  // namespace itk

#endif /* __itkRegistrationParameterScalesEstimator_txx */
