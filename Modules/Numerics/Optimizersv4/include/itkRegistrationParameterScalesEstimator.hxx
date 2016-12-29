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
#ifndef itkRegistrationParameterScalesEstimator_hxx
#define itkRegistrationParameterScalesEstimator_hxx

#include "itkRegistrationParameterScalesEstimator.h"

#include "itkCompositeTransform.h"
#include "itkPointSet.h"
#include "itkObjectToObjectMetric.h"

namespace itk
{

template< typename TMetric >
RegistrationParameterScalesEstimator< TMetric >
::RegistrationParameterScalesEstimator()
{
  // estimate parameter scales of the moving transform
  this->m_TransformForward = true;

  // number for random sampling
  this->m_NumberOfRandomSamples = 0;

  // default sampling strategy
  this->m_SamplingStrategy = FullDomainSampling;

  // the default radius of the central region for sampling
  this->m_CentralRegionRadius = 5;

  // the metric object must be set before EstimateScales()
}

/** Estimate the trusted scale for steps. It returns the voxel spacing. */
template< typename TMetric >
typename RegistrationParameterScalesEstimator< TMetric >::FloatType
RegistrationParameterScalesEstimator< TMetric >
::EstimateMaximumStepSize()
{
  this->CheckAndSetInputs();

  const VirtualSpacingType& spacing = this->m_Metric->GetVirtualSpacing();

  const SizeValueType dim = this->GetDimension();

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

/** Validate and set metric and its transforms. */
template< typename TMetric >
bool
RegistrationParameterScalesEstimator< TMetric >
::CheckAndSetInputs()
{
if (m_Metric.IsNull())
    {
    itkExceptionMacro("RegistrationParameterScalesEstimator: the metric is ITK_NULLPTR");
    }

  if (this->m_Metric->GetMovingTransform() == ITK_NULLPTR)
    {
    itkExceptionMacro("RegistrationParameterScalesEstimator: this->m_MovingTransform in the metric is ITK_NULLPTR.");
    }
  if (this->m_Metric->GetFixedTransform() == ITK_NULLPTR)
    {
    itkExceptionMacro("RegistrationParameterScalesEstimator: this->m_FixedTransform in the metric is ITK_NULLPTR.");
    }

  return true;
}

/** Get the transform being estimated scales for. */
template< typename TMetric >
const TransformBaseTemplate<typename TMetric::MeasureType> *
RegistrationParameterScalesEstimator< TMetric >
::GetTransform()
{
  if (m_TransformForward)
    {
    return this->m_Metric->GetMovingTransform();
    }
  else
    {
    return this->m_Metric->GetFixedTransform();
    }
}

/** Get the dimension of the target transformed to. */
template< typename TMetric >
itk::SizeValueType
RegistrationParameterScalesEstimator< TMetric >
::GetDimension()
{
  if (m_TransformForward)
    {
    return MovingDimension;
    }
  else
    {
    return FixedDimension;
    }
}

/** Check if the transform being optimized has local support. */

template< typename TMetric >
bool
RegistrationParameterScalesEstimator< TMetric >
::IsDisplacementFieldTransform()
{
  if( this->m_TransformForward && this->m_Metric->GetMovingTransform()->GetTransformCategory() == MovingTransformType::DisplacementField )
    {
    return true;
    }
  else if( !this->m_TransformForward && this->m_Metric->GetFixedTransform()->GetTransformCategory() == FixedTransformType::DisplacementField )
    {
    return true;
    }
  return false;
}

template< typename TMetric >
bool
RegistrationParameterScalesEstimator< TMetric >
::IsBSplineTransform()
{
  bool isBSplineTransform = false;

  if( this->m_TransformForward && this->m_Metric->GetMovingTransform()->GetTransformCategory() == MovingTransformType::BSpline )
    {
    isBSplineTransform = true;
    }
  else if( !this->m_TransformForward && this->m_Metric->GetFixedTransform()->GetTransformCategory() == FixedTransformType::BSpline )
    {
    isBSplineTransform = true;
    }

  // We need to check for the case where the fixed/moving transform is
  // a composite transform with optimizing B-spline transforms.
  // The CompositeTransform class function GetTransformCategory() handles
  // this scenario for displacement field transforms but we need to duplicate
  // the analogous b-spline case here.

  if( !isBSplineTransform )
    {
    if( this->m_TransformForward )
      {
      typedef CompositeTransform<FloatType, MovingDimension> CompositeTransformType;
      typename CompositeTransformType::Pointer compositeTransform = dynamic_cast<CompositeTransformType *>( const_cast<MovingTransformType *>( this->m_Metric->GetMovingTransform() ) );

      if( compositeTransform )
        {
        isBSplineTransform = true;
        for( signed long tind = static_cast<signed long>( compositeTransform->GetNumberOfTransforms() ) - 1; tind >= 0; tind-- )
          {
          if( compositeTransform->GetNthTransformToOptimize( tind ) &&
            ( compositeTransform->GetNthTransformConstPointer( tind )->GetTransformCategory() != MovingTransformType::BSpline ) )
            {
            isBSplineTransform = false;
            break;
            }
          }
        }
      }
    else // !this->m_TransformForward
      {
      typedef CompositeTransform<FloatType, FixedDimension> CompositeTransformType;
      typename CompositeTransformType::Pointer compositeTransform = dynamic_cast<CompositeTransformType *>( const_cast<FixedTransformType *>( this->m_Metric->GetFixedTransform() ) );

      if( compositeTransform )
        {
        isBSplineTransform = true;
        for( signed long tind = static_cast<signed long>( compositeTransform->GetNumberOfTransforms() ) - 1; tind >= 0; tind-- )
          {
          if( compositeTransform->GetNthTransformToOptimize( tind ) &&
            ( compositeTransform->GetNthTransformConstPointer( tind )->GetTransformCategory() != FixedTransformType::BSpline ) )
            {
            isBSplineTransform = false;
            break;
            }
          }
        }
      }
    }

  return isBSplineTransform;
}


template< typename TMetric >
bool
RegistrationParameterScalesEstimator< TMetric >
::TransformHasLocalSupportForScalesEstimation()
{
  if( this->IsDisplacementFieldTransform() || this->IsBSplineTransform() )
    {
    return true;
    }
  else
    {
    return false;
    }
}

/** Get the number of scales. */
template< typename TMetric >
SizeValueType
RegistrationParameterScalesEstimator< TMetric >
::GetNumberOfLocalParameters()
{
  if (this->GetTransformForward())
    {
    return this->m_Metric->GetMovingTransform()->GetNumberOfLocalParameters();
    }
  else
    {
    return this->m_Metric->GetFixedTransform()->GetNumberOfLocalParameters();
    }
}

/** Update the transform with a change in parameters. */
template< typename TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::UpdateTransformParameters(const ParametersType &deltaParameters)
{
  // Apply the delta parameters to the transform
  if (this->m_TransformForward)
    {
    typename MovingTransformType::Pointer movingTransform = const_cast<MovingTransformType *>(this->m_Metric->GetMovingTransform());
    ParametersType &step = const_cast<ParametersType &>(deltaParameters);
    movingTransform->UpdateTransformParameters(step);
    }
  else
    {
    typename FixedTransformType::Pointer fixedTransform = const_cast<FixedTransformType *>(this->m_Metric->GetFixedTransform());
    ParametersType &step = const_cast<ParametersType &>(deltaParameters);
    fixedTransform->UpdateTransformParameters(step);
    }
}

/** Transform a physical point to a new physical point.
 *  We want to compute shift in physical space so that the scales is not
 *  sensitive to spacings and directions of voxel sampling.
 */
template< typename TMetric >
template< typename TTargetPointType >
void
RegistrationParameterScalesEstimator< TMetric >
::TransformPoint(const VirtualPointType &point, TTargetPointType &mappedPoint)
{
  if (this->GetTransformForward())
    {
    mappedPoint = this->m_Metric->GetMovingTransform()->TransformPoint( point );
    }
  else
    {
    mappedPoint = this->m_Metric->GetFixedTransform()->TransformPoint( point );
    }
}

/** Get the squared norms of the transform Jacobians w.r.t parameters at a point */
template< typename TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::ComputeSquaredJacobianNorms( const VirtualPointType  & point, ParametersType & squareNorms )
{
  const SizeValueType numPara = this->GetNumberOfLocalParameters();
  const SizeValueType dim = this->GetDimension();
  JacobianType jacobian(dim,numPara);

  if (this->GetTransformForward())
    {
    this->m_Metric->GetMovingTransform()->ComputeJacobianWithRespectToParameters(point, jacobian);

    for (SizeValueType p=0; p<numPara; p++)
      {
      squareNorms[p] = NumericTraits< typename ParametersType::ValueType >::ZeroValue();
      for (SizeValueType d=0; d<dim; d++)
        {
        squareNorms[p] += jacobian[d][p] * jacobian[d][p];
        }
      }
    }
  else
    {
    this->m_Metric->GetFixedTransform()->ComputeJacobianWithRespectToParameters(point, jacobian);

    for (SizeValueType p=0; p<numPara; p++)
      {
      squareNorms[p] = NumericTraits< typename ParametersType::ValueType >::ZeroValue();
      for (SizeValueType d=0; d<dim; d++)
        {
        squareNorms[p] += jacobian[d][p] * jacobian[d][p];
        }
      }
    }
}

/** Sample the virtual domain with phyical points
 *  and store the results into this->m_SamplePoints.
 */
template< typename TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SampleVirtualDomain()
{
  if ( !(this->m_SamplingTime < this->GetTimeStamp()) && !(this->m_SamplingTime < this->m_Metric->GetVirtualDomainTimeStamp()) )
    {
    return;
    }

  if( ! this->m_Metric->SupportsArbitraryVirtualDomainSamples() && ! this->m_VirtualDomainPointSet )
    {
    itkExceptionMacro(" The assigned metric does not support aribitrary virtual domain sampling, "
                      " yet this->m_VirtualDomainPointSet has not been assigned. " );
    }

  if (m_SamplingStrategy == VirtualDomainPointSetSampling)
    {
    this->SampleVirtualDomainWithPointSet();
    }
  else if (m_SamplingStrategy == CornerSampling)
    {
    this->SampleVirtualDomainWithCorners();
    }
  else if (m_SamplingStrategy == RandomSampling)
    {
    this->SampleVirtualDomainRandomly();
    }
  else if (m_SamplingStrategy == CentralRegionSampling)
    {
    this->SampleVirtualDomainWithCentralRegion();
    }
  else
    {
    this->SampleVirtualDomainFully();
    }

  // Sanity check
  if( this->m_SamplePoints.size() == 0 )
    {
    itkExceptionMacro("No sample points were created.");
    }

  this->Modified();
  this->m_SamplingTime = this->GetTimeStamp();
}

/**
 * Set the sampling strategy automatically for scales estimation.
 */
template< typename TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SetScalesSamplingStrategy()
{
  if( this->m_VirtualDomainPointSet )
    {
    this->SetSamplingStrategy(VirtualDomainPointSetSampling);
    }
  else if( this->TransformHasLocalSupportForScalesEstimation() )
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
template< typename TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SetStepScaleSamplingStrategy()
{
  if( this->m_VirtualDomainPointSet )
    {
    this->SetSamplingStrategy(VirtualDomainPointSetSampling);
    }
  else if( this->TransformHasLocalSupportForScalesEstimation() )
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
template< typename TMetric >
bool
RegistrationParameterScalesEstimator< TMetric >
::CheckGeneralAffineTransform()
{
  if (this->GetTransformForward())
    {
    return this->CheckGeneralAffineTransformTemplated<MovingTransformType>();
    }
  else
    {
    return this->CheckGeneralAffineTransformTemplated<FixedTransformType>();
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
template< typename TMetric >
template< typename TTransform >
bool
RegistrationParameterScalesEstimator< TMetric >
::CheckGeneralAffineTransformTemplated()
{
  typedef typename TTransform::ScalarType ScalarType;
  const SizeValueType InputSpaceDimension = TTransform::InputSpaceDimension;
  const SizeValueType OutputSpaceDimension = TTransform::OutputSpaceDimension;

  typedef Transform<ScalarType, InputSpaceDimension, OutputSpaceDimension> TransformBaseType;

  const TransformBaseType *transform = dynamic_cast< const TransformBaseType * >( this->GetTransform() );


  if(transform)
    {
    return transform->IsLinear();
    }

  return false;
}

/**
 *  Get the index of the virtual image center.
 */
template< typename TMetric >
typename RegistrationParameterScalesEstimator< TMetric >::VirtualIndexType
RegistrationParameterScalesEstimator< TMetric >
::GetVirtualDomainCentralIndex()
{
  VirtualRegionType region = this->m_Metric->GetVirtualRegion();
  const SizeValueType dim = this->GetDimension();

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
template< typename TMetric >
typename RegistrationParameterScalesEstimator< TMetric >::VirtualRegionType
RegistrationParameterScalesEstimator< TMetric >
::GetVirtualDomainCentralRegion()
{
  VirtualIndexType centralIndex = this->GetVirtualDomainCentralIndex();

  VirtualRegionType region = this->m_Metric->GetVirtualRegion();
  const SizeValueType dim = this->GetDimension();

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
template< typename TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SampleVirtualDomainWithCentralRegion()
{
  VirtualRegionType centralRegion = this->GetVirtualDomainCentralRegion();
  SampleVirtualDomainWithRegion(centralRegion);
}

/**
 *  Sample the virtual domain with all voxels inside a region.
 */
template< typename TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SampleVirtualDomainWithRegion(VirtualRegionType region)
{
  VirtualImageConstPointer image = this->m_Metric->GetVirtualImage();
  const SizeValueType total = region.GetNumberOfPixels();
  this->m_SamplePoints.resize(total);

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
    this->m_SamplePoints[count] = point;
    ++regionIter;
    ++count;
    }
}

/**
 *  Sample the virtual domain with the points at image corners.
 *  And store the results into this->m_SamplePoints.
 */
template< typename TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SampleVirtualDomainWithCorners()
{
  VirtualImageConstPointer image = this->m_Metric->GetVirtualImage();

  VirtualRegionType region = this->m_Metric->GetVirtualRegion();
  VirtualIndexType firstCorner = region.GetIndex();
  VirtualIndexType corner;
  VirtualPointType point;

  VirtualSizeType size = region.GetSize();
  const unsigned int cornerNumber = 1 << VirtualDimension; // 2^Dimension

  this->m_SamplePoints.resize(cornerNumber);

  for(unsigned int i=0; i<cornerNumber; i++)
    {
    for (unsigned int d=0; d<VirtualDimension; d++)
      {
      const unsigned int bit = (unsigned int) (( i & (1 << d) ) != 0); // 0 or 1
      corner[d] = firstCorner[d] + bit * (size[d] - 1);
      }

    image->TransformIndexToPhysicalPoint(corner, point);
    this->m_SamplePoints[i] = point;
    }
}

/**
 * Sample the physical points of the virtual domain in a uniform random distribution.
 */
template< typename TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SampleVirtualDomainRandomly()
{
  VirtualImageConstPointer image = this->m_Metric->GetVirtualImage();

  if (m_NumberOfRandomSamples == 0)
    {
    const SizeValueType total = this->m_Metric->GetVirtualRegion().GetNumberOfPixels();
    if (total <= SizeOfSmallDomain)
      {
      this->m_NumberOfRandomSamples = total;
      }
    else
      {
      FloatType ratio = 1 + std::log((FloatType)total/SizeOfSmallDomain);
      //ratio >= 1 since total/SizeOfSmallDomain > 1

      this->m_NumberOfRandomSamples = static_cast<int>(SizeOfSmallDomain * ratio);
      if (m_NumberOfRandomSamples > total)
        {
        this->m_NumberOfRandomSamples = total;
        }
      }
    }

  this->m_SamplePoints.resize(m_NumberOfRandomSamples);

  // Set up a random iterator within the user specified virtual image region.
  typedef ImageRandomConstIteratorWithIndex<VirtualImageType> RandomIterator;
  RandomIterator randIter( image, this->m_Metric->GetVirtualRegion() );

  VirtualPointType point;

  randIter.SetNumberOfSamples( this->m_NumberOfRandomSamples );
  randIter.GoToBegin();
  for (SizeValueType i=0; i<m_NumberOfRandomSamples; i++)
    {
    image->TransformIndexToPhysicalPoint( randIter.GetIndex(), point );
    this->m_SamplePoints[i] = point;
    ++randIter;
    }
}

/**
 * Sample the virtual domain using a point set.
 */
template< typename TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SampleVirtualDomainWithPointSet()
{
  /* The virtual domain point set must already be supplied */
  if( ! this->m_VirtualDomainPointSet )
    {
    itkExceptionMacro("The virtual domain point set has not been set.");
    }
  if( this->m_VirtualDomainPointSet->GetNumberOfPoints() < 1 )
    {
    itkExceptionMacro("The virtual domain point set has no points.");
    }

  this->m_SamplePoints.resize( this->m_VirtualDomainPointSet->GetNumberOfPoints() );

  typename VirtualPointSetType::PointsContainerConstIterator it( this->m_VirtualDomainPointSet->GetPoints()->Begin() );
  SizeValueType count = 0;
  while( it != this->m_VirtualDomainPointSet->GetPoints()->End() )
    {
    this->m_SamplePoints[count] = it.Value();
    ++count;
    ++it;
    }
}

/**
 * Sample the virtual domain fully with all pixels.
 */
template< typename TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::SampleVirtualDomainFully()
{
  VirtualRegionType region = this->m_Metric->GetVirtualRegion();
  this->SampleVirtualDomainWithRegion(region);
}

/**
 * Print the information about this class.
 */
template< typename TMetric >
void
RegistrationParameterScalesEstimator< TMetric >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "MetricType   = " << std::endl;
  os << indent << typeid(MetricType).name()  << std::endl;

  os << indent << "m_SamplePoints.size = " << std::endl;
  os << indent << this->m_SamplePoints.size()  << std::endl;

  os << indent << "m_TransformForward = " << this->m_TransformForward << std::endl;
  os << indent << "m_SamplingStrategy = " << this->m_SamplingStrategy << std::endl;

  os << indent << "m_VirtualDomainPointSet = " << this->m_VirtualDomainPointSet.GetPointer() << std::endl;
}

}  // namespace itk

#endif /* itkRegistrationParameterScalesEstimator_hxx */
