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
#ifndef itkJointHistogramMutualInformationGetValueAndDerivativeThreader_hxx
#define itkJointHistogramMutualInformationGetValueAndDerivativeThreader_hxx

#include "itkJointHistogramMutualInformationGetValueAndDerivativeThreader.h"

namespace itk
{

template< typename TDomainPartitioner, typename TImageToImageMetric, typename TJointHistogramMetric >
JointHistogramMutualInformationGetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TJointHistogramMetric >
::JointHistogramMutualInformationGetValueAndDerivativeThreader() :
  m_JointHistogramMIPerThreadVariables( ITK_NULLPTR ),
  m_JointAssociate( ITK_NULLPTR )
{}


template< typename TDomainPartitioner, typename TImageToImageMetric, typename TJointHistogramMetric >
JointHistogramMutualInformationGetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TJointHistogramMetric >
::~JointHistogramMutualInformationGetValueAndDerivativeThreader()
{
  delete[] this->m_JointHistogramMIPerThreadVariables;
}


template< typename TDomainPartitioner, typename TImageToImageMetric, typename TJointHistogramMetric >
void
JointHistogramMutualInformationGetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TJointHistogramMetric >
::BeforeThreadedExecution()
{
  Superclass::BeforeThreadedExecution();

  /* Store the casted pointer to avoid dynamic casting in tight loops. */
  this->m_JointAssociate = dynamic_cast< TJointHistogramMetric * >( this->m_Associate );
  if( this->m_JointAssociate == ITK_NULLPTR )
    {
    itkExceptionMacro("Dynamic casting of associate pointer failed.");
    }

  const ThreadIdType numThreadsUsed = this->GetNumberOfThreadsUsed();
  delete[] this->m_JointHistogramMIPerThreadVariables;
  this->m_JointHistogramMIPerThreadVariables = new AlignedJointHistogramMIPerThreadStruct[ numThreadsUsed ];

  for( ThreadIdType i = 0; i < numThreadsUsed; ++i )
    {
    if( this->m_JointHistogramMIPerThreadVariables[i].JointPDFInterpolator.IsNull() )
      {
      this->m_JointHistogramMIPerThreadVariables[i].JointPDFInterpolator = JointPDFInterpolatorType::New();
      }
    this->m_JointHistogramMIPerThreadVariables[i].JointPDFInterpolator->SetInputImage( this->m_JointAssociate->m_JointPDF );
    if( this->m_JointHistogramMIPerThreadVariables[i].FixedImageMarginalPDFInterpolator.IsNull() )
      {
      this->m_JointHistogramMIPerThreadVariables[i].FixedImageMarginalPDFInterpolator = MarginalPDFInterpolatorType::New();
      }
    this->m_JointHistogramMIPerThreadVariables[i].FixedImageMarginalPDFInterpolator->SetInputImage( this->m_JointAssociate->m_FixedImageMarginalPDF );
    if( this->m_JointHistogramMIPerThreadVariables[i].MovingImageMarginalPDFInterpolator.IsNull() )
      {
      this->m_JointHistogramMIPerThreadVariables[i].MovingImageMarginalPDFInterpolator = MarginalPDFInterpolatorType::New();
      }
    this->m_JointHistogramMIPerThreadVariables[i].MovingImageMarginalPDFInterpolator->SetInputImage( this->m_JointAssociate->m_MovingImageMarginalPDF );
    }
}

template< typename TDomainPartitioner, typename TImageToImageMetric, typename TJointHistogramMetric >
void
JointHistogramMutualInformationGetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TJointHistogramMetric >
::AfterThreadedExecution()
{
  Superclass::AfterThreadedExecution();

  // The Superclass does not generate a valid m_Value for this metric.  We have to calculate it
  // here, but only if there are 1 or more valid points. Otherwise the Superclass
  // will have already set a default value and issued a warning.
  if( this->m_JointAssociate->GetNumberOfValidPoints() > 0 )
    {
    this->m_JointAssociate->m_Value = this->m_JointAssociate->ComputeValue();
    }
}

template< typename TDomainPartitioner, typename TImageToImageMetric, typename TJointHistogramMetric >
bool
JointHistogramMutualInformationGetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TJointHistogramMetric >
::ProcessPoint( const VirtualIndexType &,
                const VirtualPointType &        virtualPoint,
                const FixedImagePointType &,
                const FixedImagePixelType &     fixedImageValue,
                const FixedImageGradientType &,
                const MovingImagePointType &,
                const MovingImagePixelType &    movingImageValue,
                const MovingImageGradientType & movingImageGradient,
                MeasureType &,
                DerivativeType &                localDerivativeReturn,
                const ThreadIdType              threadId ) const
{
  // check that the moving image sample is within the range of the true min
  // and max, hence being within the moving image mask
  if ( movingImageValue < this->m_JointAssociate->m_MovingImageTrueMin )
    {
    return false;
    }
  else if ( movingImageValue > this->m_JointAssociate->m_MovingImageTrueMax )
    {
    return false;
    }
  /** the scalingfactor is the MI specific scaling of the image gradient and jacobian terms */
  InternalComputationValueType scalingfactor = NumericTraits< InternalComputationValueType >::ZeroValue(); // for scaling the jacobian terms

  JointPDFPointType jointPDFpoint;
  this->m_JointAssociate->ComputeJointPDFPoint( fixedImageValue, movingImageValue, jointPDFpoint );
  // Make sure the point is inside th joint pdf.
  if ( ! this->m_JointHistogramMIPerThreadVariables[threadId].JointPDFInterpolator->IsInsideBuffer( jointPDFpoint ) )
    {
    return false;
    }
  InternalComputationValueType jointPDFValue = this->m_JointHistogramMIPerThreadVariables[threadId].JointPDFInterpolator->Evaluate( jointPDFpoint );
  SizeValueType ind = 1;
  InternalComputationValueType dJPDF = this->ComputeJointPDFDerivative( jointPDFpoint, threadId , ind );
  typename MarginalPDFType::PointType mind;
  mind[0] = jointPDFpoint[ind];
  InternalComputationValueType movingImagePDFValue =
    this->m_JointHistogramMIPerThreadVariables[threadId].MovingImageMarginalPDFInterpolator->Evaluate(mind);
  InternalComputationValueType dMmPDF =
    this->ComputeMovingImageMarginalPDFDerivative( mind , threadId );

  const InternalComputationValueType eps = 1.e-16;
  if( jointPDFValue > eps &&  movingImagePDFValue > eps )
    {
    const InternalComputationValueType pRatio =
                            std::log(jointPDFValue)-std::log(movingImagePDFValue);
    const InternalComputationValueType & term1 = dJPDF*pRatio;
    const InternalComputationValueType & term2 = this->m_JointAssociate->m_Log2 * dMmPDF * jointPDFValue / movingImagePDFValue;
    scalingfactor =  ( term2 - term1 );
    }  // end if-block to check non-zero bin contribution
  else
    {
    scalingfactor = NumericTraits< InternalComputationValueType >::ZeroValue();
    }

  /* Use a pre-allocated jacobian object for efficiency */
  typedef JacobianType & JacobianReferenceType;
  JacobianReferenceType jacobian = this->m_GetValueAndDerivativePerThreadVariables[threadId].MovingTransformJacobian;
  JacobianReferenceType jacobianPositional = this->m_GetValueAndDerivativePerThreadVariables[threadId].MovingTransformJacobianPositional;

  /** For dense transforms, this returns identity */
  this->m_JointAssociate->GetMovingTransform()->
    ComputeJacobianWithRespectToParametersCachedTemporaries(virtualPoint,
                                                            jacobian,
                                                            jacobianPositional);

  for ( NumberOfParametersType par = 0; par < this->GetCachedNumberOfLocalParameters(); par++ )
    {
    InternalComputationValueType sum = NumericTraits< InternalComputationValueType >::ZeroValue();
    for ( SizeValueType dim = 0; dim < TImageToImageMetric::MovingImageDimension; dim++ )
      {
      sum += scalingfactor * jacobian(dim, par) * movingImageGradient[dim];
      }
    localDerivativeReturn[par] = sum;
    }
  return true;
}

template< typename TDomainPartitioner, typename TImageToImageMetric, typename TJointHistogramMetric >
typename JointHistogramMutualInformationGetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TJointHistogramMetric >::InternalComputationValueType
JointHistogramMutualInformationGetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TJointHistogramMetric >
::ComputeFixedImageMarginalPDFDerivative( const MarginalPDFPointType & margPDFpoint,
                                          const ThreadIdType threadId ) const
{
  InternalComputationValueType offset = 0.5*this->m_JointPDFSpacing[0];
  InternalComputationValueType eps = this->m_JointPDFSpacing[0];
  MarginalPDFPointType         leftpoint = margPDFpoint;
  leftpoint[0] -= offset;
  MarginalPDFPointType  rightpoint = margPDFpoint;
  rightpoint[0] += offset;
  if (leftpoint[0] < eps )
    {
    leftpoint[0] = eps;
    }
  if (rightpoint[0] < eps )
    {
    rightpoint[0] = eps;
    }
  if (leftpoint[0] > 1.0 )
    {
    leftpoint[0] = 1.0;
    }
  if (rightpoint[0] > 1.0 )
    {
    rightpoint[0] = 1.0;
    }
  InternalComputationValueType delta = rightpoint[0]-leftpoint[0];
  if ( delta > NumericTraits< InternalComputationValueType >::ZeroValue() )
    {
    InternalComputationValueType deriv = this->m_ThreaderFixedImageMarginalPDFInterpolator[threadId]->Evaluate(rightpoint) -
      this->m_ThreaderFixedImageMarginalPDFInterpolator[threadId]->Evaluate(leftpoint);
    return deriv/delta;
    }
  else
    {
    return NumericTraits< InternalComputationValueType >::ZeroValue();
    }
}

template< typename TDomainPartitioner, typename TImageToImageMetric, typename TJointHistogramMetric >
typename JointHistogramMutualInformationGetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TJointHistogramMetric >::InternalComputationValueType
JointHistogramMutualInformationGetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TJointHistogramMetric >
::ComputeMovingImageMarginalPDFDerivative( const MarginalPDFPointType & margPDFpoint,
                                           const ThreadIdType threadId ) const
{
  InternalComputationValueType offset = 0.5 * this->m_JointAssociate->m_JointPDFSpacing[0];
  InternalComputationValueType eps = this->m_JointAssociate->m_JointPDFSpacing[0];
  MarginalPDFPointType  leftpoint = margPDFpoint;
  leftpoint[0] -= offset;
  MarginalPDFPointType  rightpoint = margPDFpoint;
  rightpoint[0] += offset;
  if( leftpoint[0] < eps )
    {
    leftpoint[0] = eps;
    }
  if( rightpoint[0] < eps )
    {
    rightpoint[0] = eps;
    }
  if( leftpoint[0] > 1.0 )
    {
    leftpoint[0] = 1.0;
    }
  if( rightpoint[0] > 1.0  )
    {
    rightpoint[0] = 1.0;
    }
  InternalComputationValueType delta = rightpoint[0] - leftpoint[0];
  if ( delta > NumericTraits< InternalComputationValueType >::ZeroValue() )
    {
    InternalComputationValueType deriv =
      this->m_JointHistogramMIPerThreadVariables[threadId].MovingImageMarginalPDFInterpolator->Evaluate(rightpoint) -
      this->m_JointHistogramMIPerThreadVariables[threadId].MovingImageMarginalPDFInterpolator->Evaluate(leftpoint);
    return deriv/delta;
    }
  else
    {
    return NumericTraits< InternalComputationValueType >::ZeroValue();
    }
}

template< typename TDomainPartitioner, typename TImageToImageMetric, typename TJointHistogramMetric >
typename JointHistogramMutualInformationGetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TJointHistogramMetric >::InternalComputationValueType
JointHistogramMutualInformationGetValueAndDerivativeThreader< TDomainPartitioner, TImageToImageMetric, TJointHistogramMetric >
::ComputeJointPDFDerivative( const JointPDFPointType & jointPDFpoint,
                             const ThreadIdType threadId,
                             const SizeValueType ind ) const
{
  InternalComputationValueType offset = 0.5 * this->m_JointAssociate->m_JointPDFSpacing[ind];
  InternalComputationValueType eps = this->m_JointAssociate->m_JointPDFSpacing[ind];
  JointPDFPointType  leftpoint = jointPDFpoint;
  leftpoint[ind] -= offset;
  JointPDFPointType  rightpoint = jointPDFpoint;
  rightpoint[ind] += offset;

  if (leftpoint[ind] < eps )
    {
    leftpoint[ind] = eps;
    }

  if (rightpoint[ind] < eps )
    {
    rightpoint[ind] = eps;
    }

  if (leftpoint[ind] > 1.0 )
    {
    leftpoint[ind] = 1.0;
    }

  if (rightpoint[ind] > 1.0 )
    {
    rightpoint[ind] = 1.0;
    }

  InternalComputationValueType delta = rightpoint[ind] - leftpoint[ind];
  InternalComputationValueType deriv = NumericTraits< InternalComputationValueType >::ZeroValue();
  if ( delta > NumericTraits< InternalComputationValueType >::ZeroValue() )
    {
    deriv = this->m_JointHistogramMIPerThreadVariables[threadId].JointPDFInterpolator->Evaluate(rightpoint)-
          this->m_JointHistogramMIPerThreadVariables[threadId].JointPDFInterpolator->Evaluate(leftpoint);
    return deriv/delta;
    }
  return deriv;
}

} // end namespace itk

#endif
