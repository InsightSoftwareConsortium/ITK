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
#ifndef itkJointHistogramMutualInformationComputeJointPDFThreaderBase_hxx
#define itkJointHistogramMutualInformationComputeJointPDFThreaderBase_hxx

#include "itkJointHistogramMutualInformationComputeJointPDFThreaderBase.h"

#include "itkImageRegionIterator.h"

namespace itk
{

template< typename TDomainPartitioner, typename TJointHistogramMetric >
JointHistogramMutualInformationComputeJointPDFThreaderBase< TDomainPartitioner, TJointHistogramMetric >
::JointHistogramMutualInformationComputeJointPDFThreaderBase():
  m_JointHistogramMIPerThreadVariables( ITK_NULLPTR )
{
}

template< typename TDomainPartitioner, typename TJointHistogramMetric >
JointHistogramMutualInformationComputeJointPDFThreaderBase< TDomainPartitioner, TJointHistogramMetric >
::~JointHistogramMutualInformationComputeJointPDFThreaderBase()
{
  delete[] this->m_JointHistogramMIPerThreadVariables;
}

template< typename TDomainPartitioner, typename TJointHistogramMetric >
void
JointHistogramMutualInformationComputeJointPDFThreaderBase< TDomainPartitioner, TJointHistogramMetric >
::BeforeThreadedExecution()
{
  const ThreadIdType numThreadsUsed = this->GetNumberOfThreadsUsed();
  delete[] this->m_JointHistogramMIPerThreadVariables;
  this->m_JointHistogramMIPerThreadVariables = new AlignedJointHistogramMIPerThreadStruct[ numThreadsUsed ];
  for( ThreadIdType i = 0; i < numThreadsUsed; ++i )
    {
    if( this->m_JointHistogramMIPerThreadVariables[i].JointHistogram.IsNull() )
      {
      this->m_JointHistogramMIPerThreadVariables[i].JointHistogram = JointHistogramType::New();
      }
    this->m_JointHistogramMIPerThreadVariables[i].JointHistogram->CopyInformation( this->m_Associate->m_JointPDF );
    this->m_JointHistogramMIPerThreadVariables[i].JointHistogram->SetRegions( this->m_Associate->m_JointPDF->GetLargestPossibleRegion() );
    this->m_JointHistogramMIPerThreadVariables[i].JointHistogram->Allocate();
    this->m_JointHistogramMIPerThreadVariables[i].JointHistogram->FillBuffer( NumericTraits< SizeValueType >::ZeroValue() );
    this->m_JointHistogramMIPerThreadVariables[i].JointHistogramCount = NumericTraits< SizeValueType >::ZeroValue();
    }
}

template< typename TDomainPartitioner, typename TJointHistogramMetric >
void
JointHistogramMutualInformationComputeJointPDFThreaderBase< TDomainPartitioner, TJointHistogramMetric >
::ProcessPoint( const VirtualIndexType & itkNotUsed(virtualIndex),
                const VirtualPointType & virtualPoint,
                const ThreadIdType threadId )
{
  typename AssociateType::Superclass::FixedImagePointType     mappedFixedPoint;
  typename AssociateType::Superclass::FixedImagePixelType     fixedImageValue;
  typename AssociateType::Superclass::FixedImageGradientType  fixedImageGradients;
  typename AssociateType::Superclass::MovingImagePointType    mappedMovingPoint;
  typename AssociateType::Superclass::MovingImagePixelType    movingImageValue;
  typename AssociateType::Superclass::MovingImageGradientType movingImageGradients;
  bool                                                        pointIsValid = false;

  try
    {
    pointIsValid = this->m_Associate->TransformAndEvaluateFixedPoint( virtualPoint, mappedFixedPoint, fixedImageValue );
    if( pointIsValid )
      {
      pointIsValid = this->m_Associate->TransformAndEvaluateMovingPoint( virtualPoint, mappedMovingPoint, movingImageValue );
      }
    }
  catch( ExceptionObject & exc )
    {
    //NOTE: there must be a cleaner way to do this:
    std::string msg("Caught exception: \n");
    msg += exc.what();
    ExceptionObject err(__FILE__, __LINE__, msg);
    throw err;
    }

  /** Add the paired intensity points to the joint histogram */
  if( pointIsValid )
    {
    JointPDFPointType jointPDFpoint;
    this->m_Associate->ComputeJointPDFPoint( fixedImageValue, movingImageValue, jointPDFpoint );
    JointPDFIndexType jointPDFIndex;
    this->m_JointHistogramMIPerThreadVariables[threadId].JointHistogram->TransformPhysicalPointToIndex( jointPDFpoint, jointPDFIndex );
    if( this->m_JointHistogramMIPerThreadVariables[threadId].JointHistogram->GetBufferedRegion().IsInside( jointPDFIndex ) )
      {
      typename JointHistogramType::PixelType jointHistogramPixel;
      jointHistogramPixel = this->m_JointHistogramMIPerThreadVariables[threadId].JointHistogram->GetPixel( jointPDFIndex );
      jointHistogramPixel++;
      this->m_JointHistogramMIPerThreadVariables[threadId].JointHistogram->SetPixel( jointPDFIndex, jointHistogramPixel );
      this->m_JointHistogramMIPerThreadVariables[threadId].JointHistogramCount++;
      }
    }
}

template< typename TDomainPartitioner, typename TJointHistogramMetric >
void
JointHistogramMutualInformationComputeJointPDFThreaderBase< TDomainPartitioner, TJointHistogramMetric >
::AfterThreadedExecution()
{
  const ThreadIdType numberOfThreadsUsed = this->GetNumberOfThreadsUsed();

  typedef typename JointHistogramType::PixelType JointHistogramPixelType;
  this->m_Associate->m_JointHistogramTotalCount = NumericTraits<SizeValueType>::ZeroValue();
  for( ThreadIdType i = 0; i < numberOfThreadsUsed; ++i )
    {
    this->m_Associate->m_JointHistogramTotalCount += this->m_JointHistogramMIPerThreadVariables[i].JointHistogramCount;
    }

  if( this->m_Associate->m_JointHistogramTotalCount == 0 )
    {
    this->m_Associate->m_JointPDF->FillBuffer( NumericTraits< SizeValueType >::ZeroValue() );
    return;
    }

  typedef ImageRegionIterator< JointPDFType > JointPDFIteratorType;
  JointPDFIteratorType jointPDFIt( this->m_Associate->m_JointPDF, this->m_Associate->m_JointPDF->GetBufferedRegion() );
  jointPDFIt.GoToBegin();
  typedef ImageRegionConstIterator< JointHistogramType > JointHistogramIteratorType;
  std::vector< JointHistogramIteratorType > jointHistogramPerThreadIts;
  for( ThreadIdType i = 0; i < numberOfThreadsUsed; ++i )
    {
    jointHistogramPerThreadIts.push_back( JointHistogramIteratorType( this->m_JointHistogramMIPerThreadVariables[i].JointHistogram,
        this->m_JointHistogramMIPerThreadVariables[i].JointHistogram->GetBufferedRegion() ) );
    jointHistogramPerThreadIts[i].GoToBegin();
    }

  JointHistogramPixelType jointHistogramPixel;
  while( !jointPDFIt.IsAtEnd() )
    {
    jointHistogramPixel = NumericTraits< JointHistogramPixelType >::ZeroValue();
    for( ThreadIdType i = 0; i < numberOfThreadsUsed; ++i )
      {
      jointHistogramPixel += jointHistogramPerThreadIts[i].Get();
      ++jointHistogramPerThreadIts[i];
      }
    jointPDFIt.Set( static_cast< JointPDFValueType >( jointHistogramPixel ) / static_cast< JointPDFValueType >( this->m_Associate->m_JointHistogramTotalCount ) );
    ++jointPDFIt;
    }
}

} // end namespace itk

#endif
