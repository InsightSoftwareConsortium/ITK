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
#ifndef __itkJointHistogramMutualInformationComputeJointPDFThreaderBase_hxx
#define __itkJointHistogramMutualInformationComputeJointPDFThreaderBase_hxx

#include "itkJointHistogramMutualInformationComputeJointPDFThreaderBase.h"

#include "itkImageRegionIterator.h"

namespace itk
{

template< class TDomainPartitioner, class TJointHistogramMetric >
JointHistogramMutualInformationComputeJointPDFThreaderBase< TDomainPartitioner, TJointHistogramMetric >
::JointHistogramMutualInformationComputeJointPDFThreaderBase()
{
}

template< class TDomainPartitioner, class TJointHistogramMetric >
void
JointHistogramMutualInformationComputeJointPDFThreaderBase< TDomainPartitioner, TJointHistogramMetric >
::BeforeThreadedExecution()
{
  this->m_JointHistogramPerThread.resize( this->GetNumberOfThreadsUsed() );
  this->m_JointHistogramCountPerThread.resize( this->GetNumberOfThreadsUsed() );
  for( ThreadIdType i = 0; i < this->GetNumberOfThreadsUsed(); ++i )
    {
    if( this->m_JointHistogramPerThread[i].IsNull() )
      {
      this->m_JointHistogramPerThread[i] = JointHistogramType::New();
      }
    this->m_JointHistogramPerThread[i]->CopyInformation( this->m_Associate->m_JointPDF );
    this->m_JointHistogramPerThread[i]->SetRegions( this->m_Associate->m_JointPDF->GetLargestPossibleRegion() );
    this->m_JointHistogramPerThread[i]->Allocate();
    this->m_JointHistogramPerThread[i]->FillBuffer( NumericTraits< SizeValueType >::Zero );
    this->m_JointHistogramCountPerThread[i] = NumericTraits< SizeValueType >::Zero;
    }
}

template< class TDomainPartitioner, class TJointHistogramMetric >
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
    this->m_JointHistogramPerThread[threadId]->TransformPhysicalPointToIndex( jointPDFpoint, jointPDFIndex );
    if( this->m_JointHistogramPerThread[threadId]->GetBufferedRegion().IsInside( jointPDFIndex ) )
      {
      typename JointHistogramType::PixelType jointHistogramPixel;
      jointHistogramPixel = this->m_JointHistogramPerThread[threadId]->GetPixel( jointPDFIndex );
      jointHistogramPixel++;
      this->m_JointHistogramPerThread[threadId]->SetPixel( jointPDFIndex, jointHistogramPixel );
      this->m_JointHistogramCountPerThread[threadId]++;
      }
    }
}

template< class TDomainPartitioner, class TJointHistogramMetric >
void
JointHistogramMutualInformationComputeJointPDFThreaderBase< TDomainPartitioner, TJointHistogramMetric >
::AfterThreadedExecution()
{
  const ThreadIdType numberOfThreadsUsed = this->GetNumberOfThreadsUsed();

  typedef typename JointHistogramType::PixelType JointHistogramPixelType;
  this->m_Associate->m_JointHistogramTotalCount = NumericTraits<SizeValueType>::Zero;
  for( ThreadIdType i = 0; i < numberOfThreadsUsed; ++i )
    {
    this->m_Associate->m_JointHistogramTotalCount += this->m_JointHistogramCountPerThread[i];
    }

  if( this->m_Associate->m_JointHistogramTotalCount == 0 )
    {
    this->m_Associate->m_JointPDF->FillBuffer( NumericTraits< SizeValueType >::Zero );
    return;
    }

  typedef ImageRegionIterator< JointPDFType > JointPDFIteratorType;
  JointPDFIteratorType jointPDFIt( this->m_Associate->m_JointPDF, this->m_Associate->m_JointPDF->GetBufferedRegion() );
  jointPDFIt.GoToBegin();
  typedef ImageRegionConstIterator< JointHistogramType > JointHistogramIteratorType;
  std::vector< JointHistogramIteratorType > jointHistogramPerThreadIts;
  for( ThreadIdType i = 0; i < numberOfThreadsUsed; ++i )
    {
    jointHistogramPerThreadIts.push_back( JointHistogramIteratorType( this->m_JointHistogramPerThread[i],
        this->m_JointHistogramPerThread[i]->GetBufferedRegion() ) );
    jointHistogramPerThreadIts[i].GoToBegin();
    }

  JointHistogramPixelType jointHistogramPixel;
  while( !jointPDFIt.IsAtEnd() )
    {
    jointHistogramPixel = NumericTraits< JointHistogramPixelType >::Zero;
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
