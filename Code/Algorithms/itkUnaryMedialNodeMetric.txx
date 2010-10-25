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
#ifndef __itkUnaryMedialNodeMetric_txx
#define __itkUnaryMedialNodeMetric_txx

#include "itkUnaryMedialNodeMetric.h"

namespace itk
{
/**
 * Constructor.
 */
template< int VDimensions >
UnaryMedialNodeMetric< VDimensions >
::UnaryMedialNodeMetric()
{
  // Initialize the metric value to 0.
  m_MetricResult = 0;
}

/**
 * Function that actually calculates the metric value of the inputs.
 * Should be called after the inputs are set using the SetMedialNodes
 * function.
 */
template< int VDimensions >
void
UnaryMedialNodeMetric< VDimensions >
::Initialize(void)
{
  // Calculate the metric.

  // Eigenvalues from image A and image B.
  EigenvalueType eigenvalueA;
  EigenvalueType eigenvalueB;

  eigenvalueA = m_MedialNodeA->GetVotedEigenvalues();
  eigenvalueB = m_MedialNodeB->GetVotedEigenvalues();

  double difference = 0;

  // Iterate through N-1 dimensions and compute the sum of
  // the difference in eigenvalue squared. N-1
  // eigenvalues are independent.
  for ( int i = 0; i < VDimensions - 1; ++i )
    {
    double differenceTemp = ( eigenvalueA(i) - eigenvalueB(i) );
    difference += vcl_pow(differenceTemp, 2);
    }

  // Eigenvalue term for the metric.
  double eigenResult = 1 - ( 2 * difference );

  // Compute the scale term for the metric.
  double scaleA = m_MedialNodeA->GetMeanCoreAtomDiameter();
  double scaleB = m_MedialNodeB->GetMeanCoreAtomDiameter();

  double scaleResult = 1 - vcl_fabs( ( scaleA - scaleB ) / ( scaleA + scaleB ) );

  // The final metric calculation.
  m_MetricResult = eigenResult * scaleResult;
}

/**
 * Function to set the current medial nodes to be analyzed.
 */
template< int VDimensions >
void
UnaryMedialNodeMetric< VDimensions >
::SetMedialNodes(MedialNodeType *medialNodeA, MedialNodeType *medialNodeB)
{
  m_MedialNodeA = medialNodeA;
  m_MedialNodeB = medialNodeB;
}

/**
 * Print Self
 */
template< int VDimensions >
void
UnaryMedialNodeMetric< VDimensions >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
