/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUnaryMedialNodeMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkUnaryMedialNodeMetric_txx
#define _itkUnaryMedialNodeMetric_txx

#include "itkUnaryMedialNodeMetric.h"

namespace itk
{

  /**
   * Constructor.
   */
template <int VDimensions>
UnaryMedialNodeMetric<VDimensions>
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
template <int VDimensions>
void
UnaryMedialNodeMetric<VDimensions>
::Initialize(void)
{
  // Calculate the metric. 

  // Eigenvalues from image A and image B.
  EigenvalueType eigenvalueA;
  EigenvalueType eigenvalueB;

  eigenvalueA = m_MedialNodeA->GetVotedEigenvalues();
  eigenvalueB = m_MedialNodeB->GetVotedEigenvalues();

  double differenceTemp = 0;
  double difference = 0;

  double eigenResult = 0;
  double scaleResult = 0;

  // Iterate through N-1 dimensions and compute the sum of 
  // the difference in eigenvalue squared. N-1 
  // eigenvalues are independent.
  for(int i = 0; i < VDimensions - 1; ++i)
    {
    differenceTemp = (eigenvalueA(i) - eigenvalueB(i));
    difference += pow(differenceTemp,2);
    }

  // Eigenvalue term for the metric.
  eigenResult = 1-(2*difference);

  // Compute the scale term for the metric.
  double scaleA = m_MedialNodeA->GetMeanCoreAtomDiameter();
  double scaleB = m_MedialNodeB->GetMeanCoreAtomDiameter();

  scaleResult = 1 - abs( (scaleA - scaleB)/(scaleA + scaleB) );

  // The final metric calculation.
  m_MetricResult = eigenResult * scaleResult;
}
 
/**
 * Function to set the current medial nodes to be analyzed.
 */
template <int VDimensions>
void
UnaryMedialNodeMetric<VDimensions>
::SetMedialNodes(MedialNodeType * medialNodeA, MedialNodeType * medialNodeB)
{
  m_MedialNodeA = medialNodeA;
  m_MedialNodeB = medialNodeB;
} 

/**
 * Print Self
 */
template <int VDimensions>
void
UnaryMedialNodeMetric<VDimensions>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk

#endif
