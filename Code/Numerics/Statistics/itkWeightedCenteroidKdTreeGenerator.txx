/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWeightedCenteroidKdTreeGenerator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWeightedCenteroidKdTreeGenerator_txx
#define __itkWeightedCenteroidKdTreeGenerator_txx

namespace itk{ 
  namespace Statistics{

template< class TSample >
WeightedCenteroidKdTreeGenerator< TSample >
::WeightedCenteroidKdTreeGenerator()
{
}

template< class TSample >
void
WeightedCenteroidKdTreeGenerator< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

template< class TSample >
inline KdTreeGenerator< TSample >::KdTreeNodeType*
WeightedCenteroidKdTreeGenerator< TSample >
::GenerateNonterminalNode(int beginIndex,
                          int endIndex,
                          MeasurementVectorType &lowerBound,
                          MeasurementVectorType &upperBound)
{
  MeasurementType dimensionLowerBound ;
  MeasurementType dimensionUpperBound ;
  MeasurementType partitionValue ;
  unsigned int partitionDimension = 0 ;
  KdTreeNodeType* left ;
  KdTreeNodeType* right ;
  int i, j ;
  MeasurementType spread ;
  MeasurementType maxSpread ;
  int medianIndex ;
  SubsamplePointer subsample = this->GetSubsample() ;

  // calculates the weighted centeroid which is the vector sum
  // of all the associated instances.
  typename KdTreeNodeType::CenteroidType weightedCenteroid ;
  MeasurementVectorType tempVector ;
  weightedCenteroid.Fill(NumericTraits< MeasurementType >::Zero) ;
  for (i = beginIndex ; i < endIndex ; i++)
    {
      tempVector = subsample->GetMeasurementVectorByIndex(i) ;
      for(j = 0 ; j < (int)MeasurementVectorSize ; j++)
        {
          weightedCenteroid[j] += tempVector[j] ;
        }
    }

  // find most widely spread dimension
  FindSampleBoundAndMean< SubsampleType >(this->GetSubsample(), 
                                          beginIndex, endIndex, 
                                          m_TempLowerBound, m_TempUpperBound,
                                          m_TempMean) ;

  maxSpread = NumericTraits< MeasurementType >::min() ;
  for (i = 0 ; i < (int)MeasurementVectorSize ; i++)
    {
      spread = m_TempUpperBound[i] - m_TempLowerBound[i] ;
      if (spread >= maxSpread)
        {
          maxSpread = spread ;
          partitionDimension = i ;
        }
    }

  // find median and partition this node using the quick select algorithm
  medianIndex = (endIndex - beginIndex) / 2 ;
  partitionValue = 
    QuickSelect< SubsampleType >(this->GetSubsample(), 
                                 partitionDimension, 
                                 beginIndex, endIndex, medianIndex, 
                                 m_TempMean[partitionDimension]) ;
  medianIndex += beginIndex - 1 ;

  dimensionLowerBound = lowerBound[partitionDimension] ;
  dimensionUpperBound = upperBound[partitionDimension] ;

  upperBound[partitionDimension] = partitionValue ;
  left = GenerateTreeLoop(beginIndex, medianIndex, lowerBound, upperBound);
  upperBound[partitionDimension] = dimensionUpperBound ;

  lowerBound[partitionDimension] = partitionValue ;
  right = GenerateTreeLoop(medianIndex, endIndex, lowerBound, upperBound ) ;
  lowerBound[partitionDimension] = dimensionLowerBound ;

  return new KdTreeWeightedCenteroidNonterminalNode< SourceSampleType >
    (partitionDimension, partitionValue,
     left, right, weightedCenteroid, endIndex - beginIndex) ;
}

} // end of namespace Statistics 
} // end of namespace itk

#endif















