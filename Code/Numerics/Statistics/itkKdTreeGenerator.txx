/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKdTreeGenerator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkKdTreeGenerator_txx
#define __itkKdTreeGenerator_txx

namespace itk{ 
namespace Statistics{

template< class TSample >
KdTreeGenerator< TSample >
::KdTreeGenerator()
{
  m_Subsample = SubsampleType::New() ;
}

template< class TSample >
void
KdTreeGenerator< TSample >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Source Sample: " << m_SourceSample << std::endl ;
  os << indent << "Bucket Size: " << m_BucketSize << std::endl ;
}

template< class TSample >
void
KdTreeGenerator< TSample >
::SetSample(SourceSamplePointer sample)
{
  m_SourceSample = sample ;
  m_Subsample->SetSample(sample) ;
  m_Subsample->InitializeWithAllInstances() ;
}


template< class TSample >
void
KdTreeGenerator< TSample >
::SetBucketSize(int size)
{
  m_BucketSize = size ;
}

template< class TSample >
void
KdTreeGenerator< TSample >
::GenerateData()
{
  if (m_SourceSample == 0)
    {
      return ;
    }
  
  if (m_Tree == 0)
    {
      m_Tree = KdTreeType::New() ;
      m_Tree->SetSample(m_SourceSample) ;
      m_Tree->SetBucketSize(m_BucketSize) ;
    }

  MeasurementVectorType lowerBound ;
  MeasurementVectorType upperBound ;

  for(unsigned int d = 0 ; d < MeasurementVectorSize ; d++)
    {
      lowerBound[d] = NumericTraits< MeasurementType >::min() ;
      upperBound[d] = NumericTraits< MeasurementType >::max() ;
    }

  KdTreeNodeType* root = this->GenerateTreeLoop(0, m_Subsample->Size(), lowerBound, upperBound) ;
  m_Tree->SetRoot(root) ;
}

template< class TSample >
inline void 
KdTreeGenerator< TSample >
::DumpVector(MeasurementVectorType &vec)
{
  for (unsigned int i = 0 ; i < MeasurementVectorSize ; i++)
    {
      std::cout << vec[i] << "\t" ;
    }
  std::cout << std::endl ;
}

template< class TSample >
inline KdTreeGenerator< TSample >::KdTreeNodeType*
KdTreeGenerator< TSample >
::GenerateNonterminalNode(int beginIndex,
                          int endIndex,
                          MeasurementVectorType &lowerBound,
                          MeasurementVectorType &upperBound)
{
  typedef typename KdTreeType::KdTreeNodeType NodeType ;
  MeasurementType dimensionLowerBound ;
  MeasurementType dimensionUpperBound ;
  MeasurementType partitionValue ;
  unsigned int partitionDimension ;
  NodeType* left ;
  NodeType* right ;
  unsigned int i ;
  MeasurementType spread ;
  MeasurementType maxSpread ;
  int medianIndex ;

  // find most widely spread dimension
  FindSampleBoundAndMean< SubsampleType >(this->GetSubsample(), 
                                          beginIndex, endIndex, 
                                          m_TempLowerBound, m_TempUpperBound,
                                          m_TempMean) ;

  maxSpread = NumericTraits< MeasurementType >::min() ;
  for (i = 0 ; i < MeasurementVectorSize ; i++)
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
    QuickSelect< SubsampleType >(m_Subsample, 
                                 partitionDimension, 
                                 beginIndex, endIndex, medianIndex, 
                                 m_TempMean[partitionDimension]) ;
  medianIndex += beginIndex - 1 ;

  // save bounds for cutting dimension
  dimensionLowerBound = lowerBound[partitionDimension] ;
  dimensionUpperBound = upperBound[partitionDimension] ;

  upperBound[partitionDimension] = partitionValue ;
  left = GenerateTreeLoop(beginIndex, medianIndex, lowerBound, upperBound);
  upperBound[partitionDimension] = dimensionUpperBound ;

  lowerBound[partitionDimension] = partitionValue ;
  right = GenerateTreeLoop(medianIndex, endIndex, lowerBound, upperBound ) ;
  lowerBound[partitionDimension] = dimensionLowerBound ;

  return new KdTreeNonterminalNode< SourceSampleType >(partitionDimension, 
                                                       partitionValue,
                                                       left,
                                                       right) ;
}

template< class TSample >
inline KdTreeGenerator< TSample >::KdTreeNodeType*
KdTreeGenerator< TSample >
::GenerateTreeLoop(int beginIndex,
                   int endIndex,
                   MeasurementVectorType &lowerBound,
                   MeasurementVectorType &upperBound) 
{
  if (endIndex - beginIndex <= m_BucketSize) 
    {
      // numberOfInstances small, make a terminal node
      if (endIndex == beginIndex)
        {
          // return the pointer to empty terminal node
          return m_Tree->GetEmptyTerminalNode() ;
        }
      else
        {
          KdTreeTerminalNode< SourceSampleType >* ptr = 
            new KdTreeTerminalNode< SourceSampleType >(); 

          for (int j = beginIndex ; j < endIndex ; j++)
            {

              ptr->AddInstanceIdentifier(m_Subsample->GetInstanceIdentifier(j)) ;
            }
          // return a terminal node
          return ptr ; 
        }
    }
  else 
    {
      return this->GenerateNonterminalNode(beginIndex, endIndex, 
                                           lowerBound, upperBound) ;
    }
}

} // end of namespace Statistics 
} // end of namespace itk

#endif















