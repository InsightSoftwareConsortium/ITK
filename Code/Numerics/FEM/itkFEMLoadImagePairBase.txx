/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkFEMLoadImagePairBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

=========================================================================*/
#ifndef _itkFEMLoadImagePairBase_txx_
#define _itkFEMLoadImagePairBase_txx_

#include "itkFEMLoadImagePairBase.h"
#ifndef Float
#define Float double
#endif
namespace itk {
namespace fem {

/**
 * Compute the image based gravity load - implemented in the derived class.
 */
template<class TReference,class TTarget>
vnl_vector<Float> LoadImagePairBase<TReference , TTarget>::Fg(vnl_vector<Float> InVec) {
  
  vnl_vector<Float> OutVec(ImageDimension,0.0);
 
  return OutVec;

};

template<class TReference,class TTarget>
LoadImagePairBase<TReference , TTarget>::LoadImagePairBase()
{
  m_transform = TransformType::New();
  for (int i=0; i<ImageDimension; i++)
  {
    m_RefRadius[i] = 1;
    m_TarRadius[i] = 1;
  }

};

template<class TReference,class TTarget>
void LoadImagePairBase<TReference , TTarget>::SetReferenceImage(ReferenceType* R ) 
{ 
  m_RefImage = R; 
   // GET DATA SIZE  BUG!! FIXME!! MUST BE BETTER WAY TO GET SIZE
  typedef typename ImageRegionIteratorWithIndex<TReference>  IterType;
  IterType Iter (m_RefImage,m_RefImage->GetLargestPossibleRegion() );
  Iter.GoToEnd();
  typename ReferenceType::IndexType Ind = Iter.GetIndex();    
  for (int i=0; i< ImageDimension; i++) m_RefSize[i]=Ind[i]+1;
};

template<class TReference,class TTarget>
void LoadImagePairBase<TReference , TTarget>::SetTargetImage(TargetType* T ) 
{    // GET DATA SIZE  BUG!! FIXME!! MUST BE BETTER WAY TO GET SIZE
  m_TarImage=T; 
  typedef typename ImageRegionIteratorWithIndex<TTarget>  IterType;
  IterType Iter (m_RefImage,m_RefImage->GetLargestPossibleRegion() );
  Iter.GoToEnd();
  typename ReferenceType::IndexType Ind = Iter.GetIndex();   
  for (int i=0; i< ImageDimension; i++) m_RefSize[i]=Ind[i]+1; 
};



} // end namespace fem
} // end namespace itk

#endif
