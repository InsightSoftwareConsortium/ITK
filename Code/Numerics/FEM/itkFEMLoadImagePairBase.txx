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


} // end namespace fem
} // end namespace itk

#endif
