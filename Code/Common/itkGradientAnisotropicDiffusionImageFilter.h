/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientAnisotropicDiffusionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkGradientAnisotropicDiffusionImageFilter_h
#define __itkGradientAnisotropicDiffusionImageFilter_h

#include "itkNeighborhoodOperator.h"
#include "itkImage.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
#include "itkAnisotropicDiffusionImageFilter.h"

namespace itk
{

template <class TInnerProduct,  class TIterator>
struct AnisoDiffuseGrad2D : public DiffusionStrategy
{
  AnisoDiffuseGrad2D() {}
  AnisoDiffuseGrad2D(float c) : DiffusionStrategy(c) {}
  virtual void operator()(void *, void *);
};

template <class TInnerProduct, class TIterator>
struct AnisoDiffuseGradND : public DiffusionStrategy
{
  AnisoDiffuseGradND() {}
  AnisoDiffuseGradND(float c) : DiffusionStrategy(c) {}
  virtual void operator()(void *, void *);
};
  
/**
 * \class GradientAnisotropicDiffusionImageFilter
 *
 */
template <class TPixel, unsigned int VDimension=2>
class ITK_EXPORT GradientAnisotropicDiffusionImageFilter :
    public AnisotropicDiffusionImageFilter< TPixel, VDimension>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef GradientAnisotropicDiffusionImageFilter Self;

  /**
   * Standard Superclass typedef support.
   */
  typedef AnisotropicDiffusionImageFilter<TPixel, VDimension> Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(GradientAnisotropicDiffusionImageFilter,
               AnisotropicDiffusionImageFilter);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

protected:
  GradientAnisotropicDiffusionImageFilter() {}
  virtual ~GradientAnisotropicDiffusionImageFilter() {}
  GradientAnisotropicDiffusionImageFilter(const Self&) {}
  void operator=(const Self&) {}

  virtual UpdateStrategy *GetUpdateStrategy()
  {
    return new UpdateStrategyScalar<ImageType>;
  }
  virtual DiffusionStrategy *GetDiffusionStrategy()
  {
    typedef RegionNonBoundaryNeighborhoodIterator<TPixel, VDimension> RNI;
    typedef RegionBoundaryNeighborhoodIterator<TPixel, VDimension> RBI;
    typedef NeighborhoodAlgorithm::IteratorInnerProduct<RNI> SNIP;
    typedef NeighborhoodAlgorithm::BoundsCheckingIteratorInnerProduct<RBI> SBIP;
    
    if (VDimension == 2)
      {
        return new CompositeDiffusionStrategy(
                                           new AnisoDiffuseGrad2D<SNIP, RNI>(),
                                           new AnisoDiffuseGrad2D<SBIP, RBI>(),
                                           this->GetConductanceParameter());
      }
    else
      {
        return new CompositeDiffusionStrategy(
                                           new AnisoDiffuseGradND<SNIP, RNI>(),
                                           new AnisoDiffuseGradND<SBIP, RBI>(),
                                           this->GetConductanceParameter());
      }
                                          
  }
  virtual CopyStrategy *GetCopyStrategy()
  {
    return new CopyStrategyScalar<ImageType>;
  }

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientAnisotropicDiffusionImageFilter.txx"
#endif

#endif
