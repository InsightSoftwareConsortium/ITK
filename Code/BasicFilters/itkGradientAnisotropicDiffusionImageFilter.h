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
#include "itkImageTraits.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
#include "itkAnisotropicDiffusionImageFilter.h"

namespace itk
{

template <class TInnerProduct,  class TIterator>
struct ITK_EXPORT AnisoDiffuseGrad2D : public DiffusionStrategy
{
  AnisoDiffuseGrad2D() {}
  AnisoDiffuseGrad2D(float c) : DiffusionStrategy(c) {}
  virtual void operator()(void *, void *);
};

template <class TInnerProduct, class TIterator>
struct ITK_EXPORT AnisoDiffuseGradND : public DiffusionStrategy
{
  AnisoDiffuseGradND() {}
  AnisoDiffuseGradND(float c) : DiffusionStrategy(c) {}
  virtual void operator()(void *, void *);
};
  
/**
 * \class GradientAnisotropicDiffusionImageFilter
 *
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT GradientAnisotropicDiffusionImageFilter :
    public AnisotropicDiffusionImageFilter<TInputImage, TOutputImage>
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef GradientAnisotropicDiffusionImageFilter Self;
  typedef AnisotropicDiffusionImageFilter<TInputImage, TOutputImage> Superclass;

 /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  typedef typename Superclass::OutputPixelType OutputPixelType;
  typedef typename Superclass::OutputInternalPixelType OutputInternalPixelType;
  typedef typename Superclass::InputPixelType InputPixelType;
  typedef typename Superclass::InputInternalPixelType InputInternalPixelType;
  enum { ImageDimension = Superclass::ImageDimension };
  
  /**
   * Image typedef support
   */
  typedef Superclass::InputImageType  InputImageType;
  typedef Superclass::OutputImageType OutputImageType;

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
  { return new UpdateStrategyScalar<OutputImageType, OutputImageType>; }
  
  virtual DiffusionStrategy *GetDiffusionStrategy()
  {
    typedef RegionNonBoundaryNeighborhoodIterator<OutputImageType> RNI;
    typedef RegionBoundaryNeighborhoodIterator<OutputImageType> RBI;
    typedef NeighborhoodAlgorithm::IteratorInnerProduct<RNI,
      NeighborhoodOperator<OutputPixelType, ImageDimension> > SNIP;
    typedef NeighborhoodAlgorithm::BoundsCheckingIteratorInnerProduct<RBI,
      NeighborhoodOperator<OutputPixelType, ImageDimension> > SBIP;
    
    if (ImageDimension == 2)
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
  { return new CopyStrategyScalar<InputImageType, OutputImageType>; }

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientAnisotropicDiffusionImageFilter.txx"
#endif

#endif
