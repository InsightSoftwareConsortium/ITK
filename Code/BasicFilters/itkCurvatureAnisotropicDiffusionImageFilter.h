/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvatureAnisotropicDiffusionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkCurvatureAnisotropicDiffusionImageFilter_h
#define __itkCurvatureAnisotropicDiffusionImageFilter_h

#include "itkNeighborhoodOperator.h"
#include "itkImage.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
#include "itkRegionNonBoundaryNeighborhoodIterator.h"
#include "itkAnisotropicDiffusionImageFilter.h"

namespace itk
{

template <class TInnerProduct,  class TIterator>
struct ITK_EXPORT AnisoDiffuseCurve2D : public DiffusionStrategy
{
  AnisoDiffuseCurve2D() {}
  AnisoDiffuseCurve2D(float c) : DiffusionStrategy(c) {}
  virtual void operator()(void *, void *);
};

template <class TInnerProduct,  class TIterator>
struct ITK_EXPORT AnisoDiffuseCurveND : public DiffusionStrategy
{
  AnisoDiffuseCurveND() {}
  AnisoDiffuseCurveND(float c) : DiffusionStrategy(c) {}
  virtual void operator()(void *, void *);
};
  
/**
 * \class CurvatureAnisotropicDiffusionImageFilter
 *
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT CurvatureAnisotropicDiffusionImageFilter :
    public AnisotropicDiffusionImageFilter< TInputImage, TOutputImage >
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef CurvatureAnisotropicDiffusionImageFilter Self;
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
  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(CurvatureAnisotropicDiffusionImageFilter,
               AnisotropicDiffusionImageFilter);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

protected:
  CurvatureAnisotropicDiffusionImageFilter() {}
  virtual ~CurvatureAnisotropicDiffusionImageFilter() {}
  CurvatureAnisotropicDiffusionImageFilter(const Self&) {}
  void operator=(const Self&) {}

  virtual UpdateStrategy *GetUpdateStrategy()
  { return new UpdateStrategyScalar<OutputImageType, OutputImageType>;}
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
                                    new AnisoDiffuseCurve2D<SNIP, RNI>(),
                                    new AnisoDiffuseCurve2D<SBIP, RBI>(),
                                    this->GetConductanceParameter());
      }
    else
      {
        return new CompositeDiffusionStrategy(
                                    new AnisoDiffuseCurveND<SNIP, RNI>(),
                                    new AnisoDiffuseCurveND<SBIP, RBI>(),
                                    this->GetConductanceParameter());
      }
  }

  virtual CopyStrategy *GetCopyStrategy()
  { return new CopyStrategyScalar<InputImageType, OutputImageType>;  }
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCurvatureAnisotropicDiffusionImageFilter.txx"
#endif

#endif
