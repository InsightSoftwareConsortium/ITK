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
struct AnisoDiffuseCurve2D : public DiffusionStrategy
{
  AnisoDiffuseCurve2D() {}
  AnisoDiffuseCurve2D(float c) : DiffusionStrategy(c) {}
  virtual void operator()(void *, void *);
};

template <class TInnerProduct,  class TIterator>
struct AnisoDiffuseCurveND : public DiffusionStrategy
{
  AnisoDiffuseCurveND() {}
  AnisoDiffuseCurveND(float c) : DiffusionStrategy(c) {}
  virtual void operator()(void *, void *);
};
  
/**
 * \class CurvatureAnisotropicDiffusionImageFilter
 *
 */
template <class TPixel, unsigned int VDimension=2>
class ITK_EXPORT CurvatureAnisotropicDiffusionImageFilter :
    public AnisotropicDiffusionImageFilter< TPixel, VDimension>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef CurvatureAnisotropicDiffusionImageFilter Self;

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
  {
    return new CopyStrategyScalar<ImageType>;
  }
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCurvatureAnisotropicDiffusionImageFilter.txx"
#endif

#endif
