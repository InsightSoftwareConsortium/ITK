/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorAnisotropicDiffusionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkVectorAnisotropicDiffusionImageFilter_h
#define __itkVectorAnisotropicDiffusionImageFilter_h

#include "itkNeighborhoodOperator.h"
#include "itkImage.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkRegionBoundaryNeighborhoodIterator.h"
#include "itkRegionNonBoundaryNeighborhoodIterator.h"
#include "itkAnisotropicDiffusionImageFilter.h"
#include "itkVector.h"

namespace itk
{

template<class TImage>
struct ITK_EXPORT UpdateStrategyVector: public UpdateStrategy
{
  UpdateStrategyVector() {}
  virtual void operator() (void *, void *) const;
};
    
template<class TImage>
struct ITK_EXPORT AvgGradMagSquaredVector
{
  typedef typename TImage::PixelType PixelType;
  typedef typename TImage::VectorValueType VectorValueType;
  enum { ImageDimension  = TImage::ImageDimension };
  enum { VectorDimension = PixelType::VectorDimension };
  
  AvgGradMagSquaredVector() {}
  float operator() (TImage *, const ImageRegion<ImageDimension> &) const;
};

template<class TImage>
struct ITK_EXPORT CopyStrategyVector: public CopyStrategy
{
  CopyStrategyVector() {};
  virtual void operator()(void *, void *) const;
};

template <class TInnerProduct, class TIterator>
struct ITK_EXPORT AnisoDiffuseVector2D : public DiffusionStrategy
{
  typedef typename TIterator::ImageType ImageType;
  typedef typename ImageType::ScalarValueType ScalarValueType;
  typedef typename ImageType::VectorValueType VectorValueType;
  typedef typename ImageType::PixelType PixelType;
  enum { ImageDimension = ImageType::ImageDimension };
  enum { VectorDimension = PixelType::VectorDimension };
  
  AnisoDiffuseVector2D() {}
  AnisoDiffuseVector2D(float c) : DiffusionStrategy(c) {}
  virtual void operator()(void *, void *);
};

template <class TInnerProduct, class TIterator>
struct ITK_EXPORT AnisoDiffuseVectorND : public DiffusionStrategy
{
  typedef typename TIterator::ImageType ImageType;
  typedef typename ImageType::ScalarValueType ScalarValueType;
  typedef typename ImageType::VectorValueType VectorValueType;
  typedef typename ImageType::PixelType PixelType;
  enum { ImageDimension = ImageType::ImageDimension };
  enum { VectorDimension = PixelType::VectorDimension };
  
  AnisoDiffuseVectorND() {}
  AnisoDiffuseVectorND(float c) : DiffusionStrategy(c) {}
  virtual void operator()(void *, void *);
};
  
/**
 * \class VectorAnisotropicDiffusionImageFilter
 *
 */
template <class TPixel, unsigned int VDimension=2>
class ITK_EXPORT VectorAnisotropicDiffusionImageFilter :
    public AnisotropicDiffusionImageFilter< TPixel, VDimension>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef VectorAnisotropicDiffusionImageFilter Self;

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
  itkTypeMacro(VectorAnisotropicDiffusionImageFilter,
               AnisotropicDiffusionImageFilter);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

protected:
  VectorAnisotropicDiffusionImageFilter() {}
  virtual ~VectorAnisotropicDiffusionImageFilter() {}
  VectorAnisotropicDiffusionImageFilter(const Self&) {}
  void operator=(const Self&) {}

  virtual UpdateStrategy *GetUpdateStrategy()
  {
    return new UpdateStrategyVector<ImageType>;
  }

  virtual DiffusionStrategy *GetDiffusionStrategy()
  {
    typedef RegionNonBoundaryNeighborhoodIterator<TPixel, VDimension> RNI;
    typedef RegionBoundaryNeighborhoodIterator<TPixel, VDimension> RBI;
    typedef NeighborhoodAlgorithm
      ::VectorComponentIteratorInnerProduct<RNI> SNIP;
    typedef NeighborhoodAlgorithm
      ::BoundsCheckingVectorComponentIteratorInnerProduct<RBI> SBIP;

    if (VDimension == 2)
      {
        return new CompositeDiffusionStrategy(
                                          new AnisoDiffuseVector2D<SNIP,RNI>(),
                                          new AnisoDiffuseVector2D<SBIP,RBI>(),
                                          this->GetConductanceParameter());
      }
    else
      {
        return new CompositeDiffusionStrategy(
                                          new AnisoDiffuseVectorND<SNIP,RNI>(),
                                          new AnisoDiffuseVectorND<SBIP,RBI>(),
                                          this->GetConductanceParameter());
      }                                
  }

  virtual CopyStrategy *GetCopyStrategy()
  {
    return new CopyStrategyVector<ImageType>;
  }
    
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorAnisotropicDiffusionImageFilter.txx"
#endif

#endif
