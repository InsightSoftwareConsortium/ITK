/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnisotropicDiffusionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkAnisotropicDiffusionImageFilter_h
#define __itkAnisotropicDiffusionImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImageTraits.h"

namespace itk
{
/**
 *  \class AvgGradMagSquared
 * Function object that returns the average gradient magnitude squared
 * of scalar pixel values in an image region.  This is a helper class for
 * AnisotropicDiffusionImageFilter.
 *
 */
template <class TImageType>
struct ITK_EXPORT AvgGradMagSquared
{
  typedef typename ImageTraits<TImageType>::PixelType PixelType;
  typedef typename ImageTraits<TImageType>::RegionType RegionType;
  enum { ImageDimension = ImageTraits<TImageType>::ImageDimension };
  AvgGradMagSquared() {}
  PixelType operator() (TImageType *, const RegionType &) const;
};

struct ITK_EXPORT CopyStrategy
{
  CopyStrategy() {}
  virtual void operator()(void *, void *) const = 0;
};

template <class TInputImage, class TOutputImage>
struct ITK_EXPORT CopyStrategyScalar : public CopyStrategy
{
  CopyStrategyScalar() {}
  virtual void operator()(void*, void *) const;
};
  
struct ITK_EXPORT DiffusionStrategy
{
  DiffusionStrategy(float c) : m_ConductanceTerm(c) {}
  DiffusionStrategy() : m_ConductanceTerm(0.0f) {}
  virtual ~DiffusionStrategy () {};
  virtual void operator()(void *, void *) = 0;
  float m_ConductanceTerm;
};

struct ITK_EXPORT CompositeDiffusionStrategy : public DiffusionStrategy
{
  CompositeDiffusionStrategy(float c) : DiffusionStrategy(c) {}
  CompositeDiffusionStrategy(DiffusionStrategy *first,
                             DiffusionStrategy *second, float c)
    : DiffusionStrategy(c), a(first), b(second)
  {
    a->m_ConductanceTerm = c;
    b->m_ConductanceTerm = c;
  }
  CompositeDiffusionStrategy() {}
  ~CompositeDiffusionStrategy()
  {
    delete a;
    delete b;
  }  
  virtual void operator()(void *d1, void *d2)
  {
    a->operator()(d1, d2);
    b->operator()(d1, d2);
  }
  DiffusionStrategy *a;
  DiffusionStrategy *b;
};

struct ITK_EXPORT UpdateStrategy
{
  UpdateStrategy() : m_Multiplier(1.0f) {}
  virtual void operator()(void *, void *) const = 0;
  float m_Multiplier;
};

template<class TInputImage, class TOutputImage>
struct ITK_EXPORT UpdateStrategyScalar : public UpdateStrategy
{
  UpdateStrategyScalar() {} 
  virtual void operator()(void *, void *) const;
};

/**
 * \class AnisotropicDiffusionImageFilter
 * This class is the base class for a set of non-linear diffusion filters
 * that perform anisotropic diffusion.  It defines a common interface and
 * several default method implementations.
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT AnisotropicDiffusionImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage > 
{
public:
  /**
   * Standard "Self" & Superclass typedef.
   */
  typedef AnisotropicDiffusionImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename  TInputImage::PixelType InputPixelType;
  typedef typename  TInputImage::InternalPixelType InputInternalPixelType;
  enum { ImageDimension = TOutputImage::ImageDimension };
  
  /**
   * Image typedef support
   */
  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(AnisotropicDiffusionImageFilter, ImageToImageFilter);
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Computes the output.
   */
  void GenerateData();
  
  /**
   * Sets the total number of times the filter will cycle on the image.
   */
  itkSetMacro(Iterations, unsigned long);

  /**
   * Returns the total number of times the filter will cycle on the image.
   */
  itkGetMacro(Iterations, unsigned long);

  /**
   * Sets the free conductance parameter used in the conductance function.
   */
  itkSetMacro(ConductanceParameter, float);

  /**
   * Returns the free conductance parameter used in the conductance function.
   */
  itkGetMacro(ConductanceParameter, float);

  /**
   * Sets the size of the time step for each iteration.
   */
  itkSetMacro(TimeStep, float);

  /**
   * Returns the size of the time step for each iteration.
   */
  itkGetMacro(TimeStep, float);
  
protected:
  AnisotropicDiffusionImageFilter() {}
  virtual ~AnisotropicDiffusionImageFilter() {}
  AnisotropicDiffusionImageFilter(const Self&) {}
  void operator=(const Self&) {}

  virtual DiffusionStrategy *GetDiffusionStrategy() = 0;
  virtual UpdateStrategy *GetUpdateStrategy() = 0;
  virtual CopyStrategy *GetCopyStrategy() = 0;
private:
  /**
   * Free parameter in the conductance function.
   */
  float m_ConductanceParameter;

  /**
   * Total number of times the filter will cycle on the image.
   */
  unsigned int m_Iterations;

  /**
   * The size of the time step for each iteration. 
   */
  float m_TimeStep;
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAnisotropicDiffusionImageFilter.txx"
#endif

#endif
