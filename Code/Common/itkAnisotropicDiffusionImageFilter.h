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

#include "itkFilterImageToImage.h"

namespace itk
{

template <class TPixel, unsigned long VDimension>
struct AvgGradMagSquared
{
  AvgGradMagSquared() {}
  TPixel operator() (Image<TPixel, VDimension> *,
                     const ImageRegion<VDimension> &) const;
};

struct CopyStrategy
{
  CopyStrategy() {}
  virtual void operator()(void *, void *) const = 0;
};

template <class TPixel>
struct CopyStrategyScalar : public CopyStrategy
{
  CopyStrategyScalar() {}
  virtual void operator()(void*, void *) const;
};
  
struct DiffusionStrategy
{
  DiffusionStrategy(float c) : m_ConductanceTerm(c) {}
  DiffusionStrategy() : m_ConductanceTerm(0.0f) {}
  virtual void operator()(void *, void *) = 0;
  float m_ConductanceTerm;
};

struct CompositeDiffusionStrategy : public DiffusionStrategy
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

struct UpdateStrategy
{
  UpdateStrategy() : m_Multiplier(1.0f) {}
  virtual void operator()(void *, void *) const = 0;
  float m_Multiplier;
};

template<class TImage>
struct UpdateStrategyScalar : public UpdateStrategy
{
  UpdateStrategyScalar() {} 
  virtual void operator()(void *, void *) const;
};

  
template <class TPixel, unsigned int VDimension=2>
class ITK_EXPORT AnisotropicDiffusionImageFilter :
    public FilterImageToImage< Image<TPixel, VDimension>,
                               Image<TPixel, VDimension> > 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef AnisotropicDiffusionImageFilter Self;

  /**
   * Standard Superclass typedef support.
   */
  typedef FilterImageToImage< Image<TPixel, VDimension>,
    Image<TPixel, VDimension> > Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Image typedef support
   */
  typedef Image<TPixel, VDimension> ImageType;

  /**
   * Scalar value type support
   */
  typedef typename ImageType::ScalarValueType ScalarValueType;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(AnisotropicDiffusionImageFilter, FilterImageToImage);
  
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
