/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvatureFlowImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkCurvatureFlowImageFilter_h
#define _itkCurvatureFlowImageFilter_h

#include "itkLevelSetImageFilter.h"

namespace itk
{

/** \class CurvatureFlowImageFilter
  * \brief Denoise an image using curvature driven flow.
  *
  * CurvatureFlowImageFilter implements a curvature driven image denoising algorithm.
  * Iso-brightness contours in the input image are viewed as a level set.
  * The level set is then evolved using a curvature-based speed function.
  *
  * The advantage of this approach is that sharp boundaries are preserved
  * with smoothing occuring only within a region.
  *
  * Note that unlike level set segmenetation algorithms,
  * the image to be denoised is already the level set and can be set
  * directly as the input using the SetInput() method.
  *
  * Narrowbanding is not supported in this class.
  * 
  * Reference:
  * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
  * Cambridge Press, Chapter 16, Second edition, 1999.
  *
  * Possible improvements:
  * - At each iteration, the algorithm is highly parallelizable.
  * Future implementation should take advantage of this.
  *
  * \sa LevelSetImageFilter
  *
  */
template <class TLevelSet> 
class ITK_EXPORT CurvatureFlowImageFilter : 
  public LevelSetImageFilter<TLevelSet>
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef CurvatureFlowImageFilter Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef LevelSetImageFilter<TLevelSet> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(CurvatureFlowImageFilter, LevelSetImageFilter);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Typedef support for level set related types.
   */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
  typedef typename LevelSetType::LevelSetImageType  LevelSetImageType;
  typedef typename LevelSetType::LevelSetPointer  LevelSetPointer;
  typedef typename LevelSetType::PixelType  PixelType;

  /**
   * Index typedef support
   */
  typedef Index<LevelSetType::SetDimension> IndexType;

  /**
   * Set the debugging mode
   */
  itkSetMacro( DebugOn, bool );

protected:
  CurvatureFlowImageFilter();
  ~CurvatureFlowImageFilter(){};
  CurvatureFlowImageFilter(const Self&){};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream& os, Indent indent);

  virtual void Initialize();
  void GenerateData();

private:
  bool m_DebugOn;

};


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCurvatureFlowImageFilter.txx"
#endif

#endif
