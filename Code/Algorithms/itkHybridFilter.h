/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHybridFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHybridFilter_h
#define __itkHybridFilter_h

#include "itkBalloonForceFilter.h"
#include "itkGibbsPriorFilter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{

/** \class HybridFilter
 * \brief the filter try to combine the gibbs prior model and deformable model into
 *  a segmentation framework, the output of Gibbs Prior model will be automatically set 
 *  as the input of deformable model
 *
 * \ingroup HybridSegmentation 
 *
 */
template <class TInputImage, class TOutputImage, class TInputMesh, 
  class TOutputMesh>
class ITK_EXPORT HybridFilter:
  public ImageToImageFilter<TInputImage,TOutputImage>
{

public:
  /** Standard class typedefs. */
  typedef HybridFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage> Superclass;
  typedef SmartPointer<Self>                Pointer;
  typedef SmartPointer<const Self>      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /**  Smoothing filter type. */
  typedef BalloonForceFilter<TInputMesh,TOutputMesh>  BalloonForceFilterType;

  /**  Derivative along one dimension filter type. */
  typedef GibbsPriorFilter<TInputImage,TOutputImage>  GibbsPriorFilterType;

  /**  Pointer to a balloon force filter. */
  typedef typename BalloonForceFilterType::Pointer  BalloonForceFilterPointer;

  /**  Pointer to a gibbs prior filter. */
  typedef typename GibbsPriorFilterType::Pointer  GibbsPriorFilterPointer;                                  
  /** Iterator type. */
  typedef ImageRegionIteratorWithIndex< TOutputImage > OutputImageIterator;
  
  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Set potential of the balloon force filter 
   * using the output of gibbs prior filter */
  void SetPotential( void );

  /** Sent object region labelled by the deformable 
   * model to the gibbs prior model for parameter update  */
  void SetObjectRegion( void );

  /** Set the balloon force filter and gibbs prior filter */
  void SetBalloonForceFilter(BalloonForceFilterPointer  bffilter);
  void SetGibbsPriorFilter(GibbsPriorFilterPointer  gpfilter);

  /** Algorithm specific methods. */
  void Advance();
  void SetGibbsInput();

protected:
  HybridFilter();
  virtual ~HybridFilter() {};
  
  /** Generate data. */
  virtual void GenerateData();

private:
  HybridFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  BalloonForceFilterPointer   m_BalloonForceFilter;
  GibbsPriorFilterPointer   m_GibbsPriorFilter;
  int m_IterNum;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHybridFilter.txx"
#endif

#endif
