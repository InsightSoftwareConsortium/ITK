/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLinearInterpolateFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/
#ifndef _itkLinearInterpolateFunction_h
#define _itkLinearInterpolateFunction_h

#include "itkImageFunction.h"
#include "itkSize.h"

namespace itk
{

/** 
 * \class LinearInterpolateFunction
 * \brief Linearly interpolate an image.
 *
 * LinearInterpolateFunction linearly interpolates an image at 
 * non-integer pixel position. This class is template over
 * the input image type.
 *
 * This function works for N-dimensional images.
 *
 */
template <class TInputImage >
class ITK_EXPORT LinearInterpolateFunction : 
  public ImageFunction<TInputImage,double> 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef LinearInterpolateFunction Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageFunction<TInputImage,double> Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self> Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /** 
   * Set the input image.
   */
  virtual void SetInputImage( InputImageType * ptr );

  /**
   * Evalulate the function at specified index.
   */
  virtual double Evaluate( const IndexType& index );

  /**
   * Evalulate the function at non-integer coordinates.
   */
  virtual double Evaluate( double * dblIndex );

  /**
   * Get the evaluated function value.
   */
  double GetValue() const
    { return m_Value; }

protected:
  LinearInterpolateFunction(){};
  LinearInterpolateFunction( const Self& ){};

  ~LinearInterpolateFunction(){};

  void operator=( const Self& ){};
  void PrintSelf(std::ostream& os, Indent indent);

private:

  Size<ImageDimension>        m_ImageSize; 
  unsigned long               m_Neighbors;

  double                      m_Lower[ImageDimension];
  double                      m_Upper[ImageDimension];
  double                      m_Distance[ImageDimension];

  double                      m_Value;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLinearInterpolateFunction.txx"
#endif

#endif
