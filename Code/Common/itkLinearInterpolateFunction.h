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
 * LinearInterpolateFunction linearly interpolates image intensity at
 * a integer or non-integer pixel position. This class is templated
 * over the input image type.
 *
 * This function works for N-dimensional images.
 * */
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
   * InputImageType typedef support.
   */
  typedef TInputImage InputImageType;

  /**
   * Dimension underlying input image.
   */
  enum { ImageDimension = InputImageType::ImageDimension };

  /**
   * Index typedef support.
   */
  typedef Index<ImageDimension> IndexType;

  /** 
   * Set the input image.
   *
   * Specify the image to be interpolated.
   *
   * This method is NOT thread safe.
   */
  virtual void SetInputImage( InputImageType * ptr );

  /**
   * Evaluate the function at specified index.
   *
   * Returns the image intensity at a specified integer
   * coordinate position, or zero if the indicated position
   * is outside the image.
   *
   * This method is believed to be thread safe.
   */
  virtual double Evaluate( const IndexType& index );

  /**
   * Evaluate the function at non-integer coordinates.
   *
   * Returns the linearly interpolated image intensity at a specified
   * coordinate position, which need not be integer-valued.  Pixels
   * outside the image are considered to be zero.
   *
   * This method is believed to be thread safe.
   */
  virtual double Evaluate( double * dblIndex );

protected:
  LinearInterpolateFunction(){};
  LinearInterpolateFunction( const Self& ){};

  ~LinearInterpolateFunction(){};

  void operator=( const Self& ){};
  void PrintSelf(std::ostream& os, Indent indent);

private:

  Size<ImageDimension>   m_ImageSize;  // Dimensions of the image
  unsigned long          m_Neighbors;  // Size of interpolation neighborhood

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLinearInterpolateFunction.txx"
#endif

#endif
