/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEntropyPreservingGradientMagnitudeImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

==========================================================================*/
#ifndef _itkEntropyPreservingGradientMagnitudeImageFunction_h
#define _itkEntropyPreservingGradientMagnitudeImageFunction_h

#include "itkImageFunction.h"
#include "itkIndex.h"

namespace itk
{

/**
 * \class EntropyPreservingGradientMagnitudeImageFunction
 * \brief Calculate an entropy satisfying gradient magnitude.
 *
 * EntropyPreservingGradientMagnitudeImageFunction calculates an entropy satisfying image
 * gradient magnitude. This is class is templated over the
 * input image type.
 *
 * In level set methods, the propagating front can form corners
 * as it evolves. At these singularities, the front is no longer
 * differentiable and a weak solution must be constructed to
 * continue the solution.
 *
 * Viscosity or entropy solution can be formed by using upwind
 * finite differencing. Depending on the flow direction, either a forward
 * or backward differencing scheme is used. This is to ensure that only
 * grid points upstream in the flow is used to update the current point.
 *
 * In this function, the flow direction can be specified by the sign
 * of the speed value set via the SetSpeed() method.
 *
 * Reference:
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Chapter 6, Second edition, 1999.
 *
 * Possible improvements:
 * - the use of Neighborhood operators may improve efficiency.
 *
 */
template < class TInputImage >
class ITK_EXPORT EntropyPreservingGradientMagnitudeImageFunction :
  public ImageFunction< TInputImage, double >
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef EntropyPreservingGradientMagnitudeImageFunction Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef ImageFunction<TInputImage, double> Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * InputImageType typedef support.
   */
  typedef TInputImage InputImageType;

  /**
   * Dimension of the underlying image.
   */
  enum { ImageDimension = InputImageType::ImageDimension };

  /**
   * Index typedef support.
   */
  typedef typename Superclass::IndexType IndexType;

  /**
   * Set the input image.
   */
  virtual void SetInputImage( InputImageType * ptr );

  /**
   * Set the speed parameter.
   */
  void SetSpeed( double value )
    { m_Speed = value; }

  /**
   * Get the speed parameter.
   */
  double GetSpeed() const
    { return m_Speed; }

  /**
   * Evalulate the function at specified index
   */
  virtual double Evaluate( const IndexType& index ) const;

  /**
   * Get the magnitude from last evaluation
   */
  double GetMagnitude() const
    { return m_Magnitude; }

protected:
  EntropyPreservingGradientMagnitudeImageFunction(){};
  EntropyPreservingGradientMagnitudeImageFunction( const Self& ){};
  ~EntropyPreservingGradientMagnitudeImageFunction(){};
  void operator=( const Self& ){};
  void PrintSelf(std::ostream& os, Indent indent);

private:
  Size<ImageDimension>    m_ImageSize;
  bool                    m_ImageSizeOK;

  double                  m_Speed;
  mutable double          m_Magnitude;

  mutable IndexType       m_NeighIndex;
  mutable double          m_CenterValue;
  mutable double          m_DiffValue;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEntropyPreservingGradientMagnitudeImageFunction.txx"
#endif

#endif
