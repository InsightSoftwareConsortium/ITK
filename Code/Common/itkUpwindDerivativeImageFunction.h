/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUpwindDerivativeImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

==========================================================================*/
#ifndef _itkUpwindDerivativeImageFunction_h
#define _itkUpwindDerivativeImageFunction_h

#include "itkImageFunction.h"

namespace itk
{

/**
 * \class UpwindDerivativeImageFunction
 * \brief Calculate the derivative using only upwind neighbors.
 *
 * UpwindDerivativeImageFunction calculates a derivative using only upwind
 * neighbors. The flow direction can be specified by the sign of
 * the speed value set via the SetSpeed() method.
 *
 * If the speed is positive, the backward difference operator is
 * used. If speed is negative, the forward difference operator is
 * used.
 *
 * This class is templated over the input image type.
 *
 * Reference:
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Chapter 6, Second edition, 1999.
 *
 * Possible improvements:
 * - the use of Neighborhood operators may improve efficiency.
 */
template <class TInputImage >
class ITK_EXPORT UpwindDerivativeImageFunction :
  public ImageFunction< TInputImage, double >
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef UpwindDerivativeImageFunction Self;

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
  typedef Index<ImageDimension> IndexType;

  /**
   * Set the input image.
   */
  virtual void SetInputImage( InputImageType * ptr );

  /**
   * Set the speed parameter.
   */
  virtual void SetSpeed( double value )
  { m_Speed = value; }

  /**
   * Get the speed parameter.
   */
  virtual double GetSpeed() const
    { return m_Speed; }

  /**
   * Evalulate the function at specified index
   */
  virtual double Evaluate( const IndexType& index )
  {
    return ( this->Evaluate( index, 0 ) );
  }

  /**
   * Evalulate the function at specified index
   */
  virtual double Evaluate( const IndexType& index, unsigned int dim = 0 );

  /**
   * Evaluate the function at a non-integer position
   */
  virtual double Evaluate( double coord[] )
  {
    return ( this->Evaluate( coord, 0 ) );
  }

  /**
   * Evaluate the function at a non-integer position
   */
  virtual double Evaluate( double coord[], unsigned int dim = 0 )
    {
      IndexType index;
      for( int j = 0; j < ImageDimension; j++ )
        {
          index[j] = vnl_math_rnd( coord[j] );
        }
      return ( this->Evaluate( index, dim ) );
    };

  /**
   * Get the derivative from last evaluation
   */
  virtual double GetDerivative() const
  { return m_Derivative; }

protected:
  UpwindDerivativeImageFunction(){};
  UpwindDerivativeImageFunction( const Self& ){};

  ~UpwindDerivativeImageFunction(){};

  void operator=( const Self& ){};
  void PrintSelf(std::ostream& os, Indent indent);

  double                  m_Speed;
  double                  m_Derivative;

private:
  Size<ImageDimension>    m_ImageSize;
  bool                    m_ImageSizeOK;

  IndexType               m_NeighIndex;
  double                  m_CenterValue;
  double                  m_DiffValue;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkUpwindDerivativeImageFunction.txx"
#endif

#endif

