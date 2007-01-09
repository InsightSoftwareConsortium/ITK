/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkGaussianSpatialObject_h
#define __itkGaussianSpatialObject_h

#include "itkAffineTransform.h"
#include "itkFixedArray.h"
#include "itkSpatialObject.h"
#include "itkEllipseSpatialObject.h"

namespace itk
{

/** \class GaussianSpatialObject
 * 
 * \brief Represents a multivariate Gaussian function.
 *
 * The Gaussian function G(x) is given by
 * \f[
 * G(\vec{x}) = m e^{-\|\Sigma^{-1} \vec{x}\|^2 / 2},
 * \f]
 * where m is a scaling factor set by SetMaximum(), and \f$\Sigma\f$ is
 * the (invertible) matrix associated to the IndexToObjectTransform of
 * the object.  If \f$\Sigma\f$ is symmetric and positive definite,
 * and m is chosen so that the integral of G(x) is 1, 
 * then G will denote a normal distribution with mean 0 and
 * covariance matrix \f$\Sigma\f$.
 */

template < unsigned int TDimension = 3 >
class GaussianSpatialObject 
  : public SpatialObject< TDimension >
{

public:

  typedef GaussianSpatialObject               Self;
  typedef double                              ScalarType;
  typedef SmartPointer < Self >               Pointer;
  typedef SmartPointer < const Self >         ConstPointer;
  typedef SpatialObject< TDimension >         Superclass;
  typedef SmartPointer< Superclass >          SuperclassPointer;
  typedef typename Superclass::PointType      PointType;
  typedef typename Superclass::TransformType  TransformType;
  typedef typename Superclass::BoundingBoxType        BoundingBoxType;

  itkStaticConstMacro(NumberOfDimensions, unsigned int,
                      TDimension);

  itkNewMacro( Self );
  itkTypeMacro( GaussianSpatialObject, SpatialObject );

  /** The Radius determines the bounding box, and which points are
   * considered to be inside the SpatialObject.  All points with
   * z-score less than the radius are in the object.  */
  itkSetMacro(Radius,ScalarType);
  itkGetConstReferenceMacro(Radius,ScalarType);

  /** The maximum value of the Gaussian (its value at the origin of
   * the spatial object coordinate system). */
  itkSetMacro(Maximum,ScalarType);
  itkGetConstReferenceMacro(Maximum,ScalarType);

  /** If the matrix S is returned by
   * this->GetIndexToObjectTransform()->GetMatrix(), then SquaredZScore(x)
   * returns |Sx| squared.  */
  ScalarType SquaredZScore( const PointType& point ) const;

  /** Returns the value of the Gaussian at the given point.  */ 
  virtual bool ValueAt( const PointType & point, ScalarType & value, 
                        unsigned int depth=0,
                        char * name=NULL) const;
     
  /** Return true if the object provides a method to evaluate the value 
   * at the specified point, false otherwise.*/
  virtual bool IsEvaluableAt( const PointType & point, 
                              unsigned int depth=0,
                              char * name=NULL) const;

  /** Test whether a point is inside or outside the object */ 
  virtual bool IsInside( const PointType & point,
                         unsigned int depth,
                         char * name) const;
  
  /** Test whether a point is inside or outside the object 
   *  For computational speed purposes, it is faster if the method does not
   *  check the name of the class and the current depth */ 
  virtual bool IsInside( const PointType & point) const;

  /** This function needs to be called every time one of the object's
   *  components is changed. */ 
  virtual bool ComputeLocalBoundingBox() const;

  /** Returns the sigma=m_Radius level set of the Gaussian function, as an
   * EllipseSpatialObject.  */
  typename EllipseSpatialObject< TDimension >::Pointer GetEllipsoid() const;

protected:
  GaussianSpatialObject(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
   
  GaussianSpatialObject( void );
  ~GaussianSpatialObject( void );

  ScalarType m_Maximum;
  ScalarType m_Radius;

  /** Print the object information in a stream. */
  virtual void PrintSelf( std::ostream& os, Indent indent ) const; 

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianSpatialObject.txx"
#endif

#endif // __itkGaussianSpatialObject_h
