/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrator3D2DRecursive.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/

#ifndef __itkRegistrator3D2DRecursive_h
#define __itkRegistrator3D2DRecursive_h

#include <itkRegistrator3D2D.h>
#include <itkKalmanFilter.h>

namespace itk
{

/** \class Registrator3D2DRecursive
 * \brief Perform rigid-body registration between a set of 3D points
 *        and a set of 2D points.
 *
 *  Registrator3D2D performs 3D rigid registration 
 *  between a set of 3D points and a set of 2D points.
 *  
 *  Each 2D point is associated with an unique point in the 3D set. 
 *  The 2D point and is assumed to be the projection of the 3D one
 *  after and unkown 3D rigid transformation. The projections parameters
 *  should be provided encapsulated in a 'intrinsic' transformation.
 *  An extrinsic tranformation accounts for ridig movements of the 3D set
 *  of points before the projection.
 *
 *  The method should return a 3D rigid transformation that after 
 *  projection maps the the set of 3D point on top of the 2D set.
 *  
 *  This method was part of a registration method developed by Alan Liu 
 *  during his Ph.D. Thesis at the University of North Carolina at Chapel Hill
 *  with the advisory of Elizabeth Bullit, MD.  The code was modified and
 *  adapted for Insight toolkit by Luis Ibanez.
 *
 *  liu@cs.unc.edu
 *  bullitt@med.unc.edu
 *  ibanez@cs.unc.edu
 */
 
class Registrator3D2DRecursive : public Registrator3D2D   
{

public:
  /**
   * Constructor of a registrator object
   */
  Registrator3D2DRecursive();

  /**
   * Destructor of a registrator object
   */
  ~Registrator3D2DRecursive();

  /**
   * Initialize the registration process
   */
  void PerformRegistration( void );

  /**
   * Execute the recursive estimation
   */
  void RecursiveEstimation( void );

private:

  /**
   * Estimator is a Kalman Filter that will recursively 
   * estimate the 6 parameters of the rigid transformation
   * for one iteration of the registration process.
   */
  KalmanFilter<double,6>   Estimator;

};

  
} // namespace itk

#endif
  
