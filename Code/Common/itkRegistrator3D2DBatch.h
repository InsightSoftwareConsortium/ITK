/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrator3D2DBatch.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.
  
==========================================================================*/

#ifndef __itkRegistrator3D2DBatch_h
#define __itkRegistrator3D2DBatch_h

#include <itkRegistrator3D2D.h>

namespace itk
{

/** \class Registrator3D2DBatch
 * \brief Rigid registration in batch mode.
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
class Registrator3D2DBatch : public Registrator3D2D  
{
public:

  /**
   * Constructor of a registrator object
   */
  Registrator3D2DBatch();

  /**
   * Destructor of a registrator object
   */
  ~Registrator3D2DBatch();

  /**
   * Set the list of associated pairs of 3D point and 2D points.
   * \sa AssociatedPoints
   */
  virtual void LoadAssociatedPoints(const vector<PairPoint3D2D>& externalList);

  /**
   * Initialize the registration process
   */
  void PerformRegistration( void );

  /**
   * This method computes the Jacobian that relates the 2D coordinates of a 3D
   * point projection with the parameters of a rigid transformation. These
   * parameteres are represented as translations along the 3 orthogonal axis
   * and rotations around the 3 orthogonal axis. 
   */
  void ComputeJacobian(void);

private:

   /** 
   * The Jacobian contains the dependencies between the registration
   * parameters and the 2D coordinates of points
   *
   */
  vnl_matrix<double> * m_Jacobian; 

  /** 
   * PlanarError is the vector containing the differences of 2D coordinates
   * between the 2D points and the projections of 3D points.
   *
   */
  vnl_vector<double> * m_PlanarError; 

  /**
   * Allocate memory for internal arrays containg data used for solving the
   * least square problem
   */
  void Allocate(void);

  /**
   * This method solve the Least Square Problem using Levenberg-Marquard
   * method.
   */
  void LeastSquareSolution(void);
};

} // end namespace itk

#endif
