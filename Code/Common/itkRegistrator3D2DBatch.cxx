/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrator3D2DBatch.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkRegistrator3D2DBatch.h"
#include <vnl/algo/vnl_svd.h>
#include <vnl/vnl_fastops.h>

namespace itk
{

/*********************************************************************
 *
 *        Creator
 *
 *********************************************************************/
Registrator3D2DBatch::Registrator3D2DBatch()
{

  m_PlanarError = 0;
  m_Jacobian    = 0;

}

/*********************************************************************
 *
 *        Destructor
 *
 *********************************************************************/
Registrator3D2DBatch::~Registrator3D2DBatch()
{
  if( m_Jacobian ) 
    {
    delete m_Jacobian;
    m_Jacobian = 0;
    }

  if( m_PlanarError ) 
    {
    delete m_PlanarError;
    m_PlanarError = 0;
    }
}

/*****************************************************
 *
 *          Compute Jacobian
 *
 *****************************************************/
void Registrator3D2DBatch::ComputeJacobian(void) 
{

  Transform3D cameraTotal = m_IntrinsicTransform * m_ExtrinsicTransform *
    cumulatedCorrectionTransform;

  double sumSquares = 0.0;

  Potential  = 0.0;
  const double pr2 = m_PotentialRange * m_PotentialRange;

  const unsigned int numPoints = m_AssociatedPoints.size();

  // this section performs only the minimum set of operations
  // needed to get the 2D projection.  Doing a complete 4x4 
  // will use some more operations.
  // Unfortunately, in this way the code depends on the storage
  // order used for the transfrom implementation.
  // Column major order is assumed here.
  
  const double Xx = cameraTotal(0,0);
  const double Xy = cameraTotal(0,1);
  const double Xz = cameraTotal(0,2);
  const double Xw = cameraTotal(0,3);

  const double Yx = cameraTotal(1,0);
  const double Yy = cameraTotal(1,1);
  const double Yz = cameraTotal(1,2);
  const double Yw = cameraTotal(1,3);

  const double Wx = cameraTotal(3,0);
  const double Wy = cameraTotal(3,1);
  const double Wz = cameraTotal(3,2);
  const double Ww = cameraTotal(3,3);

  vnl_matrix<double> & mJacobian = *m_Jacobian;
  
  for(unsigned int i=0; i<m_AssociatedPoints.size(); i+= 2) 
    {

    const Point3D pp = m_AssociatedPoints[i].GetPoint3D();

    const double x = pp[0];
    const double y = pp[1];
    const double z = pp[2];

    const double Qx = Xx*x +  Xy*y + Xz*z + Xw;
    const double Qy = Yx*x +  Yy*y + Yz*z + Yw;

    const double Qw = Wx*x +  Wy*y + Wz*z + Ww;

    const double Qwi2 = 1.0/(Qw*Qw);

    mJacobian(0, i ) = ( Xx*Qw - Wx*Qx ) * Qwi2; 
    mJacobian(0,i+1) = ( Yx*Qw - Wx*Qy ) * Qwi2;

    mJacobian(1, i ) = ( Xy*Qw - Wy*Qx ) * Qwi2;
    mJacobian(1,i+1) = ( Yy*Qw - Wy*Qy ) * Qwi2;

    mJacobian(2, i ) = ( Xz*Qw - Wz*Qx ) * Qwi2; 
    mJacobian(2,i+1) = ( Yz*Qw - Wz*Qy ) * Qwi2;

    mJacobian(3, i ) = (Qw*(y*Xz-z*Xy)-Qx*(y*Wz-z*Wy)) * Qwi2; 
    mJacobian(3,i+1) = (Qw*(y*Yz-z*Yy)-Qy*(y*Wz-z*Wy)) * Qwi2;  

    mJacobian(4, i ) = (Qw*(z*Xx-x*Xz)-Qx*(z*Wx-x*Wz)) * Qwi2; 
    mJacobian(4,i+1) = (Qw*(z*Yx-x*Yz)-Qy*(z*Wx-x*Wz)) * Qwi2; 

    mJacobian(5, i ) = (Qw*(x*Xy-y*Xx)-Qx*(x*Wy-y*Wx)) * Qwi2; 
    mJacobian(5,i+1) = (Qw*(x*Yy-y*Yx)-Qy*(x*Wy-y*Wx)) * Qwi2; 

    Point3D win = cameraTotal * pp;
    Point2D qq = m_AssociatedPoints[i].GetPoint2D();
    
    const double dx =  qq[0] - win[0];
    const double dy =  qq[1] - win[1];

    m_PlanarError[i]		=  dx;
    m_PlanarError[i+1]	=  dy;

    const double squaredDistance = dx*dx + dy*dy;
    sumSquares += squaredDistance;
    Potential  += 1.0/(1.0+(squaredDistance/pr2));

    }
    
  MeanSquareError = sumSquares  / (double)(numPoints * 2);

}





/*****************************************************
 *
 *          Perform Registration 
 *
 *****************************************************/
void Registrator3D2DBatch::PerformRegistration(void) 
{

  const double StableConvergenceRate = 1e-7;
  MeanSquareError = InTolerance - Tolerance;
	
  double oldMeanSquareError = 0.0;

  if( m_AssociatedPoints.empty() ) 
    {
    throw Registrator3D2DException("There are no associated points");
    }


  // Initialize vectors;
  {
  for(unsigned int i = 0; i<6; i++) {
  Delta[i] = 0.0;
  }
  }

  numberOfIterations = 0;
  while (  numberOfIterations < maxNumberOfIterations )
    {
	
    if (MeanSquareError < Tolerance) 
      {
      stopRegistration = true;
      break;
      }

    Transform3D translationCorrection;
    // translationCorrection.Translate(Delta[0],Delta[1],Delta[2]);

    Transform3D rotationCorrectionX;
    Transform3D rotationCorrectionY;
    Transform3D rotationCorrectionZ;
    // rotationCorrectionX.RotateX(Delta[3]);
    // rotationCorrectionY.RotateY(Delta[4]);
    // rotationCorrectionZ.RotateZ(Delta[5]);

    cumulatedCorrectionTransform *= translationCorrection;
    cumulatedCorrectionTransform *= rotationCorrectionX;
    cumulatedCorrectionTransform *= rotationCorrectionY;
    cumulatedCorrectionTransform *= rotationCorrectionZ;

    ComputeJacobian();		


    if( MeanSquareError < Tolerance ) {
    StopRegistration();
    break;
    }

    if (MeanSquareError > InTolerance) 
      {
      stopRegistration = true;
      break;
      }
	
    const double ConvergenceRate = 
      fabs(MeanSquareError-oldMeanSquareError)/MeanSquareError;

    if( ConvergenceRate < StableConvergenceRate )
      {
      StopRegistration();
      break;
      }


    oldMeanSquareError = MeanSquareError;


    // do a least squares solution modification
    try 
      {
      LeastSquareSolution();
      }
    catch( exception e ) 
      {
      cerr << e.what() << endl;
      break;
      }


    numberOfIterations++;
    }

	
}







/****************************************************************
 *
 * This method uses a Least Square Algorithm to find "Delta"
 *		from:
 *					Jacobian*Delta = PlanarError	
 *
 *		Jacobian is the matrix relating x,y error to variation of
 *		translations and rotations of points in 3D space
 *
 *		PlanarError is the diference between planar points and 
 *		the current projection of the associated 3D points
 *
 ****************************************************************/
void Registrator3D2DBatch::LeastSquareSolution(void)
{

  const int dof = 6;
  const int colSize = 2*m_AssociatedPoints.size();

  if( colSize == 0 ) return;

  vnl_matrix<double> M(dof,dof);

  // compute Jacobian^t * Jacobian
  // this matrix is column ordered
  // in a one dimensional array
  vnl_fastops::AtA(*m_Jacobian, &M);

  // Add Uncertainties
  for(int c=0; c<dof; c++) {
  M[c][c] += m_Uncertain[c];
  }

  vnl_vector<double> JP = (*m_Jacobian) * (*m_PlanarError);

  Delta = vnl_svd<double>(M).solve(JP); 
  

}






/*****************************************************
 *
 *      Load associated points
 *
 *****************************************************/
void Registrator3D2DBatch::LoadAssociatedPoints(const vector<PairPoint3D2D> & externalList)
{	
  Registrator3D2D::LoadAssociatedPoints( externalList );
  Allocate();
}






/*****************************************************
 *
 *      Allocate memory for internal arrays
 *
 *****************************************************/
void Registrator3D2DBatch::Allocate(void)
{

  const unsigned int numPoints = m_AssociatedPoints.size();
  const unsigned int column    = numPoints * 2;

  if( m_Jacobian ) 
    {
    delete m_Jacobian;
    }
  
  try 
    {
    m_Jacobian = new vnl_matrix<double>(6,column);
    }
  catch(...) 
    {
    throw Registrator3D2DException("Registration Iterations: memory allocation for Jacobian");
    }


  if( m_PlanarError )
    {
    delete m_PlanarError;
    }

  try 
    {
    m_PlanarError = new vnl_vector<double>(column);
    }
  catch(...)
    {
    throw Registrator3D2DException("Registration Iterations: memory allocation for PlanarError");
    }
}

} // end namespace itk
