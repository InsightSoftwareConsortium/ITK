/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrator3D2DRecursive.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkRegistrator3D2DRecursive.h"

namespace itk
{

/*********************************************************************
 *
 *        Creator
 *
 *********************************************************************/
Registrator3D2DRecursive::Registrator3D2DRecursive()
{

}



/*********************************************************************
 *
 *        Destructor
 *
 *********************************************************************/
Registrator3D2DRecursive::~Registrator3D2DRecursive()
{

}



/*****************************************************
 *
 *          Perform Registration 
 *
 *****************************************************/
void Registrator3D2DRecursive::PerformRegistration(void) 
{

  const double StableConvergenceRate = 1e-7;
  MeanSquareError = InTolerance - Tolerance;
	
  double oldMeanSquareError = 0.0;

  if( AssociatedPoints.empty() ) 
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
	
    if( MeanSquareError < Tolerance ) 
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


    if( MeanSquareError < Tolerance ) {
    StopRegistration();
    break;
    }

    if( MeanSquareError > InTolerance ) 
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


    // Do a least squares solution 
    try 
      {
      RecursiveEstimation();
      }
    catch( exception e ) 
      {
      cerr << e.what() << endl;
      break;
      }


    numberOfIterations++;
    }

	
}





/********************************************************************
 *
 * This method uses a Kalman Filter to recursivelly estimate "Delta"
 *		from:
 *					Jacobian*Delta = PlanarError	
 *
 *		Jacobian is the matrix relating x,y error to variation of
 *		translations and rotations of points in 3D space
 *
 *		PlanarError is the diference between planar points and 
 *		the current projection of the associated 3D points
 *
 *    The recursive nature of the Kalman Filter make unnecesary to
 *    store the whole Jacobian and PlanarError, only the variance of 
 *    the estimator is keep instead.
 *
 ****************************************************************/
void Registrator3D2DRecursive::RecursiveEstimation(void)
{

  Transform3D cameraTotal = intrinsicTransform * extrinsicTransform *
    cumulatedCorrectionTransform;

  double sumSquares = 0.0;

  Potential  = 0.0;
  const double pr2 = potentialRange * potentialRange;

  const unsigned int numPoints = AssociatedPoints.size();


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


  const unsigned int dof = 6;

  Estimator.clearEstimation();

  vnl_matrix<double> variance(dof,dof);
  variance.set_identity();
  for(unsigned int diag=0; diag<dof; diag++)
    {
    variance[diag][diag] = Uncertain[diag];
    }


  Estimator.setVariance( variance );

  vnl_vector_fixed<double,6>  PredictorX;
  vnl_vector_fixed<double,6>  PredictorY;

  for( unsigned int i=0; i<numPoints; i++) 
    {

    const Point3D pp = AssociatedPoints[i].GetPoint3D();

    const double x = pp[0];
    const double y = pp[1];
    const double z = pp[2];

    const double Qx = Xx*x +  Xy*y + Xz*z + Xw;
    const double Qy = Yx*x +  Yy*y + Yz*z + Yw;

    const double Qw = Wx*x +  Wy*y + Wz*z + Ww;

    const double Qwi2 = 1.0/(Qw*Qw);

    Point3D win = cameraTotal * pp;
    Point2D qq = AssociatedPoints[i].GetPoint2D();
    
    const double dx =  qq[0] - win[0];
    const double dy =  qq[1] - win[1];

    PredictorX(0) = ( Xx*Qw - Wx*Qx ) * Qwi2; 
    PredictorY(0) = ( Yx*Qw - Wx*Qy ) * Qwi2;

    PredictorX(1) = ( Xy*Qw - Wy*Qx ) * Qwi2;
    PredictorY(1) = ( Yy*Qw - Wy*Qy ) * Qwi2;

    PredictorX(2) = ( Xz*Qw - Wz*Qx ) * Qwi2; 
    PredictorY(2) = ( Yz*Qw - Wz*Qy ) * Qwi2;

    PredictorX(3) = (Qw*(y*Xz-z*Xy)-Qx*(y*Wz-z*Wy)) * Qwi2; 
    PredictorY(3) = (Qw*(y*Yz-z*Yy)-Qy*(y*Wz-z*Wy)) * Qwi2;  

    PredictorX(4) = (Qw*(z*Xx-x*Xz)-Qx*(z*Wx-x*Wz)) * Qwi2; 
    PredictorY(4) = (Qw*(z*Yx-x*Yz)-Qy*(z*Wx-x*Wz)) * Qwi2; 

    PredictorX(5) = (Qw*(x*Xy-y*Xx)-Qx*(x*Wy-y*Wx)) * Qwi2; 
    PredictorY(5) = (Qw*(x*Yy-y*Yx)-Qy*(x*Wy-y*Wx)) * Qwi2; 

    const double squaredDistance = dx*dx + dy*dy;
    sumSquares += squaredDistance;
    Potential  += 1.0/(1.0+(squaredDistance/pr2));

    Estimator.updateWithNewMeasure( dx, PredictorX );
    Estimator.updateWithNewMeasure( dy, PredictorY );

    }

  Delta = Estimator.getEstimator();

  MeanSquareError = sumSquares / (numPoints * 2);
  

}

} // namespace itk
