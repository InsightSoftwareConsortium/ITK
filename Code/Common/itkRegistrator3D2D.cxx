/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrator3D2D.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkRegistrator3D2D.h"
#include <vnl/algo/vnl_svd.h>
#include <vnl/vnl_fastops.h>



/*********************************************************************
 *
 *        Creator
 *
 *********************************************************************/
itkRegistrator3D2D::itkRegistrator3D2D()
{

  Tolerance	  = 0.001;
  InTolerance = 1.000;
  Potential   = 0.0;
  potentialRange = 0.001;

  for(unsigned int i=0; i<6; i++) 
  {
    Delta[i]     = 0.0;
    Uncertain[i] = 0.0;
  }

  maxNumberOfIterations = 10;

  PlanarError = 0;
  Jacobian    = 0;

}



/*********************************************************************
 *
 *        Destructor
 *
 *********************************************************************/
itkRegistrator3D2D::~itkRegistrator3D2D()
{
  if( Jacobian ) 
  {
    delete Jacobian;
    Jacobian = 0;
  }

  if( PlanarError ) 
  {
    delete PlanarError;
    PlanarError = 0;
  }

}



/*********************************************************************
 *
 *    Set the Tolerance in the MeanSquareError to stop registration    
 *
 *********************************************************************/
void itkRegistrator3D2D::SetTolerance(double tol) 
{
	Tolerance = tol;
}



/*********************************************************************
 *
 *    Set the InTolerance in the MeanSquareError to stop registration    
 *
 *********************************************************************/
void itkRegistrator3D2D::SetInTolerance(double tol) 
{
	InTolerance = tol;
}


/*********************************************************************
 *
 *    Set Potential Range
 *
 *********************************************************************/
void itkRegistrator3D2D::SetPotentialRange(double range) 
{
	potentialRange = range;
}



/*********************************************************************
 *
 *    Set the maximum number of iterations before to stop registration    
 *
 *********************************************************************/
void itkRegistrator3D2D::SetMaximumNumberOfIterations(unsigned int num) 
{
  maxNumberOfIterations = num;
}



/*********************************************************************
 *
 *    Set Translational Uncertainties 
 *
 *********************************************************************/
void itkRegistrator3D2D::SetTranslationUncertainty(
                              const Vector3D & uncertainty)
{
	Uncertain[0] = uncertainty[0];
	Uncertain[1] = uncertainty[1];
	Uncertain[2] = uncertainty[2];
}


/*********************************************************************
 *
 *    Set Rotational Uncertainties 
 *
 *********************************************************************/
void itkRegistrator3D2D::SetRotationUncertainty(
                              const Vector3D & uncertainty)
{
	Uncertain[3] = uncertainty[0];
	Uncertain[4] = uncertainty[1];
	Uncertain[5] = uncertainty[2];
}




/*****************************************************
 *
 *      Get the value of the Mean Square Error  
 *
 *****************************************************/
double itkRegistrator3D2D::GetMeanSquareError(void) const
{
	return MeanSquareError;
}



/*****************************************************
 *
 *      Get the value of the Potential  
 *
 *****************************************************/
double itkRegistrator3D2D::GetPotential(void) const
{
	return Potential;
}


/*****************************************************
 *
 *      Get the value of the Potential  
 *
 *****************************************************/
double itkRegistrator3D2D::GetPotentialRange(void) const
{
	return potentialRange;
}




/*****************************************************
 *
 *        Stop the registration process      
 *
 *****************************************************/
void itkRegistrator3D2D::StopRegistration(void) 
{
	stopRegistration = true;
}



/*****************************************************
 *
 *    Set the extrinsic transformation        
 *
 *****************************************************/
void itkRegistrator3D2D::SetExtrinsicTransform(const Transform3D & matrix)
{

  extrinsicTransform = matrix;
  ResetRegistration();

}




/*****************************************************
 *
 *    Set the intrinsic transformation        
 *
 *****************************************************/
void itkRegistrator3D2D::SetIntrinsicTransform(const Transform3D & matrix)
{

  intrinsicTransform = matrix;
  ResetRegistration();

}



/*****************************************************
 *
 *          Compute Jacobian
 *
 *****************************************************/
void itkRegistrator3D2D::ComputeJacobian(void) 
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


  vnl_matrix<double> & mJacobian = *Jacobian;
  
  for(unsigned int i=0; i<AssociatedPoints.size(); i+= 2) 
  {

    const Point3D pp = AssociatedPoints[i].GetPoint3D();

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
    Point2D qq = AssociatedPoints[i].GetPoint2D();
    
    const double dx =  qq[0] - win[0];
    const double dy =  qq[1] - win[1];

    PlanarError[i]		=  dx;
    PlanarError[i+1]	=  dy;

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
void itkRegistrator3D2D::PerformRegistration(void) 
{

	const double StableConvergenceRate = 1e-7;
  MeanSquareError = InTolerance - Tolerance;
	
  double oldMeanSquareError = 0.0;

  if( AssociatedPoints.empty() ) 
  {
    throw itkRegistrator3D2DException("There are no associated points");
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
void itkRegistrator3D2D::LeastSquareSolution(void)
{

	const int dof = 6;
	const int colSize = 2*AssociatedPoints.size();

  if( colSize == 0 ) return;

  vnl_matrix<double> M(dof,dof);

	// compute Jacobian^t * Jacobian
	// this matrix is column ordered
	// in a one dimensional array
  vnl_fastops::AtA(*Jacobian, &M);

  // Add Uncertainties
  for(int c=0; c<dof; c++) {
    M[c][c] += Uncertain[c];
    }

  vnl_vector<double> JP = (*Jacobian) * (*PlanarError);

  Delta = vnl_svd<double>(M).solve(JP); 
  

}






/*****************************************************
 *
 *      Load associated points
 *
 *****************************************************/
void itkRegistrator3D2D::LoadAssociatedPoints(const vector<PairPoint3D2D> & externalList)
{	
	AssociatedPoints.clear();
  AssociatedPoints.assign(externalList.begin(),externalList.end());
  Allocate();
}





/*****************************************************
 *
 *      Reset Registration
 *
 *****************************************************/
void itkRegistrator3D2D::ResetRegistration(void)
{
  cumulatedCorrectionTransform.set_identity();
}




/*****************************************************
 *
 *      Return the Transformation
 *
 *****************************************************/
const itkRegistrator3D2D::Transform3D & itkRegistrator3D2D::GetTransformation( void ) const
{
  return cumulatedCorrectionTransform;
}



/*****************************************************
 *
 *      Return the real number of iterations
 *
 *****************************************************/
unsigned int itkRegistrator3D2D::GetNumberOfIterationsPerformed( void ) const
{
  return numberOfIterations;
}





/*****************************************************
 *
 *      Allocate memory for internal arrays
 *
 *****************************************************/
void itkRegistrator3D2D::Allocate(void)
{

	const unsigned int numPoints = AssociatedPoints.size();
  const unsigned int column    = numPoints * 2;

  if( Jacobian ) 
  {
    delete Jacobian;
  }
  
  try 
  {
    Jacobian = new vnl_matrix<double>(6,column);
  }
  catch(...) 
  {
		throw itkRegistrator3D2DException("Registration Iterations: memory allocation for Jacobian");
  }


  if( PlanarError )
  {
    delete PlanarError;
  }

  try 
  {
    PlanarError = new vnl_vector<double>(column);
  }
  catch(...)
  {
		throw itkRegistrator3D2DException("Registration Iterations: memory allocation for PlanarError");
  }



}



