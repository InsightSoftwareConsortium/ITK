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

}



/*********************************************************************
 *
 *        Destructor
 *
 *********************************************************************/
itkRegistrator3D2D::~itkRegistrator3D2D()
{
 
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
 *      Load associated points
 *
 *****************************************************/
void itkRegistrator3D2D::LoadAssociatedPoints(const vector<PairPoint3D2D> & externalList)
{	
	AssociatedPoints.clear();
  AssociatedPoints.assign(externalList.begin(),externalList.end());
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





