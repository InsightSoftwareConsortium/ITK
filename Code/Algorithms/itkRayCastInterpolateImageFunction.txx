/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkRayCastInterpolateImageFunction.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkRayCastInterpolateImageFunction_txx
#define _itkRayCastInterpolateImageFunction_txx

#include "itkRayCastInterpolateImageFunction.h"

#include "vnl/vnl_math.h"


#if 0
#include "iomanip.h"
#include "stdio.h"

#define DEBUG_RAY_CAST_INTERPOLATOR_PATH
#define DEBUG_RAY_CAST_INTERPOLATOR
#endif

namespace itk
{


/* -----------------------------------------------------------------------
   Constructor
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::RayCastInterpolateImageFunction() 
{
  m_Threshold = 0.;

  m_FocalPoint[0] = 0.;
  m_FocalPoint[1] = 0.;
  m_FocalPoint[2] = 0.;

  this->ZeroState();
}


/* -----------------------------------------------------------------------
   PrintSelf
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "Threshold: " << m_Threshold << std::endl;
  os << indent << "Transform: " << m_Transform << std::endl;
  os << indent << "FocalPoint: " << m_FocalPoint << std::endl;
  os << indent << "Interpolator: " << m_Interpolator << std::endl;
  
}


/* -----------------------------------------------------------------------
   Initialise() - Initialise the object
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::Initialise(void) const
{

  // Save the dimensions of the volume and calculate the bounding box

  this->RecordVolumeDimensions();

  // Calculate the planes and corners which define the volume.

  this->DefineCorners();
  this->CalcPlanesAndCorners();
}


/* -----------------------------------------------------------------------
   RecordVolumeDimensions() - Record volume dimensions and resolution
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::RecordVolumeDimensions(void) const
{
  const double *spacing=m_Image->GetSpacing();
  SizeType dim=m_Image->GetLargestPossibleRegion().GetSize();

  m_NumberOfVoxelsInX = dim[0];
  m_NumberOfVoxelsInY = dim[1];
  m_NumberOfVoxelsInZ = dim[2];

  m_VoxelDimensionInX = spacing[0];
  m_VoxelDimensionInY = spacing[1];
  m_VoxelDimensionInZ = spacing[2];
}


/* -----------------------------------------------------------------------
   DefineCorners() - Define the corners of the volume
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::DefineCorners(void) const
{
  // Define corner positions as if at the origin

  m_BoundingCorner[0][0] 
    = m_BoundingCorner[1][0] 
    = m_BoundingCorner[2][0] 
    = m_BoundingCorner[3][0] = 0;

  m_BoundingCorner[4][0] 
    = m_BoundingCorner[5][0] 
    = m_BoundingCorner[6][0] 
    = m_BoundingCorner[7][0] 
    = m_VoxelDimensionInX*m_NumberOfVoxelsInX;
  
  m_BoundingCorner[1][1] 
    = m_BoundingCorner[3][1] 
    = m_BoundingCorner[5][1] 
    = m_BoundingCorner[7][1] 
    = m_VoxelDimensionInY*m_NumberOfVoxelsInY;

  m_BoundingCorner[0][1] 
    = m_BoundingCorner[2][1] 
    = m_BoundingCorner[4][1] 
    = m_BoundingCorner[6][1] = 0;
 
  m_BoundingCorner[0][2] 
    = m_BoundingCorner[1][2] 
    = m_BoundingCorner[4][2] 
    = m_BoundingCorner[5][2] 
    = m_VoxelDimensionInZ*m_NumberOfVoxelsInZ;

  m_BoundingCorner[2][2] 
    = m_BoundingCorner[3][2] 
    = m_BoundingCorner[6][2] 
    = m_BoundingCorner[7][2] = 0;

#ifdef DEBUG_RAY_CAST_INTERPOLATOR_PATH
  int i;
  cout << endl << "Corners of volume: " << endl;
  for (i=0; i<8; i++)
    { 
    cout << "   " << setw(4) << i 
         << setw(8) << setprecision(4) << m_BoundingCorner[i][0] << ", "
         << setw(8) << setprecision(4) << m_BoundingCorner[i][1] << ", "
         << setw(8) << setprecision(4) << m_BoundingCorner[i][2] << endl;
    }
#endif
}



/* -----------------------------------------------------------------------
   SetBoundingBox() - Define the volume corners according a bounding box
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::SetBoundingBox(double position[3], double size[3]) const

{
  m_BoundingCorner[0][0] 
    = m_BoundingCorner[1][0] 
    = m_BoundingCorner[2][0] 
    = m_BoundingCorner[3][0] = position[0];

  m_BoundingCorner[4][0] 
    = m_BoundingCorner[5][0] 
    = m_BoundingCorner[6][0] 
    = m_BoundingCorner[7][0] = position[0] + size[0];
                                                                        
  m_BoundingCorner[1][1] 
    = m_BoundingCorner[3][1] 
    = m_BoundingCorner[5][1] 
    = m_BoundingCorner[7][1] = position[1];

  m_BoundingCorner[0][1] 
    = m_BoundingCorner[2][1] 
    = m_BoundingCorner[4][1] 
    = m_BoundingCorner[6][1] = position[1] + size[1];
                                                                       
  m_BoundingCorner[0][2] 
    = m_BoundingCorner[1][2] 
    = m_BoundingCorner[4][2] 
    = m_BoundingCorner[5][2] = position[2];

  m_BoundingCorner[2][2] 
    = m_BoundingCorner[3][2] 
    = m_BoundingCorner[6][2] 
    = m_BoundingCorner[7][2] = position[2] + size[2];

#ifdef DEBUG_RAY_CAST_INTERPOLATOR_PATH
  int i;
  cout << endl << "Corners of volume: " << endl;
  fo r(i=0; i<8; i++) 
    cout << "   " << setw(4) << i 
         << setw(8) << setprecision(4) << m_BoundingCorner[i][0] << ", "
         << setw(8) << setprecision(4) << m_BoundingCorner[i][1] << ", "
         << setw(8) << setprecision(4) << m_BoundingCorner[i][2] << endl;
#endif

  // Set the corners of the volume to the new bounding box

  this->CalcPlanesAndCorners();
}


/* -----------------------------------------------------------------------
   CalcPlanesAndCorners() - Calculate the planes and corners of the volume.
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::CalcPlanesAndCorners(void) const
{
  int j;


  // find the equations of the planes

  int c1, c2, c3, c4;
        
  for (j=0; j<6; j++) 
    {                                // loop around for planes
    switch (j) {                // which corners to take
    case 0:
      c1=1; c2=2; c3=3; c4=0;
      break;
    case 1:
      c1=4; c2=5; c3=6; c4=7;
      break;
    case 2:
      c1=5; c2=3; c3=7; c4=1;
      break;
    case 3:
      c1=2; c2=4; c3=6; c4=0;
      break;
    case 4:
      c1=1; c2=5; c3=0; c4=4;
      break;
    case 5:
      c1=3; c2=7; c3=2; c4=6;
      break;
    }

    
    double line1x, line1y, line1z;
    double line2x, line2y, line2z;

    // lines from one corner to another in x,y,z dirns
    line1x = m_BoundingCorner[c1][0] - m_BoundingCorner[c2][0];
    line2x = m_BoundingCorner[c1][0] - m_BoundingCorner[c3][0];
    
    line1y = m_BoundingCorner[c1][1] - m_BoundingCorner[c2][1];
    line2y = m_BoundingCorner[c1][1] - m_BoundingCorner[c3][1];
    
    line1z = m_BoundingCorner[c1][2] - m_BoundingCorner[c2][2];
    line2z = m_BoundingCorner[c1][2] - m_BoundingCorner[c3][2];
    
    double A, B, C, D;
        
    // take cross product
    A = line1y*line2z - line2y*line1z;
    B = line2x*line1z - line1x*line2z;
    C = line1x*line2y - line2x*line1y;

    // find constant
    D = -(   A*m_BoundingCorner[c1][0] 
             + B*m_BoundingCorner[c1][1] 
             + C*m_BoundingCorner[c1][2] );

    // initialise plane value and normalise
    m_BoundingPlane[j][0] = A/sqrt(A*A + B*B + C*C);
    m_BoundingPlane[j][1] = B/sqrt(A*A + B*B + C*C);
    m_BoundingPlane[j][2] = C/sqrt(A*A + B*B + C*C);
    m_BoundingPlane[j][3] = D/sqrt(A*A + B*B + C*C);
    
    if ( (A*A + B*B + C*C) == 0 ) 
      {
      ExceptionObject err(__FILE__, __LINE__);
      err.SetLocation( "RayCastInterpolateImageFunction" );
      err.SetDescription( "Division by zero (planes) "
                          "- CalcPlanesAndCorners().");
      throw err;
      }
    }

#ifdef DEBUG_RAY_CAST_INTERPOLATOR_PATH
  cout << endl << "Planes of volume: " << endl;
  for (j=0; j<6; j++)
    { 
    cout << "   " << setw(4) << j
         << setw(8) << setprecision(4) << m_BoundingPlane[j][0] << ", "
         << setw(8) << setprecision(4) << m_BoundingPlane[j][1] << ", "
         << setw(8) << setprecision(4) << m_BoundingPlane[j][2] << ", "
         << setw(8) << setprecision(4) << m_BoundingPlane[j][3] << endl;
    }
#endif
}


/* -----------------------------------------------------------------------
   CalcRayIntercepts() - Calculate the ray intercepts with the volume.
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
bool 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::CalcRayIntercepts() const
{
  double maxInterDist, interDist;
  double cornerVect[4][3];
  int cross[4][3], noInterFlag[6];
  int nSidesCrossed, crossFlag, c[4];
  double ax, ay, az, bx, by, bz;
  double cubeInter[6][3];
  double denom;
        
  int i,j, k; 
  int NoSides = 6;  // =6 to allow truncation: =4 to remove truncated rays


#ifdef DEBUG_RAY_CAST_INTERPOLATOR_PATH
  cout << endl << "Ray position: "
       << setw(24) << setprecision(16) << m_CurrentRayPositionInMM[0] << ", "
       << setw(24) << setprecision(16) << m_CurrentRayPositionInMM[1] << ", "
       << setw(24) << setprecision(16) << m_CurrentRayPositionInMM[2]
       << endl << "Ray direction: "
       << setw(24) << setprecision(16) << m_RayDirectionInMM[0] << ", "
       << setw(24) << setprecision(16) << m_RayDirectionInMM[1] << ", "
       << setw(24) << setprecision(16) << m_RayDirectionInMM[2] << endl << endl;
#endif

  // Calculate intercept of ray with planes

  double interceptx[6], intercepty[6], interceptz[6];
  double d[6];

  for( j=0; j<NoSides; j++) 
    {
      
    denom = (  m_BoundingPlane[j][0]*m_RayDirectionInMM[0] 
               + m_BoundingPlane[j][1]*m_RayDirectionInMM[1] 
               + m_BoundingPlane[j][2]*m_RayDirectionInMM[2]);
      
    if( (long)(denom*100) != 0 )
      {
      d[j] = -(   m_BoundingPlane[j][3] 
                  + m_BoundingPlane[j][0]*m_CurrentRayPositionInMM[0]
                  + m_BoundingPlane[j][1]*m_CurrentRayPositionInMM[1] 
                  + m_BoundingPlane[j][2]*m_CurrentRayPositionInMM[2] ) / denom;
      
      interceptx[j] = m_CurrentRayPositionInMM[0] + d[j]*m_RayDirectionInMM[0];
      intercepty[j] = m_CurrentRayPositionInMM[1] + d[j]*m_RayDirectionInMM[1];
      interceptz[j] = m_CurrentRayPositionInMM[2] + d[j]*m_RayDirectionInMM[2];
      
      noInterFlag[j] = 1;  //OK
        
#ifdef DEBUG_RAY_CAST_INTERPOLATOR_PATH
      cout << "Plane " << setw(1) << j << " intercept: "
           << setw(24) << setprecision(16) << interceptx[j] << ", "
           << setw(24) << setprecision(16) << intercepty[j] << ", "
           << setw(24) << setprecision(16) << interceptz[j] << endl;
#endif
      }
    else
      {
      noInterFlag[j] = 0;  //NOT OK
      }
    }
  

  nSidesCrossed = 0;
  for( j=0; j<NoSides; j++ ) 
    {
      
    // Work out which corners to use
      
    if( j==0 ) 
      {
      c[0] = 0; c[1] = 1; c[2] = 3; c[3] = 2;
      }
    else if( j==1 ) 
      {
      c[0] = 4; c[1] = 5; c[2] = 7; c[3] = 6;
      }
    else if( j==2 ) 
      {
      c[0] = 1; c[1] = 5; c[2] = 7; c[3] = 3;
      }
    else if( j==3 ) 
      {
      c[0] = 0; c[1] = 2; c[2] = 6; c[3] = 4;
      }
    else if( j==4 ) 
      { //TOP
      c[0] = 0; c[1] = 1; c[2] = 5; c[3] = 4;
      }
    else if( j==5 ) 
      { //BOTTOM
      c[0] = 2; c[1] = 3; c[2] = 7; c[3] = 6;
      }

    // Calculate vectors from corner of ct volume to intercept.
    for( i=0; i<4; i++ ) 
      {
      if( noInterFlag[j]==1 ) 
        {
        cornerVect[i][0] = m_BoundingCorner[c[i]][0] - interceptx[j];
        cornerVect[i][1] = m_BoundingCorner[c[i]][1] - intercepty[j];
        cornerVect[i][2] = m_BoundingCorner[c[i]][2] - interceptz[j];
        }
      else if( noInterFlag[j]==0 ) 
        {
        cornerVect[i][0] = 0;
        cornerVect[i][1] = 0;
        cornerVect[i][2] = 0;
        }

      }

    // Do cross product with these vectors
    for( i=0; i<4; i++ ) 
      {
      if( i==3 )
        {
        k = 0;
        }
      else
        {
        k = i+1;
        }
      ax = cornerVect[i][0];
      ay = cornerVect[i][1];
      az = cornerVect[i][2];
      bx = cornerVect[k][0];
      by = cornerVect[k][1];
      bz = cornerVect[k][2];

      // The int and divide by 100 are to avoid rounding errors.  If
      // these are not included then you get values fluctuating around
      // zero and so in the subsequent check, all the values are not
      // above or below zero.  NB. If you "INT" by too much here though
      // you can get problems in the corners of your volume when rays
      // are allowed to go through more than one plane.
      cross[i][0] = (int)((ay*bz - az*by)/100);
      cross[i][1] = (int)((az*bx - ax*bz)/100);
      cross[i][2] = (int)((ax*by - ay*bx)/100);
      }

    // See if a sign change occured between all these cross products
    // if not, then the ray went through this plane

    crossFlag=0;
    for( i=0; i<3; i++ ) 
      {
      if( (   cross[0][i]<=0 
              && cross[1][i]<=0 
              && cross[2][i]<=0 
              && cross[3][i]<=0)

          || (   cross[0][i]>=0 
                 && cross[1][i]>=0 
                 && cross[2][i]>=0 
                 && cross[3][i]>=0) )
        {
        crossFlag++;
        }
      }


    if( crossFlag==3 && noInterFlag[j]==1 ) 
      {
      cubeInter[nSidesCrossed][0] = interceptx[j];
      cubeInter[nSidesCrossed][1] = intercepty[j];
      cubeInter[nSidesCrossed][2] = interceptz[j];
      nSidesCrossed++;
      }

    } // End of loop over all four planes

  m_RayStartCoordInMM[0] = cubeInter[0][0];
  m_RayStartCoordInMM[1] = cubeInter[0][1];
  m_RayStartCoordInMM[2] = cubeInter[0][2];

  m_RayEndCoordInMM[0] = cubeInter[1][0];
  m_RayEndCoordInMM[1] = cubeInter[1][1];
  m_RayEndCoordInMM[2] = cubeInter[1][2];

  if( nSidesCrossed >= 5 )
    {
    cerr << "WARNING: No. of sides crossed equals: " << nSidesCrossed << endl;
    }
#ifdef DEBUG_RAY_CAST_INTERPOLATOR_PATH
  cout << endl << "No. of sides crossed: " << nSidesCrossed << endl;
#endif

  // If 'nSidesCrossed' is larger than 2, this means that the ray goes through
  // a corner of the volume and due to rounding errors, the ray is 
  // deemed to go through more than two planes.  To obtain the correct
  // start and end positions we choose the two intercept values which
  // are furthest from each other.

        
  if( nSidesCrossed >= 3 ) 
    {
    maxInterDist = 0;
    for( j=0; j<nSidesCrossed-1; j++ )
      {
      for( k=j+1; k<nSidesCrossed; k++ )
        {
        interDist = 0;
        for( i=0; i<3; i++ )
          { 
          interDist += (cubeInter[j][i] - cubeInter[k][i])*
            (cubeInter[j][i] - cubeInter[k][i]);
          }
        if( interDist > maxInterDist )
          {
          maxInterDist = interDist;

          m_RayStartCoordInMM[0] = cubeInter[j][0];
          m_RayStartCoordInMM[1] = cubeInter[j][1];
          m_RayStartCoordInMM[2] = cubeInter[j][2];
          
          m_RayEndCoordInMM[0] = cubeInter[k][0];
          m_RayEndCoordInMM[1] = cubeInter[k][1];
          m_RayEndCoordInMM[2] = cubeInter[k][2];
          }
        }
      }
    nSidesCrossed = 2;
    }

#ifdef DEBUG_RAY_CAST_INTERPOLATOR_PATH
  cout << endl 
       << "Start coordinate (mm):      "
       << setw(12) << setprecision(6) << m_RayStartCoordInMM[0] << ", "
       << setw(12) << setprecision(6) << m_RayStartCoordInMM[1] << ", "
       << setw(12) << setprecision(6) << m_RayStartCoordInMM[2] << endl 
       << "End coordinate (mm):        " 
       << setw(12) << setprecision(6) << m_RayEndCoordInMM[0] << ", "
       << setw(12) << setprecision(6) << m_RayEndCoordInMM[1] << ", "
       << setw(12) << setprecision(6) << m_RayEndCoordInMM[2] << endl;
#endif

  if (nSidesCrossed == 2 )
    {
    return true;
    }
  else
    {
    return false;
    }
}


/* -----------------------------------------------------------------------
   SetRay() - Set the position and direction of the ray
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
bool 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::SetRay(OutputPointType RayPosn, DirectionType RayDirn) const
{

  // Store the position and direction of the ray
  const double *spacing=m_Image->GetSpacing();
  SizeType dim=m_Image->GetLargestPossibleRegion().GetSize();

  // we need to translate the _center_ of the volume to the origin
  m_NumberOfVoxelsInX = dim[0];
  m_NumberOfVoxelsInY = dim[1];
  m_NumberOfVoxelsInZ = dim[2];
                      
  m_VoxelDimensionInX = spacing[0];
  m_VoxelDimensionInY = spacing[1];
  m_VoxelDimensionInZ = spacing[2];

  m_CurrentRayPositionInMM[0] = 
    RayPosn[0] + 0.5*m_VoxelDimensionInX*(double)m_NumberOfVoxelsInX; 

  m_CurrentRayPositionInMM[1] = 
    RayPosn[1] + 0.5*m_VoxelDimensionInY*(double)m_NumberOfVoxelsInY; 

  m_CurrentRayPositionInMM[2] = 
    RayPosn[2] + 0.5*m_VoxelDimensionInZ*(double)m_NumberOfVoxelsInZ; 
                                        
  m_RayDirectionInMM[0] = RayDirn[0];
  m_RayDirectionInMM[1] = RayDirn[1];
  m_RayDirectionInMM[2] = RayDirn[2];

  // Compute the ray path for this coordinate in mm

  m_ValidRay = this->CalcRayIntercepts();

  if (! m_ValidRay) 
    {
    Reset();
    return false;
    }

  // Convert the start and end coordinates of the ray to voxels

  this->EndPointsInVoxels();

  /* Calculate the ray direction vector in voxels and hence the voxel
     increment required to traverse the ray, and the number of
     interpolation points on the ray.

     This routine also shifts the coordinate frame by half a voxel for
     two of the directional components (i.e. those lying within the
     planes of voxels being traversed). */

  this->CalcDirnVector();


  /* Reduce the length of the ray until both start and end
     coordinates lie inside the volume. */

  m_ValidRay = this->AdjustRayLength();

  // Reset the iterator to the start of the ray.

  Reset();

  return m_ValidRay;
}


/* -----------------------------------------------------------------------
   EndPointsInVoxels() - Convert the endpoints to voxels
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::EndPointsInVoxels(void) const
{
  m_RayVoxelStartPosition[0] = m_RayStartCoordInMM[0]/m_VoxelDimensionInX;
  m_RayVoxelStartPosition[1] = m_RayStartCoordInMM[1]/m_VoxelDimensionInY;
  m_RayVoxelStartPosition[2] = m_RayStartCoordInMM[2]/m_VoxelDimensionInZ;

  m_RayVoxelEndPosition[0] = m_RayEndCoordInMM[0]/m_VoxelDimensionInX;
  m_RayVoxelEndPosition[1] = m_RayEndCoordInMM[1]/m_VoxelDimensionInY;
  m_RayVoxelEndPosition[2] = m_RayEndCoordInMM[2]/m_VoxelDimensionInZ;

#ifdef DEBUG_RAY_CAST_INTERPOLATOR
  printf("m_RayVoxelStartPosition[0] %24.16f = %24.16f/%24.16f\n", 
         m_RayVoxelStartPosition[0], m_RayStartCoordInMM[0], m_VoxelDimensionInX);
  printf("m_RayVoxelEndPosition[0] %24.16f = %24.16f/%24.16f\n\n", 
         m_RayVoxelEndPosition[0], m_RayEndCoordInMM[0], m_VoxelDimensionInX);
  
  printf("m_RayVoxelStartPosition[1] %24.16f = %24.16f/%24.16f\n", 
         m_RayVoxelStartPosition[1], m_RayStartCoordInMM[1], m_VoxelDimensionInY);
  printf("m_RayVoxelEndPosition[1] %24.16f = %24.16f/%24.16f\n\n", 
         m_RayVoxelEndPosition[1], m_RayEndCoordInMM[1], m_VoxelDimensionInY);
  
  printf("m_RayVoxelStartPosition[2] %24.16f = %24.16f/%24.16f\n", 
         m_RayVoxelStartPosition[2], m_RayStartCoordInMM[2], m_VoxelDimensionInZ);
  printf("m_RayVoxelEndPosition[2] %24.16f = %24.16f/%24.16f\n\n", 
         m_RayVoxelEndPosition[2], m_RayEndCoordInMM[2], m_VoxelDimensionInZ);
  
  cout << "Start coordinate (vox):     "
       << setw(12) << setprecision(6) << m_RayVoxelStartPosition[0] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelStartPosition[1] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelStartPosition[2] << endl 
       << "End coordinate (vox):       " 
       << setw(12) << setprecision(6) << m_RayVoxelEndPosition[0] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelEndPosition[1] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelEndPosition[2] << endl;
#endif
}


/* -----------------------------------------------------------------------
   CalcDirnVector() - Calculate the incremental direction vector in voxels.
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::CalcDirnVector(void) const
{
  double xNum, yNum, zNum;

#ifdef DEBUG_RAY_CAST_INTERPOLATOR
  printf("start[0] = %24.16f\n(int)start[0] = %d\nfloor(start[0]) = %f\n", 
         m_RayVoxelStartPosition[0], 
         (int)m_RayVoxelStartPosition[0], floor(m_RayVoxelStartPosition[0]));

  cout << "No. of voxels prior to dirn calc: " << m_TotalRayVoxelPlanes << endl
       << "Start posn prior to dirn calc: "
       << setw(12) << setprecision(6) << m_RayVoxelStartPosition[0] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelStartPosition[1] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelStartPosition[2] << endl 
       << "End posn prior to dirn calc:   " 
       << setw(12) << setprecision(6) << m_RayVoxelEndPosition[0] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelEndPosition[1] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelEndPosition[2] << endl 
       << "Step size prior to dirn calc:   " 
       << setw(12) << setprecision(6) << m_VoxelIncrement[0] << ", "
       << setw(12) << setprecision(6) << m_VoxelIncrement[1] << ", "
       << setw(12) << setprecision(6) << m_VoxelIncrement[2] << endl;
#endif

  // Calculate the number of voxels in each direction

  xNum = fabs(m_RayVoxelStartPosition[0] - m_RayVoxelEndPosition[0]);
  yNum = fabs(m_RayVoxelStartPosition[1] - m_RayVoxelEndPosition[1]);
  zNum = fabs(m_RayVoxelStartPosition[2] - m_RayVoxelEndPosition[2]);

#ifdef DEBUG_RAY_CAST_INTERPOLATOR
  cout << "No. voxels in each direction:   " 
       << setw(12) << setprecision(6) << xNum << ", "
       << setw(12) << setprecision(6) << yNum << ", "
       << setw(12) << setprecision(6) << zNum << endl;
#endif

  // The direction iterated in is that with the greatest number of voxels
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  // Iterate in X direction

  if( (xNum >= yNum) && (xNum >= zNum) ) 
    {

#ifdef DEBUG_RAY_CAST_INTERPOLATOR  
    cout << "Iterating in 'x' direction" << endl;
#endif

    if( m_RayVoxelStartPosition[0] < m_RayVoxelEndPosition[0] ) 
      { 
      m_VoxelIncrement[0] = 1;

      m_VoxelIncrement[1] 
        = (m_RayVoxelStartPosition[1] 
           - m_RayVoxelEndPosition[1])/(m_RayVoxelStartPosition[0] 
                                        - m_RayVoxelEndPosition[0]);
          
      m_VoxelIncrement[2] 
        = (m_RayVoxelStartPosition[2] 
           - m_RayVoxelEndPosition[2])/(m_RayVoxelStartPosition[0] 
                                        - m_RayVoxelEndPosition[0]);
      }
    else 
      {
      m_VoxelIncrement[0] = -1;

      m_VoxelIncrement[1] 
        = -(m_RayVoxelStartPosition[1] 
            - m_RayVoxelEndPosition[1])/(m_RayVoxelStartPosition[0] 
                                         - m_RayVoxelEndPosition[0]);

      m_VoxelIncrement[2] 
        = -(m_RayVoxelStartPosition[2]
            - m_RayVoxelEndPosition[2])/(m_RayVoxelStartPosition[0] 
                                         - m_RayVoxelEndPosition[0]);
      }

    // This section is to alter the start position in order to 
    // place the center of the voxels in there correct positions,
    // rather than placing them at the corner of voxels which is
    // what happens if this is not carried out.  The reason why
    // x has no -0.5 is because this is the direction we are going
    // to iterate in and therefore we wish to go from center to 
    // center rather than finding the surrounding voxels.
    
    m_RayVoxelStartPosition[1] 
      += ( (int)m_RayVoxelStartPosition[0]
           - m_RayVoxelStartPosition[0])*m_VoxelIncrement[1]*m_VoxelIncrement[0] 
      + 0.5*m_VoxelIncrement[1] - 0.5;

    m_RayVoxelStartPosition[2] 
      += ( (int)m_RayVoxelStartPosition[0] 
           - m_RayVoxelStartPosition[0])*m_VoxelIncrement[2]*m_VoxelIncrement[0]
      + 0.5*m_VoxelIncrement[2] - 0.5;

    m_RayVoxelStartPosition[0] 
      = (int)m_RayVoxelStartPosition[0] + 0.5*m_VoxelIncrement[0];

    m_TotalRayVoxelPlanes = (int)xNum;

    m_TraversalDirection = TRANSVERSE_IN_X;
    }

  // Iterate in Y direction

  else if( (yNum >= xNum) && (yNum >= zNum) ) 
    {

#ifdef DEBUG_RAY_CAST_INTERPOLATOR  
    cout << "Iterating in 'y' direction" << endl;
#endif

    if( m_RayVoxelStartPosition[1] < m_RayVoxelEndPosition[1] ) 
      {
      m_VoxelIncrement[1] = 1;

      m_VoxelIncrement[0] 
        = (m_RayVoxelStartPosition[0] 
           - m_RayVoxelEndPosition[0])/(m_RayVoxelStartPosition[1] 
                                        - m_RayVoxelEndPosition[1]);

      m_VoxelIncrement[2] 
        = (m_RayVoxelStartPosition[2] 
           - m_RayVoxelEndPosition[2])/(m_RayVoxelStartPosition[1] 
                                        - m_RayVoxelEndPosition[1]);
      }
    else 
      {
      m_VoxelIncrement[1] = -1;

      m_VoxelIncrement[0] 
        = -(m_RayVoxelStartPosition[0] 
            - m_RayVoxelEndPosition[0])/(m_RayVoxelStartPosition[1] 
                                         - m_RayVoxelEndPosition[1]);

      m_VoxelIncrement[2] 
        = -(m_RayVoxelStartPosition[2] 
            - m_RayVoxelEndPosition[2])/(m_RayVoxelStartPosition[1] 
                                         - m_RayVoxelEndPosition[1]);
      }

    m_RayVoxelStartPosition[0] 
      += ( (int)m_RayVoxelStartPosition[1] 
           - m_RayVoxelStartPosition[1])*m_VoxelIncrement[0]*m_VoxelIncrement[1] 
      + 0.5*m_VoxelIncrement[0] - 0.5;

    m_RayVoxelStartPosition[2] 
      += ( (int)m_RayVoxelStartPosition[1] 
           - m_RayVoxelStartPosition[1])*m_VoxelIncrement[2]*m_VoxelIncrement[1] 
      + 0.5*m_VoxelIncrement[2] - 0.5;

    m_RayVoxelStartPosition[1] 
      = (int)m_RayVoxelStartPosition[1] + 0.5*m_VoxelIncrement[1];
    
    m_TotalRayVoxelPlanes = (int)yNum;

    m_TraversalDirection = TRANSVERSE_IN_Y;
    }

  // Iterate in Z direction

  else 
    {

#ifdef DEBUG_RAY_CAST_INTERPOLATOR  
    cout << "Iterating in 'z' direction" << endl;
#endif

    if( m_RayVoxelStartPosition[2] < m_RayVoxelEndPosition[2] ) 
      {
      m_VoxelIncrement[2] = 1;
          
      m_VoxelIncrement[0]
        = (m_RayVoxelStartPosition[0] 
           - m_RayVoxelEndPosition[0])/(m_RayVoxelStartPosition[2] 
                                        - m_RayVoxelEndPosition[2]);

      m_VoxelIncrement[1] 
        = (m_RayVoxelStartPosition[1] 
           - m_RayVoxelEndPosition[1])/(m_RayVoxelStartPosition[2] 
                                        - m_RayVoxelEndPosition[2]);
      }
    else 
      {
      m_VoxelIncrement[2] = -1;

      m_VoxelIncrement[0] 
        = -(m_RayVoxelStartPosition[0] 
            - m_RayVoxelEndPosition[0])/(m_RayVoxelStartPosition[2] 
                                         - m_RayVoxelEndPosition[2]);

      m_VoxelIncrement[1] 
        = -(m_RayVoxelStartPosition[1] 
            - m_RayVoxelEndPosition[1])/(m_RayVoxelStartPosition[2] 
                                         - m_RayVoxelEndPosition[2]);
      }

    m_RayVoxelStartPosition[0] 
      += ( (int)m_RayVoxelStartPosition[2] 
           - m_RayVoxelStartPosition[2])*m_VoxelIncrement[0]*m_VoxelIncrement[2] 
      + 0.5*m_VoxelIncrement[0] - 0.5;

    m_RayVoxelStartPosition[1] 
      += ( (int)m_RayVoxelStartPosition[2] 
           - m_RayVoxelStartPosition[2])*m_VoxelIncrement[1]*m_VoxelIncrement[2] 
      + 0.5*m_VoxelIncrement[1] - 0.5;

    m_RayVoxelStartPosition[2] 
      = (int)m_RayVoxelStartPosition[2] + 0.5*m_VoxelIncrement[2];
    
    m_TotalRayVoxelPlanes = (int)zNum;

    m_TraversalDirection = TRANSVERSE_IN_Z;
    }

#ifdef DEBUG_RAY_CAST_INTERPOLATOR
  cout << "Voxel increment:   " 
       << setw(12) << setprecision(6) << m_VoxelIncrement[0] << ", "
       << setw(12) << setprecision(6) << m_VoxelIncrement[1] << ", "
       << setw(12) << setprecision(6) << m_VoxelIncrement[2] << endl
       << "Adjusted start coord (vox): "
       << setw(12) << setprecision(6) << m_RayVoxelStartPosition[0] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelStartPosition[1] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelStartPosition[2] << endl 
       << "Adjusted end coord (vox):   " 
       << setw(12) << setprecision(6) << m_RayVoxelEndPosition[0] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelEndPosition[1] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelEndPosition[2] << endl << endl;
#endif
}


/* -----------------------------------------------------------------------
   AdjustRayLength() - Ensure that the ray lies within the volume
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
bool 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::AdjustRayLength(void) const
{
  bool startOK, endOK;

  int Istart[3];
  int Idirn[3];

#ifdef DEBUG_RAY_CAST_INTERPOLATOR
  cout << "No. of voxels prior to clipping: " << m_TotalRayVoxelPlanes << endl
       << "Start posn prior to clipping: "
       << setw(12) << setprecision(6) << m_RayVoxelStartPosition[0] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelStartPosition[1] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelStartPosition[2] << endl 
       << "End posn prior to clipping:   " 
       << setw(12) << setprecision(6) << m_RayVoxelEndPosition[0] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelEndPosition[1] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelEndPosition[2] << endl 
       << "Step size prior to clipping:   " 
       << setw(12) << setprecision(6) << m_VoxelIncrement[0] << ", "
       << setw(12) << setprecision(6) << m_VoxelIncrement[1] << ", "
       << setw(12) << setprecision(6) << m_VoxelIncrement[2] << endl;
#endif

  if (m_TraversalDirection == TRANSVERSE_IN_X) 
    {
    Idirn[0] = 0;    
    Idirn[1] = 1;    
    Idirn[2] = 1;
    }
  else if (m_TraversalDirection == TRANSVERSE_IN_Y) 
    {
    Idirn[0] = 1;    
    Idirn[1] = 0;    
    Idirn[2] = 1;
    }
  else if (m_TraversalDirection == TRANSVERSE_IN_Z) 
    {
    Idirn[0] = 1;    
    Idirn[1] = 1;    
    Idirn[2] = 0;
    }
  else 
    {
    ExceptionObject err(__FILE__, __LINE__);
    err.SetLocation( "RayCastInterpolateImageFunction" );
    err.SetDescription( "The ray traversal direction is unset "
                        "- AdjustRayLength().");
    throw err;
    return false;
    }


  do 
    {

    startOK = false;
    endOK = false;

    Istart[0] = (int) floor(m_RayVoxelStartPosition[0]);
    Istart[1] = (int) floor(m_RayVoxelStartPosition[1]);
    Istart[2] = (int) floor(m_RayVoxelStartPosition[2]);

#ifdef DEBUG_RAY_CAST_INTERPOLATOR
    cout << endl << "Start test position (vox): " << endl 

         << "Istart[0]: " << Istart[0] << " = (int) floor(" 
         << m_RayVoxelStartPosition[0] << ")" << endl

         << "Istart[1]: " << Istart[1] << " = (int) floor(" 
         << m_RayVoxelStartPosition[1] << ")" << endl

         << "Istart[2]: " << Istart[2] << " = (int) floor(" 
         << m_RayVoxelStartPosition[2] << ")" << endl;
#endif

    if( (Istart[0] >= 0) && (Istart[0] + Idirn[0] < m_NumberOfVoxelsInX) &&
        (Istart[1] >= 0) && (Istart[1] + Idirn[1] < m_NumberOfVoxelsInY) &&
        (Istart[2] >= 0) && (Istart[2] + Idirn[2] < m_NumberOfVoxelsInZ) ) 
      {

      startOK = true;
      }
    else 
      {
      m_RayVoxelStartPosition[0] += m_VoxelIncrement[0];
      m_RayVoxelStartPosition[1] += m_VoxelIncrement[1];
      m_RayVoxelStartPosition[2] += m_VoxelIncrement[2];

      m_TotalRayVoxelPlanes--;
      }

    Istart[0] = (int) floor(m_RayVoxelStartPosition[0] 
                            + m_TotalRayVoxelPlanes*m_VoxelIncrement[0]);

    Istart[1] = (int) floor(m_RayVoxelStartPosition[1] 
                            + m_TotalRayVoxelPlanes*m_VoxelIncrement[1]);

    Istart[2] = (int) floor(m_RayVoxelStartPosition[2] 
                            + m_TotalRayVoxelPlanes*m_VoxelIncrement[2]);
  
#ifdef DEBUG_RAY_CAST_INTERPOLATOR
    cout << endl << "End test position (vox): " << endl 

         << "Istart[0]: " << Istart[0] << " = (int) floor(" 
         << m_RayVoxelStartPosition[0] << " + " 
         << m_TotalRayVoxelPlanes*m_VoxelIncrement[0] << ")" << endl

         << "Istart[1]: " << Istart[1] << " = (int) floor(" 
         << m_RayVoxelStartPosition[1] << " + " 
         << m_TotalRayVoxelPlanes*m_VoxelIncrement[1] << ")" << endl

         << "Istart[2]: " << Istart[2] << " = (int) floor(" 
         << m_RayVoxelStartPosition[2] << " + " 
         << m_TotalRayVoxelPlanes*m_VoxelIncrement[2] << ")" << endl;
#endif

    if( (Istart[0] >= 0) && (Istart[0] + Idirn[0] < m_NumberOfVoxelsInX) &&
        (Istart[1] >= 0) && (Istart[1] + Idirn[1] < m_NumberOfVoxelsInY) &&
        (Istart[2] >= 0) && (Istart[2] + Idirn[2] < m_NumberOfVoxelsInZ) ) 
      {

      endOK = true;
      }
    else 
      {
      m_TotalRayVoxelPlanes--;
      }        

    } while ( (! (startOK && endOK)) && (m_TotalRayVoxelPlanes > 1) );


#ifdef DEBUG_RAY_CAST_INTERPOLATOR
  cout << endl << "Start within volume (vox): "
       << setw(12) << setprecision(6) << m_RayVoxelStartPosition[0] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelStartPosition[1] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelStartPosition[2]
       << endl << "End within volume (vox):   " 
       << setw(12) << setprecision(6) << m_RayVoxelEndPosition[0] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelEndPosition[1] << ", "
       << setw(12) << setprecision(6) << m_RayVoxelEndPosition[2] 
       << endl << "Valid ray?:   " << (startOK && endOK) << endl;
#endif

  return (startOK && endOK);
}


/* -----------------------------------------------------------------------
   Reset() - Reset the iterator to the start of the ray.
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::Reset(void) const
{
  int i;

  m_NumVoxelPlanesTraversed = -1;
    
  // If this is a valid ray...

  if (m_ValidRay) 
    {
    for (i=0; i<3; i++)
      {
      m_Position3Dvox[i] = m_RayVoxelStartPosition[i];
      }
    this->InitialiseVoxelPointers();
    }

  // otherwise set parameters to zero
  
  else 
    {
    for (i=0; i<3; i++)
      {
      m_RayVoxelStartPosition[i] = 0.;
      }
    for (i=0; i<3; i++)
      {
      m_RayVoxelEndPosition[i] = 0.;
      }    
    for (i=0; i<3; i++)
      {
      m_VoxelIncrement[i] = 0.;
      }      
    m_TraversalDirection = UNDEFINED_DIRECTION;
    
    m_TotalRayVoxelPlanes = 0;
    
    for (i=0; i<4; i++)
      {
      m_RayIntersectionVoxels[i] = 0;
      }
    for (i=0; i<3; i++)
      {
      m_RayIntersectionVoxelIndex[i] = 0;
      }
    }
}


/* -----------------------------------------------------------------------
   InitialiseVoxelPointers() - Obtain pointers to the first four voxels
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::InitialiseVoxelPointers(void) const
{
  IndexType index;

  int Ix, Iy, Iz;

  Ix = (int)(m_RayVoxelStartPosition[0]);
  Iy = (int)(m_RayVoxelStartPosition[1]);
  Iz = (int)(m_RayVoxelStartPosition[2]);

  m_RayIntersectionVoxelIndex[0] = Ix;
  m_RayIntersectionVoxelIndex[1] = Iy;
  m_RayIntersectionVoxelIndex[2] = Iz;

  switch( m_TraversalDirection ) 
    
    {
    case TRANSVERSE_IN_X: 
    {

    if( (Ix >= 0) && (Ix     < m_NumberOfVoxelsInX) && 
        (Iy >= 0) && (Iy + 1 < m_NumberOfVoxelsInY) && 
        (Iz >= 0) && (Iz + 1 < m_NumberOfVoxelsInZ)) 
      {

      index[0]=Ix; index[1]=Iy; index[2]=Iz; 
      m_RayIntersectionVoxels[0]
        = m_Image->GetBufferPointer() + m_Image->ComputeOffset(index);

      index[0]=Ix; index[1]=Iy+1; index[2]=Iz; 
      m_RayIntersectionVoxels[1] 
        = ( m_Image->GetBufferPointer() + m_Image->ComputeOffset(index) );

      index[0]=Ix; index[1]=Iy; index[2]=Iz+1; 
      m_RayIntersectionVoxels[2] 
        = ( m_Image->GetBufferPointer() + m_Image->ComputeOffset(index) );

      index[0]=Ix; index[1]=Iy+1; index[2]=Iz+1; 
      m_RayIntersectionVoxels[3] 
        = ( m_Image->GetBufferPointer() + m_Image->ComputeOffset(index) );
      }
    else
      m_RayIntersectionVoxels[0] 
        = m_RayIntersectionVoxels[1] 
        = m_RayIntersectionVoxels[2] 
        = m_RayIntersectionVoxels[3] = NULL;

    break;
    }

    case TRANSVERSE_IN_Y: 
    {

    if( (Ix >= 0) && (Ix + 1 < m_NumberOfVoxelsInX) && 
        (Iy >= 0) && (Iy     < m_NumberOfVoxelsInY) && 
        (Iz >= 0) && (Iz + 1 < m_NumberOfVoxelsInZ)) 
      {

      index[0]=Ix; index[1]=Iy; index[2]=Iz; 
      m_RayIntersectionVoxels[0] = ( m_Image->GetBufferPointer()
                                     + m_Image->ComputeOffset(index) );

      index[0]=Ix+1; index[1]=Iy; index[2]=Iz; 
      m_RayIntersectionVoxels[1] = ( m_Image->GetBufferPointer()
                                     + m_Image->ComputeOffset(index) );

      index[0]=Ix; index[1]=Iy; index[2]=Iz+1; 
      m_RayIntersectionVoxels[2] = ( m_Image->GetBufferPointer()
                                     + m_Image->ComputeOffset(index) );

      index[0]=Ix+1; index[1]=Iy; index[2]=Iz+1; 
      m_RayIntersectionVoxels[3] = ( m_Image->GetBufferPointer()
                                     + m_Image->ComputeOffset(index) );
      }
    else
      m_RayIntersectionVoxels[0] 
        = m_RayIntersectionVoxels[1] 
        = m_RayIntersectionVoxels[2] 
        = m_RayIntersectionVoxels[3] = NULL;        

    break;
    }

    case TRANSVERSE_IN_Z: 
    {

    if( (Ix >= 0) && (Ix + 1 < m_NumberOfVoxelsInX)   && 
        (Iy >= 0) && (Iy + 1 < m_NumberOfVoxelsInY) && 
        (Iz >= 0) && (Iz     < m_NumberOfVoxelsInZ)) 
      {

      index[0]=Ix; index[1]=Iy; index[2]=Iz; 
      m_RayIntersectionVoxels[0] = ( m_Image->GetBufferPointer()
                                     + m_Image->ComputeOffset(index) );

      index[0]=Ix+1; index[1]=Iy; index[2]=Iz; 
      m_RayIntersectionVoxels[1] = ( m_Image->GetBufferPointer()
                                     + m_Image->ComputeOffset(index) );

      index[0]=Ix; index[1]=Iy+1; index[2]=Iz; 
      m_RayIntersectionVoxels[2] = ( m_Image->GetBufferPointer()
                                     + m_Image->ComputeOffset(index) );

      index[0]=Ix+1; index[1]=Iy+1; index[2]=Iz; 
      m_RayIntersectionVoxels[3] = ( m_Image->GetBufferPointer()
                                     + m_Image->ComputeOffset(index) );

      }
    else
      m_RayIntersectionVoxels[0] 
        = m_RayIntersectionVoxels[1]
        = m_RayIntersectionVoxels[2]
        = m_RayIntersectionVoxels[3] = NULL;

    break;
    }

    default: 
    {
    ExceptionObject err(__FILE__, __LINE__);
    err.SetLocation( "RayCastInterpolateImageFunction" );
    err.SetDescription( "The ray traversal direction is unset "
                        "- InitialiseVoxelPointers().");
    throw err;
    return;
    }
    }  
}


/* -----------------------------------------------------------------------
   NextPoint() - Step along the ray.
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
bool 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::NextPoint(void) const
{
  if (! m_ValidRay) 
    {
    return false;
    }
  // The first time this routine is called 
  // 'm_NumVoxelPlanesTraversed' should equal -1.

  m_NumVoxelPlanesTraversed++;


  // Have we finished stepping along the ray?

  if (m_NumVoxelPlanesTraversed == m_TotalRayVoxelPlanes)
    {
    return false;
    }

  // Are we trying to step beyond the end of the ray?

  if (m_NumVoxelPlanesTraversed > m_TotalRayVoxelPlanes) 
    {
    ExceptionObject err(__FILE__, __LINE__);
    err.SetLocation( "RayCastInterpolateImageFunction" );
    err.SetDescription( "The end of the ray has already been reached - NextPoint().");
    throw err;
    return false;
    }


  /* If 'm_NumVoxelPlanesTraversed' is greater than zero then this isn't the first voxel and
     we need to increment the voxel pointers. This means that each time this
     routine exits the four voxel pointers will be pointing at the 
     correct voxels surrounding the current position on the ray. */

  if (m_NumVoxelPlanesTraversed > 0) 
    {
    this->IncrementVoxelPointers();
    }
  return true;
}


/* -----------------------------------------------------------------------
   IncrementVoxelPointers() - Increment the voxel pointers
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::IncrementVoxelPointers(void) const
{
  double xBefore = m_Position3Dvox[0];
  double yBefore = m_Position3Dvox[1];
  double zBefore = m_Position3Dvox[2];

  m_Position3Dvox[0] += m_VoxelIncrement[0];
  m_Position3Dvox[1] += m_VoxelIncrement[1];
  m_Position3Dvox[2] += m_VoxelIncrement[2];

  int dx = ((int) m_Position3Dvox[0]) - ((int) xBefore);
  int dy = ((int) m_Position3Dvox[1]) - ((int) yBefore);
  int dz = ((int) m_Position3Dvox[2]) - ((int) zBefore);

  m_RayIntersectionVoxelIndex[0] += dx;
  m_RayIntersectionVoxelIndex[1] += dy;
  m_RayIntersectionVoxelIndex[2] += dz;

  int m_TotalRayVoxelPlanes 
    = dx + dy*m_NumberOfVoxelsInX + dz*m_NumberOfVoxelsInX*m_NumberOfVoxelsInY;

  m_RayIntersectionVoxels[0] += m_TotalRayVoxelPlanes;
  m_RayIntersectionVoxels[1] += m_TotalRayVoxelPlanes;
  m_RayIntersectionVoxels[2] += m_TotalRayVoxelPlanes;
  m_RayIntersectionVoxels[3] += m_TotalRayVoxelPlanes;
}


/* -----------------------------------------------------------------------
   GetCurrentIntensity() - Get the intensity of the current ray point.
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
double 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::GetCurrentIntensity(void) const
{
  double a, b, c, d;
  double y, z;

  if (! m_ValidRay)
    {
    return 0;
    }
  a = (double) (*m_RayIntersectionVoxels[0]);
  b = (double) (*m_RayIntersectionVoxels[1] - a);
  c = (double) (*m_RayIntersectionVoxels[2] - a);
  d = (double) (*m_RayIntersectionVoxels[3] - a - b - c);
  
  switch( m_TraversalDirection ) 
    {
    case TRANSVERSE_IN_X: {

    y = m_Position3Dvox[1] - floor(m_Position3Dvox[1]);
    z = m_Position3Dvox[2] - floor(m_Position3Dvox[2]);
    break;                                            
    }                                                    
                                                    
    case TRANSVERSE_IN_Y: {                            
                                                          
    y = m_Position3Dvox[0] - floor(m_Position3Dvox[0]);
    z = m_Position3Dvox[2] - floor(m_Position3Dvox[2]);
    break;                                            
    }                                                    
                                                        
    case TRANSVERSE_IN_Z: {                            
                                                          
    y = m_Position3Dvox[0] - floor(m_Position3Dvox[0]);
    z = m_Position3Dvox[1] - floor(m_Position3Dvox[1]);
    break;
    }

    default: 
    {
    ExceptionObject err(__FILE__, __LINE__);
    err.SetLocation( "RayCastInterpolateImageFunction" );
    err.SetDescription( "The ray traversal direction is unset "
                        "- GetCurrentIntensity().");
    throw err;
    return 0;
    }
    }
  
  return a + b*y + c*z + d*y*z;
}


/* -----------------------------------------------------------------------
   GetCurrentDensity() - The current density of intensity above threshold
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
double 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::GetCurrentDensity(double threshold) const
{
  double vox0, vox1, vox2, vox3;
  double a, b, c, d;
  double y, z;

  if (! m_ValidRay)
    {
    return 0;
    }
  vox0 = (*m_RayIntersectionVoxels[0] > threshold) ? 1. : 0.;
  vox1 = (*m_RayIntersectionVoxels[1] > threshold) ? 1. : 0.;
  vox2 = (*m_RayIntersectionVoxels[2] > threshold) ? 1. : 0.;
  vox3 = (*m_RayIntersectionVoxels[3] > threshold) ? 1. : 0.;
  
  a = vox0;
  b = vox1 - a;
  c = vox2 - a;
  d = vox3 - a - b - c;
  
  switch( m_TraversalDirection ) 
    {
    case TRANSVERSE_IN_X: {

    y = (double)(m_Position3Dvox[1] - (int)m_Position3Dvox[1]);
    z = (double)(m_Position3Dvox[2] - (int)m_Position3Dvox[2]);
    break;
    }

    case TRANSVERSE_IN_Y: {
      
    y = (double)(m_Position3Dvox[0] - (int)m_Position3Dvox[0]);
    z = (double)(m_Position3Dvox[2] - (int)m_Position3Dvox[2]);
    break;
    }
    
    case TRANSVERSE_IN_Z: {
      
    y = (double)(m_Position3Dvox[0] - (int)m_Position3Dvox[0]);
    z = (double)(m_Position3Dvox[1] - (int)m_Position3Dvox[1]);
    break;
    }

    default: {
    ExceptionObject err(__FILE__, __LINE__);
    err.SetLocation( "RayCastInterpolateImageFunction" );
    err.SetDescription( "The ray traversal direction is unset "
                        "- GetCurrentDensity().");
    throw err;
    return 0;
    }
    }
  
  return a + b*y + c*z + d*y*z;
}


/* -----------------------------------------------------------------------
   IncrementIntensities() - Increment the intensities of the current ray point
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::IncrementIntensities(double increment) const
{
  short inc = (short) floor(increment + 0.5);

  if (! m_ValidRay)
    {
    return;
    }
  *m_RayIntersectionVoxels[0] += inc;
  *m_RayIntersectionVoxels[1] += inc;
  *m_RayIntersectionVoxels[2] += inc;
  *m_RayIntersectionVoxels[3] += inc;

  return;
}


/* -----------------------------------------------------------------------
   IntegrateAboveThreshold() - Integrate intensities above a threshold.
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
bool 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::IntegrateAboveThreshold(double &integral, double threshold) const
{
  int iRayPoint = 0;
  double intensity;
  double posn3D_x, posn3D_y, posn3D_z;

  integral = 0.;

  // Check if this is a valid ray
  
  if (! m_ValidRay)
    {
    return false;
    }
  /* Step along the ray as quickly as possible 
     integrating the interpolated intensities. */

#ifdef DEBUG_RAY_CAST_INTERPOLATOR
  cout.setf(ios::fixed);

  cout << endl << "Number of voxels: " << this->GetNumberOfVoxels() << endl
    
       << "   " << setw(4) << "n" << " ("
    
       << setw(8) << "x" << ", "
       << setw(8) << "y" << ", "
       << setw(8) << "z" << ") "
    
       << setw(8) << "value" << endl;
#endif

  for (m_NumVoxelPlanesTraversed=0; 
       m_NumVoxelPlanesTraversed<m_TotalRayVoxelPlanes; 
       m_NumVoxelPlanesTraversed++) 
    {
    intensity = this->GetCurrentIntensity();

#ifdef DEBUG_RAY_CAST_INTERPOLATOR
    this->GetCurrentCoord3D(posn3D_x, posn3D_y, posn3D_z);

    cout << "   " << setw(4) << iRayPoint++ << " ("

         << setw(8) << setprecision(4) << posn3D_x << ", "
         << setw(8) << setprecision(4) << posn3D_y << ", "
         << setw(8) << setprecision(4) << posn3D_z << ") "
                                         
         << setw(8) << setprecision(4) << intensity << endl;
#endif

    if (intensity > threshold)
      {
      integral += intensity - threshold;
      }
    this->IncrementVoxelPointers();
    }

  /* The ray passes through the volume one plane of voxels at a time,
     however, if its moving diagonally the ray points will be further
     apart so account for this by scaling by the distance moved. */

  integral *= this->GetRayPointSpacing();

#ifdef DEBUG_RAY_CAST_INTERPOLATOR
  cout << "Integrated intensities above threshold: " << integral << endl
       << "(scaled by ray point spacing: " << this->GetRayPointSpacing() << ")" 
       << endl;
#endif

  return true;
}


/* -----------------------------------------------------------------------
   GetCurrentCoord3D() - Get the current ray position in mm.
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::GetCurrentCoord3D(double &x, double &y, double &z) const
{
  if (! m_ValidRay) 
    {
    x = 0.;
    y = 0.;
    z = 0.;

    return;
    }

  switch( m_TraversalDirection ) 
    {
    case TRANSVERSE_IN_X: {
    x = m_Position3Dvox[0]*m_VoxelDimensionInX;
    y = (m_Position3Dvox[1] + 0.5)*m_VoxelDimensionInY;
    z = (m_Position3Dvox[2] + 0.5)*m_VoxelDimensionInZ;

    break;
    }

    case TRANSVERSE_IN_Y: {
    x = (m_Position3Dvox[0] + 0.5)*m_VoxelDimensionInX;
    y = m_Position3Dvox[1]*m_VoxelDimensionInY;
    z = (m_Position3Dvox[2] + 0.5)*m_VoxelDimensionInZ;

    break;
    }

    case TRANSVERSE_IN_Z: {
    x = (m_Position3Dvox[0] + 0.5)*m_VoxelDimensionInX;
    y = (m_Position3Dvox[1] + 0.5)*m_VoxelDimensionInY;
    z = m_Position3Dvox[2]*m_VoxelDimensionInZ;

    break;
    }

    default: 
    {
    ExceptionObject err(__FILE__, __LINE__);
    err.SetLocation( "RayCastInterpolateImageFunction" );
    err.SetDescription( "The ray traversal direction is unset "
                        "- GetCurrentCoord3D().");
    throw err;
    return;
    }
    }  
}


/* -----------------------------------------------------------------------
   GetCurrentCoord3D() - Get the current ray position in voxels.
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::GetCurrentVoxelCoord3D(double &x, double &y, double &z) const
{
  if (! m_ValidRay) 
    {
    x = 0.;
    y = 0.;
    z = 0.;

    return;
    }

  switch( m_TraversalDirection ) 
    {
    case TRANSVERSE_IN_X: {
    x = m_Position3Dvox[0];
    y = m_Position3Dvox[1] + 0.5;
    z = m_Position3Dvox[2] + 0.5;

    break;
    }

    case TRANSVERSE_IN_Y: {
    x = m_Position3Dvox[0] + 0.5;
    y = m_Position3Dvox[1];
    z = m_Position3Dvox[2] + 0.5;

    break;
    }

    case TRANSVERSE_IN_Z: {
    x = m_Position3Dvox[0] + 0.5;
    y = m_Position3Dvox[1] + 0.5;
    z = m_Position3Dvox[2];

    break;
    }

    default: 
    {
    ExceptionObject err(__FILE__, __LINE__);
    err.SetLocation( "RayCastInterpolateImageFunction" );
    err.SetDescription( "The ray traversal direction is unset "
                        "- GetVoxelCurrentCoord3D().");
    throw err;
    return;
    }
    }  

}


/* -----------------------------------------------------------------------
   GetCurrentCoord3D() - Get the current ray position in voxels.
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::GetCurrentVoxels(PixelType &voxel1, PixelType &voxel2, 
                   PixelType &voxel3, PixelType &voxel4) const
{
  if (m_ValidRay && m_RayIntersectionVoxels) 
    {
    voxel1 = *m_RayIntersectionVoxels[0];
    voxel2 = *m_RayIntersectionVoxels[1];
    voxel3 = *m_RayIntersectionVoxels[2];
    voxel4 = *m_RayIntersectionVoxels[3];
    }
  else 
    {
    voxel1 = 0;
    voxel2 = 0;
    voxel3 = 0;
    voxel4 = 0;
    } 
}



/* -----------------------------------------------------------------------
   Evaluate at image index position
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
typename RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::OutputType
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::Evaluate( const PointType& point ) const
{
  double integral = 0;

  OutputPointType transformedFocalPoint 
    = m_Transform->TransformPoint( m_FocalPoint );

  DirectionType direction = transformedFocalPoint - point;
  
  this->ZeroState();
  this->Initialise();

  this->SetRay(point, direction);
  this->IntegrateAboveThreshold(integral, m_Threshold);

  return ( static_cast<OutputType>( integral ));
}



/* -----------------------------------------------------------------------
   ZeroState() - Set the default (zero) state of the object
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void  
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::ZeroState()  const
{
  int i;

  m_ValidRay = false;

  m_NumberOfVoxelsInX = 0;
  m_NumberOfVoxelsInY = 0;
  m_NumberOfVoxelsInZ = 0;

  m_VoxelDimensionInX = 0;
  m_VoxelDimensionInY = 0;
  m_VoxelDimensionInZ = 0;  

  for (i=0; i<3; i++)
    {
    m_CurrentRayPositionInMM[i] = 0.; 
    }
  for (i=0; i<3; i++)
    {
    m_RayDirectionInMM[i] = 0.;
    }
  for (i=0; i<3; i++)
    {
    m_RayVoxelStartPosition[i] = 0.;
    }
  for (i=0; i<3; i++)
    {
    m_RayVoxelEndPosition[i] = 0.;
    }
  for (i=0; i<3; i++)
    {
    m_VoxelIncrement[i] = 0.;
    }
  m_TraversalDirection = UNDEFINED_DIRECTION;

  m_TotalRayVoxelPlanes = 0;
  m_NumVoxelPlanesTraversed = -1;
  
  for (i=0; i<4; i++)
    {
    m_RayIntersectionVoxels[i] = 0;
    }
  for (i=0; i<3; i++)
    {
    m_RayIntersectionVoxelIndex[i] = 0;
    }
}


} // namespace itk


#endif
