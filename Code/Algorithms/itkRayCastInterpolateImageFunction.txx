/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkRayCastInterpolateImageFunction.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkRayCastInterpolateImageFunction_txx
#define _itkRayCastInterpolateImageFunction_txx

#include "itkRayCastInterpolateImageFunction.h"

#include "vnl/vnl_math.h"


// Put the helper class in an anonymous namespace so that it is not
// exposed to the user
namespace {

/** Helper class to maintain state when casting a ray.  This helper
 * class keeps the RayCastInterpolateImageFunction thread safe.
 */
template <class TInputImage, class TCoordRep = float>
class RayCastHelper
{
public:
  /** Constants for the image dimensions */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** 
   * Type of the Transform Base class 
   * The fixed image should be a 3D image
   */
  typedef itk::Transform<TCoordRep,3,3> TransformType;

  typedef typename TransformType::Pointer            TransformPointer;
  typedef typename TransformType::InputPointType     InputPointType;
  typedef typename TransformType::OutputPointType    OutputPointType;
  typedef typename TransformType::ParametersType     TransformParametersType;
  typedef typename TransformType::JacobianType       TransformJacobianType;

  typedef typename TInputImage::SizeType             SizeType;
  typedef itk::Vector<TCoordRep, 3>                  DirectionType;
  typedef itk::Point<TCoordRep, 3>                   PointType;

  typedef TInputImage InputImageType;
  typedef typename InputImageType::PixelType         PixelType;
  typedef typename InputImageType::IndexType         IndexType;

  /**
   * Set the image class
   */
  void SetImage(const InputImageType *input)
    {
      m_Image = input;
    }
    
  /** 
   *  Initialise the ray using the position and direction of a line.
   *
   *  \param RayPosn       The position of the ray in 3D (mm).
   *  \param RayDirn       The direction of the ray in 3D (mm).
   *
   *  \return True if this is a valid ray.
   */
  bool SetRay(OutputPointType RayPosn, DirectionType RayDirn);
    

  /** \brief
   *  Integrate the interpolated intensities along the ray and
   *  return the result.
   *
   *  This routine can be called after instantiating the ray and
   *  calling SetProjectionCoord2D() or Reset(). It may then be called
   *  as many times thereafter for different 2D projection
   *  coordinates.
   *
   *  \param integral      The integrated intensities along the ray.
   *
   * \return True if a valid ray was specified.  
   */
  bool Integrate(double &integral) {
    return IntegrateAboveThreshold(integral, 0);
  };
    

  /** \brief
   * Integrate the interpolated intensities above a given threshold,
   * along the ray and return the result. 
   *
   * This routine can be called after instantiating the ray and
   * calling SetProjectionCoord2D() or Reset(). It may then be called
   * as many times thereafter for different 2D projection
   * coordinates.
   *
   * \param integral      The integrated intensities along the ray.
   * \param threshold     The integration threshold [default value: 0] 
   *
   * \return True if a valid ray was specified.  
   */
  bool IntegrateAboveThreshold(double &integral, double threshold);

  /** \brief
   * Increment each of the intensities of the 4 planar voxels
   * surrounding the current ray point.
   *
   * \parameter increment      Intensity increment for each of the current 4 voxels
   */
  void IncrementIntensities(double increment=1);

  /// Reset the iterator to the start of the ray.
  void Reset(void);
    
  /// Return the interpolated intensity of the current ray point.
  double GetCurrentIntensity(void) const;
    
  /// Return the ray point spacing in mm
  double GetRayPointSpacing(void) const {
    typename InputImageType::SpacingType spacing=this->m_Image->GetSpacing();
      
    if (m_ValidRay)
      return sqrt(  m_VoxelIncrement[0]*spacing[0]*m_VoxelIncrement[0]*spacing[0]
                    + m_VoxelIncrement[1]*spacing[1]*m_VoxelIncrement[1]*spacing[1]
                    + m_VoxelIncrement[2]*spacing[2]*m_VoxelIncrement[2]*spacing[2] );
    else
      return 0.;
  };

  /// Set the initial zero state of the object 
  void ZeroState();

  /// Initialise the object
  void Initialise(void);
    
protected:
  /// Calculate the endpoint coordinats of the ray in voxels.
  void EndPointsInVoxels(void);
    
  /** 
   * Calculate the incremental direction vector in voxels, 'dVoxel',
   * required to traverse the ray. 
   */
  void CalcDirnVector(void);
    
  /** 
   * Reduce the length of the ray until both start and end
   * coordinates lie inside the volume.
   *
   * \return True if a valid ray has been, false otherwise.
   */
  bool AdjustRayLength(void);
    
  /** 
   *   Obtain pointers to the four voxels surrounding the point where the ray
   *   enters the volume. 
   */ 
  void InitialiseVoxelPointers(void);
    
  /// Increment the voxel pointers surrounding the current point on the ray.
  void IncrementVoxelPointers(void);
    
  /// Record volume dimensions and resolution
  void RecordVolumeDimensions(void);
    
  /// Define the corners of the volume
  void DefineCorners(void);
    
  /** \brief
   * Calculate the planes which define the volume.
   *
   * Member function to calculate the equations of the planes of 4 of
   * the sides of the volume, calculate the positions of the 8 corners
   * of the volume in mm in World, also calculate the values of the
   * slopes of the lines which go to make up the volume( defined as
   * lines in cube x,y,z dirn and then each of these lines has a slope
   * in the world x,y,z dirn [3]) and finally also to return the length
   * of the sides of the lines in mm. 
   */
  void CalcPlanesAndCorners(void);
    
  /** \brief
   *  Calculate the ray intercepts with the volume.
   *
   *  See where the ray cuts the volume, check that truncation does not occur,
   *  if not, then start ray where it first intercepts the volume and set 
   *  x_max to be where it leaves the volume.
   *
   *  \return True if a valid ray has been specified, false otherwise.
   */
  bool CalcRayIntercepts(void);

  /**
   *   The ray is traversed by stepping in the axial direction
   *   that enables the greatest number of planes in the volume to be
   *   intercepted.
   */
  typedef enum {
    UNDEFINED_DIRECTION=0,        //!< Undefined                         
    TRANSVERSE_IN_X,              //!< x
    TRANSVERSE_IN_Y,              //!< y
    TRANSVERSE_IN_Z,              //!< z
    LAST_DIRECTION
  } TraversalDirection;

  // Cache the image in the structure. Skip the smart pointer for
  // efficiency. This inner class will go in/out of scope with every
  // call to Evaluate()
  const InputImageType *m_Image;
    
  /// Flag indicating whether the current ray is valid
  bool m_ValidRay;
    
  /** \brief
   * The start position of the ray in voxels. 
   *
   * NB. Two of the components of this coordinate (i.e. those lying within
   * the planes of voxels being traversed) will be shifted by half a
   * voxel. This enables indices of the neighbouring voxels within the plane
   * to be determined by simply casting to 'int' and optionally adding 1.
   */
  double m_RayVoxelStartPosition[3];
    
  /** \brief
   * The end coordinate of the ray in voxels.
   *
   * NB. Two of the components of this coordinate (i.e. those lying within
   * the planes of voxels being traversed) will be shifted by half a
   * voxel. This enables indices of the neighbouring voxels within the plane
   * to be determined by simply casting to 'int' and optionally adding 1.
   */
  double m_RayVoxelEndPosition[3];
    
    
  /** \brief
   * The current coordinate on the ray in voxels.
   *
   * NB. Two of the components of this coordinate (i.e. those lying within
   * the planes of voxels being traversed) will be shifted by half a
   * voxel. This enables indices of the neighbouring voxels within the plane
   * to be determined by simply casting to 'int' and optionally adding 1.
   */
  double m_Position3Dvox[3];
    
  /** The incremental direction vector of the ray in voxels. */
  double m_VoxelIncrement[3];
    
  /// The direction in which the ray is incremented thorough the volume (x, y or z).
  TraversalDirection m_TraversalDirection;
    
  /// The total number of planes of voxels traversed by the ray.
  int m_TotalRayVoxelPlanes;
    
  /// The current number of planes of voxels traversed by the ray.
  int m_NumVoxelPlanesTraversed;
    
  /// Pointers to the current four voxels surrounding the ray's trajectory.
  const PixelType *m_RayIntersectionVoxels[4];
    
  /**
   * The voxel coordinate of the bottom-left voxel of the current
   * four voxels surrounding the ray's trajectory. 
   */
  int m_RayIntersectionVoxelIndex[3];
    
  /// The dimension in voxels of the 3D volume in along the x axis
  int m_NumberOfVoxelsInX;
  /// The dimension in voxels of the 3D volume in along the y axis
  int m_NumberOfVoxelsInY;
  /// The dimension in voxels of the 3D volume in along the z axis
  int m_NumberOfVoxelsInZ;
    
  /// Voxel dimension in x
  double m_VoxelDimensionInX;
  /// Voxel dimension in y
  double m_VoxelDimensionInY;
  /// Voxel dimension in z
  double m_VoxelDimensionInZ;
    
  /// The coordinate of the point at which the ray enters the volume in mm.
  double m_RayStartCoordInMM[3];
  /// The coordinate of the point at which the ray exits the volume in mm.
  double m_RayEndCoordInMM[3];
    
    
  /** \brief
      Planes which define the boundary of the volume in mm 
      (six planes and four parameters: Ax+By+Cz+D). */
  double m_BoundingPlane[6][4];
  /// The eight corners of the volume (x,y,z coordinates for each).
  double m_BoundingCorner[8][3];
    
  /// The position of the ray
  double m_CurrentRayPositionInMM[3];
    
  /// The direction of the ray
  double m_RayDirectionInMM[3];  
    
};



/* -----------------------------------------------------------------------
   Initialise() - Initialise the object
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastHelper<TInputImage, TCoordRep>
::Initialise(void) 
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
RayCastHelper<TInputImage, TCoordRep>
::RecordVolumeDimensions(void) 
{
  typename InputImageType::SpacingType spacing=this->m_Image->GetSpacing();
  SizeType dim=this->m_Image->GetLargestPossibleRegion().GetSize();

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
RayCastHelper<TInputImage, TCoordRep>
::DefineCorners(void) 
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

}



/* -----------------------------------------------------------------------
   CalcPlanesAndCorners() - Calculate the planes and corners of the volume.
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastHelper<TInputImage, TCoordRep>
::CalcPlanesAndCorners(void) 
{
  int j;


  // find the equations of the planes

  int c1, c2, c3;
        
  for (j=0; j<6; j++) 
    {                                // loop around for planes
    switch (j)
      {                // which corners to take
      case 0:
        c1=1; c2=2; c3=3;;
        break;
      case 1:
        c1=4; c2=5; c3=6;
        break;
      case 2:
        c1=5; c2=3; c3=7;
        break;
      case 3:
        c1=2; c2=4; c3=6;
        break;
      case 4:
        c1=1; c2=5; c3=0;
        break;
      case 5:
        c1=3; c2=7; c3=2;
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
      itk::ExceptionObject err(__FILE__, __LINE__);
      err.SetLocation( "RayCastInterpolateImageFunction" );
      err.SetDescription( "Division by zero (planes) "
                          "- CalcPlanesAndCorners().");
      throw err;
      }
    }

}


/* -----------------------------------------------------------------------
   CalcRayIntercepts() - Calculate the ray intercepts with the volume.
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
bool 
RayCastHelper<TInputImage, TCoordRep>
::CalcRayIntercepts() 
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
    std::cerr << "WARNING: No. of sides crossed equals: " << nSidesCrossed << std::endl;
    }

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
RayCastHelper<TInputImage, TCoordRep>
::SetRay(OutputPointType RayPosn, DirectionType RayDirn) 
{

  // Store the position and direction of the ray
  typename TInputImage::SpacingType spacing=this->m_Image->GetSpacing();
  SizeType dim=this->m_Image->GetLargestPossibleRegion().GetSize();

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
RayCastHelper<TInputImage, TCoordRep>
::EndPointsInVoxels(void) 
{
  m_RayVoxelStartPosition[0] = m_RayStartCoordInMM[0]/m_VoxelDimensionInX;
  m_RayVoxelStartPosition[1] = m_RayStartCoordInMM[1]/m_VoxelDimensionInY;
  m_RayVoxelStartPosition[2] = m_RayStartCoordInMM[2]/m_VoxelDimensionInZ;

  m_RayVoxelEndPosition[0] = m_RayEndCoordInMM[0]/m_VoxelDimensionInX;
  m_RayVoxelEndPosition[1] = m_RayEndCoordInMM[1]/m_VoxelDimensionInY;
  m_RayVoxelEndPosition[2] = m_RayEndCoordInMM[2]/m_VoxelDimensionInZ;

}


/* -----------------------------------------------------------------------
   CalcDirnVector() - Calculate the incremental direction vector in voxels.
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastHelper<TInputImage, TCoordRep>
::CalcDirnVector(void) 
{
  double xNum, yNum, zNum;

  // Calculate the number of voxels in each direction

  xNum = fabs(m_RayVoxelStartPosition[0] - m_RayVoxelEndPosition[0]);
  yNum = fabs(m_RayVoxelStartPosition[1] - m_RayVoxelEndPosition[1]);
  zNum = fabs(m_RayVoxelStartPosition[2] - m_RayVoxelEndPosition[2]);

  // The direction iterated in is that with the greatest number of voxels
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  // Iterate in X direction

  if( (xNum >= yNum) && (xNum >= zNum) ) 
    {
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
}


/* -----------------------------------------------------------------------
   AdjustRayLength() - Ensure that the ray lies within the volume
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
bool 
RayCastHelper<TInputImage, TCoordRep>
::AdjustRayLength(void) 
{
  bool startOK, endOK;

  int Istart[3];
  int Idirn[3];

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
    itk::ExceptionObject err(__FILE__, __LINE__);
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


  return (startOK && endOK);
}


/* -----------------------------------------------------------------------
   Reset() - Reset the iterator to the start of the ray.
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastHelper<TInputImage, TCoordRep>
::Reset(void) 
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
RayCastHelper<TInputImage, TCoordRep>
::InitialiseVoxelPointers(void) 
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
        = this->m_Image->GetBufferPointer() + this->m_Image->ComputeOffset(index);

      index[0]=Ix; index[1]=Iy+1; index[2]=Iz; 
      m_RayIntersectionVoxels[1] 
        = ( this->m_Image->GetBufferPointer() + this->m_Image->ComputeOffset(index) );

      index[0]=Ix; index[1]=Iy; index[2]=Iz+1; 
      m_RayIntersectionVoxels[2] 
        = ( this->m_Image->GetBufferPointer() + this->m_Image->ComputeOffset(index) );

      index[0]=Ix; index[1]=Iy+1; index[2]=Iz+1; 
      m_RayIntersectionVoxels[3] 
        = ( this->m_Image->GetBufferPointer() + this->m_Image->ComputeOffset(index) );
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
      m_RayIntersectionVoxels[0] = ( this->m_Image->GetBufferPointer()
                                     + this->m_Image->ComputeOffset(index) );

      index[0]=Ix+1; index[1]=Iy; index[2]=Iz; 
      m_RayIntersectionVoxels[1] = ( this->m_Image->GetBufferPointer()
                                     + this->m_Image->ComputeOffset(index) );

      index[0]=Ix; index[1]=Iy; index[2]=Iz+1; 
      m_RayIntersectionVoxels[2] = ( this->m_Image->GetBufferPointer()
                                     + this->m_Image->ComputeOffset(index) );

      index[0]=Ix+1; index[1]=Iy; index[2]=Iz+1; 
      m_RayIntersectionVoxels[3] = ( this->m_Image->GetBufferPointer()
                                     + this->m_Image->ComputeOffset(index) );
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
      m_RayIntersectionVoxels[0] = ( this->m_Image->GetBufferPointer()
                                     + this->m_Image->ComputeOffset(index) );

      index[0]=Ix+1; index[1]=Iy; index[2]=Iz; 
      m_RayIntersectionVoxels[1] = ( this->m_Image->GetBufferPointer()
                                     + this->m_Image->ComputeOffset(index) );

      index[0]=Ix; index[1]=Iy+1; index[2]=Iz; 
      m_RayIntersectionVoxels[2] = ( this->m_Image->GetBufferPointer()
                                     + this->m_Image->ComputeOffset(index) );

      index[0]=Ix+1; index[1]=Iy+1; index[2]=Iz; 
      m_RayIntersectionVoxels[3] = ( this->m_Image->GetBufferPointer()
                                     + this->m_Image->ComputeOffset(index) );

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
    itk::ExceptionObject err(__FILE__, __LINE__);
    err.SetLocation( "RayCastInterpolateImageFunction" );
    err.SetDescription( "The ray traversal direction is unset "
                        "- InitialiseVoxelPointers().");
    throw err;
    return;
    }
    }  
}




/* -----------------------------------------------------------------------
   IncrementVoxelPointers() - Increment the voxel pointers
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastHelper<TInputImage, TCoordRep>
::IncrementVoxelPointers(void) 
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

  int totalRayVoxelPlanes 
    = dx + dy*m_NumberOfVoxelsInX + dz*m_NumberOfVoxelsInX*m_NumberOfVoxelsInY;

  m_RayIntersectionVoxels[0] += totalRayVoxelPlanes;
  m_RayIntersectionVoxels[1] += totalRayVoxelPlanes;
  m_RayIntersectionVoxels[2] += totalRayVoxelPlanes;
  m_RayIntersectionVoxels[3] += totalRayVoxelPlanes;
}


/* -----------------------------------------------------------------------
   GetCurrentIntensity() - Get the intensity of the current ray point.
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
double 
RayCastHelper<TInputImage, TCoordRep>
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
    itk::ExceptionObject err(__FILE__, __LINE__);
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
   IncrementIntensities() - Increment the intensities of the current ray point
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void 
RayCastHelper<TInputImage, TCoordRep>
::IncrementIntensities(double increment)
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
RayCastHelper<TInputImage, TCoordRep>
::IntegrateAboveThreshold(double &integral, double threshold) 
{
  double intensity;
//  double posn3D_x, posn3D_y, posn3D_z;

  integral = 0.;

  // Check if this is a valid ray
  
  if (! m_ValidRay)
    {
    return false;
    }
  /* Step along the ray as quickly as possible 
     integrating the interpolated intensities. */

  for (m_NumVoxelPlanesTraversed=0; 
       m_NumVoxelPlanesTraversed<m_TotalRayVoxelPlanes; 
       m_NumVoxelPlanesTraversed++) 
    {
    intensity = this->GetCurrentIntensity();

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

  return true;
}



/* -----------------------------------------------------------------------
   ZeroState() - Set the default (zero) state of the object
   ----------------------------------------------------------------------- */

template<class TInputImage, class TCoordRep>
void  
RayCastHelper<TInputImage, TCoordRep>
::ZeroState()  
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
}; // end of anonymous namespace


namespace itk
{

/**************************************************************************
 *
 *
 * Rest of this code is the actual RayCastInterpolateImageFunction
 * class
 *
 *
 **************************************************************************/



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
  os << indent << "FocalPoint: " << m_FocalPoint << std::endl;
  os << indent << "Transform: " << m_Transform.GetPointer() << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  
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

  RayCastHelper<TInputImage, TCoordRep> ray;
  ray.SetImage( this->m_Image );
  ray.ZeroState();
  ray.Initialise();

  ray.SetRay(point, direction);
  ray.IntegrateAboveThreshold(integral, m_Threshold);

  return ( static_cast<OutputType>( integral ));
}

template<class TInputImage, class TCoordRep>
typename RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::OutputType
RayCastInterpolateImageFunction< TInputImage, TCoordRep >
::EvaluateAtContinuousIndex( const ContinuousIndexType& index ) const
{
  OutputPointType point;
  m_Image->TransformContinuousIndexToPhysicalPoint(index, point);
  
  return this->Evaluate( point );
}

} // namespace itk


#endif
