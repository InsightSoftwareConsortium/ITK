/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRayCastInterpolateImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRayCastInterpolateImageFunction_h
#define _itkRayCastInterpolateImageFunction_h

#include "itkInterpolateImageFunction.h"
#include "itkTransform.h"
#include "itkVector.h"

namespace itk
{

/** \class RayCastInterpolateImageFunction
 * \brief Projective interpolation of an image at specified positions.
 *
 * RayCastInterpolateImageFunction casts rays through a 3-dimensional
 * image and uses bilinear interpolation to integrate each plane of
 * voxels traversed.
 * 
 * \warning This interpolator works for 3-dimensional images only.
 *
 * \ingroup ImageFunctions
 */
template <class TInputImage, class TCoordRep = float>
class ITK_EXPORT RayCastInterpolateImageFunction : 
  public InterpolateImageFunction<TInputImage,TCoordRep> 
{
public:
  /** Standard class typedefs. */
  typedef RayCastInterpolateImageFunction Self;
  typedef InterpolateImageFunction<TInputImage,TCoordRep> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Constants for the image dimensions */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** 
   * Type of the Transform Base class 
   * The fixed image should be a 3D image
   */
  typedef Transform<TCoordRep,3,3> TransformType;

  typedef typename TransformType::Pointer            TransformPointer;
  typedef typename TransformType::InputPointType     InputPointType;
  typedef typename TransformType::OutputPointType    OutputPointType;
  typedef typename TransformType::ParametersType     TransformParametersType;
  typedef typename TransformType::JacobianType       TransformJacobianType;

  typedef typename Superclass::InputPixelType        PixelType;

  typedef typename TInputImage::SizeType             SizeType;

  typedef itk::Vector<double, 3>                     DirectionType;

  /**  Type of the Interpolator Base class */
  typedef InterpolateImageFunction<TInputImage,TCoordRep> InterpolatorType;

  typedef typename InterpolatorType::Pointer         InterpolatorPointer;

  
  /** Run-time type information (and related methods). */
  itkTypeMacro(RayCastInterpolateImageFunction, InterpolateImageFunction);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** RealType typedef support. */
  typedef typename Superclass::RealType RealType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;



  /** \brief
   * Interpolate the image at a point position.
   *
   * Returns the interpolated image intensity at a 
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. 
   */
  virtual OutputType Evaluate( const PointType& point ) const;

  /** Interpolate the image at a continuous index position
   *
   * Returns the interpolated image intensity at a 
   * specified index position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * Subclasses must override this method.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. 
   */
  virtual OutputType EvaluateAtContinuousIndex( 
     const ContinuousIndexType & index ) const {return 0; };


  /** Connect the Transform. */
  itkSetObjectMacro( Transform, TransformType );
  /** Get a pointer to the Transform.  */
  itkGetObjectMacro( Transform, TransformType );
 
  /** Connect the Interpolator. */
  itkSetObjectMacro( Interpolator, InterpolatorType );
  /** Get a pointer to the Interpolator.  */
  itkGetObjectMacro( Interpolator, InterpolatorType );

  /** Connect the Interpolator. */
  itkSetMacro( FocalPoint, InputPointType );
  /** Get a pointer to the Interpolator.  */
  itkGetMacro( FocalPoint, InputPointType );

  /** Connect the Transform. */
  itkSetMacro( Threshold, double );
  /** Get a pointer to the Transform.  */
  itkGetMacro( Threshold, double );
 
  /** Check if a point is inside the image buffer.
   * \warning For efficiency, no validity checking of
   * the input image pointer is done. */
  inline bool IsInsideBuffer( const PointType & point ) const
    { 
      return true;
    }

  /** 
   *  Initialise the ray using the position and direction of a line.
   *
   *  \param RayPosn       The position of the ray in 3D (mm).
   *  \param RayDirn       The direction of the ray in 3D (mm).
   *
   *  \return True if this is a valid ray.
   */
  bool SetRay(OutputPointType RayPosn, DirectionType RayDirn) const;


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
  bool Integrate(double &integral) const {
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
  bool IntegrateAboveThreshold(double &integral, double threshold) const;

  /** \brief
   * Increment each of the intensities of the 4 planar voxels surrounding the 
   * current ray point.
   *
   * \parameter increment      Intensity increment for each of the current 4 voxels
   */
  void IncrementIntensities(double increment=1) const;


  /** \brief
   * Step along the ray.
   *
   * This routine can be called after iteratively to step along a given ray.
   * To specify a new ray call: 'SetProjectionCoord2D()' first. To re-traverse
   * the current ray call: 'Reset()'.
   *
   * \return False if there are no more points on the ray.
   */
  bool NextPoint(void) const;

  /// Reset the iterator to the start of the ray.
  void Reset(void) const;
  
  /** \brief
   * Get the current ray position in mm.
   *
   * The origin for this coordinate is the corner of the (0,0,0) voxel.
   *
   * \param x        The x component of the current ray coordinate
   * \param y        The y component of the current ray coordinate
   * \param z        The z component of the current ray coordinate
   */
  void GetCurrentCoord3D(double &x, double &y, double &z) const;

  /**
   *   Get the current ray position in voxels.
   *
   *   \param x        The x component of the current ray coordinate
   *   \param y        The y component of the current ray coordinate
   *   \param z        The z component of the current ray coordinate
   */
  void GetCurrentVoxelCoord3D(double &x, double &y, double &z) const;

  /// Return the interpolated intensity of the current ray point.
  double GetCurrentIntensity(void) const;

  /** \brief
   * Return the density of intensity for the current ray point.
   *
   * This function determines which of the 4 planar voxels surrounding the 
   * current ray point are above the threshold value and interpolates these 
   * zero or unity values. The returned value therefore also lies between zero 
   * and one and estimates the extent to which the current ray point is "filled"
   * with intensity.
   *
   * \param threshold        Voxels are considered "set" if above this threshold
   *
   * \return The extent to which the current ray point is "filled"
   * with intensity.
   */
  double GetCurrentDensity(double threshold=0.) const;

  /// Return a pointer to the four voxels surrounding the current ray position
  void GetCurrentVoxels(PixelType &voxel1, PixelType &voxel2, 
                        PixelType &voxel3, PixelType &voxel4) const;

  /// Return the number of voxels on the current ray
  int GetNumberOfVoxels(void) const {return m_NumVoxelPlanesTraversed;};

  /// Return the ray point spacing in mm
  double GetRayPointSpacing(void) const {
    const double *spacing=m_Image->GetSpacing();

    if (m_ValidRay)
      return sqrt(  m_VoxelIncrement[0]*spacing[0]*m_VoxelIncrement[0]*spacing[0]
                  + m_VoxelIncrement[1]*spacing[1]*m_VoxelIncrement[1]*spacing[1]
                  + m_VoxelIncrement[2]*spacing[2]*m_VoxelIncrement[2]*spacing[2] );
    else
      return 0.;
  };

  /** \brief
   * Define the corners of the volume according a specified bounding box.
   *
   * \param position          The position of the bounding box in mm. 
   * \param size              The size of the bounding box in mm. 
   */
  void SetBoundingBox(double position[3], double size[3]) const;


protected:

  /// Constructor
  RayCastInterpolateImageFunction();

  /// Destructor
  ~RayCastInterpolateImageFunction(){};

  /// Print the object
  void PrintSelf(std::ostream& os, Indent indent) const;


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
  
  
  /// Transformation used to calculate the new focal point position
  TransformPointer m_Transform;

  /// The focal point or position of the ray source
  InputPointType m_FocalPoint;

  /// The threshold above which voxels along the ray path are integrated.
  double m_Threshold;

  /// Flag indicating whether the current ray is valid
  mutable bool m_ValidRay;

  /** \brief
   * The start position of the ray in voxels. 
   *
   * NB. Two of the components of this coordinate (i.e. those lying within
   * the planes of voxels being traversed) will be shifted by half a
   * voxel. This enables indices of the neighbouring voxels within the plane
   * to be determined by simply casting to 'int' and optionally adding 1.
   */
  mutable double m_RayVoxelStartPosition[3];

  /** \brief
   * The end coordinate of the ray in voxels.
   *
   * NB. Two of the components of this coordinate (i.e. those lying within
   * the planes of voxels being traversed) will be shifted by half a
   * voxel. This enables indices of the neighbouring voxels within the plane
   * to be determined by simply casting to 'int' and optionally adding 1.
   */
  mutable double m_RayVoxelEndPosition[3];


  /** \brief
   * The current coordinate on the ray in voxels.
   *
   * NB. Two of the components of this coordinate (i.e. those lying within
   * the planes of voxels being traversed) will be shifted by half a
   * voxel. This enables indices of the neighbouring voxels within the plane
   * to be determined by simply casting to 'int' and optionally adding 1.
   */
  mutable double m_Position3Dvox[3];

  /** The incremental direction vector of the ray in voxels. */
  mutable double m_VoxelIncrement[3];

  /// The direction in which the ray is incremented thorough the volume (x, y or z).
  mutable TraversalDirection m_TraversalDirection;

  /// The total number of planes of voxels traversed by the ray.
  mutable int m_TotalRayVoxelPlanes;

  /// The current number of planes of voxels traversed by the ray.
  mutable int m_NumVoxelPlanesTraversed;

  /// Pointers to the current four voxels surrounding the ray's trajectory.
  mutable const PixelType *m_RayIntersectionVoxels[4];

  /**
   * The voxel coordinate of the bottom-left voxel of the current
   * four voxels surrounding the ray's trajectory. 
   */
  mutable int m_RayIntersectionVoxelIndex[3];


  /// The dimension in voxels of the 3D volume in along the x axis
  mutable int m_NumberOfVoxelsInX;
  /// The dimension in voxels of the 3D volume in along the y axis
  mutable int m_NumberOfVoxelsInY;
  /// The dimension in voxels of the 3D volume in along the z axis
  mutable int m_NumberOfVoxelsInZ;

  /// Voxel dimension in x
  mutable double m_VoxelDimensionInX;
  /// Voxel dimension in y
  mutable double m_VoxelDimensionInY;
  /// Voxel dimension in z
  mutable double m_VoxelDimensionInZ;

  /// The coordinate of the point at which the ray enters the volume in mm.
  mutable double m_RayStartCoordInMM[3];
  /// The coordinate of the point at which the ray exits the volume in mm.
  mutable double m_RayEndCoordInMM[3];


  /** \brief
      Planes which define the boundary of the volume in mm 
      (six planes and four parameters: Ax+By+Cz+D). */
  mutable double m_BoundingPlane[6][4];
  /// The eight corners of the volume (x,y,z coordinates for each).
  mutable double m_BoundingCorner[8][3];

  /// The position of the ray
  mutable double m_CurrentRayPositionInMM[3];
  
  /// The direction of the ray
  mutable double m_RayDirectionInMM[3];  

  /// Pointer to the interpolator
  InterpolatorPointer m_Interpolator;


  /// Calculate the endpoint coordinats of the ray in voxels.
  void EndPointsInVoxels(void) const;

  /** 
   * Calculate the incremental direction vector in voxels, 'dVoxel',
   * required to traverse the ray. 
   */
  void CalcDirnVector(void) const;

  /** 
   * Reduce the length of the ray until both start and end
   * coordinates lie inside the volume.
   *
   * \return True if a valid ray has been, false otherwise.
   */
  bool AdjustRayLength(void) const;

  /** 
   *   Obtain pointers to the four voxels surrounding the point where the ray
   *   enters the volume. 
   */ 
  void InitialiseVoxelPointers(void) const;

  /// Increment the voxel pointers surrounding the current point on the ray.
  void IncrementVoxelPointers(void) const;

  /// Initialise the object
  void Initialise(void) const;

  /// Record volume dimensions and resolution
  void RecordVolumeDimensions(void) const;

  /// Define the corners of the volume
  void DefineCorners(void) const;

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
  void CalcPlanesAndCorners(void) const;
      
  /** \brief
   *   Calculate the ray intercepts with the volume.
   *
   *   See where the ray cuts the volume, check that truncation does not occur,
   *   if not, then start ray where it first intercepts the volume and set 
   *   x_max to be where it leaves the volume.
   *
   *   \return True if a valid ray has been specified, false otherwise.
   */
  bool CalcRayIntercepts(void) const;


private:
  RayCastInterpolateImageFunction( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  /// Set the initial zero state of the object 
  void ZeroState() const;
 

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRayCastInterpolateImageFunction.txx"
#endif

#endif
