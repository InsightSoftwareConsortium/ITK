/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkPhasedArray3DSpecialCoordinatesImage_h
#define itkPhasedArray3DSpecialCoordinatesImage_h

#include "itkSpecialCoordinatesImage.h"
#include "itkPoint.h"
#include "itkMath.h"
#include "itkDefaultPixelAccessor.h"
#include "itkNeighborhoodAccessorFunctor.h"

namespace itk
{
/** \class PhasedArray3DSpecialCoordinatesImage
 *  \brief Templated 3D nonrectilinear-coordinate image class for
 *  phased-array "range" images.
 *
 * \verbatim
 *
 * y-axis <--------------------+
 *                             |\
 *                          /  | \
 *                          `~-|  \
 *                       /     |   \
 *                        ele- |    \
 *                    / vation |     \
 * projection                  |      v x-axis
 * to y-z plane -> o           |
 *                             v z-axis
 *
 * \endverbatim
 *
 * In a phased array "range" image, a point in space is represented by
 * the angle  between its projection onto the x-z plane and the z-axis
 * (the azimuth coordinate), the angle between its projection onto the
 * y-z plane and the z-axis (the elevation coordinate), and by its
 * distance from the origin (the radius).  See the diagram above,
 * which illustrates elevation.
 *
 * The equations form performing the conversion from Cartesian coordinates to
 * 3D phased array coordinates are as follows:
 *
 * azimuth = arctan(x/y)
 * elevation = arctan(y/z)
 * radius = std::sqrt(x^2 + y^2 + z^2)
 *
 * The reversed transforms are:
 *
 * z = radius / std::sqrt(1 + (tan(azimuth))^2 + (tan(elevation))^2 );
 * x = z * std::tan(azimuth)
 * y = z * std::tan(elevation)
 *
 * PhasedArray3DSpecialCoordinatesImages are templated over a pixel
 * type and follow the SpecialCoordinatesImage interface.  The data in
 * an image is  arranged in a 1D array as
 * [radius-index][elevation-index][azimuth-index] with azimuth-index
 * varying most rapidly.  The Index type reverses the order so that
 * Index[0] = azimuth-index, Index[1] = elevation-index, and Index[2]
 * = radius-index.
 *
 * Azimuth is discretized into m_AzimuthAngularSeparation intervals
 * per angular voxel, the most negative azimuth interval containing
 * data is then mapped to azimuth-index=0, and the largest azimuth
 * interval containing data is then mapped to azimuth-index=( number
 * of samples along azimuth axis - 1 ). Elevation is discretized in
 * the same manner.  This way, the mapping to Cartesian space is
 * symmetric about the z axis such that the line defined by
 * azimuth/2,elevation/2 = z-axis.  Radius is discretized into
 * m_RadiusSampleSize units per angular voxel.  The smallest range
 * interval containing data is then mapped to radius-index=0, such
 * that radius = m_FirstSampleDistance + (radius-index *
 * m_RadiusSampleSize).
 *
 * \sa SpecialCoordinatesImage
 *
 * \ingroup ImageObjects
 * \ingroup ITKCommon
 */
template <typename TPixel>
class ITK_TEMPLATE_EXPORT PhasedArray3DSpecialCoordinatesImage : public SpecialCoordinatesImage<TPixel, 3>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(PhasedArray3DSpecialCoordinatesImage);

  /** Standard class type aliases */
  using Self = PhasedArray3DSpecialCoordinatesImage;
  using Superclass = SpecialCoordinatesImage<TPixel, 3>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ConstWeakPointer = WeakPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PhasedArray3DSpecialCoordinatesImage, SpecialCoordinatesImage);

  /** Pixel type alias support Used to declare pixel type in filters
   * or other operations. */
  using PixelType = TPixel;

  /** Typedef alias for PixelType */
  using ValueType = TPixel;

  /** Internal Pixel representation. Used to maintain a uniform API
   * with Image Adaptors and allow to keep a particular internal
   * representation of data while showing a different external
   * representation. */
  using InternalPixelType = TPixel;

  using IOPixelType = typename Superclass::IOPixelType;

  /** Accessor type that convert data between internal and external
   *  representations.  */
  using AccessorType = DefaultPixelAccessor<PixelType>;

  /** Accessor functor to choose between accessors: DefaultPixelAccessor for
   * the Image, and DefaultVectorPixelAccessor for the vector image. The
   * functor provides a generic API between the two accessors. */
  using AccessorFunctorType = DefaultPixelAccessorFunctor<Self>;

  /** Typedef for the functor used to access a neighborhood of pixel
   * pointers. */
  using NeighborhoodAccessorFunctorType = NeighborhoodAccessorFunctor<Self>;

  /** Dimension of the image.  This constant is used by functions that are
   * templated over image type (as opposed to being templated over pixel type
   * and dimension) when they need compile time access to the dimension of
   * the image. */
  static constexpr unsigned int ImageDimension = 3;

  /** Index type alias support An index is used to access pixel values. */
  using IndexType = typename Superclass::IndexType;
  using IndexValueType = typename Superclass::IndexValueType;

  /** Offset type alias support An offset is used to access pixel values. */
  using OffsetType = typename Superclass::OffsetType;

  /** Size type alias support A size is used to define region bounds. */
  using SizeType = typename Superclass::SizeType;
  using SizeValueType = typename Superclass::SizeValueType;

  /** Container used to store pixels in the image. */
  using PixelContainer = ImportImageContainer<SizeValueType, PixelType>;

  /** Region type alias support A region is used to specify a subset of
   *  an image.
   */
  using RegionType = typename Superclass::RegionType;

  /** Spacing type alias support  Spacing holds the "fake" size of a
   *  pixel, making each pixel look like a 1 unit hyper-cube to filters
   *  that were designed for normal images and that therefore use
   *  m_Spacing.  The spacing is the geometric distance between image
   *  samples.
   */
  using SpacingType = typename Superclass::SpacingType;

  /** Origin type alias support  The origin is the "fake" geometric
   *  coordinates of the index (0,0).  Also for use w/ filters designed
   *  for normal images.
   */
  using PointType = typename Superclass::PointType;

  /** A pointer to the pixel container. */
  using PixelContainerPointer = typename PixelContainer::Pointer;
  using PixelContainerConstPointer = typename PixelContainer::ConstPointer;

  /** \brief Get the continuous index from a physical point
   *
   * Returns true if the resulting index is within the image, false otherwise.
   * \sa Transform */
  template <typename TCoordRep, typename TIndexRep>
  bool
  TransformPhysicalPointToContinuousIndex(const Point<TCoordRep, 3> &     point,
                                          ContinuousIndex<TIndexRep, 3> & index) const
  {
    const RegionType region = this->GetLargestPossibleRegion();
    const double     maxAzimuth = region.GetSize(0) - 1;
    const double     maxElevation = region.GetSize(1) - 1;

    // Convert Cartesian coordinates into angular coordinates
    TCoordRep azimuth = Math::pi_over_2;
    TCoordRep elevation = Math::pi_over_2;
    if (point[2] != 0.0)
    {
      azimuth = std::atan(point[0] / point[2]);
      elevation = std::atan(point[1] / point[2]);
    }
    const TCoordRep radius = std::sqrt(point[0] * point[0] + point[1] * point[1] + point[2] * point[2]);

    // Convert the "proper" angular coordinates into index format
    index[0] = static_cast<TCoordRep>((azimuth / m_AzimuthAngularSeparation) + (maxAzimuth / 2.0));
    index[1] = static_cast<TCoordRep>((elevation / m_ElevationAngularSeparation) + (maxElevation / 2.0));
    index[2] = static_cast<TCoordRep>(((radius - m_FirstSampleDistance) / m_RadiusSampleSize));

    // Now, check to see if the index is within allowed bounds
    const bool isInside = region.IsInside(index);

    return isInside;
  }

  /** Get the index (discrete) from a physical point.
   * Floating point index results are truncated to integers.
   * Returns true if the resulting index is within the image, false otherwise
   * \sa Transform */
  template <typename TCoordRep>
  bool
  TransformPhysicalPointToIndex(const Point<TCoordRep, 3> & point, IndexType & index) const
  {
    const RegionType region = this->GetLargestPossibleRegion();
    const double     maxAzimuth = region.GetSize(0) - 1;
    const double     maxElevation = region.GetSize(1) - 1;

    // Convert Cartesian coordinates into angular coordinates
    TCoordRep azimuth = Math::pi_over_2;
    TCoordRep elevation = Math::pi_over_2;
    if (point[2] != 0.0)
    {
      azimuth = std::atan(point[0] / point[2]);
      elevation = std::atan(point[1] / point[2]);
    }
    const TCoordRep radius = std::sqrt(point[0] * point[0] + point[1] * point[1] + point[2] * point[2]);

    // Convert the "proper" angular coordinates into index format
    index[0] = static_cast<IndexValueType>((azimuth / m_AzimuthAngularSeparation) + (maxAzimuth / 2.0));
    index[1] = static_cast<IndexValueType>((elevation / m_ElevationAngularSeparation) + (maxElevation / 2.0));
    index[2] = static_cast<IndexValueType>(((radius - m_FirstSampleDistance) / m_RadiusSampleSize));

    // Now, check to see if the index is within allowed bounds
    const bool isInside = region.IsInside(index);

    return isInside;
  }

  /** Get a physical point (in the space which
   * the origin and spacing information comes from)
   * from a continuous index (in the index space)
   * \sa Transform */
  template <typename TCoordRep, typename TIndexRep>
  void
  TransformContinuousIndexToPhysicalPoint(const ContinuousIndex<TIndexRep, 3> & index,
                                          Point<TCoordRep, 3> &                 point) const
  {
    const RegionType region = this->GetLargestPossibleRegion();
    const double     maxAzimuth = region.GetSize(0) - 1;
    const double     maxElevation = region.GetSize(1) - 1;

    // Convert the index into proper angular coordinates
    const TCoordRep azimuth = (index[0] - (maxAzimuth / 2.0)) * m_AzimuthAngularSeparation;
    const TCoordRep elevation = (index[1] - (maxElevation / 2.0)) * m_ElevationAngularSeparation;
    const TCoordRep radius = (index[2] * m_RadiusSampleSize) + m_FirstSampleDistance;

    // Convert the angular coordinates into Cartesian coordinates
    const TCoordRep tanOfAzimuth = std::tan(azimuth);
    const TCoordRep tanOfElevation = std::tan(elevation);

    point[2] =
      static_cast<TCoordRep>(radius / std::sqrt(1 + tanOfAzimuth * tanOfAzimuth + tanOfElevation * tanOfElevation));
    point[1] = static_cast<TCoordRep>(point[2] * tanOfElevation);
    point[0] = static_cast<TCoordRep>(point[2] * tanOfAzimuth);
  }

  /** Get a physical point (in the space which
   * the origin and spacing information comes from)
   * from a discrete index (in the index space)
   *
   * \sa Transform */
  template <typename TCoordRep>
  void
  TransformIndexToPhysicalPoint(const IndexType & index, Point<TCoordRep, 3> & point) const
  {
    const RegionType region = this->GetLargestPossibleRegion();
    const double     maxAzimuth = region.GetSize(0) - 1;
    const double     maxElevation = region.GetSize(1) - 1;

    // Convert the index into proper angular coordinates
    const TCoordRep azimuth = (static_cast<double>(index[0]) - (maxAzimuth / 2.0)) * m_AzimuthAngularSeparation;
    const TCoordRep elevation = (static_cast<double>(index[1]) - (maxElevation / 2.0)) * m_ElevationAngularSeparation;
    const TCoordRep radius = (static_cast<double>(index[2]) * m_RadiusSampleSize) + m_FirstSampleDistance;

    // Convert the angular coordinates into Cartesian coordinates
    const TCoordRep tanOfAzimuth = std::tan(azimuth);
    const TCoordRep tanOfElevation = std::tan(elevation);

    point[2] =
      static_cast<TCoordRep>(radius / std::sqrt(1.0 + tanOfAzimuth * tanOfAzimuth + tanOfElevation * tanOfElevation));
    point[1] = static_cast<TCoordRep>(point[2] * tanOfElevation);
    point[0] = static_cast<TCoordRep>(point[2] * tanOfAzimuth);
  }

  /**  Set the number of radians between each azimuth unit.   */
  itkSetMacro(AzimuthAngularSeparation, double);

  /**  Set the number of radians between each elevation unit.   */
  itkSetMacro(ElevationAngularSeparation, double);

  /**  Set the number of cartesian units between each unit along the R .  */
  itkSetMacro(RadiusSampleSize, double);

  /**  Set the distance to add to the radius. */
  itkSetMacro(FirstSampleDistance, double);

  template <typename TCoordRep>
  void TransformLocalVectorToPhysicalVector(FixedArray<TCoordRep, 3> &) const
  {}

  template <typename TCoordRep>
  void
  TransformPhysicalVectorToLocalVector(const FixedArray<TCoordRep, 3> &, FixedArray<TCoordRep, 3> &) const
  {}

  /** Return the Pixel Accessor object */
  AccessorType
  GetPixelAccessor()
  {
    return AccessorType();
  }

  /** Return the Pixel Accesor object */
  const AccessorType
  GetPixelAccessor() const
  {
    return AccessorType();
  }

  /** Return the NeighborhoodAccessor functor */
  NeighborhoodAccessorFunctorType
  GetNeighborhoodAccessor()
  {
    return NeighborhoodAccessorFunctorType();
  }

  /** Return the NeighborhoodAccessor functor */
  const NeighborhoodAccessorFunctorType
  GetNeighborhoodAccessor() const
  {
    return NeighborhoodAccessorFunctorType();
  }

protected:
  PhasedArray3DSpecialCoordinatesImage()
  {
    m_RadiusSampleSize = 1;
    m_AzimuthAngularSeparation = 1 * (2.0 * itk::Math::pi / 360.0);   // 1
                                                                      // degree
    m_ElevationAngularSeparation = 1 * (2.0 * itk::Math::pi / 360.0); // 1
                                                                      // degree
    m_FirstSampleDistance = 0;
  }

  ~PhasedArray3DSpecialCoordinatesImage() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  double m_AzimuthAngularSeparation;   // in radians
  double m_ElevationAngularSeparation; // in radians
  double m_RadiusSampleSize;
  double m_FirstSampleDistance;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPhasedArray3DSpecialCoordinatesImage.hxx"
#endif

#endif
