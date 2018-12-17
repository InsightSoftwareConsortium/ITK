/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkSpatialObjectPoint_h
#define itkSpatialObjectPoint_h

#include "itkPoint.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkRGBAPixel.h"

namespace itk
{
/** \class SpatialObjectPoint
 * \brief Point used for spatial objets
 *
 * This class contains all the functions necessary to define a point
 *
 * \sa TubeSpatialObjectPoint SurfaceSpatialObjectPoint
 * \ingroup ITKSpatialObjects
 */

template< unsigned int TPointDimension = 3 >
class ITK_TEMPLATE_EXPORT SpatialObjectPoint
{
public:

  /** Constructor. */
  SpatialObjectPoint();

  /** Default destructor. */
  virtual ~SpatialObjectPoint() = default;

  using Self = SpatialObjectPoint;
  using PointType = Point< double, TPointDimension >;
  using VectorType = vnl_vector< double >;
  using PixelType = RGBAPixel< float >;
  using ColorType = PixelType;

  /** Get the SpatialObjectPoint Id. */
  int GetID() const;

  /** Set the SpatialObjectPoint Id. */
  void SetID(const int newID);

  /** Return a pointer to the point object. */
  const PointType & GetPosition() const;

  /** Set the point object. Couldn't use macros for these methods. */
  void SetPosition(const PointType & newX);

  template <typename... TCoordinate>
  void SetPosition(
    const double firstCoordinate,
    const TCoordinate... otherCoordinate)
    {
    static_assert((1 + sizeof...(otherCoordinate)) == TPointDimension,
      "The number of coordinates must be equal to the dimensionality!");
    const double coordinates[] =
      {
      firstCoordinate, static_cast<double>(otherCoordinate)...
      };
    m_X = coordinates;
    }

  /** Copy one SpatialObjectPoint to another */
  SpatialObjectPoint & operator=(const SpatialObjectPoint & ) = default;

  /** Set/Get color of the point */
  const PixelType & GetColor() const;

  void SetColor(const PixelType & color);

  void SetColor(float r, float g, float b, float a = 1);

  /** Set/Get red color of the point */
  void SetRed(float r);

  float GetRed() const;

  /** Set/Get Green color of the point */
  void SetGreen(float g);

  float GetGreen() const;

  /** Set/Get blue color of the point */
  void SetBlue(float b);

  float GetBlue() const;

  /** Set/Get alpha value of the point */
  void SetAlpha(float a);

  float GetAlpha() const;

  /** PrintSelf method */
  void Print(std::ostream & os) const;

protected:

  /** PrintSelf method */
  virtual void PrintSelf(std::ostream & os, Indent indent) const;

  /** A unique ID assigned to this SpatialObjectPoint */
  int m_ID{-1};

  /** Position of the point */
  PointType m_X;

  /** Color of the point */
  PixelType m_Color;
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectPoint.hxx"
#endif

#endif // itkSpatialObjectPoint_h
