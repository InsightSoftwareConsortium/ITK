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

#include "itkSpatialObject.h"

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

template < unsigned int TDimension, class TSpatialObjectPointType>
class PointBasedSpatialObject;

template< unsigned int TPointDimension = 3>
class ITK_TEMPLATE_EXPORT SpatialObjectPoint
{
public:

  /** Constructor. */
  SpatialObjectPoint();

  /** Default destructor. */
  virtual ~SpatialObjectPoint() = default;

  using Self = SpatialObjectPoint;

  using SpatialObjectType = PointBasedSpatialObject<TPointDimension, Self>;

  using PointType = Point< double, TPointDimension >;
  using VectorType = vnl_vector< double >;
  using ColorType = RGBAPixel< float >;

  /** Set the SpatialObjectPoint Id. */
  itkSetMacro( Id, int );

  /** Get the SpatialObjectPoint Id. */
  itkGetMacro( Id, int );

  /** Set the point object. */
  void SetPositionInObjectSpace(const PointType & newPositionInObjectSpace)
  { m_PositionInObjectSpace = newPositionInObjectSpace; }

  template <typename... TCoordinate>
  void SetPositionInObjectSpace(
    const double firstCoordinate,
    const TCoordinate... otherCoordinate)
    {
    static_assert((1 + sizeof...(otherCoordinate)) == TPointDimension,
      "The number of coordinates must be equal to the dimensionality!");
    const double coordinates[] =
      {
      firstCoordinate, static_cast<double>(otherCoordinate)...
      };
    m_PositionInObjectSpace = coordinates;
    }

  /** Return a pointer to the point object. */
  const PointType & GetPositionInObjectSpace() const
  { return m_PositionInObjectSpace; }

  itkSetConstObjectMacro( SpatialObject, SpatialObjectType );

  /** Set the position in world coordinates, using the
   *    spatialObject's objectToWorld transform, inverse */
  void SetPosition( const PointType & point );

  /** Returns the position in world coordinates, using the
   *    spatialObject's objectToWorld transform */
  PointType GetPosition() const;

  /** Copy one SpatialObjectPoint to another */
  Self & operator=(const SpatialObjectPoint & rhs );

  /** Set/Get color of the point */
  itkSetMacro( Color, ColorType );
  itkGetMacro( Color, ColorType );
  itkGetConstReferenceMacro( Color, ColorType );

  /** Set the color */
  void SetColor(float r, float g, float b, float a = 1);

  /** Set/Get red color of the point */
  void SetRed(float r)
  { m_Color.SetRed( r ); }

  float GetRed() const
  { return m_Color.GetRed(); }

  /** Set/Get Green color of the point */
  void SetGreen(float g)
  { m_Color.SetGreen( g ); }

  float GetGreen() const
  { return m_Color.GetGreen(); }

  /** Set/Get blue color of the point */
  void SetBlue(float b)
  { m_Color.SetBlue( b ); }

  float GetBlue() const
  { return m_Color.GetBlue(); }

  /** Set/Get alpha value of the point */
  void SetAlpha(float a)
  { m_Color.SetAlpha( a ); }

  float GetAlpha() const
  { return m_Color.GetAlpha(); }

  /** PrintSelf method */
  void Print(std::ostream & os) const
  { this->PrintSelf( os, 3 ); }

protected:

  /** PrintSelf method */
  virtual void PrintSelf(std::ostream & os, Indent indent) const;

  /** A unique ID assigned to this SpatialObjectPoint */
  int m_Id;

  /** Position of the point */
  PointType m_PositionInObjectSpace;

  /** Color of the point */
  ColorType m_Color;

  const typename SpatialObjectType::Pointer m_SpatialObject;
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialObjectPoint.hxx"
#endif

#endif // itkSpatialObjectPoint_h
