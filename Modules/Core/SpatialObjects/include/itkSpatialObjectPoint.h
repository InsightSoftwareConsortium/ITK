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
#ifndef itkSpatialObjectPoint_h
#define itkSpatialObjectPoint_h

#include "itkSpatialObject.h"

#include "itkPoint.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkRGBAPixel.h"


namespace itk
{
/**
 *\class SpatialObjectPoint
 * \brief Point used for spatial objets
 *
 * This class contains all the functions necessary to define a point
 *
 * \sa TubeSpatialObjectPoint SurfaceSpatialObjectPoint
 * \ingroup ITKSpatialObjects
 */

template <unsigned int TDimension, class TSpatialObjectPointType>
class PointBasedSpatialObject;

template <unsigned int TPointDimension = 3>
class ITK_TEMPLATE_EXPORT SpatialObjectPoint
{
public:
  using PointDimensionType = unsigned int;

  static constexpr PointDimensionType PointDimension = TPointDimension;

  /** Constructor. */
  SpatialObjectPoint();

  /** Copy Constructor. */
  SpatialObjectPoint(const SpatialObjectPoint & other);

  /** Default destructor. */
  virtual ~SpatialObjectPoint() = default;

  virtual const char *
  GetNameOfClass() const
  {
    return "SpatialObjectPoint";
  };

  using Self = SpatialObjectPoint;

  using SpatialObjectType = SpatialObject<TPointDimension>;

  using PointType = Point<double, TPointDimension>;
  using VectorType = vnl_vector<double>;
  using ColorType = RGBAPixel<double>;

  /** Set the SpatialObjectPoint Id. */
  void
  SetId(int id)
  {
    m_Id = id;
  }

  /** Get the SpatialObjectPoint Id. */
  int
  GetId() const
  {
    return m_Id;
  }

  /** Set the point object. */
  void
  SetPositionInObjectSpace(const PointType & newPositionInObjectSpace)
  {
    m_PositionInObjectSpace = newPositionInObjectSpace;
  }

  template <typename... TCoordinate>
  void
  SetPositionInObjectSpace(const double firstCoordinate, const TCoordinate... otherCoordinate)
  {
    static_assert((1 + sizeof...(otherCoordinate)) == TPointDimension,
                  "The number of coordinates must be equal to the dimensionality!");
    const double coordinates[] = { firstCoordinate, static_cast<double>(otherCoordinate)... };
    m_PositionInObjectSpace = coordinates;
  }

  /** Return a pointer to the point object. */
  const PointType &
  GetPositionInObjectSpace() const
  {
    return m_PositionInObjectSpace;
  }

  void
  SetSpatialObject(SpatialObjectType * so)
  {
    m_SpatialObject = so;
  }

  SpatialObjectType *
  GetSpatialObject() const
  {
    return m_SpatialObject;
  }

  /** Set the position in world coordinates, using the
   *    spatialObject's objectToWorld transform, inverse */
  void
  SetPositionInWorldSpace(const PointType & point);

  /** Returns the position in world coordinates, using the
   *    spatialObject's objectToWorld transform */
  PointType
  GetPositionInWorldSpace() const;

  /** Copy one SpatialObjectPoint to another */
  Self &
  operator=(const SpatialObjectPoint & rhs);

  /** Set/Get color of the point */
  void
  SetColor(ColorType color)
  {
    m_Color = color;
  }

  ColorType
  GetColor() const
  {
    return m_Color;
  }

  /** Set the color */
  void
  SetColor(double r, double g, double b, double a = 1);

  /** Set/Get red color of the point */
  void
  SetRed(double r)
  {
    m_Color.SetRed(r);
  }

  double
  GetRed() const
  {
    return m_Color.GetRed();
  }

  /** Set/Get Green color of the point */
  void
  SetGreen(double g)
  {
    m_Color.SetGreen(g);
  }

  double
  GetGreen() const
  {
    return m_Color.GetGreen();
  }

  /** Set/Get blue color of the point */
  void
  SetBlue(double b)
  {
    m_Color.SetBlue(b);
  }

  double
  GetBlue() const
  {
    return m_Color.GetBlue();
  }

  /** Set/Get alpha value of the point */
  void
  SetAlpha(double a)
  {
    m_Color.SetAlpha(a);
  }

  double
  GetAlpha() const
  {
    return m_Color.GetAlpha();
  }

  void
  SetTagScalarValue(const std::string & tag, double value);

  bool
  GetTagScalarValue(const std::string & tag, double & value) const;

  double
  GetTagScalarValue(const std::string & tag) const;

  std::map<std::string, double> &
  GetTagScalarDictionary();

  const std::map<std::string, double> &
  GetTagScalarDictionary() const;

  void
  SetTagScalarDictionary(const std::map<std::string, double> & dict);

  /** PrintSelf method */
  void
  Print(std::ostream & os) const
  {
    this->PrintSelf(os, 3);
  }

protected:
  /** PrintSelf method */
  virtual void
  PrintSelf(std::ostream & os, Indent indent) const;

  /** A unique ID assigned to this SpatialObjectPoint */
  int m_Id;

  /** Position of the point */
  PointType m_PositionInObjectSpace;

  /** Color of the point */
  ColorType m_Color;

  /** Additional scalar properties of the point */
  std::map<std::string, double> m_ScalarDictionary;


  // The SpatialObjectPoint keeps a reference to its owning parent
  // spatial object for its spatial context. A WeakPointer is used to
  // avoid a memory leak.
  WeakPointer<SpatialObjectType> m_SpatialObject;
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSpatialObjectPoint.hxx"
#endif

#endif // itkSpatialObjectPoint_h
