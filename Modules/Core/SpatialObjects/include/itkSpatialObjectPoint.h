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

  /** Constructor. This one defines the number of dimensions in the
   * SpatialObjectPoint */
  SpatialObjectPoint();

  /** Default destructor. */
  virtual ~SpatialObjectPoint();

  typedef SpatialObjectPoint               Self;
  typedef Point< double, TPointDimension > PointType;
  typedef vnl_vector< double >             VectorType;
  typedef RGBAPixel< float >               PixelType;
  typedef PixelType                        ColorType;

  /** Get the SpatialObjectPoint Id. */
  int GetID() const;

  /** Set the SpatialObjectPoint Id. */
  void SetID(const int newID);

  /** Return a pointer to the point object. */
  const PointType & GetPosition() const;

  /** Set the point object. Couldn't use macros for these methods. */
  void SetPosition(const PointType & newX);

  void SetPosition(const double x0, const double x1);

  void SetPosition(const double x0, const double x1, const double x2);

  /** Copy one SpatialObjectPoint to another */
  Self & operator=(const SpatialObjectPoint & rhs);

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
  int m_ID;

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
