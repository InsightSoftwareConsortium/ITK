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
#ifndef itkGaussianSpatialObject_hxx
#define itkGaussianSpatialObject_hxx

#include <cmath>
#include "itkGaussianSpatialObject.h"

namespace itk
{
/** Constructor */
template <unsigned int TDimension>
GaussianSpatialObject<TDimension>::GaussianSpatialObject()
{
  this->SetTypeName("GaussianSpatialObject");

  this->Clear();

  this->Update();
}

template <unsigned int TDimension>
void
GaussianSpatialObject<TDimension>::Clear()
{
  Superclass::Clear();

  m_CenterInObjectSpace.Fill(0.0);
  m_RadiusInObjectSpace = 1.0;
  m_SigmaInObjectSpace = 1.0;
  m_Maximum = 1.0;

  this->Modified();
}

/** The z-score is the root mean square of the z-scores along
 *  each principal axis. */
template <unsigned int TDimension>
typename GaussianSpatialObject<TDimension>::ScalarType
GaussianSpatialObject<TDimension>::SquaredZScoreInObjectSpace(const PointType & point) const
{
  ScalarType r = 0;
  for (unsigned int i = 0; i < TDimension; i++)
  {
    r += point[i] * point[i];
  }
  return r / (m_SigmaInObjectSpace * m_SigmaInObjectSpace);
}

/** The z-score is the root mean square of the z-scores along
 *  each principal axis. */
template <unsigned int TDimension>
typename GaussianSpatialObject<TDimension>::ScalarType
GaussianSpatialObject<TDimension>::SquaredZScoreInWorldSpace(const PointType & point) const
{
  PointType transformedPoint = this->GetObjectToWorldTransformInverse()->TransformPoint(point);

  return this->SquaredZScoreInObjectSpace(transformedPoint);
}


/** Test whether a point is inside or outside the object.
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth. */
template <unsigned int TDimension>
bool
GaussianSpatialObject<TDimension>::IsInsideInObjectSpace(const PointType & point) const
{
  if (m_RadiusInObjectSpace > itk::Math::eps)
  {
    if (this->GetMyBoundingBoxInObjectSpace()->IsInside(point))
    {
      double r = 0;
      for (unsigned int i = 0; i < TDimension; i++)
      {
        r += (point[i] - m_CenterInObjectSpace[i]) * (point[i] - m_CenterInObjectSpace[i]);
      }

      r /= (m_RadiusInObjectSpace * m_RadiusInObjectSpace);

      if (r <= 1.0)
      {
        return true;
      }
    }
  }

  return false;
}

/** Compute the bounds of the Gaussian (as determined by the
 *  specified radius). */
template <unsigned int TDimension>
void
GaussianSpatialObject<TDimension>::ComputeMyBoundingBox()
{
  itkDebugMacro("Computing Guassian bounding box");

  PointType pnt1;
  PointType pnt2;
  for (unsigned int i = 0; i < TDimension; i++)
  {
    pnt1[i] = m_CenterInObjectSpace[i] - m_RadiusInObjectSpace;
    pnt2[i] = m_CenterInObjectSpace[i] + m_RadiusInObjectSpace;
  }

  this->GetModifiableMyBoundingBoxInObjectSpace()->SetMinimum(pnt1);
  this->GetModifiableMyBoundingBoxInObjectSpace()->SetMaximum(pnt1);
  this->GetModifiableMyBoundingBoxInObjectSpace()->ConsiderPoint(pnt2);
  this->GetModifiableMyBoundingBoxInObjectSpace()->ComputeBoundingBox();
}

/** Returns the value at one point. */
template <unsigned int TDimension>
bool
GaussianSpatialObject<TDimension>::ValueAtInObjectSpace(const PointType &   point,
                                                        double &            value,
                                                        unsigned int        depth,
                                                        const std::string & name) const
{
  itkDebugMacro("Getting the value of the ellipse at " << point);
  if (this->GetTypeName().find(name) != std::string::npos)
  {
    if (IsInsideInObjectSpace(point))
    {
      const double zsq = this->SquaredZScoreInObjectSpace(point);
      value = m_Maximum * (ScalarType)std::exp(-zsq / 2.0);
      return true;
    }
  }

  if (depth > 0)
  {
    if (Superclass::ValueAtChildrenInObjectSpace(point, value, depth - 1, name))
    {
      return true;
    }
  }

  value = this->GetDefaultOutsideValue();
  return false;
}

/** Returns the sigma=m_Radius level set of the Gaussian function, as an
 * EllipseSpatialObject. */
template <unsigned int TDimension>
typename EllipseSpatialObject<TDimension>::Pointer
GaussianSpatialObject<TDimension>::GetEllipsoid() const
{
  using EllipseType = itk::EllipseSpatialObject<TDimension>;
  typename EllipseType::Pointer ellipse = EllipseType::New();

  ellipse->SetRadiusInObjectSpace(m_RadiusInObjectSpace);
  ellipse->SetCenterInObjectSpace(m_CenterInObjectSpace);

  ellipse->GetModifiableObjectToWorldTransform()->SetFixedParameters(
    this->GetObjectToWorldTransform()->GetFixedParameters());
  ellipse->GetModifiableObjectToWorldTransform()->SetParameters(this->GetObjectToWorldTransform()->GetParameters());

  ellipse->Update();

  return ellipse;
}

/** InternalClone */
template <unsigned int TDimension>
typename LightObject::Pointer
GaussianSpatialObject<TDimension>::InternalClone() const
{
  // Default implementation just copies the parameters from
  // this to new transform.
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval = dynamic_cast<Self *>(loPtr.GetPointer());
  if (rval.IsNull())
  {
    itkExceptionMacro(<< "downcast to type " << this->GetNameOfClass() << " failed.");
  }
  rval->SetMaximum(this->GetMaximum());
  rval->SetRadiusInObjectSpace(this->GetRadiusInObjectSpace());
  rval->SetSigmaInObjectSpace(this->GetSigmaInObjectSpace());
  rval->SetCenterInObjectSpace(this->GetCenterInObjectSpace());

  return loPtr;
}

/** Print Self function. */
template <unsigned int TDimension>
void
GaussianSpatialObject<TDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Maximum: " << m_Maximum << std::endl;
  os << "Radius: " << m_RadiusInObjectSpace << std::endl;
  os << "Sigma: " << m_SigmaInObjectSpace << std::endl;
  os << "Center: " << m_CenterInObjectSpace << std::endl;
}
} // end namespace itk

#endif
