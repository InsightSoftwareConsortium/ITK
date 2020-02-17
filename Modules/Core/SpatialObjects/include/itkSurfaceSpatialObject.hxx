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
#ifndef itkSurfaceSpatialObject_hxx
#define itkSurfaceSpatialObject_hxx


#include "itkMath.h"
#include "itkSurfaceSpatialObject.h"

namespace itk
{
/** Constructor */
template <unsigned int TDimension>
SurfaceSpatialObject<TDimension>::SurfaceSpatialObject()
{
  this->SetTypeName("SurfaceSpatialObject");

  this->Clear();

  this->Update();
}

template <unsigned int TDimension>
void
SurfaceSpatialObject<TDimension>::Clear()
{
  Superclass::Clear();

  this->GetProperty().SetRed(1);
  this->GetProperty().SetGreen(0);
  this->GetProperty().SetBlue(0);
  this->GetProperty().SetAlpha(1);

  this->Modified();
}

/** InternalClone */
template <unsigned int TDimension>
typename LightObject::Pointer
SurfaceSpatialObject<TDimension>::InternalClone() const
{
  // Default implementation just copies the parameters from
  // this to new transform.
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval = dynamic_cast<Self *>(loPtr.GetPointer());
  if (rval.IsNull())
  {
    itkExceptionMacro(<< "downcast to type " << this->GetNameOfClass() << " failed.");
  }

  return loPtr;
}

/** Print the surface object */
template <unsigned int TDimension>
void
SurfaceSpatialObject<TDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "SurfaceSpatialObject(" << this << ")" << std::endl;
  Superclass::PrintSelf(os, indent);
}

/** Approximate the normals of the surface */
template <unsigned int TDimension>
bool
SurfaceSpatialObject<TDimension>::Approximate3DNormals()
{
  if (TDimension != 3)
  {
    itkExceptionMacro("Approximate3DNormals works only in 3D");
  }

  if (this->m_Points.size() < 3)
  {
    itkExceptionMacro("Approximate3DNormals requires at least 3 points");
  }

  typename SurfacePointListType::iterator it = this->m_Points.begin();
  typename SurfacePointListType::iterator itEnd = this->m_Points.end();

  while (it != itEnd)
  {
    // Try to find 3 points close to the corresponding point
    SurfacePointType pt = *it;
    PointType        pos = (*it).GetPositionInObjectSpace();

    std::list<int> badId;
    unsigned int   identifier[3];
    double         absvec = 0;
    do
    {
      identifier[0] = 0;
      identifier[1] = 0;
      identifier[2] = 0;

      float max[3];
      max[0] = 99999999;
      max[1] = 99999999;
      max[2] = 99999999;

      typename SurfacePointListType::const_iterator it2 = this->m_Points.begin();

      int i = 0;
      while (it2 != this->m_Points.end())
      {
        if (it2 == it)
        {
          i++;
          it2++;
          continue;
        }

        bool                           badPoint = false;
        std::list<int>::const_iterator itBadId = badId.begin();
        while (itBadId != badId.end())
        {
          if (*itBadId == i)
          {
            badPoint = true;
            break;
          }
          itBadId++;
        }

        if (badPoint)
        {
          i++;
          it2++;
          continue;
        }

        PointType pos2 = (*it2).GetPositionInObjectSpace();
        float     distance = pos2.EuclideanDistanceTo(pos);

        // Check that the point is not the same as some previously defined
        bool valid = true;
        for (auto & j : identifier)
        {
          PointType p = this->m_Points[j].GetPositionInObjectSpace();
          float     d = pos2.EuclideanDistanceTo(p);
          if (Math::AlmostEquals(d, 0.0f))
          {
            valid = false;
            break;
          }
        }

        if (Math::AlmostEquals(distance, 0.0f) || !valid)
        {
          i++;
          it2++;
          continue;
        }

        if (distance < max[0])
        {
          max[2] = max[1];
          max[1] = max[0];
          max[0] = distance;
          identifier[0] = i;
        }
        else if (distance < max[1])
        {
          max[2] = max[1];
          max[1] = distance;
          identifier[1] = i;
        }
        else if (distance < max[2])
        {
          max[2] = distance;
          identifier[2] = i;
        }
        i++;
        it2++;
      }

      if ((identifier[0] == identifier[1]) || (identifier[1] == identifier[2]) || (identifier[0] == identifier[2]))
      {
        std::cout << "Cannot find 3 distinct points!" << std::endl;
        std::cout << identifier[0] << " : " << identifier[1] << " : " << identifier[2] << std::endl;
        std::cout << max[0] << " : " << max[1] << " : " << max[2] << std::endl;
        return false;
      }

      PointType v1 = this->m_Points[identifier[0]].GetPositionInObjectSpace();
      PointType v2 = this->m_Points[identifier[1]].GetPositionInObjectSpace();
      PointType v3 = this->m_Points[identifier[2]].GetPositionInObjectSpace();

      double coa = -(v1[1] * (v2[2] - v3[2]) + v2[1] * (v3[2] - v1[2]) + v3[1] * (v1[2] - v2[2]));
      double cob = -(v1[2] * (v2[0] - v3[0]) + v2[2] * (v3[0] - v1[0]) + v3[2] * (v1[0] - v2[0]));
      double coc = -(v1[0] * (v2[1] - v3[1]) + v2[0] * (v3[1] - v1[1]) + v3[0] * (v1[1] - v2[1]));

      absvec = -std::sqrt((double)((coa * coa) + (cob * cob) + (coc * coc)));

      if (Math::AlmostEquals(absvec, 0.0))
      {
        badId.push_back(identifier[2]);
      }
      else
      {
        CovariantVectorType normal;
        normal[0] = coa / absvec;
        normal[1] = cob / absvec;
        normal[2] = coc / absvec;
        (*it).SetNormalInObjectSpace(normal);
      }
    } while ((Math::AlmostEquals(absvec, 0.0)) && (badId.size() < this->m_Points.size() - 1));

    if (Math::AlmostEquals(absvec, 0.0))
    {
      std::cout << "Approximate3DNormals Failed!" << std::endl;
      std::cout << identifier[0] << " : " << identifier[1] << " : " << identifier[2] << std::endl;
      std::cout << badId.size() << " : " << this->m_Points.size() - 1 << std::endl;
      return false;
    }

    it++;
  }

  return true;
}

} // end namespace itk

#endif
