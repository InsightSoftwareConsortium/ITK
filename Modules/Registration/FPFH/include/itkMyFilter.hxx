/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMyFilter_hxx
#define itkMyFilter_hxx


#include "math.h"
#include "itkMyFilter.h"

namespace itk
{

template <typename TInputPointSet, typename TOutputPointSet>
MyFilter<TInputPointSet, TOutputPointSet>::MyFilter()
{}

template <typename TInputPointSet, typename TOutputPointSet>
typename MyFilter<TInputPointSet, TOutputPointSet>::Vector4d
MyFilter<TInputPointSet, TOutputPointSet>::ComputePairFeatures(const Vector3d & p1,
                                                               const Vector3d & n1,
                                                               const Vector3d & p2,
                                                               const Vector3d & n2)
{
  Vector4d result;
  Vector3d dp2p1 = p2 - p1;
  result[3] = dp2p1.GetNorm();

  if (result[3] == 0.0)
  {
    return Vector4d();
  }

  auto n1_copy = n1;
  auto n2_copy = n2;

  double angle1 = n1_copy * (dp2p1) / result[3];
  double angle2 = n2_copy * (dp2p1) / result[3];
  if (std::acos(std::fabs(angle1)) > std::acos(std::fabs(angle2)))
  {
    n1_copy = n2;
    n2_copy = n1;
    dp2p1 *= -1.0;
    result[2] = -angle2;
  }
  else
  {
    result[2] = angle1;
  }

  auto   v = itk::CrossProduct(dp2p1, n1_copy);
  double v_norm = v.GetNorm();
  if (v_norm == 0.0)
  {
    return Vector4d();
  }
  v /= v_norm;
  auto w = itk::CrossProduct(n1_copy, v);
  result[1] = v * (n2_copy);
  result[0] = std::atan2(w * (n2_copy), n1_copy * (n2_copy));

  return result;
}

template <typename TInputPointSet, typename TOutputPointSet>
typename MyFilter<TInputPointSet, TOutputPointSet>::FeatureType
MyFilter<TInputPointSet, TOutputPointSet>::ComputeSPFHFeature(TInputPointSet * input,
                                                              TInputPointSet * input_normals,
                                                              unsigned int     radius)
{
  PointsLocatorTypePointer kdtree = PointsLocatorType::New();
  kdtree->SetPoints(input->GetPoints());
  kdtree->Initialize();

  std::cout << "KdTree is " << kdtree << std::endl;

  FeatureType feature;
  feature.resize(33 * (unsigned int)input->GetNumberOfPoints());
  std::cout << "feature size is " << feature.size();

  Vector3d temp_point_vector1, temp_point_vector2;
  Vector3d temp_normal_vector1, temp_normal_vector2;

  for (int i = 0; i < (unsigned int)input->GetNumberOfPoints(); i++)
  {
    auto point = input->GetPoint(i);
    auto normal = input_normals->GetPoint(i);

    // std::cout << "Point " << point << std::endl;

    typename PointsLocatorType::NeighborsIdentifierType indices;
    kdtree->FindPointsWithinRadius(point, radius, indices);

    std::cout << i << " Number of points are " << indices.size() << std::endl;

    if (indices.size() > 1)
    {
      // only compute SPFH feature when a point has neighbors
      double hist_incr = 100.0 / (double)(indices.size() - 1);
      for (size_t k = 1; k < indices.size(); k++)
      {
        auto point2 = input->GetPoint(indices[k]);
        auto normal2 = input_normals->GetPoint(indices[k]);

        // skip the point itself, compute histogram
        for (int ik = 0; ik < 3; ++ik)
        {
          temp_point_vector1[ik] = point[ik];
          temp_normal_vector1[ik] = normal[ik];
          temp_point_vector2[ik] = point2[ik];
          temp_normal_vector2[ik] = normal2[ik];
        }

        Vector4d pair_feature =
          ComputePairFeatures(temp_point_vector1, temp_normal_vector1, temp_point_vector2, temp_normal_vector2);

        int h_index = (int)(floor(11 * (pair_feature[0] + M_PI) / (2.0 * M_PI)));
        if (h_index < 0)
        {
          h_index = 0;
        }
        if (h_index >= 11)
        {
          h_index = 10;
        }
        unsigned int temp_index = h_index * 33 + i;
        feature[temp_index] = hist_incr + feature[temp_index];

        h_index = (int)(floor(11 * (pair_feature[1] + 1.0) * 0.5));
        if (h_index < 0)
        {
          h_index = 0;
        }
        if (h_index >= 11)
        {
          h_index = 10;
        }
        temp_index = (h_index + 11) * 33 + i;
        feature[temp_index] = hist_incr + feature[temp_index];

        h_index = (int)(floor(11 * (pair_feature[2] + 1.0) * 0.5));
        if (h_index < 0)
        {
          h_index = 0;
        }
        if (h_index >= 11)
        {
          h_index = 10;
        }
        temp_index = (h_index + 22) * 33 + i;
        feature[temp_index] = hist_incr + feature[temp_index];
      }
    }
  }

  std::cout << "Feature " << feature.size() << std::endl;
  return feature;
}

template <typename TInputPointSet, typename TOutputPointSet>
void
MyFilter<TInputPointSet, TOutputPointSet>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template <typename TInputPointSet, typename TOutputPointSet>
void
MyFilter<TInputPointSet, TOutputPointSet>::GenerateData()
{
  auto input = this->GetInput();
  auto output = this->GetOutput();
  auto inPts = input->GetPoints();

  itkDebugMacro(<< "Executing connectivity");

  //  Check input/allocate storage
  IdentifierType numPts = input->GetNumberOfPoints();
  if (numPts < 1)
  {
    itkDebugMacro(<< "No data to connect!");
    return;
  }
  return;
}

} // end namespace itk

#endif // itkMyFilter_hxx
