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
#ifndef itkPointFeature_hxx
#define itkPointFeature_hxx


#include "math.h"
#include "itkPointFeature.h"
#include "itkMultiThreaderBase.h"

namespace itk
{

template <typename TInputPointSet, typename TOutputPointSet>
PointFeature<TInputPointSet, TOutputPointSet>::PointFeature()
{}

template <typename TInputPointSet, typename TOutputPointSet>
typename PointFeature<TInputPointSet, TOutputPointSet>::Vector4d
PointFeature<TInputPointSet, TOutputPointSet>::ComputePairFeatures(const Vector3d & p1,
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
typename PointFeature<TInputPointSet, TOutputPointSet>::FeatureTypePointer
PointFeature<TInputPointSet, TOutputPointSet>::ComputeSPFHFeature(TInputPointSet * input,
                                                                  TInputPointSet * input_normals,
                                                                  double           radius,
                                                                  unsigned int     neighbors)
{
  PointsLocatorTypePointer kdtree = PointsLocatorType::New();
  kdtree->SetPoints(input->GetPoints());
  kdtree->Initialize();

  unsigned long int   num_of_points = input->GetNumberOfPoints();
  std::vector<double> feature1(33 * num_of_points, 0);

  auto ProcessPoint = [&](int i) {
    auto point = input->GetPoint(i);
    auto normal = input_normals->GetPoint(i);

    typename PointsLocatorType::NeighborsIdentifierType indices;
    kdtree->FindClosestNPoints(point, neighbors, indices);

    if (indices.size() > 1)
    {
      std::vector<std::pair<double, int>> neighbor_vect;
      neighbor_vect.reserve(indices.size());

      for (size_t k = 0; k < indices.size(); k++)
      {
        auto   point_diff = point - input->GetPoint(indices[k]);
        double dist = point_diff.GetNorm();

        // skip the point itself
        if (dist == 0.0)
          continue;

        if (dist < radius)
        {
          dist = dist * dist;
          neighbor_vect.push_back(std::make_pair(dist, indices[k]));
        }
      }

      unsigned int neighbor_count = std::min(neighbors, (unsigned int)neighbor_vect.size());

      // only compute SPFH feature when a point has neighbors
      double hist_incr = 100.0 / (double)neighbor_count;
      for (size_t k = 0; k < neighbor_count; k++)
      {
        auto point2 = input->GetPoint(neighbor_vect[k].second);
        auto normal2 = input_normals->GetPoint(neighbor_vect[k].second);

        Vector3d temp_point_vector1, temp_point_vector2;
        Vector3d temp_normal_vector1, temp_normal_vector2;

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

        int h_index = (int)(floor(11 * (pair_feature[0] + itk::Math::pi) / (2.0 * itk::Math::pi)));
        if (h_index < 0)
        {
          h_index = 0;
        }
        if (h_index >= 11)
        {
          h_index = 10;
        }
        unsigned int temp_index = h_index * num_of_points + i;
        feature1[temp_index] = hist_incr + feature1[temp_index];

        h_index = (int)(floor(11 * (pair_feature[1] + 1.0) * 0.5));
        if (h_index < 0)
        {
          h_index = 0;
        }
        if (h_index >= 11)
        {
          h_index = 10;
        }
        temp_index = (h_index + 11) * num_of_points + i;
        feature1[temp_index] = hist_incr + feature1[temp_index];

        h_index = (int)(floor(11 * (pair_feature[2] + 1.0) * 0.5));
        if (h_index < 0)
        {
          h_index = 0;
        }
        if (h_index >= 11)
        {
          h_index = 10;
        }
        temp_index = (h_index + 22) * num_of_points + i;
        feature1[temp_index] = hist_incr + feature1[temp_index];
      }
    }
  };

  itk::MultiThreaderBase::Pointer mt = itk::MultiThreaderBase::New();
  mt->ParallelizeArray(0, num_of_points, ProcessPoint, nullptr);

  // This is done to optimize the code by avoiding GetElement, SetElement overhead.
  auto feature = FeatureType::New();
  feature->CastToSTLContainer() = feature1;
  return feature;
}

template <typename TInputPointSet, typename TOutputPointSet>
void
PointFeature<TInputPointSet, TOutputPointSet>::ComputeFPFHFeature(TInputPointSet * input,
                                                                  TInputPointSet * input_normals,
                                                                  double           radius,
                                                                  unsigned int     neighbors)
{
  unsigned long int   num_of_points = input->GetNumberOfPoints();
  std::vector<double> fpfh2(33 * num_of_points, 0.0);

  PointsLocatorTypePointer kdtree = PointsLocatorType::New();
  kdtree->SetPoints(input->GetPoints());
  kdtree->Initialize();

  auto spfh = ComputeSPFHFeature(input, input_normals, radius, neighbors);

  auto & spfh1 = spfh->CastToSTLContainer();

  // Method to perform processing in parallel
  auto ProcessPoint = [&](int i) {
    auto point = input->GetPoint(i);

    typename PointsLocatorType::NeighborsIdentifierType indices;
    kdtree->FindClosestNPoints(point, neighbors, indices);

    if (indices.size() > 1)
    {
      double sum[3] = { 0.0, 0.0, 0.0 };

      std::vector<std::pair<float, int>> neighbor_vect;
      neighbor_vect.reserve(indices.size());

      for (size_t k = 0; k < indices.size(); k++)
      {
        auto   point_diff = point - input->GetPoint(indices[k]);
        double dist = point_diff.GetNorm();

        // skip the point itself
        if (dist == 0.0)
          continue;

        if (dist < radius)
        {
          dist = dist * dist;
          neighbor_vect.push_back(std::make_pair(dist, indices[k]));
        }
      }

      // Take only first neighbors in sorted order
      unsigned int neighbor_count = std::min(neighbors, (unsigned int)neighbor_vect.size());
      for (size_t k = 0; k < neighbor_count; k++)
      {
        for (int j = 0; j < 33; j++)
        {
          double val = spfh1[j * num_of_points + neighbor_vect[k].second] / neighbor_vect[k].first;
          sum[j / 11] += val;
          fpfh2[j * num_of_points + i] = fpfh2[j * num_of_points + i] + val;
        }
      }

      for (int j = 0; j < 3; j++)
      {
        if (sum[j] != 0.0)
        {
          sum[j] = 100.0 / sum[j];
        }
      }

      for (int j = 0; j < 33; j++)
      {
        fpfh2[j * num_of_points + i] = fpfh2[j * num_of_points + i] * sum[j / 11];
        fpfh2[j * num_of_points + i] = fpfh2[j * num_of_points + i] + spfh1[j * num_of_points + i];
      }
    }
  };

  itk::MultiThreaderBase::Pointer mt = itk::MultiThreaderBase::New();
  mt->ParallelizeArray(0, num_of_points, ProcessPoint, nullptr);

  // This is done to optimize the code by avoiding GetElement, SetElement overhead.
  this->m_FpfhFeature = FeatureType::New();
  this->m_FpfhFeature->CastToSTLContainer() = fpfh2;
}


template <typename TInputPointSet, typename TOutputPointSet>
void
PointFeature<TInputPointSet, TOutputPointSet>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

template <typename TInputPointSet, typename TOutputPointSet>
void
PointFeature<TInputPointSet, TOutputPointSet>::GenerateData()
{
  auto                  input = this->GetInput();
  [[maybe_unused]] auto output = this->GetOutput(); // Exercise function
  [[maybe_unused]] auto inPts = input->GetPoints(); // Exercise function

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

#endif // itkPointFeature_hxx
