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
#ifndef itkDeformableSimplexMesh3DGradientConstraintForceFilter_hxx
#define itkDeformableSimplexMesh3DGradientConstraintForceFilter_hxx

#include "itkNumericTraits.h"
#include "itkMath.h"
#include "itkPrintHelper.h"

#include <set>

namespace itk
{

template <typename TInputMesh, typename TOutputMesh>
DeformableSimplexMesh3DGradientConstraintForceFilter<TInputMesh, TOutputMesh>::
  DeformableSimplexMesh3DGradientConstraintForceFilter()
  : m_Range(1)
{}

template <typename TInputMesh, typename TOutputMesh>
DeformableSimplexMesh3DGradientConstraintForceFilter<TInputMesh, TOutputMesh>::
  ~DeformableSimplexMesh3DGradientConstraintForceFilter()
{
  this->Clear();
}

template <typename TInputMesh, typename TOutputMesh>
void
DeformableSimplexMesh3DGradientConstraintForceFilter<TInputMesh, TOutputMesh>::PrintSelf(std::ostream & os,
                                                                                         Indent         indent) const
{
  using namespace print_helper;

  Superclass::PrintSelf(os, indent);

  os << indent << "Range: " << m_Range << std::endl;

  os << indent << "StartVoxel: ";
  if (m_StartVoxel != nullptr)
  {
    os << *m_StartVoxel << std::endl;
  }
  else
  {
    os << "(null)" << std::endl;
  }

  os << indent << "Positive: " << m_Positive << std::endl;
  os << indent << "Negative: " << m_Negative << std::endl;

  itkPrintSelfObjectMacro(Image);
}

template <typename TInputMesh, typename TOutputMesh>
void
DeformableSimplexMesh3DGradientConstraintForceFilter<TInputMesh, TOutputMesh>::Clear()
{
  delete m_StartVoxel;
  m_StartVoxel = nullptr;
  std::vector<ImageVoxel *>::iterator it;
  for (it = m_Positive.begin(); it != m_Positive.end(); ++it)
  {
    delete *it;
  }
  m_Positive.erase(m_Positive.begin(), m_Positive.end());
  for (it = m_Negative.begin(); it != m_Negative.end(); ++it)
  {
    delete *it;
  }
  m_Negative.erase(m_Negative.begin(), m_Negative.end());
}

template <typename TInputMesh, typename TOutputMesh>
int
DeformableSimplexMesh3DGradientConstraintForceFilter<TInputMesh, TOutputMesh>::Signi(double a)
{
  if (a < 0)
  {
    return -1;
  }
  if (a > 0)
  {
    return 1;
  }
  else
  {
    return 0;
  }
}

template <typename TInputMesh, typename TOutputMesh>
double
DeformableSimplexMesh3DGradientConstraintForceFilter<TInputMesh, TOutputMesh>::NextVoxel(const double * pp,
                                                                                         int *          ic,
                                                                                         double *       x,
                                                                                         double *       y,
                                                                                         double *       z)
{
  const double dp[3]{ pp[0], pp[1], pp[2] };

  double dlx = NAN;
  if (dp[0] >= 0.0)
  {
    dlx = ((ic[0] + 1) * 1 - *x) / dp[0];
    if (dlx <= 0)
    {
      dlx = ((ic[0] + 2) * 1 - *x) / dp[0];
    }
  }
  else
  {
    dlx = (ic[0] * 1 - *x) / dp[0];
    if (dlx <= 0)
    {
      dlx = ((ic[0] - 1) * 1 - *x) / dp[0];
    }
  }
  double dly = NAN;
  if (dp[1] >= 0.0)
  {
    dly = ((ic[1] + 1) * 1 - *y) / dp[1];
    if (dly <= 0)
    {
      dly = ((ic[1] + 2) * 1 - *y) / dp[1];
    }
  }
  else
  {
    dly = (ic[1] * 1 - *y) / dp[1];
    if (dly <= 0)
    {
      dly = ((ic[1] - 1) * 1 - *y) / dp[1];
    }
  }

  double dlz = NAN;
  if (dp[2] >= 0.0)
  {
    dlz = ((ic[2] + 1) * 1 - *z) / dp[2];
    if (dlz <= 0)
    {
      dlz = ((ic[2] + 2) * 1 - *z) / dp[2];
    }
  }
  else
  {
    dlz = (ic[2] * 1 - *z) / dp[2];
    if (dlz <= 0)
    {
      dlz = ((ic[2] - 1) * 1 - *z) / dp[2];
    }
  }

  double d = NAN;
  if ((dlx < dly) && (dlx < dlz))
  {
    d = dlx;
    ic[0] += this->Signi(dp[0]);
  }
  else if ((dly < dlz) && (dly < dlx))
  {
    d = dly;
    ic[1] += this->Signi(dp[1]);
  }
  else
  {
    d = dlz;
    ic[2] += this->Signi(dp[2]);
  }

  *x += pp[0] * d;
  *y += pp[1] * d;
  *z += pp[2] * d;

  return d;
}

template <typename TInputMesh, typename TOutputMesh>
void
DeformableSimplexMesh3DGradientConstraintForceFilter<TInputMesh, TOutputMesh>::ComputeExternalForce(
  SimplexMeshGeometry *     data,
  const GradientImageType * gradientImage)
{
  // image coordinate
  GradientIndexType coord;
  coord[0] = static_cast<ImageIndexValueType>(data->pos[0]);
  coord[1] = static_cast<ImageIndexValueType>(data->pos[1]);
  coord[2] = static_cast<ImageIndexValueType>(data->pos[2]);

  const OriginalImageType::PointType &   orgn = m_Image->GetOrigin();
  const OriginalImageType::SpacingType & sp = m_Image->GetSpacing();

  if (m_StartVoxel)
  {
    this->Clear();
  }

  OriginalImageIndexType index;
  // ***keep in mind to add direction cosines in here once we have them
  double x = data->pos[0] - orgn[0];
  double y = data->pos[1] - orgn[1];
  double z = data->pos[2] - orgn[2];

  // need to divide by image spacing if it was not one
  int ic[3] = { ic[0] = static_cast<ImageIndexValueType>(x / sp[0]),
                ic[1] = static_cast<ImageIndexValueType>(y / sp[1]),
                ic[2] = static_cast<ImageIndexValueType>(z / sp[2]) };

  if (ic[0] >= 0 && ic[0] < this->m_ImageWidth && ic[1] >= 0 && ic[1] < this->m_ImageHeight && ic[2] >= 0 &&
      ic[2] < this->m_ImageDepth)
  {

    const SIDEEnum side = SIDEEnum::BOTH; // make sure you can set half segment as well but for now
                                          // we just set it to full segment

    int vpos[3] = { ic[0], ic[1], ic[2] };

    double dp[3];
    if (Math::AlmostEquals(data->normal[0], itk::NumericTraits<SimplexMeshGeometry::CovariantVectorType>::ValueType{}))
    {
      dp[0] = 1e-6;
    }
    else
    {
      dp[0] = data->normal[0];
    }

    if (Math::AlmostEquals(data->normal[1], itk::NumericTraits<SimplexMeshGeometry::CovariantVectorType>::ValueType{}))
    {
      dp[1] = 1e-6;
    }
    else
    {
      dp[1] = data->normal[1];
    }

    if (Math::AlmostEquals(data->normal[2], itk::NumericTraits<SimplexMeshGeometry::CovariantVectorType>::ValueType{}))
    {
      dp[2] = 1e-6;
    }
    else
    {
      dp[2] = data->normal[2];
    }

    index[0] = ic[0];
    index[1] = ic[1];
    index[2] = ic[2];
    double pos[3];
    pos[0] = data->pos[0];
    pos[1] = data->pos[1];
    pos[2] = data->pos[2];

    m_StartVoxel = new ImageVoxel(vpos, pos, static_cast<double>(m_Image->GetPixel(index)), 0.0, 0);
    auto * current = new ImageVoxel(vpos, pos, static_cast<double>(m_Image->GetPixel(index)), 0.0, 0);
    m_Positive.push_back(current);

    // scan normal side
    if (side == SIDEEnum::NORMAL || side == SIDEEnum::BOTH)
    {
      int    i = 0;
      bool   stop = false;
      double dist = 0.0;

      while (!stop)
      {
        const double a = NextVoxel(dp, ic, &x, &y, &z);

        pos[0] += a * dp[0];
        pos[1] += a * dp[1];
        pos[2] += a * dp[2];
        dist += a;

        stop = ((dist > m_Range) || !(ic[0] >= 0 && ic[0] < this->m_ImageWidth && ic[1] >= 0 &&
                                      ic[1] < this->m_ImageHeight && ic[2] >= 0 && ic[2] < this->m_ImageDepth));

        if (!stop)
        {
          vpos[0] = ic[0];
          vpos[1] = ic[1];
          vpos[2] = ic[2];
          index[0] = ic[0];
          index[1] = ic[1];
          index[2] = ic[2];

          current = new ImageVoxel(vpos, pos, static_cast<double>(m_Image->GetPixel(index)), dist, ++i);
          m_Positive.push_back(current);
          stop = current->GetDistance() > m_Range;
        }
      }
    }

    // scan inverse normal side
    if (side == SIDEEnum::INVERSE || side == SIDEEnum::BOTH)
    {
      pos[0] = data->pos[0];
      pos[1] = data->pos[1];
      pos[2] = data->pos[2];
      // ***keep in mind to add direction cosines in here once we have them
      x = data->pos[0] - orgn[0];
      y = data->pos[1] - orgn[1];
      z = data->pos[2] - orgn[2];
      // need to divide by image spacing if it was not one
      ic[0] = static_cast<ImageIndexValueType>(x / sp[0]);
      ic[1] = static_cast<ImageIndexValueType>(y / sp[1]);
      ic[2] = static_cast<ImageIndexValueType>(z / sp[2]);

      dp[0] *= -1.0;
      dp[1] *= -1.0;
      dp[2] *= -1.0;
      int    ii = 0;
      bool   stop = false;
      double dist = 0.0;

      while (!stop)
      {
        const double a = NextVoxel(dp, ic, &x, &y, &z);

        pos[0] += a * dp[0];
        pos[1] += a * dp[1];
        pos[2] += a * dp[2];
        dist += a;

        stop = ((dist > m_Range) || !(ic[0] >= 0 && ic[0] < this->m_ImageWidth && ic[1] >= 0 &&
                                      ic[1] < this->m_ImageHeight && ic[2] >= 0 && ic[2] < this->m_ImageDepth));

        if (!stop)
        {
          vpos[0] = ic[0];
          vpos[1] = ic[1];
          vpos[2] = ic[2];
          index[0] = ic[0];
          index[1] = ic[1];
          index[2] = ic[2];

          current = new ImageVoxel(vpos, pos, static_cast<double>(m_Image->GetPixel(index)), dist, --ii);
          m_Negative.push_back(current);
          stop = current->GetDistance() > m_Range;
        }
      }
    }
  }
  else
  {
    m_StartVoxel = nullptr;
  }

  // now fun begins try to use all the above


  double            max = 0;
  GradientIndexType coord2{};
  for (auto & imgVoxel : m_Positive)
  {
    GradientIndexType coord3;
    coord3[0] = static_cast<GradientIndexValueType>(imgVoxel->GetX());
    coord3[1] = static_cast<GradientIndexValueType>(imgVoxel->GetY());
    coord3[2] = static_cast<GradientIndexValueType>(imgVoxel->GetZ());

    const GradientType & gradient3 = gradientImage->GetPixel(coord3);

    PointType vec_for;
    vec_for[0] = gradient3[0];
    vec_for[1] = gradient3[1];
    vec_for[2] = gradient3[2];
    // check magnitude

    const double mag = std::sqrt(dot_product(vec_for.GetVnlVector(), vec_for.GetVnlVector()));
    if (mag > max)
    {
      max = mag;
      coord2 = coord3;
    }
  }

  const GradientType & gradient0 = gradientImage->GetPixel(coord);
  const GradientType & gradient2 = gradientImage->GetPixel(coord2);

  PointType vec_for;
  vec_for[0] = gradient2[0];
  vec_for[1] = gradient2[1];
  vec_for[2] = gradient2[2];

  // now check highest gradient magnitude direction
  const double mag = dot_product(vec_for.GetVnlVector(), data->normal.GetVnlVector());
  if (mag > 0)
  {
    vec_for[0] -= gradient0[0];
    vec_for[1] -= gradient0[1];
    vec_for[2] -= gradient0[2];
  }
  else
  {
    vec_for.Fill(0);
  }

  data->externalForce[0] = vec_for[0];
  data->externalForce[1] = vec_for[1];
  data->externalForce[2] = vec_for[2];
}
} // namespace itk

#endif
