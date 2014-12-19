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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkDeformableSimplexMesh3DBalloonForceFilter_hxx
#define itkDeformableSimplexMesh3DBalloonForceFilter_hxx

#include "itkDeformableSimplexMesh3DBalloonForceFilter.h"
#include "itkNumericTraits.h"

#include <set>

namespace itk
{
/* Constructor. */
template< typename TInputMesh, typename TOutputMesh >
DeformableSimplexMesh3DBalloonForceFilter< TInputMesh, TOutputMesh >
::DeformableSimplexMesh3DBalloonForceFilter()

{
  m_Kappa = 0.1;
}

template< typename TInputMesh, typename TOutputMesh >
DeformableSimplexMesh3DBalloonForceFilter< TInputMesh, TOutputMesh >
::~DeformableSimplexMesh3DBalloonForceFilter()
{}

/* PrintSelf. */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableSimplexMesh3DBalloonForceFilter< TInputMesh, TOutputMesh >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Kappa = " << m_Kappa << std::endl;
} /* End PrintSelf. */

/** Compute model Displacement according to image gradient forces */
template< typename TInputMesh, typename TOutputMesh >
void
DeformableSimplexMesh3DBalloonForceFilter< TInputMesh, TOutputMesh >
::ComputeExternalForce(SimplexMeshGeometry *data, const GradientImageType *gradientImage)
{
  PointType         vec_for, tmp_vec_1, tmp_vec_2, tmp_vec_3;
  GradientIndexType coord, coord2, tmp_co_1, tmp_co_2, tmp_co_3;

  coord[0] = static_cast< GradientIndexValueType >( data->pos[0] );
  coord[1] = static_cast< GradientIndexValueType >( data->pos[1] );
  coord[2] = static_cast< GradientIndexValueType >( data->pos[2] );

  coord2[0] = static_cast< GradientIndexValueType >( std::ceil(data->pos[0]) );
  coord2[1] = static_cast< GradientIndexValueType >( std::ceil(data->pos[1]) );
  coord2[2] = static_cast< GradientIndexValueType >( std::ceil(data->pos[2]) );

  tmp_co_1[0] = coord2[0];
  tmp_co_1[1] = coord[1];
  tmp_co_1[2] = coord[2];

  tmp_co_2[0] = coord[0];
  tmp_co_2[1] = coord2[1];
  tmp_co_2[2] = coord[2];

  tmp_co_3[0] = coord[0];
  tmp_co_3[1] = coord[1];
  tmp_co_3[2] = coord2[2];

  if ( ( coord[0] >= 0 ) && ( coord[1] >= 0 ) && ( coord[2] >= 0 )
       && ( coord2[0] < this->GetImageWidth() ) && ( coord2[1] < this->GetImageHeight() )
       && ( coord2[2] < this->GetImageDepth() ) )
    {
    vec_for[0] = gradientImage->GetPixel(coord)[0];
    vec_for[1] = gradientImage->GetPixel(coord)[1];
    vec_for[2] = gradientImage->GetPixel(coord)[2];

    tmp_vec_1[0] = gradientImage->GetPixel(tmp_co_1)[0] - gradientImage->GetPixel(coord)[0];
    tmp_vec_1[1] = gradientImage->GetPixel(tmp_co_1)[1] - gradientImage->GetPixel(coord)[1];
    tmp_vec_1[2] = gradientImage->GetPixel(tmp_co_1)[2] - gradientImage->GetPixel(coord)[2];
    tmp_vec_2[0] = gradientImage->GetPixel(tmp_co_2)[0] - gradientImage->GetPixel(coord)[0];
    tmp_vec_2[1] = gradientImage->GetPixel(tmp_co_2)[1] - gradientImage->GetPixel(coord)[1];
    tmp_vec_2[2] = gradientImage->GetPixel(tmp_co_2)[2] - gradientImage->GetPixel(coord)[2];
    tmp_vec_3[0] = gradientImage->GetPixel(tmp_co_3)[0] - gradientImage->GetPixel(coord)[0];
    tmp_vec_3[1] = gradientImage->GetPixel(tmp_co_3)[1] - gradientImage->GetPixel(coord)[1];
    tmp_vec_3[2] = gradientImage->GetPixel(tmp_co_3)[2] - gradientImage->GetPixel(coord)[2];

    vec_for[0] = vec_for[0] + ( ( data->pos )[0] - coord[0] ) * tmp_vec_1[0]
                 + ( ( data->pos )[1] - coord[1] ) * tmp_vec_2[0] + ( ( data->pos )[2] - coord[2] ) * tmp_vec_3[0];
    vec_for[1] = vec_for[1] + ( ( data->pos )[1] - coord[1] ) * tmp_vec_2[1]
                 + ( ( data->pos )[0] - coord[0] ) * tmp_vec_1[1] + ( ( data->pos )[2] - coord[2] ) * tmp_vec_3[1];
    vec_for[2] = vec_for[2] + ( ( data->pos )[2] - coord[2] ) * tmp_vec_3[2]
                 + ( ( data->pos )[1] - coord[1] ) * tmp_vec_2[2] + ( ( data->pos )[0] - coord[0] ) * tmp_vec_1[2];
    }
  else
    {
    vec_for.Fill(0);
    }

  double mag = dot_product( data->normal.GetVnlVector(), vec_for.GetVnlVector() );

  vec_for[0] = this->GetBeta() * mag * ( data->normal )[0]; /*num_for*/
  vec_for[1] = this->GetBeta() * mag * ( data->normal )[1]; /*num_for*/
  vec_for[2] = this->GetBeta() * mag * ( data->normal )[2]; /*num_for*/

  vec_for[0] += m_Kappa * data->normal[0];
  vec_for[1] += m_Kappa * data->normal[1];
  vec_for[2] += m_Kappa * data->normal[2];

  //mag = vec_for.GetVectorFromOrigin().GetNorm();

  //if (mag > 0.5)
  //{
  //  for (int i=0; i<3; i++)
  //    vec_for[i] = (0.5 * vec_for[i])/mag;
  //}
  //
  data->externalForce[0] = vec_for[0];
  data->externalForce[1] = vec_for[1];
  data->externalForce[2] = vec_for[2];
}
} /* end namespace itk. */

#endif //itkDeformableSimplexMesh3DBalloonForceFilter_hxx
