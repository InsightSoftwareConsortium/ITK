/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkDeformableSimplexMesh3DBalloonForceFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDeformableSimplexMesh3DBalloonForceFilter_TXX
#define __itkDeformableSimplexMesh3DBalloonForceFilter_TXX

#include "itkDeformableSimplexMesh3DBalloonForceFilter.h"
#include "itkNumericTraits.h"
//#include "itkMapContainer.h"

#include <set>

namespace itk
  {

  /* Constructor. */
  template <typename TInputMesh, typename TOutputMesh>
    DeformableSimplexMesh3DBalloonForceFilter<TInputMesh, TOutputMesh> 
    ::DeformableSimplexMesh3DBalloonForceFilter()

    {
    /*m_Step = 0;
    m_Iterations = 20;
    m_Alpha = 0.2;
    m_Gamma = 0.05;
    m_Rigidity = 1;*/

    //OutputMeshPointer output = OutputMeshType::New();
    //this->ProcessObject::SetNumberOfRequiredOutputs(1);
    //this->ProcessObject::SetNthOutput(0, output.GetPointer());

    //m_Data = NULL;
    }

  template <typename TInputMesh, typename TOutputMesh>
    DeformableSimplexMesh3DBalloonForceFilter<TInputMesh, TOutputMesh>
    ::~DeformableSimplexMesh3DBalloonForceFilter()
    {
    }


  /* PrintSelf. */
  template <typename TInputMesh, typename TOutputMesh>
    void
    DeformableSimplexMesh3DBalloonForceFilter<TInputMesh, TOutputMesh>
    ::PrintSelf(std::ostream& os, Indent indent) const
    {
    Superclass::PrintSelf(os,indent); 
    }/* End PrintSelf. */




  /** Compute model Displacement according to image gradient forces */
  template <typename TInputMesh, typename TOutputMesh>
    void
    DeformableSimplexMesh3DBalloonForceFilter<TInputMesh, TOutputMesh>
    ::ComputeExternalForce(SimplexMeshGeometry * data)
    {
    PointType vec_for, tmp_vec_1, tmp_vec_2, tmp_vec_3;
    GradientIndexType coord, coord2, tmp_co_1, tmp_co_2, tmp_co_3;

    coord[0] = static_cast<GradientIndexValueType>(data->pos[0]);
    coord[1] = static_cast<GradientIndexValueType>(data->pos[1]);
    coord[2] = static_cast<GradientIndexValueType>(data->pos[2]);

    coord2[0] = static_cast<GradientIndexValueType>( ceil(data->pos[0]) );
    coord2[1] = static_cast<GradientIndexValueType>( ceil(data->pos[1]) );
    coord2[2] = static_cast<GradientIndexValueType>( ceil(data->pos[2]) );

    tmp_co_1[0] = coord2[0];
    tmp_co_1[1] = coord[1];
    tmp_co_1[2] = coord[2];

    tmp_co_2[0] = coord[0];
    tmp_co_2[1] = coord2[1];
    tmp_co_2[2] = coord[2];

    tmp_co_3[0] = coord[0];
    tmp_co_3[1] = coord[1];
    tmp_co_3[2] = coord2[2];

    if ( (coord[0] >= 0) && (coord[1] >= 0) && (coord[2] >= 0) && 
      (coord2[0] < m_ImageWidth) && (coord2[1] < m_ImageHeight) && (coord2[2] < m_ImageDepth) ) 
      {
      vec_for[0] = m_Gradient->GetPixel(coord)[0];
      vec_for[1] = m_Gradient->GetPixel(coord)[1];
      vec_for[2] = m_Gradient->GetPixel(coord)[2];

      tmp_vec_1[0] = m_Gradient->GetPixel(tmp_co_1)[0] - m_Gradient->GetPixel(coord)[0];
      tmp_vec_1[1] = m_Gradient->GetPixel(tmp_co_1)[1] - m_Gradient->GetPixel(coord)[1];
      tmp_vec_1[2] = m_Gradient->GetPixel(tmp_co_1)[2] - m_Gradient->GetPixel(coord)[2];
      tmp_vec_2[0] = m_Gradient->GetPixel(tmp_co_2)[0] - m_Gradient->GetPixel(coord)[0];
      tmp_vec_2[1] = m_Gradient->GetPixel(tmp_co_2)[1] - m_Gradient->GetPixel(coord)[1];
      tmp_vec_2[2] = m_Gradient->GetPixel(tmp_co_2)[2] - m_Gradient->GetPixel(coord)[2];
      tmp_vec_3[0] = m_Gradient->GetPixel(tmp_co_3)[0] - m_Gradient->GetPixel(coord)[0];
      tmp_vec_3[1] = m_Gradient->GetPixel(tmp_co_3)[1] - m_Gradient->GetPixel(coord)[1];
      tmp_vec_3[2] = m_Gradient->GetPixel(tmp_co_3)[2] - m_Gradient->GetPixel(coord)[2];

      vec_for[0] = vec_for[0] + ((data->pos)[0]-coord[0])*tmp_vec_1[0] 
    + ((data->pos)[1]-coord[1])*tmp_vec_2[0] + ((data->pos)[2]-coord[2])*tmp_vec_3[0];
    vec_for[1] = vec_for[1] + ((data->pos)[1]-coord[1])*tmp_vec_2[1]
  + ((data->pos)[0]-coord[0])*tmp_vec_1[1] + ((data->pos)[2]-coord[2])*tmp_vec_3[1];
  vec_for[2] = vec_for[2] + ((data->pos)[2]-coord[2])*tmp_vec_3[2]
+ ((data->pos)[1]-coord[1])*tmp_vec_2[2] + ((data->pos)[0]-coord[0])*tmp_vec_1[2];
      }
    else
      {
      vec_for.Fill(0);
      }

    double mag = dot_product(data->normal.Get_vnl_vector(),vec_for.Get_vnl_vector());

    vec_for[0] = m_Beta * mag*(data->normal)[0]/*num_for*/;
    vec_for[1] = m_Beta * mag*(data->normal)[1]/*num_for*/; 
    vec_for[2] = m_Beta * mag*(data->normal)[2]/*num_for*/; 

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

#endif //__itkDeformableSimplexMesh3DBalloonForceFilter_TXX
