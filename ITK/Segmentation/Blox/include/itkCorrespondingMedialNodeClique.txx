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
#ifndef __itkCorrespondingMedialNodeClique_txx
#define __itkCorrespondingMedialNodeClique_txx

#include "itkCorrespondingMedialNodeClique.h"

namespace itk
{
template< unsigned int VImageDimension, unsigned int VCliqueSize >
CorrespondingMedialNodeClique< VImageDimension, VCliqueSize >
::CorrespondingMedialNodeClique()
{
  m_NodeCoordinates = 0;
  m_CenterOfMass = 0;
  m_TransformMatrix = 0;

  m_AverageDistance = 0;

  for ( unsigned int i = 0; i < VCliqueSize; i++ )
    {
    m_CorrespondenceValue[i] = 0;
    }
}

template< unsigned int VImageDimension, unsigned int VCliqueSize >
CorrespondingMedialNodeClique< VImageDimension, VCliqueSize >
::~CorrespondingMedialNodeClique()
{}
} // end namespace itk

#endif
