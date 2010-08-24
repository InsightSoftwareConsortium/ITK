/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCorrespondingMedialNodeClique.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
