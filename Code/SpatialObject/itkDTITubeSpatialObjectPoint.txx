/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDTITubeSpatialObjectPoint.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDTITubeSpatialObjectPoint_txx
#define __itkDTITubeSpatialObjectPoint_txx

#include "itkDTITubeSpatialObjectPoint.h"

namespace itk 
{

/** Constructor */
template< unsigned int TPointDimension >
DTITubeSpatialObjectPoint< TPointDimension >
::DTITubeSpatialObjectPoint( void ) 
{ 
  m_FA = 0;
  m_ADC = 0;
  m_GA = 0;
  m_Lambda1 = 0;
  m_Lambda2 = 0;
  m_Lambda3 = 0;

  unsigned int i;
  for(i=0;i<3;i++)
    {
    m_MinEV[i] = 0;
    m_MedEV[i] = 0;
    m_MaxEV[i] = 0;
    }

  for(i=0;i<5;i++)
    {
    m_MRI[i] = 0;
    }

  // Initialize the tensor matrix to identity
  for(i=0;i<6;i++)
    {
    m_TensorMatrix[i] = 0;
    }

  m_TensorMatrix[0] = 1;
  m_TensorMatrix[3] = 1;
  m_TensorMatrix[5] = 1;
  m_Interpolation = 0;
}

/** Destructor */
template< unsigned int TPointDimension >
DTITubeSpatialObjectPoint< TPointDimension >
::~DTITubeSpatialObjectPoint( void ) 
{
}


template< unsigned int TPointDimension >
void
DTITubeSpatialObjectPoint< TPointDimension >
::PrintSelf( std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}


template< unsigned int TPointDimension >
typename DTITubeSpatialObjectPoint< TPointDimension >::Self & 
DTITubeSpatialObjectPoint< TPointDimension >
::operator=(const DTITubeSpatialObjectPoint & rhs) 
{
  this->m_ID = rhs.m_ID;
  m_R = rhs.m_R;
  m_FA = rhs.m_FA;
  m_ADC = rhs.m_ADC;
  m_GA = rhs.m_GA;
  m_Lambda1 = rhs.m_Lambda1;
  m_Lambda2 = rhs.m_Lambda2;
  m_Lambda3 = rhs.m_Lambda3;

  unsigned int i;
  for(i=0;i<3;i++)
    {
    m_MinEV[i] = rhs.m_MinEV[i];
    m_MedEV[i] = rhs.m_MedEV[i];
    m_MaxEV[i] = rhs.m_MaxEV[i];
    }

  for(i=0;i<5;i++)
    {
    m_MRI[i] = rhs.m_MRI[i];
    }

  for(i=0;i<6;i++)
    {
    m_TensorMatrix[i] = rhs.m_TensorMatrix[i];
    }

  m_Interpolation = rhs.m_Interpolation;
  m_NumDimensions = rhs.m_NumDimensions;
  this->m_X = rhs.m_X;
  m_T = rhs.m_T;
  m_Normal1 = rhs.m_Normal1;
  m_Normal2 = rhs.m_Normal2;
  this->m_Color = rhs.m_Color;
  return * this;
}

} // end namespace itk

#endif
