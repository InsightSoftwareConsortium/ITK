/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryMedialNodeMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBinaryMedialNodeMetric_cxx
#define _itkBinaryMedialNodeMetric_cxx

#include "itkBinaryMedialNodeMetric.h"

namespace itk
{

/**
 * Constructor
 */
template <int VDimensions>
BinaryMedialNodeMetric<VDimensions>
::BinaryMedialNodeMetric()
{
  m_Result = 0.0;
  m_ShowCalc = false;
}

/**
 *
 */
template <int VDimensions>
void
BinaryMedialNodeMetric<VDimensions>
::OrderValues(void)
{
  //initialize smallest value above 1, so all values will be smaller that its initial value
  float smallest_value = 1.1;
  int smallest_index;
  int smallest_key;

  for(int i = 0; i < VDimensions; i++)
  {
    if(m_EigenA[i] < smallest_value && m_EigenA[i] != EMPTY)
    {
      smallest_value = m_EigenA[i];
      smallest_key = 1;
      smallest_index = i;
    }
    if(m_EigenB[i] < smallest_value && m_EigenB[i] != EMPTY)
    {
      smallest_value = m_EigenB[i];
      smallest_key = -1;
      smallest_index = i;
    }
  }
  //after the for loop we know the smallest value between the two arrays, its index, and
  //which array it is from. now we remove that eigen value from its array, and put it in 
  //combined_eigens

  if(smallest_key == 1)
  {
    int j = 0;
    while(m_CombinedEigenValues[j] != EMPTY){j++;}//gets next empty index of combined_eigens
    
    m_CombinedEigenValues[j] = m_EigenA[smallest_index];
    m_CombinedDistanceValues[j] = m_DistanceVectorA[smallest_index];
    m_EigenA[smallest_index] = EMPTY;
    m_CombinedEigensKey[j] = smallest_key;
  }
  else if(smallest_key == -1)
  {
    int j = 0;
    while(m_CombinedEigenValues[j] != EMPTY){j++;}//gets next empty index of combined_eigens
    
    m_CombinedEigenValues[j] = m_EigenB[smallest_index];
    m_CombinedDistanceValues[j] = m_DistanceVectorB[smallest_index];
    m_EigenB[smallest_index] = EMPTY;
    m_CombinedEigensKey[j] = smallest_key;
  }
}

/**
 * 
 */
template <int VDimensions>
void
BinaryMedialNodeMetric<VDimensions>
::Initialize(void)
{
  //itkDebugMacro(<< "itkBinaryMedialNodeMetric::Initialize() called");
  m_Result = 0.0;

  //initialize combined_eigens to EMPTY
  for(int i =0;i<VDimensions*2;i++){m_CombinedEigenValues[i] = EMPTY;}
  
  //Get eigen values from first nodes
  m_EigenA = m_NodeA1->GetVotedEigenvalues();
  m_EigenB = m_NodeB1->GetVotedEigenvalues();

  //Initialize F value arrays by getting components of distance between nodes 1 and 2
  PositionType LocationA1;
  PositionType LocationA2;
  PositionType LocationB1;
  PositionType LocationB2;
  double distanceA = 0;
  double distanceB = 0;

  //get locations of medial nods
  LocationA1 = m_NodeA1->GetVotedLocation();
  LocationA2 = m_NodeA2->GetVotedLocation();
  LocationB1 = m_NodeB1->GetVotedLocation();
  LocationB2 = m_NodeB2->GetVotedLocation();

  //calculate distance vectors
  for(int j = 0;j < VDimensions;j++)
    {
    m_DistanceVectorA[j] = LocationA1[j] - LocationA2[j];
    m_DistanceVectorB[j] = LocationB1[j] - LocationB2[j];

    distanceA += pow(m_DistanceVectorA[j],2);
    distanceB += pow(m_DistanceVectorB[j],2);
    }

  distanceA = sqrt(distanceA);
  distanceB = sqrt(distanceB);

  for(int k = 0;k < VDimensions;k++)
    {
    m_DistanceVectorA[k] /= distanceA;
    m_DistanceVectorB[k] /= distanceB;

    m_DistanceVectorA[k] = fabs(m_DistanceVectorA[k]);
    m_DistanceVectorB[k] = fabs(m_DistanceVectorB[k]);
    }
  
  OrderValues();

  if(m_ShowCalc == true)PrintCombinedEigens();

  for(int m = 0;m<VDimensions*2;m++)
  {
    m_Result += (m_CombinedDistanceValues[m]*m_CombinedEigenValues[m])*m_CombinedEigensKey[m];
  }

  m_Result = 1.0 - fabs(m_Result);

  //std::cout << "\n\nTotal Metric Difference:  " << m_Result << std::endl;

  // Returns a number between 0 and 1.0, where 1.0 is a perfect match
}

/**
 * Function to print the list of combined eigen values.
 */
template <int VDimensions>
void
BinaryMedialNodeMetric<VDimensions>
::PrintCombinedEigens()
{
  std::cout << "eigen\t\tsource\t\tdist-value" << std::endl;
  std::cout << "---------------------------------------" << std::endl;
  for(int i=0;i<VDimensions*2;i++)
  {
    std::cout << m_CombinedEigenValues[i] << "\t";
    if(m_CombinedEigensKey[i] == 1)std::cout << "\tA" << "\t\t" << m_CombinedDistanceValues[i] << std::endl;
    else if(m_CombinedEigensKey[i] == -1)std::cout << "\tB" << "\t\t" << m_CombinedDistanceValues[i] << std::endl;
  }
}
 
/**
 * 
 */
template <int VDimensions>
void
BinaryMedialNodeMetric<VDimensions>
::SetMedialNodes(MedialNode * NodeA1, MedialNode * NodeA2, MedialNode * NodeB1, MedialNode * NodeB2)
{
  m_NodeA1 = NodeA1;
  m_NodeA2 = NodeA2;
  m_NodeB1 = NodeB1;
  m_NodeB2 = NodeB2;
} 

/*
 * PrintSelf
 */
template <int VDimensions>
void
BinaryMedialNodeMetric<VDimensions>
::PrintSelf(std::ostream& os, Indent indent) const
{
Superclass::PrintSelf(os,indent);
}


} // end namespace itk

#endif
