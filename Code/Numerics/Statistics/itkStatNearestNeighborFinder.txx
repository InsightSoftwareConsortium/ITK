/*=========================================================================
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatNearestNeighborFinder.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkStatNearestNeighborFinder_txx
#define __itkStatNearestNeighborFinder_txx

#include "itkStatNearestNeighborFinder.h"

namespace itk {
  // constructor
  template<class TDataSet, class TPoint>
  NearestNeighborFinder<TDataSet, TPoint>
  ::NearestNeighborFinder() 
  {
    m_ValidInputs = false ;
    m_K = 0 ;
    m_QueryPoint = 0 ;
    m_DataSet = 0 ;
    SetAbortGenerateData(false) ;
  }
  
  // public member functions
  template<class TDataSet, class TPoint>
  void
  NearestNeighborFinder<TDataSet, TPoint>
  ::SetInputParams(int K, DataSetType* dataSet, PointType* queryPoint) 
    throw (InvalidInputError)
  {
    if (dataSet->size() < K)
      throw InvalidInputError() ;
      
    if (queryPoint->size() <= 0)
      throw InvalidInputError() ;
    
    if (K != m_K) 
      m_InputsModified = true ;
      
    if (dataSet != m_DataSet)
      m_InputsModified = true ;
      
    if (queryPoint != m_QueryPoint)
      m_InputsModified = true ;
      
    m_DataSet = dataSet ;
    m_K = K ;
    m_QueryPoint = queryPoint ;
    m_ValidInputs = true ;
  }

  template<class TDataSet, class TPoint>
  NearestNeighborFinder<TDataSet, TPoint>::OutputType
  NearestNeighborFinder<TDataSet, TPoint>
  ::GetOutput() throw (InvalidInputError) 
  {
    if (!m_ValidInputs)
      throw InvalidInputError() ;

    if (m_InputsModified)
      UpdateOutputData() ;

    return m_Output ;
  }
    
  // protected member functions 
  template<class TDataSet, class TPoint>
  void
  NearestNeighborFinder<TDataSet, TPoint>
  ::GenerateData()
  {
    m_Neighbors.resize(m_K) ;
      
    // initialize the distance vector
    DataSetType::iterator iterS = m_DataSet->begin() ;
    NeighborsType::iterator iterN = m_Neighbors.begin() ;
    while (iterN != m_Neighbors.end())
      {
        iterN->Point = *iterS ;
        iterN->Distance = GetDistance(&(*iterS), m_QueryPoint) ;
        iterS++ ;
        iterN++ ;
      }
      
    NeighborsType::iterator replaceLoc ;
    double distance ;
    while (iterS != m_DataSet->end())
      {
        distance = GetDistance(&(*iterS), m_QueryPoint) ;
        replaceLoc = FindReplacement(distance) ;
        if ( replaceLoc != m_Neighbors.end())
          {
            replaceLoc->Distance = distance ;
            replaceLoc->Point = *iterS ;
          }
        iterS++ ;
      }
    CopyOutput() ;
  }
    
  template<class TDataSet, class TPoint>
  NearestNeighborFinder<TDataSet, TPoint>::NeighborsType::iterator
  NearestNeighborFinder<TDataSet, TPoint>
  ::FindReplacement(double distance)
  {
    NeighborsType::iterator iter = m_Neighbors.begin() ;
    while (iter != m_Neighbors.end())
      {
        if (iter->Distance > distance)
          return iter ;
        
        iter++ ;
      }
    return m_Neighbors.end() ;
  }
  
  template<class TDataSet, class TPoint>
  void
  NearestNeighborFinder<TDataSet, TPoint>
  ::CopyOutput()
  {
    m_Output.resize(m_K) ;
    OutputType::iterator iterO = m_Output.begin() ;
    NeighborsType::iterator iterN = m_Neighbors.begin() ;
    
    while (iterN != m_Neighbors.end())
      {
        *iterO = iterN->Point ;
        iterO++ ;
        iterN++ ;
      }
  }
  
  template<class TDataSet, class TPoint>
  double
  NearestNeighborFinder<TDataSet, TPoint>
  ::GetDistance(PointType* A, PointType* B) 
  {
    return EuclideanDistance<PointType>(A, B) ;
  }
    
} // end of namespace itk

#endif
