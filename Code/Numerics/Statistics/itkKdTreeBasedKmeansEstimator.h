/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKdTreeBasedKmeansEstimator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkKdTreeBasedKmeansEstimator_h
#define __itkKdTreeBasedKmeansEstimator_h

#include <vector>
#include "itkObject.h"

namespace itk {
namespace Statistics {

/** \class KdTreeBasedKmeansEstimator
 * \brief 
 *
 */

template< class TKdTree >
class ITK_EXPORT KdTreeBasedKmeansEstimator: 
    public Object
{
public:
  /** Standard "Self" typedef. */
  typedef KdTreeBasedKmeansEstimator Self ;
  typedef Object Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
 
  /** Run-time type information (and related methods). */
  itkTypeMacro(KdTreeBasedKmeansEstimator, Obeject);
 
  /** Types for the KdTree data structure */
  typedef TKdTree KdTreeType ;
  typedef typename TKdTree::Pointer KdTreePointer ;
  typedef typename TKdTree::KdTreeNodeType KdTreeNodeType ;
  typedef typename TKdTree::MeasurementType MeasurementType ;
  typedef typename TKdTree::MeasurementVectorType MeasurementVectorType ;
  typedef typename TKdTree::SampleType SampleType ;
  typedef typename KdTreeNodeType::CenteroidType CenteroidType ;
  enum { MeasurementVectorSize = TKdTree::MeasurementVectorSize } ;
  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  typedef FixedArray< double, MeasurementVectorSize > ParameterType ;
  typedef std::vector< ParameterType >        ParametersType;

  /**  Set the position to initialize the optimization. */
  void SetInitialPosition(ParametersType position)
  { m_InitialPosition = position ; }

  /** Get the position to initialize the optimization. */
  ParametersType& GetInitialPosition()
  { return m_InitialPosition ; }

  /** Set/Get maximum iteration limit. */
  itkSetMacro( MaximumIteration, int );
  itkGetConstMacro( MaximumIteration, int ); 
 
  /** Set/Get the termination threshold for the squared sum
   * of changes in centeroid postions after one iteration */
  itkSetMacro( CenteroidPositionChangesThreshold, double );   
  itkGetConstMacro( CenteroidPositionChangesThreshold, double );   
  
  /** Set/Get the pointer to the KdTree */
  itkSetMacro( KdTree, KdTreePointer );   
  itkGetConstMacro( KdTree, KdTreePointer );   

  /** Get current position of the optimization. */
  ParametersType& GetCurrentPosition() 
  { return m_CurrentPosition ; }

  itkGetConstMacro( CurrentIteration, int) ;
  itkGetConstMacro( CenteroidPositionChanges, double) ;

  /** Start optimization
   * Optimization will stop when it meets either of two termination conditions,
   * the maximum iteration limit or epsilon (minimal changes in squared sum
   * of changes in centeroid positions)  */
  void StartOptimization() ;

protected:
  KdTreeBasedKmeansEstimator() ;
  virtual ~KdTreeBasedKmeansEstimator() {}

  void PrintSelf(std::ostream& os, Indent indent) const;

  class CandidateVector
  {
  public:
    CandidateVector() {}

    struct Candidate
    {
      CenteroidType Centeroid ;
      CenteroidType WeightedCenteroid ;
      int Size ;
    } ; // end of struct

    virtual ~CandidateVector() {} 

    int Size()
    { return m_Candidates.size() ; }

    void SetCenteroids(ParametersType& centeroids)
    {
      m_Candidates.resize(centeroids.size()) ;
      for (int i = 0 ; i < centeroids.size() ; i++)
        {
          Candidate candidate ;
          candidate.Centeroid = centeroids[i] ;
          candidate.WeightedCenteroid.Fill(0.0) ;
          candidate.Size = 0 ;
          m_Candidates[i] = candidate ;
        }
    }

    void GetCenteroids(ParametersType& centeroids)
    {
      int i, j ;
      centeroids.resize(this->Size()) ;
      for (i = 0 ; i < this->Size() ; i++)
        {
          centeroids[i] = m_Candidates[i].Centeroid ;
        }
    }

    void UpdateCenteroids()
    {
      int i, j ;
      for (i = 0 ; i < this->Size() ; i++)
        {
          if (m_Candidates[i].Size > 0)
            {
              for (j = 0 ; j < MeasurementVectorSize ; j++)
                {
                  m_Candidates[i].Centeroid[j] = 
                    m_Candidates[i].WeightedCenteroid[j] / 
                    double(m_Candidates[i].Size) ;
                }
            }
        }
    }

    Candidate& operator[](int index)
    { return m_Candidates[index] ; }
    

  private:
    std::vector< Candidate > m_Candidates ;
  } ; // end of class

  double GetSumOfSquaredPositionChanges(ParametersType &previous, 
                                        ParametersType &current) ;

  int GetClosestCandidate(ParameterType &measurements, 
                          std::vector< int > &validIndexes) ;

  bool IsFarther(ParameterType &pointA,
                 ParameterType &pointB,
                 MeasurementVectorType &lowerBound,
                 MeasurementVectorType &upperBound) ;

  void Filter(KdTreeNodeType* node, 
              std::vector< int > validIndexes,
              MeasurementVectorType &lowerBound, 
              MeasurementVectorType &upperBound) ;

  void CopyParameters(ParametersType &source, ParametersType &target) ;

  void GetPoint(ParameterType &point, 
                MeasurementVectorType &measurements)
  {
    for (int i = 0 ; i < MeasurementVectorSize ; i++)
      {
        point[i] = measurements[i] ;
      }
  }

  void PrintPoint(ParameterType &point)
  {
    std::cout << "[ " ;
    for (int i = 0 ; i < MeasurementVectorSize ; i++)
      {
        std::cout << point[i] << " " ;
      }
    std::cout << "]" ;
  }

private:
  int m_CurrentIteration ;
  int m_MaximumIteration ;
  double m_CenteroidPositionChanges ;
  double m_CenteroidPositionChangesThreshold ;
  KdTreePointer m_KdTree ;
  typename EuclideanDistance< ParameterType >::Pointer m_DistanceMetric ;

  ParametersType m_InitialPosition ;
  ParametersType m_CurrentPosition ;
  CandidateVector m_CandidateVector ;
  
  ParameterType m_TempVertex ;
} ; // end of class

} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKdTreeBasedKmeansEstimator.txx"
#endif


#endif
