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
#include "itk_hash_map.h"

#include "itkObject.h"

namespace itk {
namespace Statistics {

/** \class KdTreeBasedKmeansEstimator
 * \brief fast k-means algorithm implementation using k-d tree structure
 *
 * It returns k mean vectors that are centeroids of k-clusters 
 * using pre-generated k-d tree. k-d tree generation is done by 
 * the WeightedCenteroidKdTreeGenerator. The tree construction needs 
 * to be done only once. The resulting k-d tree's non-terminal nodes 
 * that have their children nodes have vector sums of measurement vectors 
 * that belong to the nodes and the number of measurement vectors 
 * in addition to the typical node boundary information and pointers to 
 * children nodes. Instead of reassigning every measurement vector to 
 * the nearest cluster centeroid and recalculating centeroid, it maintain 
 * a set of cluster centeroid candidates and using pruning algorithm that 
 * utilizes k-d tree, it updates the means of only relevant candidates at 
 * each iterations. It would be faster than traditional implementation 
 * of k-means algorithm. However, the k-d tree consumes a large amount 
 * of memory. The tree construction time and pruning algorithm's performance 
 * are important factors to the whole process's performance. If users 
 * want to use k-d tree for some purpose other than k-means estimation, 
 * they can use the KdTreeGenerator instead of the 
 * WeightedCenteroidKdTreeGenerator. It will save the tree construction
 * time and memory usage.
 *
 * \sa WeightedCenteroidKdTreeGenerator, KdTree
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
  typedef typename TKdTree::KdTreeNodeType KdTreeNodeType ;
  typedef typename TKdTree::MeasurementType MeasurementType ;
  typedef typename TKdTree::MeasurementVectorType MeasurementVectorType ;
  typedef typename TKdTree::InstanceIdentifier InstanceIdentifier ;
  typedef typename TKdTree::SampleType SampleType ;
  typedef typename KdTreeNodeType::CenteroidType CenteroidType ;
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TKdTree::MeasurementVectorSize);
  /**  Parameters type.
   *  It defines a position in the optimization search space. */
//   typedef FixedArray< double, itkGetStaticConstMacro(MeasurementVectorSize) > ParameterType ;
  typedef FixedArray< double, itkGetStaticConstMacro(MeasurementVectorSize) > ParameterType ;
  typedef std::vector< ParameterType > InternalParametersType;
  typedef Array< double > ParametersType;

  /**  Set the position to initialize the optimization. */
  void SetParameters(ParametersType& params)
  { m_Parameters = params ; }

  /** Get current position of the optimization. */
  ParametersType& GetParameters() 
  { return m_Parameters ; }

  /** Set/Get maximum iteration limit. */
  itkSetMacro( MaximumIteration, int );
  itkGetConstMacro( MaximumIteration, int ); 
 
  /** Set/Get the termination threshold for the squared sum
   * of changes in centeroid postions after one iteration */
  itkSetMacro( CenteroidPositionChangesThreshold, double );   
  itkGetConstMacro( CenteroidPositionChangesThreshold, double );   
  
  /** Set/Get the pointer to the KdTree */
  void SetKdTree(TKdTree* tree) 
  { m_KdTree = tree ; }

  TKdTree* GetKdTree() 
  { return m_KdTree ; }

  itkGetConstMacro( CurrentIteration, int) ;
  itkGetConstMacro( CenteroidPositionChanges, double) ;

  /** Start optimization
   * Optimization will stop when it meets either of two termination conditions,
   * the maximum iteration limit or epsilon (minimal changes in squared sum
   * of changes in centeroid positions)  */
  void StartOptimization() ;

  typedef itk::hash_map< InstanceIdentifier, unsigned int > ClusterLabelsType ;

  void SetUseClusterLabels(bool flag)
  { m_UseClusterLabels = flag ; }

  ClusterLabelsType* GetClusterLabels() 
  { return &m_ClusterLabels ; }

protected:
  KdTreeBasedKmeansEstimator() ;
  virtual ~KdTreeBasedKmeansEstimator() {}

  void PrintSelf(std::ostream& os, Indent indent) const;

  void FillClusterLabels(KdTreeNodeType* node, int closestIndex) ;

  /** vector of k-means candidates */
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

    /** returns the number of candidate = k */
    int Size()
    { return m_Candidates.size() ; }

    /** Initialize the centeroids with the argument.
     * At each iteration, this should be called before filtering*/
    void SetCenteroids(InternalParametersType& centeroids)
    {
      m_Candidates.resize(centeroids.size()) ;
      for (unsigned int i = 0 ; i < centeroids.size() ; i++)
        {
          Candidate candidate ;
          candidate.Centeroid = centeroids[i] ;
          candidate.WeightedCenteroid.Fill(0.0) ;
          candidate.Size = 0 ;
          m_Candidates[i] = candidate ;
        }
    }

    /** gets the centeroids (k-means) */
    void GetCenteroids(InternalParametersType& centeroids)
    {
      unsigned int i ;
      centeroids.resize(this->Size()) ;
      for (i = 0 ; i < (unsigned int)this->Size() ; i++)
        {
          centeroids[i] = m_Candidates[i].Centeroid ;
        }
    }

    /** updates the centeroids using the vector sum of measurement vectors
     * that belongs to each centeroid and the number of measurement vectors */
    void UpdateCenteroids()
    {
      unsigned int i, j ;
      for (i = 0 ; i < (unsigned int)this->Size() ; i++)
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

    /** gets the index-th candidates */
    Candidate& operator[](int index)
    { return m_Candidates[index] ; }
    

  private:
    /** internal storage for the candidates */
    std::vector< Candidate > m_Candidates ;
  } ; // end of class

  /** gets the sum of squared difference between the previous position
   * and current postion of all centeroid. This is the primary termination
   * condition for this algorithm. If the return value is less than
   * the value that was set by the SetCenteroidPositionChangesThreshold 
   * method.*/
  double GetSumOfSquaredPositionChanges(InternalParametersType &previous, 
                                        InternalParametersType &current) ;

  /** get the index of the closest candidate to the "measurements" 
   * measurement vector */
  int GetClosestCandidate(ParameterType &measurements, 
                          std::vector< int > &validIndexes) ;

  /** returns true if the "pointA is farther than pointB to the boundary */
  bool IsFarther(ParameterType &pointA,
                 ParameterType &pointB,
                 MeasurementVectorType &lowerBound,
                 MeasurementVectorType &upperBound) ;

  /** recursive pruning algorithm. the "validIndexes" vector contains
   * only the indexes of the surviving candidates for the "node" */  
  void Filter(KdTreeNodeType* node, 
              std::vector< int > validIndexes,
              MeasurementVectorType &lowerBound, 
              MeasurementVectorType &upperBound) ;

  /** copies the source parameters (k-means) to the target */
  void CopyParameters(InternalParametersType &source, InternalParametersType &target) ;

  /** copies the source parameters (k-means) to the target */
  void CopyParameters(ParametersType &source, InternalParametersType &target) ;

  /** copies the source parameters (k-means) to the target */
  void CopyParameters(InternalParametersType &source, ParametersType &target) ;

  /** imports the "measurements" measurement vector data to the "point" */ 
  void GetPoint(ParameterType &point, 
                MeasurementVectorType &measurements)
  {
    for (unsigned int i = 0 ; i < MeasurementVectorSize ; i++)
      {
        point[i] = measurements[i] ;
      }
  }

  void PrintPoint(ParameterType &point)
  {
    std::cout << "[ " ;
    for (unsigned int i = 0 ; i < MeasurementVectorSize ; i++)
      {
        std::cout << point[i] << " " ;
      }
    std::cout << "]" ;
  }

private:
  /** current number of iteration */
  int m_CurrentIteration ;
  /** maximum number of iteration. termination criterion */  
  int m_MaximumIteration ;
  /** sum of squared centeroid position changes at the current iteration */ 
  double m_CenteroidPositionChanges ;
  /** threshold for the sum of squared centeroid position changes.
   * termination criterion */
  double m_CenteroidPositionChangesThreshold ;
  /** pointer to the k-d tree */
  TKdTree* m_KdTree ;
  /** pointer to the euclidean distance funtion */
  typename EuclideanDistance< ParameterType >::Pointer m_DistanceMetric ;

  /** k-means */
  ParametersType m_Parameters ;

  CandidateVector m_CandidateVector ;
  
  ParameterType m_TempVertex ;

  bool m_UseClusterLabels ;
  bool m_GenerateClusterLabels ;
  ClusterLabelsType m_ClusterLabels ;
} ; // end of class

} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKdTreeBasedKmeansEstimator.txx"
#endif


#endif
