/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKdTree.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkKdTree_h
#define __itkKdTree_h

#include <queue>
#include <vector>

#include "itkMacro.h"
#include "itkPoint.h"
#include "itkSize.h"
#include "itkObject.h"
#include "itkNumericTraits.h"
#include "itkArray.h"

#include "itkSample.h"
#include "itkSubsample.h"

#include "itkEuclideanDistance.h"

namespace itk{ 
  namespace Statistics{

template< class TSample >
struct KdTreeNode
{
  typedef KdTreeNode< TSample> Self ;
  typedef typename TSample::MeasurementType MeasurementType ;
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize) ;
  typedef FixedArray< double, 
                      itkGetStaticConstMacro(MeasurementVectorSize) > CenteroidType ;
  typedef typename TSample::InstanceIdentifier InstanceIdentifier ;

  virtual bool IsTerminal() = 0 ;

  virtual void GetParameters(unsigned int &partitionDimension, 
                             MeasurementType &partitionValue) = 0 ;  
  virtual Self* Left() = 0 ;

  virtual Self* Right() = 0 ;

  virtual unsigned int Size() = 0 ;

  virtual void GetWeightedCenteroid(CenteroidType &centeroid) = 0 ;

  virtual void GetCenteroid(CenteroidType &centeroid) = 0 ;

  virtual InstanceIdentifier GetInstanceIdentifier(size_t index) = 0 ;

  virtual void AddInstanceIdentifier(InstanceIdentifier id) = 0 ;
} ; // end of class

template< class TSample >
struct KdTreeNonterminalNode: public KdTreeNode< TSample >
{
  typedef KdTreeNode< TSample > Superclass ;
  typedef typename Superclass::MeasurementType MeasurementType ;
  typedef typename Superclass::CenteroidType CenteroidType ;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier ;

  KdTreeNonterminalNode(unsigned int partitionDimension,
                        MeasurementType partitionValue,
                        Superclass* left,
                        Superclass* right) ;

  virtual ~KdTreeNonterminalNode() {}
  
  virtual bool IsTerminal()
  { return false ; }

  void GetParameters(unsigned int &partitionDimension, 
                     MeasurementType &partitionValue) ;

  Superclass* Left() 
  { return m_Left ; }

  Superclass* Right()
  { return m_Right ; }

  unsigned int Size()
  { return 0 ; }

  void GetWeightedCenteroid(CenteroidType &centeroid)
  { /* do nothing */ } 

  void GetCenteroid(CenteroidType &centeroid)
  { /* do nothing */ }

  InstanceIdentifier GetInstanceIdentifier(size_t index)
  { return 0 ; }

  void AddInstanceIdentifier(InstanceIdentifier id) {}

private:
  unsigned int m_PartitionDimension ;
  MeasurementType m_PartitionValue ;
  Superclass* m_Left ;
  Superclass* m_Right ;
} ; // end of class

template< class TSample >
struct KdTreeWeightedCenteroidNonterminalNode: public KdTreeNode< TSample >
{
  typedef KdTreeNode< TSample > Superclass ;
  typedef typename Superclass::MeasurementType MeasurementType ;
  typedef typename Superclass::CenteroidType CenteroidType ;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier ;

  KdTreeWeightedCenteroidNonterminalNode(unsigned int partitionDimension,
                                         MeasurementType partitionValue,
                                         Superclass* left,
                                         Superclass* right,
                                         CenteroidType &centeroid,
                                         unsigned int size) ;
  virtual ~KdTreeWeightedCenteroidNonterminalNode() {}

  virtual bool IsTerminal()
  { return false ; }

  void GetParameters(unsigned int &partitionDimension, 
                     MeasurementType &partitionValue) ;

  Superclass* Left() 
  { return m_Left ; }

  Superclass* Right()
  { return m_Right ; }

  unsigned int Size()
  { return m_Size ; }

  void GetWeightedCenteroid(CenteroidType &centeroid)
  { centeroid = m_WeightedCenteroid ; }

  void GetCenteroid(CenteroidType &centeroid)
  { centeroid = m_Centeroid ; }

  InstanceIdentifier GetInstanceIdentifier(size_t index)
  { return 0 ; }

  void AddInstanceIdentifier(InstanceIdentifier id) {}

private:
  unsigned int m_PartitionDimension ;
  MeasurementType m_PartitionValue ;
  CenteroidType m_WeightedCenteroid ;
  CenteroidType m_Centeroid ;
  unsigned int m_Size ;
  Superclass* m_Left ;
  Superclass* m_Right ;
} ; // end of class


template< class TSample >
struct KdTreeTerminalNode: public KdTreeNode< TSample >
{
  typedef KdTreeNode< TSample > Superclass ;
  typedef typename Superclass::MeasurementType MeasurementType ;
  typedef typename Superclass::CenteroidType CenteroidType ;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier ;

  KdTreeTerminalNode() {}

  virtual ~KdTreeTerminalNode() {}

  bool IsTerminal()
  { return true ; }

  void GetParameters(unsigned int &partitionDimension, 
                     MeasurementType &partitionValue) {}

  Superclass* Left() 
  { return 0 ; }

  Superclass* Right()
  { return 0 ; }

  unsigned int Size()
  { return m_InstanceIdentifiers.size() ; }

  void GetWeightedCenteroid(CenteroidType &centeroid)
  { /* do nothing */ } 

  void GetCenteroid(CenteroidType &centeroid)
  { /* do nothing */ } 

  InstanceIdentifier GetInstanceIdentifier(size_t index)
  { return m_InstanceIdentifiers[index] ; }

  void AddInstanceIdentifier(InstanceIdentifier id) 
  { m_InstanceIdentifiers.push_back(id) ;}

private:
  std::vector< InstanceIdentifier > m_InstanceIdentifiers ;
} ; // end of class

/** \class KdTree 
 *  \brief KdTree 
 */

template < class TSample >
class ITK_EXPORT KdTree : public Object
{
public:
  /** Standard class typedefs */
  typedef KdTree Self ;
  typedef Object Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(KdTree, Object);

  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  /** typedef alias for the source data container */ 
  typedef TSample SampleType ;
  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;
  typedef typename TSample::MeasurementType MeasurementType ;
  typedef typename TSample::InstanceIdentifier InstanceIdentifier ;

  itkStaticConstMacro(MeasurementVectorSize, unsigned int, 
                      TSample::MeasurementVectorSize) ;

  typedef EuclideanDistance< MeasurementVectorType > DistanceMetricType ;

  typedef KdTreeNode< TSample > KdTreeNodeType ;

  typedef std::pair< InstanceIdentifier, double > NeighborType ;

  class NearestNeighbors
  {
  public:
    NearestNeighbors() {}
    ~NearestNeighbors() {} 
    
    void resize(unsigned int k)
    {
      m_Identifiers.clear() ;
      m_Identifiers.resize(k, NumericTraits< unsigned long >::max()) ;
      m_Distances.clear() ;
      m_Distances.resize(k, NumericTraits< double >::max()) ;
      m_FarthestNeighborIndex = 0 ;
    }

    double GetLargestDistance() 
    { return m_Distances[m_FarthestNeighborIndex] ; }

    void ReplaceFarthestNeighbor(InstanceIdentifier id, double distance) 
    {
      m_Identifiers[m_FarthestNeighborIndex] = id ;
      m_Distances[m_FarthestNeighborIndex] = distance ;
      double farthestDistance = NumericTraits< double >::min() ;
      for ( unsigned int i = 0 ; i < m_Distances.size() ; i++ )
        {
          if ( m_Distances[i] > farthestDistance )
            {
              farthestDistance = m_Distances[i] ;
              m_FarthestNeighborIndex = i ;
            }
        }
    }

    std::vector< InstanceIdentifier >& GetNeighbors()
    { return m_Identifiers ; }

    std::vector< double >& GetDistances()
    { return m_Identifiers ; }

  private:
    unsigned int m_FarthestNeighborIndex ;
    std::vector< InstanceIdentifier > m_Identifiers ;
    std::vector< double > m_Distances ;
  } ;

  void SetBucketSize(unsigned int size) ;

  void SetSample(TSample* sample) ;

  TSample* GetSample()
  { return m_Sample ; }

  KdTreeNodeType* GetEmptyTerminalNode()
  { return m_EmptyTerminalNode ; } 

  void SetRoot(KdTreeNodeType* root) 
  { m_Root = root ; } 

  KdTreeNodeType* GetRoot() 
  { return m_Root ; } 

  MeasurementVectorType& GetMeasurementVector(InstanceIdentifier id)
  { return m_Sample->GetMeasurementVector(id) ; }

  /** Get the pointer to the distance metric. */
  DistanceMetricType* GetDistanceMetric()
  { return m_DistanceMetric.GetPointer() ; }

  bool BallWithinBounds(MeasurementVectorType &query, 
                        MeasurementVectorType &lowerBound,
                        MeasurementVectorType &upperBound) ;

  bool BoundsOverlapBall(MeasurementVectorType &query, 
                         MeasurementVectorType &lowerBound,
                         MeasurementVectorType &upperBound) ;

  void Search(MeasurementVectorType &query, unsigned int k) ;

  
  NearestNeighbors& GetSearchResult()
  { return m_Neighbors ; } 

  int GetNumberOfVisits()
  { return m_NumberOfVisits ; }

  void DeleteNode(KdTreeNodeType *node) ;

  void PrintTree(KdTreeNodeType *node, int level, 
                 unsigned int activeDimension) ;

protected:
  KdTree() ;
  virtual ~KdTree() ;
  void PrintSelf(std::ostream& os, Indent indent) const ;

  int SearchLoop(KdTreeNodeType* node, MeasurementVectorType &query,
                 MeasurementVectorType &lowerBound,
                 MeasurementVectorType &upperBound) ;

  void DumpVector(MeasurementVectorType &vec) ;

private:
  KdTree(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  TSample* m_Sample ;
  int m_BucketSize ;
  KdTreeNodeType* m_Root ;
  KdTreeNodeType* m_EmptyTerminalNode ;
  DistanceMetricType::Pointer m_DistanceMetric ;
  NearestNeighbors m_Neighbors ;
  MeasurementVectorType m_LowerBound ;
  MeasurementVectorType m_UpperBound ;
  int m_NumberOfVisits ;
  bool m_StopSearch ;
  NeighborType m_TempNeighbor ;
} ; // end of class

  } // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKdTree.txx"
#endif

#endif



