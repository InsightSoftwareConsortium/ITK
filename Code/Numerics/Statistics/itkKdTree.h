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

/** \class KdTreeNode 
 *  \brief This class defines the interface of its derived classes.
 * 
 * The methods defined in this class are a superset of the methods
 * defined in its subclases. Therefore, the subclasses implements only
 * part of the methods. The template argument, TSample, can be any
 * subclass of the Sample class.
 *
 * There are two categories for the subclasses, terminal and nonterminal
 * nodes. The terminal nodes stores the instance identifiers beloging to
 * them, while the nonterminal nodes don't. Therefore, the
 * AddInstanceIdentifier and the GetInstanceIdentifier have meaning only
 * with the terminal ones. The terminal nodes don't have any child (left
 * or right). For terminal nodes, the GetParameters method is void.
 *
 * \sa KdTreeNonterminalNode, KdTreeWeightedCenteroidNonterminalNode,
 * KdTreeTerminalNode 
 */
template< class TSample >
struct KdTreeNode
{
  /** type alias for itself */
  typedef KdTreeNode< TSample> Self ;
  
  /** Measurement type, not the measurement vector type */
  typedef typename TSample::MeasurementType MeasurementType ;
  
  /** Measurement vector length */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize) ;

  /** Centeroid type */
  typedef FixedArray< double, 
                      itkGetStaticConstMacro(MeasurementVectorSize) > CenteroidType ;
  
  /** Instance identifier type (index value type for the measurement
   * vector in a sample */
  typedef typename TSample::InstanceIdentifier InstanceIdentifier ;

  /** Returns true if the node is a terminal node, that is a node that
   * doesn't have any child. */
  virtual bool IsTerminal() = 0 ;

  /** Fills the partitionDimension (the dimension that was chosen to
   * split the measurement vectors belong to this node to the left and the
   * right child among k dimensions) and the partitionValue (the
   * measurement value on the partitionDimension divides the left and the
   * right child */
  virtual void GetParameters(unsigned int &partitionDimension, 
                             MeasurementType &partitionValue) = 0 ;  

  /** Returns the pointer to the left child of this node */
  virtual Self* Left() = 0 ;

  /** Returns the pointer to the right child of this node */
  virtual Self* Right() = 0 ;

  /** Returs the number of measurement vectors under this node including
   * its children */
  virtual unsigned int Size() = 0 ;

  /** Returns the vector sum of the all measurement vectors under this node */
  virtual void GetWeightedCenteroid(CenteroidType &centeroid) = 0 ;

  /** Returns the centeroid. weighted centeroid divided by the size */
  virtual void GetCenteroid(CenteroidType &centeroid) = 0 ;

  /** Retuns the instance identifier of the index-th measurement vector */
  virtual InstanceIdentifier GetInstanceIdentifier(size_t index) = 0 ;

  /** Add an instance to this node */
  virtual void AddInstanceIdentifier(InstanceIdentifier id) = 0 ;

  /** Destructor */
  virtual ~KdTreeNode() {}; // needed to subclasses will actually be deleted
} ; // end of class

/** \class KdTreeNonterminalNode
 *  \brief This is a subclass of the KdTreeNode. 
 *
 * KdTreeNonterminalNode doesn't store the information related with the
 * centeroids. Therefore, the GetWeightedCenteroid and the GetCenteroid
 * methods are void. This class should have the left and the right
 * children. If we have a sample and want to generate a KdTree without
 * the centeroid related information, we can use the KdTreeGenerator.
 * 
 * \sa KdTreeNode, KdTreeWeightedCenteroidNonterminalNode, KdTreeGenerator
 */
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

  void GetWeightedCenteroid(CenteroidType &)
  { /* do nothing */ } 

  void GetCenteroid(CenteroidType &)
  { /* do nothing */ }

  InstanceIdentifier GetInstanceIdentifier(size_t)
  { return 0 ; }

  void AddInstanceIdentifier(InstanceIdentifier) {}

private:
  unsigned int m_PartitionDimension ;
  MeasurementType m_PartitionValue ;
  Superclass* m_Left ;
  Superclass* m_Right ;
} ; // end of class

/** \class KdTreeWeightedCenteroidNonterminalNode
 *  \brief This is a subclass of the KdTreeNode. 
 * 
 * KdTreeNonterminalNode does have the information related with the
 * centeroids. Therefore, the GetWeightedCenteroid and the GetCenteroid
 * methods returns meaningful values. This class should have the left
 * and right children. If we have a sample and want to generate a KdTree
 * with the centeroid related information, we can use the
 * WeightedCenteroidKdTreeGenerator. The centeroid, the weighted
 * centeroid, and the size (the number of measurement vectors) can be
 * used to accelate the k-means estimation.
 * 
 * \sa KdTreeNode, KdTreeNonterminalNode, WeightedCenteroidKdTreeGenerator
 */
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

  InstanceIdentifier GetInstanceIdentifier(size_t)
  { return 0 ; }

  void AddInstanceIdentifier(InstanceIdentifier) {}

private:
  unsigned int m_PartitionDimension ;
  MeasurementType m_PartitionValue ;
  CenteroidType m_WeightedCenteroid ;
  CenteroidType m_Centeroid ;
  unsigned int m_Size ;
  Superclass* m_Left ;
  Superclass* m_Right ;
} ; // end of class


/** \class KdTreeTerminalNode 
 *  \brief This class is the node that doesn't have any child node. The
 *  IsTerminal method returns true for this class. This class stores the
 *  instance identifiers belonging to this node, while the nonterminal
 *  nodes do not store them. The AddInstanceIdentifier and
 *  GetInstanceIdentifier are storing and retrieving the instance
 *  identifiers belonging to this node.
 * 
 * \sa KdTreeNode, KdTreeNonterminalNode,
 * KdTreeWeightedCenteroidNonterminalNode
 */
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

  void GetParameters(unsigned int &, 
                     MeasurementType &) {}

  Superclass* Left() 
  { return 0 ; }

  Superclass* Right()
  { return 0 ; }

  unsigned int Size()
  { return static_cast<unsigned int>( m_InstanceIdentifiers.size() ); }

  void GetWeightedCenteroid(CenteroidType &)
  { /* do nothing */ } 

  void GetCenteroid(CenteroidType &)
  { /* do nothing */ } 

  InstanceIdentifier GetInstanceIdentifier(size_t index)
  { return m_InstanceIdentifiers[index] ; }

  void AddInstanceIdentifier(InstanceIdentifier id) 
  { m_InstanceIdentifiers.push_back(id) ;}

private:
  std::vector< InstanceIdentifier > m_InstanceIdentifiers ;
} ; // end of class

/** \class KdTree 
 *  \brief This class provides methods for k-nearest neighbor search and
 *  related data structures for a k-d tree. 
 *
 * An object of this class stores instance identifiers in a k-d tree
 * that is a binary tree with childrens split along a dimension among
 * k-dimensions. The dimension of the split (or partition) is determined
 * for each nonterminal node that has two children. The split process is
 * terminated when the node has no children (when the number of
 * measurement vectors is less than or equal to the size set by the
 * SetBucketSize. That is The split process is a recursive process in
 * nature and in implementation. This implementation doesn't support
 * dynamic insert and delete operations for the tree. Instead, we can
 * use the KdTreeGenerator or WeightedCenteroidKdTreeGenerator to
 * generate a static KdTree object. 
 *
 * To search k-nearest neighbor, call the Search method with the query
 * point in a k-d space and the number of nearest neighbors. The
 * GetSearchResult method returns a pointer to a NearestNeighbors object
 * with k-nearest neighbors.
 *
 * \sa KdTreeNode, KdTreeNonterminalNode,
 * KdTreeWeightedCenteroidNonterminalNode, KdTreeTerminalNode,
 * KdTreeGenerator, WeightedCenteroidKdTreeNode
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

  /** Length of the measurement vector. k in the k-d tree */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int, 
                      TSample::MeasurementVectorSize) ;

  /** DistanceMetric type for the distance calculation and comparison */
  typedef EuclideanDistance< MeasurementVectorType > DistanceMetricType ;

  /** Node type of the KdTree */
  typedef KdTreeNode< TSample > KdTreeNodeType ;

  /** Neighbor type. The first element of the std::pair is the instance
   * identifier and the second one is the distance between the measurement
   * vector identified by the first element and the query point. */
  typedef std::pair< InstanceIdentifier, double > NeighborType ;

  typedef std::vector< InstanceIdentifier > InstanceIdentifierVectorType ;

  /** \class NearestNeighbors
   * \brief data structure for storing k-nearest neighbor search result
   * (k number of Neighbors)
   * 
   * This class stores the instance identifiers and the distance values
   * of k-nearest neighbors. We can also query the farthest neighbor's
   * distance from the query point using the GetLargestDistance
   * method. 
   */
  class NearestNeighbors
  {
  public:
    /** Constructor */
    NearestNeighbors() {}
    
    /** Destructor */
    ~NearestNeighbors() {} 
    
    /** Initialize the internal instance identifier and distance holders
     * with the size, k */
    void resize(unsigned int k)
    {
      m_Identifiers.clear() ;
      m_Identifiers.resize(k, NumericTraits< unsigned long >::max()) ;
      m_Distances.clear() ;
      m_Distances.resize(k, NumericTraits< double >::max()) ;
      m_FarthestNeighborIndex = 0 ;
    }

    /** Returns the distance of the farthest neighbor from the query point */
    double GetLargestDistance() 
    { return m_Distances[m_FarthestNeighborIndex] ; }

    /** Replaces the farthest neighbor's instance identifier and
     * distance value with the id and the distance */
    void ReplaceFarthestNeighbor(InstanceIdentifier id, double distance) 
    {
      m_Identifiers[m_FarthestNeighborIndex] = id ;
      m_Distances[m_FarthestNeighborIndex] = distance ;
      double farthestDistance = NumericTraits< double >::min() ;
      const unsigned int size = static_cast<unsigned int>( m_Distances.size() );
      for ( unsigned int i = 0 ; i < size; i++ )
        {
        if ( m_Distances[i] > farthestDistance )
          {
          farthestDistance = m_Distances[i] ;
          m_FarthestNeighborIndex = i ;
          }
        }
    }

    /** Returns the vector of k-neighbors' instance identifiers */
    InstanceIdentifierVectorType GetNeighbors()
    { return m_Identifiers ; }

    /** Returns the instance identifier of the index-th neighbor among
     * k-neighbors */
    InstanceIdentifier GetNeighbor(unsigned int index)
    { return m_Identifiers[index] ; }

    /** Returns the vector of k-neighbors' instance identifiers */
    std::vector< double >& GetDistances()
    { return m_Distances ; }

  private:
    /** The index of the farthest neighbor among k-neighbors */
    unsigned int m_FarthestNeighborIndex ;
    
    /** Storage for the instance identifiers of k-neighbors */
    InstanceIdentifierVectorType m_Identifiers ;

    /** Storage for the distance values of k-neighbors from the query
     * point */
    std::vector< double > m_Distances ;
  } ;

  /** Sets the number of measurement vectors that can be stored in a
   * terminal node */
  void SetBucketSize(unsigned int size) ;

  /** Sets the input sample that provides the measurement vectors to the k-d
   * tree */
  void SetSample(TSample* sample) ;

  /** Returns the pointer to the input sample */
  TSample* GetSample()
  { return m_Sample ; }

  /** Returns the pointer to the empty terminal node. A KdTree object
   * has a single empty terminal node in memory. when the split process
   * has to create an empty terminal node, the single instance is reused
   * for this case */
  KdTreeNodeType* GetEmptyTerminalNode()
  { return m_EmptyTerminalNode ; } 

  /** Sets the root node of the KdTree that is a result of
   * KdTreeGenerator or WeightedCenteroidKdTreeGenerator. */
  void SetRoot(KdTreeNodeType* root) 
  { m_Root = root ; } 

  /** Returns the pointer to the root node. */
  KdTreeNodeType* GetRoot() 
  { return m_Root ; } 

  /** Returns the measurement vector identified by the instance
   * identifier that is an identifier defiend for the input sample */
  MeasurementVectorType GetMeasurementVector(InstanceIdentifier id)
  { return m_Sample->GetMeasurementVector(id) ; }

  /** Get the pointer to the distance metric. */
  DistanceMetricType* GetDistanceMetric()
  { return m_DistanceMetric.GetPointer() ; }

  /** Searches the k-nearest neighbors */
  InstanceIdentifierVectorType Search(MeasurementVectorType &query, 
                                      unsigned int k) ;

  /** Searches the neighbors fallen into a hypersphere */
  InstanceIdentifierVectorType Search(MeasurementVectorType &query,
                                      double radius) ;

  /** Returns the number of measurement vectors that have been visited
   * to find the k-nearest neighbors. */
  int GetNumberOfVisits()
  { return m_NumberOfVisits ; }

  /** Returns true if the intermediate k-nearest neighbors exist within
   * the the bounding box defined by the lowerBound and the
   * upperBound. Otherwise returns false. Returns false if the ball
   * defined by the distance between the query point and the farthest
   * neighbor touch the surface of the bounding box.*/
  bool BallWithinBounds(MeasurementVectorType &query, 
                        MeasurementVectorType &lowerBound,
                        MeasurementVectorType &upperBound,
                        double radius) ;

  /** Returns true if the ball defined by the distance between the query
   * point and the farthest neighbor overlaps with the bounding box
   * defined by the lower and the upper bounds.*/
  bool BoundsOverlapBall(MeasurementVectorType &query, 
                         MeasurementVectorType &lowerBound,
                         MeasurementVectorType &upperBound,
                         double radius) ;

  /** Deletes the node recursively */
  void DeleteNode(KdTreeNodeType *node) ;

  /** Prints out the tree information */
  void PrintTree(KdTreeNodeType *node, int level, 
                 unsigned int activeDimension) ;

protected:
  /** Constructor */
  KdTree() ;
  
  /** Destructor: deletes the root node and the empty terminal node. */
  virtual ~KdTree() ;

  void PrintSelf(std::ostream& os, Indent indent) const ;

  /** search loop */ 
  int NearestNeighborSearchLoop(KdTreeNodeType* node,
                                MeasurementVectorType &query,
                                MeasurementVectorType &lowerBound,
                                MeasurementVectorType &upperBound) ;

  /** search loop */ 
  int SearchLoop(KdTreeNodeType* node, MeasurementVectorType &query,
                 MeasurementVectorType &lowerBound,
                 MeasurementVectorType &upperBound) ;
private:
  KdTree(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  /** Pointer to the input sample */
  TSample* m_Sample ;
  
  /** Number of measurement vectors can be stored in a terminal node. */
  int m_BucketSize ;

  /** Pointer to the root node */
  KdTreeNodeType* m_Root ;

  /** Pointer to the empty terminal node */
  KdTreeNodeType* m_EmptyTerminalNode ;

  /** Distance metric smart pointer */
  typename DistanceMetricType::Pointer m_DistanceMetric ;

  bool m_IsNearestNeighborSearch ;
 
  double m_SearchRadius ;

  InstanceIdentifierVectorType m_Neighbors ;

  /** k-nearest neighbors */
  NearestNeighbors m_NearestNeighbors ;

  /** Temporary lower bound in the SearchLoop. */
  MeasurementVectorType m_LowerBound ;

  /** Temporary upper bound in the SearchLoop. */
  MeasurementVectorType m_UpperBound ;

  /** Number of measurment vectors to find k-nearest neighbors. */ 
  int m_NumberOfVisits ;

  /** Flag to stop the SearchLoop. */
  bool m_StopSearch ;

  /** Temporary neighbor */
  NeighborType m_TempNeighbor ;
} ; // end of class

} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKdTree.txx"
#endif

#endif



