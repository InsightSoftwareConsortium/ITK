/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKdTree.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
 * <b>Recent API changes:</b>
 * The static const macro to get the length of a measurement vector,
 * \c MeasurementVectorSize  has been removed to allow the length of a measurement
 * vector to be specified at run time. The \c typedef for \c CentroidType has
 * been changed from Array to FixedArray.
 *
 * \sa KdTreeNonterminalNode, KdTreeWeightedCentroidNonterminalNode,
 * KdTreeTerminalNode 
 */
template< class TSample >
struct KdTreeNode
{
  /** type alias for itself */
  typedef KdTreeNode< TSample> Self ;
  
  /** Measurement type, not the measurement vector type */
  typedef typename TSample::MeasurementType MeasurementType ;
  
  /** Centroid type */
  typedef Array< double > CentroidType;
  
  /** Instance identifier type (index value type for the measurement
   * vector in a sample */
  typedef typename TSample::InstanceIdentifier InstanceIdentifier ;

  /** Returns true if the node is a terminal node, that is a node that
   * doesn't have any child. */
  virtual bool IsTerminal() const = 0 ;

  /** Fills the partitionDimension (the dimension that was chosen to
   * split the measurement vectors belong to this node to the left and the
   * right child among k dimensions) and the partitionValue (the
   * measurement value on the partitionDimension divides the left and the
   * right child */
  virtual void GetParameters(unsigned int &partitionDimension, 
                             MeasurementType &partitionValue) const = 0 ;  

  /** Returns the pointer to the left child of this node */
  virtual       Self* Left()       = 0 ;
  virtual const Self* Left() const = 0 ;

  /** Returns the pointer to the right child of this node */
  virtual       Self* Right()       = 0 ;
  virtual const Self* Right() const = 0 ;

  /** Returs the number of measurement vectors under this node including
   * its children */
  virtual unsigned int Size() const = 0 ;

  /** Returns the vector sum of the all measurement vectors under this node */
  virtual void GetWeightedCentroid(CentroidType &centroid) = 0 ;

  /** Returns the centroid. weighted centroid divided by the size */
  virtual void GetCentroid(CentroidType &centroid) = 0 ;

  /** Retuns the instance identifier of the index-th measurement vector */
  virtual InstanceIdentifier GetInstanceIdentifier(size_t index) const = 0 ;

  /** Add an instance to this node */
  virtual void AddInstanceIdentifier(InstanceIdentifier id) = 0 ;

  /** Destructor */
  virtual ~KdTreeNode() {}; // needed to subclasses will actually be deleted
} ; // end of class

/** \class KdTreeNonterminalNode
 *  \brief This is a subclass of the KdTreeNode. 
 *
 * KdTreeNonterminalNode doesn't store the information related with the
 * centroids. Therefore, the GetWeightedCentroid and the GetCentroid
 * methods are void. This class should have the left and the right
 * children. If we have a sample and want to generate a KdTree without
 * the centroid related information, we can use the KdTreeGenerator.
 * 
 * \sa KdTreeNode, KdTreeWeightedCentroidNonterminalNode, KdTreeGenerator
 */
template< class TSample >
struct KdTreeNonterminalNode: public KdTreeNode< TSample >
{
  typedef KdTreeNode< TSample > Superclass ;
  typedef typename Superclass::MeasurementType MeasurementType ;
  typedef typename Superclass::CentroidType CentroidType ;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier ;

  KdTreeNonterminalNode(unsigned int partitionDimension,
                        MeasurementType partitionValue,
                        Superclass* left,
                        Superclass* right) ;

  virtual ~KdTreeNonterminalNode() {}
  
  virtual bool IsTerminal() const
  { return false ; }

  void GetParameters(unsigned int &partitionDimension, 
                     MeasurementType &partitionValue) const;

  Superclass* Left() 
  { return m_Left ; }

  Superclass* Right() 
  { return m_Right ; }

  const Superclass* Left() const
  { return m_Left ; }

  const Superclass* Right() const
  { return m_Right ; }

  unsigned int Size() const
  { return 0 ; }

  void GetWeightedCentroid(CentroidType &)
  { /* do nothing */ } 

  void GetCentroid(CentroidType &)
  { /* do nothing */ }

  InstanceIdentifier GetInstanceIdentifier(size_t) const
  { return 0 ; }

  void AddInstanceIdentifier(InstanceIdentifier) {}

private:
  unsigned int m_PartitionDimension ;
  MeasurementType m_PartitionValue ;
  Superclass* m_Left ;
  Superclass* m_Right ;
} ; // end of class

/** \class KdTreeWeightedCentroidNonterminalNode
 *  \brief This is a subclass of the KdTreeNode. 
 * 
 * KdTreeNonterminalNode does have the information related with the
 * centroids. Therefore, the GetWeightedCentroid and the GetCentroid
 * methods returns meaningful values. This class should have the left
 * and right children. If we have a sample and want to generate a KdTree
 * with the centroid related information, we can use the
 * WeightedCentroidKdTreeGenerator. The centroid, the weighted
 * centroid, and the size (the number of measurement vectors) can be
 * used to accelate the k-means estimation.
 * 
 * \sa KdTreeNode, KdTreeNonterminalNode, WeightedCentroidKdTreeGenerator
 */
template< class TSample >
struct KdTreeWeightedCentroidNonterminalNode: public KdTreeNode< TSample >
{
  typedef KdTreeNode< TSample > Superclass ;
  typedef typename Superclass::MeasurementType MeasurementType ;
  typedef typename Superclass::CentroidType CentroidType ;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier ;
  typedef typename TSample::MeasurementVectorSizeType MeasurementVectorSizeType;

  KdTreeWeightedCentroidNonterminalNode(unsigned int partitionDimension,
                                         MeasurementType partitionValue,
                                         Superclass* left,
                                         Superclass* right,
                                         CentroidType &centroid,
                                         unsigned int size) ;
  virtual ~KdTreeWeightedCentroidNonterminalNode() {}

  virtual bool IsTerminal() const
  { return false ; }

  void GetParameters(unsigned int &partitionDimension, 
                     MeasurementType &partitionValue) const ;

  /** Return the length of a measurement vector */
  MeasurementVectorSizeType GetMeasurementVectorSize() const
    {
    return m_MeasurementVectorSize;
    }

  Superclass* Left() 
  { return m_Left ; }

  Superclass* Right() 
  { return m_Right ; }


  const Superclass* Left() const
  { return m_Left ; }

  const Superclass* Right() const
  { return m_Right ; }

  unsigned int Size() const
  { return m_Size ; }

  void GetWeightedCentroid(CentroidType &centroid)
  { centroid = m_WeightedCentroid ; }

  void GetCentroid(CentroidType &centroid)
  { centroid = m_Centroid ; }

  InstanceIdentifier GetInstanceIdentifier(size_t) const
  { return 0 ; }

  void AddInstanceIdentifier(InstanceIdentifier) {}

private:
  MeasurementVectorSizeType m_MeasurementVectorSize;
  unsigned int m_PartitionDimension ;
  MeasurementType m_PartitionValue ;
  CentroidType m_WeightedCentroid ;
  CentroidType m_Centroid ;
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
 * KdTreeWeightedCentroidNonterminalNode
 */
template< class TSample >
struct KdTreeTerminalNode: public KdTreeNode< TSample >
{
  typedef KdTreeNode< TSample > Superclass ;
  typedef typename Superclass::MeasurementType MeasurementType ;
  typedef typename Superclass::CentroidType CentroidType ;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier ;

  KdTreeTerminalNode() {}

  virtual ~KdTreeTerminalNode() {}

  bool IsTerminal() const
  { return true ; }

  void GetParameters(unsigned int &, 
                     MeasurementType &) const {}

  Superclass* Left() 
  { return 0 ; }

  Superclass* Right() 
  { return 0 ; }


  const Superclass* Left() const
  { return 0 ; }

  const Superclass* Right() const
  { return 0 ; }

  unsigned int Size() const
  { return static_cast<unsigned int>( m_InstanceIdentifiers.size() ); }

  void GetWeightedCentroid(CentroidType &)
  { /* do nothing */ } 

  void GetCentroid(CentroidType &)
  { /* do nothing */ } 

  InstanceIdentifier GetInstanceIdentifier(size_t index) const
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
 * use the KdTreeGenerator or WeightedCentroidKdTreeGenerator to
 * generate a static KdTree object. 
 *
 * To search k-nearest neighbor, call the Search method with the query
 * point in a k-d space and the number of nearest neighbors. The
 * GetSearchResult method returns a pointer to a NearestNeighbors object
 * with k-nearest neighbors.
 * 
 * <b>Recent API changes:</b>
 * The static const macro to get the length of a measurement vector,
 * 'MeasurementVectorSize'  has been removed to allow the length of a measurement
 * vector to be specified at run time. Please use the function 
 * GetMeasurementVectorSize() instead.

 * \sa KdTreeNode, KdTreeNonterminalNode,
 * KdTreeWeightedCentroidNonterminalNode, KdTreeTerminalNode,
 * KdTreeGenerator, WeightedCentroidKdTreeNode
 */

template < class TSample >
class ITK_EXPORT KdTree : public Object
{
public:
  /** Standard class typedefs */
  typedef KdTree Self ;
  typedef Object Superclass ;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(KdTree, Object);

  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  /** typedef alias for the source data container */ 
  typedef TSample SampleType ;
  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;
  typedef typename TSample::MeasurementType MeasurementType ;
  typedef typename TSample::InstanceIdentifier InstanceIdentifier ;
  typedef typename TSample::FrequencyType FrequencyType ;

  typedef unsigned int                    MeasurementVectorSizeType;

  /** Get Macro to get the length of a measurement vector in the KdTree.
   * The length is obtained from the input sample. */
  itkGetConstMacro( MeasurementVectorSize, MeasurementVectorSizeType );

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
  void SetSample(const TSample* sample) ;

  /** Returns the pointer to the input sample */
  const TSample* GetSample() const
  { return m_Sample ; }

  unsigned long Size() const
  { return m_Sample->Size() ; }

  /** Returns the pointer to the empty terminal node. A KdTree object
   * has a single empty terminal node in memory. when the split process
   * has to create an empty terminal node, the single instance is reused
   * for this case */
  KdTreeNodeType* GetEmptyTerminalNode()
  { return m_EmptyTerminalNode ; } 

  /** Sets the root node of the KdTree that is a result of
   * KdTreeGenerator or WeightedCentroidKdTreeGenerator. */
  void SetRoot(KdTreeNodeType* root) 
  { m_Root = root ; } 

  /** Returns the pointer to the root node. */
  KdTreeNodeType* GetRoot() 
  { return m_Root ; } 

  /** Returns the measurement vector identified by the instance
   * identifier that is an identifier defiend for the input sample */
  const MeasurementVectorType & GetMeasurementVector(InstanceIdentifier id) const
  { return m_Sample->GetMeasurementVector(id) ; }

  /** Returns the frequency of the measurement vector identified by 
   * the instance identifier */
  FrequencyType GetFrequency(InstanceIdentifier id) const
  { return m_Sample->GetFrequency( id ) ; }

  /** Get the pointer to the distance metric. */
  DistanceMetricType* GetDistanceMetric()
  { return m_DistanceMetric.GetPointer() ; }

  /** Searches the k-nearest neighbors */
  void Search(MeasurementVectorType &query, 
              unsigned int k,
              InstanceIdentifierVectorType& result) const;

  /** Searches the neighbors fallen into a hypersphere */
  void Search(MeasurementVectorType &query,
              double radius, 
              InstanceIdentifierVectorType& result) const;

  /** Returns the number of measurement vectors that have been visited
   * to find the k-nearest neighbors. */
  int GetNumberOfVisits() const
  { return m_NumberOfVisits ; }

  /** Returns true if the intermediate k-nearest neighbors exist within
   * the the bounding box defined by the lowerBound and the
   * upperBound. Otherwise returns false. Returns false if the ball
   * defined by the distance between the query point and the farthest
   * neighbor touch the surface of the bounding box.*/
  bool BallWithinBounds(MeasurementVectorType &query, 
                        MeasurementVectorType &lowerBound,
                        MeasurementVectorType &upperBound,
                        double radius) const ;

  /** Returns true if the ball defined by the distance between the query
   * point and the farthest neighbor overlaps with the bounding box
   * defined by the lower and the upper bounds.*/
  bool BoundsOverlapBall(MeasurementVectorType &query, 
                         MeasurementVectorType &lowerBound,
                         MeasurementVectorType &upperBound,
                         double radius) const ;

  /** Deletes the node recursively */
  void DeleteNode(KdTreeNodeType *node) ;

  /** Prints out the tree information */
  void PrintTree(KdTreeNodeType *node, int level, 
                 unsigned int activeDimension) ;

  typedef typename TSample::Iterator Iterator ;
  typedef typename TSample::ConstIterator ConstIterator ;

  Iterator Begin()
  {
    typename TSample::ConstIterator iter = m_Sample->Begin() ;
    return iter; 
  }

  Iterator End() 
  {
    Iterator iter = m_Sample->End() ;
    return iter; 
  }

  ConstIterator Begin() const
  {
    typename TSample::ConstIterator iter = m_Sample->Begin() ;
    return iter; 
  }

  ConstIterator End() const
  {
    ConstIterator iter = m_Sample->End() ;
    return iter; 
  }


protected:
  /** Constructor */
  KdTree() ;
  
  /** Destructor: deletes the root node and the empty terminal node. */
  virtual ~KdTree() ;

  void PrintSelf(std::ostream& os, Indent indent) const ;

  /** search loop */ 
  int NearestNeighborSearchLoop(const KdTreeNodeType* node,
                                MeasurementVectorType &query,
                                MeasurementVectorType &lowerBound,
                                MeasurementVectorType &upperBound) const;

  /** search loop */ 
  int SearchLoop(const KdTreeNodeType* node, MeasurementVectorType &query,
                 MeasurementVectorType &lowerBound,
                 MeasurementVectorType &upperBound) const ;
private:
  KdTree(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented

  /** Pointer to the input sample */
  const TSample* m_Sample ;
  
  /** Number of measurement vectors can be stored in a terminal node. */
  int m_BucketSize ;

  /** Pointer to the root node */
  KdTreeNodeType* m_Root ;

  /** Pointer to the empty terminal node */
  KdTreeNodeType* m_EmptyTerminalNode ;

  /** Distance metric smart pointer */
  typename DistanceMetricType::Pointer m_DistanceMetric ;

  mutable bool m_IsNearestNeighborSearch ;
 
  mutable double m_SearchRadius ;

  mutable InstanceIdentifierVectorType m_Neighbors ;

  /** k-nearest neighbors */
  mutable NearestNeighbors m_NearestNeighbors ;

  /** Temporary lower bound in the SearchLoop. */
  mutable MeasurementVectorType m_LowerBound ;

  /** Temporary upper bound in the SearchLoop. */
  mutable MeasurementVectorType m_UpperBound ;

  /** Number of measurment vectors to find k-nearest neighbors. */ 
  mutable int m_NumberOfVisits ;

  /** Flag to stop the SearchLoop. */
  mutable bool m_StopSearch ;

  /** Temporary neighbor */
  mutable NeighborType m_TempNeighbor ;

  /** Measurement vector size */
  MeasurementVectorSizeType m_MeasurementVectorSize;
} ; // end of class

} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKdTree.txx"
#endif

#endif



