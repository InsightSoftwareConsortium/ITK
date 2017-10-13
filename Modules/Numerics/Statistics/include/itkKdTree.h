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
#ifndef itkKdTree_h
#define itkKdTree_h

#include <queue>
#include <vector>

#include "itkPoint.h"
#include "itkSize.h"
#include "itkObject.h"
#include "itkArray.h"

#include "itkSubsample.h"

#include "itkEuclideanDistanceMetric.h"

namespace itk
{
namespace Statistics
{
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
 * \ingroup ITKStatistics
 */
template<typename TSample>

struct ITK_TEMPLATE_EXPORT KdTreeNode
  {
  /** type alias for itself */
  typedef KdTreeNode<TSample> Self;

  /** Measurement type, not the measurement vector type */
  typedef typename TSample::MeasurementType MeasurementType;

  /** Centroid type */
  typedef Array<double> CentroidType;

  /** Instance identifier type (index value type for the measurement
   * vector in a sample */
  typedef typename TSample::InstanceIdentifier InstanceIdentifier;

  /** Returns true if the node is a terminal node, that is a node that
   * doesn't have any child. */
  virtual bool IsTerminal() const = 0;

  /** Fills the partitionDimension (the dimension that was chosen to
   * split the measurement vectors belong to this node to the left and the
   * right child among k dimensions) and the partitionValue (the
   * measurement value on the partitionDimension divides the left and the
   * right child */
  virtual void GetParameters( unsigned int &, MeasurementType & ) const = 0;

  /** Returns the pointer to the left child of this node */
  virtual Self * Left() = 0;

  /** Returns the const pointer to the left child of this node */
  virtual const Self * Left() const = 0;

  /** Returns the pointer to the right child of this node */
  virtual Self * Right() = 0;

  /** Returns the const pointer to the right child of this node */
  virtual const Self * Right() const = 0;

  /**
   * Returs the number of measurement vectors under this node including
   * its children
   */
  virtual unsigned int Size() const = 0;

  /** Returns the vector sum of the all measurement vectors under this node */
  virtual void GetWeightedCentroid( CentroidType & ) = 0;

  /** Returns the centroid. weighted centroid divided by the size */
  virtual void GetCentroid( CentroidType & ) = 0;

  /** Retuns the instance identifier of the index-th measurement vector */
  virtual InstanceIdentifier GetInstanceIdentifier( InstanceIdentifier ) const = 0;

  /** Add an instance to this node */
  virtual void AddInstanceIdentifier( InstanceIdentifier ) = 0;

  /** Destructor */
  virtual ~KdTreeNode() {}  // needed to subclasses will actually be deleted
}; // end of class

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
 * \ingroup ITKStatistics
 */
template<typename TSample>

struct ITK_TEMPLATE_EXPORT KdTreeNonterminalNode:public KdTreeNode<TSample>
  {
  typedef KdTreeNode<TSample>                     Superclass;
  typedef typename Superclass::MeasurementType    MeasurementType;
  typedef typename Superclass::CentroidType       CentroidType;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier;

  KdTreeNonterminalNode( unsigned int, MeasurementType, Superclass *,
    Superclass * );

  virtual ~KdTreeNonterminalNode() {}

  virtual bool IsTerminal() const
  {
    return false;
  }

  void GetParameters( unsigned int &, MeasurementType & ) const;

  /** Returns the pointer to the left child of this node */
  Superclass * Left()
  {
    return m_Left;
  }

  /** Returns the pointer to the right child of this node */
  Superclass * Right()
  {
    return m_Right;
  }

  /** Returns the const pointer to the left child of this node */
  const Superclass * Left() const
  {
    return m_Left;
  }

  /** Returns the const pointer to the right child of this node */
  const Superclass * Right() const
  {
    return m_Right;
  }

  /**
   * Returs the number of measurement vectors under this node including
   * its children
   */
  unsigned int Size() const
  {
    return 0;
  }

  /**
   * Returns the vector sum of the all measurement vectors under this node.
   * Do nothing for this class.
   */
  void GetWeightedCentroid( CentroidType & ) {}

  /**
   * Returns the centroid. weighted centroid divided by the size. Do nothing for
   * this class.
   */
  void GetCentroid( CentroidType & ) {}

  /**
   * Returns the identifier of the only MeasurementVector associated with
   * this node in the tree. This MeasurementVector will be used later during
   * the distance computation when querying the tree.
   */
  InstanceIdentifier GetInstanceIdentifier( InstanceIdentifier ) const
  {
    return this->m_InstanceIdentifier;
  }

  /**
   * Set the identifier of the node.
   */
  void AddInstanceIdentifier( InstanceIdentifier valueId )
  {
    this->m_InstanceIdentifier = valueId;
  }

private:

  unsigned int           m_PartitionDimension;
  MeasurementType        m_PartitionValue;
  InstanceIdentifier     m_InstanceIdentifier;
  Superclass            *m_Left;
  Superclass            *m_Right;
};  // end of class

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
 * \ingroup ITKStatistics
 */
template<typename TSample>
struct ITK_TEMPLATE_EXPORT KdTreeWeightedCentroidNonterminalNode:public KdTreeNode<TSample>
  {
  typedef KdTreeNode<TSample>                         Superclass;
  typedef typename Superclass::MeasurementType        MeasurementType;
  typedef typename Superclass::CentroidType           CentroidType;
  typedef typename Superclass::InstanceIdentifier     InstanceIdentifier;
  typedef typename TSample::MeasurementVectorSizeType MeasurementVectorSizeType;

  KdTreeWeightedCentroidNonterminalNode( unsigned int, MeasurementType,
    Superclass *, Superclass *, CentroidType &, unsigned int );

  virtual ~KdTreeWeightedCentroidNonterminalNode() {}

  /** Not a terminal node. */
  virtual bool IsTerminal() const
  {
    return false;
  }

  /** Return the parameters of the node. */
  void GetParameters( unsigned int &, MeasurementType & ) const;

  /** Return the length of a measurement vector */
  MeasurementVectorSizeType GetMeasurementVectorSize() const
  {
    return m_MeasurementVectorSize;
  }

  /** Return the left tree pointer. */
  Superclass * Left()
  {
    return m_Left;
  }

  /** Return the right tree pointer. */
  Superclass * Right()
  {
    return m_Right;
  }

  /** Return the left tree const pointer. */
  const Superclass * Left() const
  {
    return m_Left;
  }

  /** Return the right tree const pointer. */
  const Superclass * Right() const
  {
    return m_Right;
  }

  /** Return the size of the node. */
  unsigned int Size() const
  {
    return m_Size;
  }

  /**
   * Returns the vector sum of the all measurement vectors under this node.
   */
  void GetWeightedCentroid(CentroidType & centroid)
  {
    centroid = m_WeightedCentroid;
  }

  /**
   * Returns the centroid. weighted centroid divided by the size.
   */
  void GetCentroid(CentroidType & centroid)
  {
    centroid = m_Centroid;
  }

  /**
   * Returns the identifier of the only MeasurementVector associated with
   * this node in the tree. This MeasurementVector will be used later during
   * the distance computation when querying the tree.
   */
  InstanceIdentifier GetInstanceIdentifier(InstanceIdentifier) const
  {
    return this->m_InstanceIdentifier;
  }

  /**
   * Set the identifier of the node.
   */
  void AddInstanceIdentifier(InstanceIdentifier valueId)
  {
    this->m_InstanceIdentifier = valueId;
  }

private:
  MeasurementVectorSizeType     m_MeasurementVectorSize;
  unsigned int                  m_PartitionDimension;
  MeasurementType               m_PartitionValue;
  CentroidType                  m_WeightedCentroid;
  CentroidType                  m_Centroid;
  InstanceIdentifier            m_InstanceIdentifier;
  unsigned int                  m_Size;
  Superclass                   *m_Left;
  Superclass                   *m_Right;
};  // end of class

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
 * \ingroup ITKStatistics
 */
template<typename TSample>
struct ITK_TEMPLATE_EXPORT KdTreeTerminalNode:public KdTreeNode<TSample>
  {
  typedef KdTreeNode<TSample>                     Superclass;
  typedef typename Superclass::MeasurementType    MeasurementType;
  typedef typename Superclass::CentroidType       CentroidType;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier;

  KdTreeTerminalNode() {}

  virtual ~KdTreeTerminalNode()
  {
    this->m_InstanceIdentifiers.clear();
  }

  /** A terminal node. */
  bool IsTerminal() const
  {
    return true;
  }

  /** Return the parameters of the node. */
  void GetParameters( unsigned int &, MeasurementType & ) const {}

  /** Return the left tree pointer. Null for terminal nodes. */
  Superclass * Left()
  {
    return ITK_NULLPTR;
  }

  /** Return the right tree pointer. Null for terminal nodes. */
  Superclass * Right()
  {
    return ITK_NULLPTR;
  }

  /** Return the left tree const pointer. Null for terminal nodes. */
  const Superclass * Left() const
  {
    return ITK_NULLPTR;
  }

  /** Return the right tree const pointer. Null for terminal nodes. */
  const Superclass * Right() const
  {
    return ITK_NULLPTR;
  }

  /** Return the size of the node. */
  unsigned int Size() const
  {
    return static_cast< unsigned int >( m_InstanceIdentifiers.size() );
  }

  /**
   * Returns the vector sum of the all measurement vectors under this node.
   * Do nothing for this case.
   */
  void GetWeightedCentroid( CentroidType & ) {}

  /**
   * Returns the centroid. weighted centroid divided by the size.  Do nothing
   * for this case.
   */
  void GetCentroid( CentroidType & ) {}

  /**
   * Returns the identifier of the only MeasurementVector associated with
   * this node in the tree. This MeasurementVector will be used later during
   * the distance computation when querying the tree.
   */
  InstanceIdentifier GetInstanceIdentifier( InstanceIdentifier index ) const
  {
    return m_InstanceIdentifiers[index];
  }

  /**
   * Set the identifier of the node.
   */
  void AddInstanceIdentifier( InstanceIdentifier id )
  {
    m_InstanceIdentifiers.push_back( id );
  }

private:
  std::vector< InstanceIdentifier > m_InstanceIdentifiers;
};  // end of class

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
 * \ingroup ITKStatistics
 */

template<typename TSample>
class ITK_TEMPLATE_EXPORT KdTree:public Object
{
public:
  /** Standard class typedefs */
  typedef KdTree                     Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(KdTree, Object);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** typedef alias for the source data container */
  typedef TSample                                 SampleType;
  typedef typename TSample::MeasurementVectorType MeasurementVectorType;
  typedef typename TSample::MeasurementType       MeasurementType;
  typedef typename TSample::InstanceIdentifier    InstanceIdentifier;
  typedef typename TSample::AbsoluteFrequencyType AbsoluteFrequencyType;

  typedef unsigned int MeasurementVectorSizeType;

  /** Get Macro to get the length of a measurement vector in the KdTree.
   * The length is obtained from the input sample. */
  itkGetConstMacro( MeasurementVectorSize, MeasurementVectorSizeType );

  /** DistanceMetric type for the distance calculation and comparison */
  typedef EuclideanDistanceMetric< MeasurementVectorType > DistanceMetricType;

  /** Node type of the KdTree */
  typedef KdTreeNode<TSample> KdTreeNodeType;

  /** Neighbor type. The first element of the std::pair is the instance
   * identifier and the second one is the distance between the measurement
   * vector identified by the first element and the query point. */
  typedef std::pair< InstanceIdentifier, double > NeighborType;

  typedef std::vector< InstanceIdentifier > InstanceIdentifierVectorType;

  /** \class NearestNeighbors
   * \brief data structure for storing k-nearest neighbor search result
   * (k number of Neighbors)
   *
   * This class stores the instance identifiers and the distance values
   * of k-nearest neighbors. We can also query the farthest neighbor's
   * distance from the query point using the GetLargestDistance
   * method.
   * \ingroup ITKStatistics
   */
  class NearestNeighbors
  {
  public:
    /** Constructor */
    NearestNeighbors( std::vector<double> & cache_vector) : m_FarthestNeighborIndex(0), m_Distances(cache_vector)  {}

    /** Destructor */
    ~NearestNeighbors() {}

    /** Initialize the internal instance identifier and distance holders
     * with the size, k */
    void resize( unsigned int k )
    {
      m_Identifiers.clear();
      m_Identifiers.resize( k, NumericTraits< IdentifierType >::max() );
      m_Distances.clear();
      m_Distances.resize( k, NumericTraits<double>::max() );
      m_FarthestNeighborIndex = 0;
    }

    /** Returns the distance of the farthest neighbor from the query point */
    double GetLargestDistance()
    {
      return m_Distances[m_FarthestNeighborIndex];
    }

    /** Replaces the farthest neighbor's instance identifier and
     * distance value with the id and the distance */
    void ReplaceFarthestNeighbor( InstanceIdentifier id, double distance )
    {
      m_Identifiers[m_FarthestNeighborIndex] = id;
      m_Distances[m_FarthestNeighborIndex] = distance;
      double farthestDistance = NumericTraits<double>::min();
      const unsigned int size = static_cast< unsigned int >( m_Distances.size() );
      for ( unsigned int i = 0; i < size; i++ )
        {
        if ( m_Distances[i] > farthestDistance )
          {
          farthestDistance = m_Distances[i];
          m_FarthestNeighborIndex = i;
          }
        }
    }

    /** Returns the vector of k-neighbors' instance identifiers */
    const InstanceIdentifierVectorType & GetNeighbors() const
    {
      return m_Identifiers;
    }

    /** Returns the instance identifier of the index-th neighbor among
     * k-neighbors */
    InstanceIdentifier GetNeighbor(unsigned int index) const
    {
      return m_Identifiers[index];
    }

  private:
    NearestNeighbors() ITK_DELETED_FUNCTION;
    /** The index of the farthest neighbor among k-neighbors */
    unsigned int m_FarthestNeighborIndex;

    /** Storage for the instance identifiers of k-neighbors */
    InstanceIdentifierVectorType m_Identifiers;

    /** External storage for the distance values of k-neighbors
     * from the query point. This is a reference to external
     * vector to avoid unnecessary memory copying. */
    std::vector<double> & m_Distances;
  };

  /** Sets the number of measurement vectors that can be stored in a
   * terminal node */
  void SetBucketSize( unsigned int );

  /** Sets the input sample that provides the measurement vectors to the k-d
   * tree */
  void SetSample( const TSample * );

  /** Returns the pointer to the input sample */
  const TSample * GetSample() const
  {
    return m_Sample;
  }

  SizeValueType Size() const
  {
    return m_Sample->Size();
  }

  /** Returns the pointer to the empty terminal node. A KdTree object
   * has a single empty terminal node in memory. when the split process
   * has to create an empty terminal node, the single instance is reused
   * for this case */
  KdTreeNodeType * GetEmptyTerminalNode()
  {
    return m_EmptyTerminalNode;
  }

  /** Sets the root node of the KdTree that is a result of
   * KdTreeGenerator or WeightedCentroidKdTreeGenerator. */
  void SetRoot(KdTreeNodeType *root)
  {
    if ( this->m_Root )
      {
      this->DeleteNode( this->m_Root );
      }
    this->m_Root = root;
  }

  /** Returns the pointer to the root node. */
  KdTreeNodeType * GetRoot()
  {
    return m_Root;
  }

  /** Returns the measurement vector identified by the instance
   * identifier that is an identifier defiend for the input sample */
  const MeasurementVectorType & GetMeasurementVector( InstanceIdentifier id )
    const
  {
    return m_Sample->GetMeasurementVector( id );
  }

  /** Returns the frequency of the measurement vector identified by
   * the instance identifier */
  AbsoluteFrequencyType GetFrequency(InstanceIdentifier id ) const
  {
    return m_Sample->GetFrequency( id );
  }

  /** Get the pointer to the distance metric. */
  DistanceMetricType * GetDistanceMetric()
  {
    return m_DistanceMetric.GetPointer();
  }

  /** Searches the k-nearest neighbors */
  void Search( const MeasurementVectorType &, unsigned int,
              InstanceIdentifierVectorType & ) const;

  /** Searches the k-nearest neighbors and returns
   *  the distance vector along with the distance measures.
   */
  void Search( const MeasurementVectorType &, unsigned int,
    InstanceIdentifierVectorType &, std::vector<double> & ) const;

  /** Searches the neighbors fallen into a hypersphere */
  void Search( const MeasurementVectorType &, double,
    InstanceIdentifierVectorType & ) const;

  /** Returns true if the intermediate k-nearest neighbors exist within
   * the the bounding box defined by the lowerBound and the
   * upperBound. Otherwise returns false. Returns false if the ball
   * defined by the distance between the query point and the farthest
   * neighbor touch the surface of the bounding box. */
  bool BallWithinBounds( const MeasurementVectorType &,
    MeasurementVectorType &, MeasurementVectorType &, double ) const;

  /** Returns true if the ball defined by the distance between the query
   * point and the farthest neighbor overlaps with the bounding box
   * defined by the lower and the upper bounds. */
  bool BoundsOverlapBall( const MeasurementVectorType &,
    MeasurementVectorType &, MeasurementVectorType &, double) const;

  /** Deletes the node recursively */
  void DeleteNode( KdTreeNodeType * );

  /** Prints out the tree information */
  void PrintTree( std::ostream & ) const;

  /** Prints out the tree information */
  void PrintTree( KdTreeNodeType *, unsigned int, unsigned int,
    std::ostream & os = std::cout ) const;

  /** Draw out the tree information to a ostream using
   * the format of the Graphviz dot tool. */
  void PlotTree( std::ostream & os ) const;

  /** Prints out the tree information */
  void PlotTree( KdTreeNodeType *node, std::ostream & os = std::cout ) const;

  typedef typename TSample::Iterator      Iterator;
  typedef typename TSample::ConstIterator ConstIterator;

protected:
  /** Constructor */
  KdTree();

  /** Destructor: deletes the root node and the empty terminal node. */
  virtual ~KdTree() ITK_OVERRIDE;

  virtual void PrintSelf( std::ostream & os, Indent indent ) const ITK_OVERRIDE;

  /** search loop */
  int NearestNeighborSearchLoop( const KdTreeNodeType *,
    const MeasurementVectorType &, MeasurementVectorType &,
    MeasurementVectorType &, NearestNeighbors & ) const;

  /** search loop */
  int SearchLoop( const KdTreeNodeType *, const MeasurementVectorType &,
    double, MeasurementVectorType &, MeasurementVectorType &,
    InstanceIdentifierVectorType & ) const;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(KdTree);

  /** Pointer to the input sample */
  const TSample *m_Sample;

  /** Number of measurement vectors can be stored in a terminal node. */
  int m_BucketSize;

  /** Pointer to the root node */
  KdTreeNodeType *m_Root;

  /** Pointer to the empty terminal node */
  KdTreeNodeType *m_EmptyTerminalNode;

  /** Distance metric smart pointer */
  typename DistanceMetricType::Pointer m_DistanceMetric;

  /** Measurement vector size */
  MeasurementVectorSizeType m_MeasurementVectorSize;
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKdTree.hxx"
#endif

#endif
