/*=========================================================================
  
Program:   Insight Segmentation & Registration Toolkit
Module:    itkParallelSparseFieldLevelSetImageFilter.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkParallelSparseFieldLevelSetImageFilter_h_
#define __itkParallelSparseFieldLevelSetImageFilter_h_

#include <vector>
#include "itkFiniteDifferenceImageFilter.h"
#include "itkSparseFieldLayer.h"
#include "itkObjectStore.h"
#include "itkNeighborhoodIterator.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkMultiThreader.h"
#include "itkBarrier.h"
#include "itkSemaphore.h"

namespace itk
{

/**
 * A data structure used in the ParallelSparsefieldlevelsetimagefilter to construct
 * lists of indicies and other values.
 */
template <class TNodeIndexType>
class ParallelSparseFieldLevelSetNode
{
public:
  TNodeIndexType m_Index;
  float          m_Value;
  ParallelSparseFieldLevelSetNode *Next;
  ParallelSparseFieldLevelSetNode *Previous;
};

/**
 * \class ParallelSparseFieldCityBlockNeighborList
 *
 * \brief A convenience class for storing indicies which reference neighbor
 * pixels within a neighborhood.
 * 
 * \par
 * This class creates and stores indicies for use in finding neighbors within
 * an itk::NeighborhoodIterator object.  Both an array of unsigned integer
 * indicies and an array of N dimensional offsets (from the center of the
 * neighborhood) are created and stored.  The indicies and offsets correspond
 * to the "city-block" neighbors, that is, 4-neighbors in 2d, 6-neighbors in
 * 3d, etc.
 *
 * \par
 * Order of reference is lowest index to highest index in the neighborhood.
 * For example, for 4 connectivity, the indicies refer to the following
 * neighbors:
 * \code
 *
 *  * 1 *
 *  2 * 3
 *  * 4 * 
 *
 * \endcode
 * */
template <class TNeighborhoodType>
class ParallelSparseFieldCityBlockNeighborList
{
public:
  typedef TNeighborhoodType NeighborhoodType;
  typedef typename NeighborhoodType::OffsetType OffsetType;
  typedef typename NeighborhoodType::RadiusType RadiusType;
  
  itkStaticConstMacro(Dimension, unsigned int, NeighborhoodType::Dimension);
  
  const RadiusType &GetRadius() const
  {
    return m_Radius;
  }
  
  const unsigned int &GetArrayIndex(unsigned int i) const
  {
    return m_ArrayIndex[i];
  }

  const OffsetType &GetNeighborhoodOffset(unsigned int i) const
  {
    return m_NeighborhoodOffset[i];
  }
  
  const unsigned int &GetSize() const
  {
    return m_Size;
  }
  
  ParallelSparseFieldCityBlockNeighborList();
  
  ~ParallelSparseFieldCityBlockNeighborList()
  {
    m_ArrayIndex.clear();
    m_NeighborhoodOffset.clear();
  }
  
  void Print(std::ostream &os) const;
  
private:
  char pad1[128];
  unsigned int m_Size;
  RadiusType m_Radius;
  std::vector<unsigned int> m_ArrayIndex;
  std::vector<OffsetType>   m_NeighborhoodOffset;
  char pad2[128];
};

/**
 *  \class ParallelSparseFieldLevelSetImageFilter
 *
 *  \brief This class implements a finite difference partial differential
 *  equation solver for evolving surfaces embedded in volumes as level-sets.
 *
 *  \par
 *  The ``sparse field'' approach to the level-set model is a logical extension
 *  of the classical narrow band technique, which seeks to minimize
 *  computational effort by restricting calculations to those pixels in a
 *  region of interest around the moving surface (the \f$k\f$-level curve). The
 *  sparse field method uses a narrow band that is exactly the width needed to
 *  calculate changes on the level curve for the next time step.  Because the
 *  band of grid points under consideration is so sparse, this approach has
 *  several advantages: the algorithm does exactly the number of calculations
 *  needed to determine the next position of the \f$k\f$-level curve, and the
 *  distance transform around the level curve can be recomputed at each
 *  iteration.
 *
 * \par 
 *  The sparse field algorithm works by constructing a linked list of indicies
 *  that are adjacent to the \f$k\f$-level set.  These indicies are called the
 *  ``active set''.  The values at these active set indicies define the
 *  position of the \f$k\f$-level curve.  The active set indicies are shifted
 *  to follow the distance transform embedding of the \f$k\f$-level curve as
 *  their values move in and out of a fixed numerical range about \f$k\f$. In
 *  this way, the active set is maintained as only those pixels adjacent to the
 *  evolving surface.  Calculations are then done only at indicies contained in
 *  the active set.
 *
 * \par
 *  The city-block neighborhoods of the active set indicies are maintained as
 *  separate lists called ``layers''.  At each iteration, the values at the
 *  layers are reinitialized as the distance transform from the active set.
 *  The number of layers can be adjusted according to the footprint needed for
 *  the calculations on the level curve.
 *  
 * \par
 *  Briefly, the sparse field solver algorithm is as follows:
 *
 * \par
 *  1. For each active layer index \f$x_j\f$: Compute the change at
 *  \f$u_{x_j}\f$, the grid point in the embedding, based on local
 *  geometry and external forces and using a stable numerical scheme.
 *
 *  2. For each active layer index \f$x_j\f$, add the change to the grid point
 *  value and redefine the active set indicies and those of its layers based on
 *  any value changes which have moved outside of the numerical range allowed
 *  for the active set.
 *
 *  3. Starting with the first layers adjacent to the active set and moving
 *  outwards, reconstruct the distance transform by setting values in the
 *  layers according to their neighbors.  At the very outer layers, add or
 *  remove indicies which have come into or moved out of the sparse field.
 *
 * \par HOW TO USE THIS CLASS
 *  Typically, this class should be subclassed with additional functionality
 *  for specific applications.  It is possible, however to use this solver as a
 *  filter directly by instantiating it and supplying it with an appropriate
 *  LevelSetFunction object via the SetDifferenceFunction method.  See the
 *  subclasses and their associated documentation for more information on using
 *  this class.  Also see the FiniteDifferenceImageFilter documentation for a
 *  general overview of this class of solvers.
 *
 * \par INPUTS
 * This filter takes an itk::Image as input.  The appropriate type of input
 * image is entirely determined by the application.  As a rule, however, the
 * input type is immediately converted to the output type before processing.
 * This is because the input is not assumed to be a real value type and must be
 * converted to signed, real values for the calculations.  The input values
 * will also be shifted by the \f$k\f$ isosurface value so that the algorithm
 * only needs to consider the zero level set.
 *
 * \par OUTPUTS
 * The output of the filter is the distance transform embedding of the
 * isosurface as the zero level set.  Values outside the surface will be
 * negative and values inside the surface will be positive.  The distance
 * transform only holds for those indicies in layers around the active layer.
 * Elsewhere, the values are a fixed positive or negative that is one greater
 * than the layer of greatest magnitude.  In other words, if there are three
 * layers, then inside values increase only to 4.0 and outside values only to
 * -4.0.
 *
 * \par PARAMETERS
 * The NumberOfLayers parameter controls the number of layers inside and
 * outside of the active set (see description above).  The sparse field will
 * contain 2*NumberOfLayers+1 lists of indices: the active set and city block
 * neighbors inside and outside the active set.   It is important to
 * specify enough layers to cover the footprint of your calculations.
 * Curvature calculations in three dimensions, for example, require 3 layers.
 * In two dimensions, a minimum of 2 layers is probably required.  Higher order
 * derivatives and other geometrical measures may require more layers.  If too
 * few layers are specified, then the calculations will pull values from the
 * background, which may consist of arbitrary or random values.
 *
 * \par
 * The IsoSurfaceValue indicates which value in the input represents the
 * interface of interest.  By default, this value is zero.  When the solver
 * initializes, it will subtract the IsoSurfaceValue from all values, in the
 * input, shifting the isosurface of interest to zero in the output.
 *
 * \par IMPORTANT!
 *  Read the documentation for FiniteDifferenceImageFilter before attempting to
 *  use this filter.  The solver requires that you specify a
 *  FiniteDifferenceFunction to use for calculations.  This is set using the
 *  method SetDifferenceFunction in the parent class.
 *
 * \par REFERENCES
 * Whitaker, Ross. A Level-Set Approach to 3D Reconstruction from Range Data.
 * International Journal of Computer Vision.  V. 29 No. 3, 203-231. 1998.
 *
 * \par
 * Sethian, J.A. Level Set Methods. Cambridge University Press. 1996.
 *
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT ParallelSparseFieldLevelSetImageFilter :
  public FiniteDifferenceImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs */
  typedef ParallelSparseFieldLevelSetImageFilter  Self;
  typedef FiniteDifferenceImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**Typedefs from the superclass */
  typedef typename Superclass::TimeStepType TimeStepType;
  typedef typename Superclass::FiniteDifferenceFunctionType FiniteDifferenceFunctionType;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ParallelSparseFieldLevelSetImageFilter, FiniteDifferenceImageFilter);
  
  /** Information derived from the image types. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;
  typedef typename OutputImageType::IndexType IndexType;

  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  typedef typename OutputImageType::PixelType PixelType;
  
  typedef typename OutputImageType::RegionType ThreadRegionType;
  
  /** The data type used in numerical computations.  Derived from the output
   *  image type. */
  typedef typename OutputImageType::ValueType ValueType;
  
  /** Node type used in parallel sparse field layer lists. */
  typedef ParallelSparseFieldLevelSetNode<IndexType> LayerNodeType;
  
  /** A list type used in the algorithm. */
  typedef SparseFieldLayer<LayerNodeType> LayerType;
  typedef typename LayerType::Pointer LayerPointerType;
  
  /** A type for a list of LayerPointerTypes */
  typedef std::vector<LayerPointerType> LayerListType;
  
  /** Type used for storing status information */
  typedef signed char StatusType;
  
  /** The type of the image used to index status information.  Necessary for
   *  the internals of the algorithm. */
  typedef Image<StatusType, itkGetStaticConstMacro(ImageDimension)> StatusImageType;
  
  /** Memory pre-allocator used to manage layer nodes in a multi-threaded
   *  environment. */
  typedef ObjectStore<LayerNodeType> LayerNodeStorageType;
  
  typedef Offset<itkGetStaticConstMacro(ImageDimension)> OffsetType;
  
  /** Set/Get the number of layers to use in the sparse field.  Argument is the
   *  number of layers on ONE side of the active layer, so the total layers in
   *   the sparse field is 2 * NumberOfLayers +1*/
  itkSetMacro(NumberOfLayers, StatusType);
  itkGetMacro(NumberOfLayers, StatusType);

  /** Set/Get the value of the isosurface to use in the input image. */
  itkSetMacro(IsoSurfaceValue, ValueType);
  itkGetMacro(IsoSurfaceValue, ValueType);

  /** Get the RMS change calculated in the PREVIOUS iteration.  This value is
   *  the square root of the average square of the change value of all pixels
   *  updated during the previous iteration. */
  itkGetMacro(RMSChange, ValueType);

protected:
  ParallelSparseFieldLevelSetImageFilter();
  ~ParallelSparseFieldLevelSetImageFilter() {}
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  /** Connectivity information for examining neighbor pixels.   */
  ParallelSparseFieldCityBlockNeighborList < NeighborhoodIterator<OutputImageType> >
    m_NeighborList;
  
  /** The constant gradient to maintain between isosurfaces in the
      spare-field of the level-set image.  This value defaults to 1.0 */
  static double m_ConstantGradientValue;
  
  /** Multiplicative identity of the ValueType. */
  static ValueType m_ValueOne;
  
  /** Additive identity of the ValueType. */
  static ValueType m_ValueZero;
  
  /** Special status value which indicates a pending change to a more positive
   *  sparse field. */
  static StatusType m_StatusActiveChangingUp;
  
  /** Special status value which indicates a pending change to a more negative
   *  sparse field. */
  static StatusType m_StatusActiveChangingDown;
  
  /** Special status value used as a default for indicies which have no
      meaningful status. */
  static StatusType m_StatusNull;
  
  /** Special status value which indicates pending change to another sparse
   *  field layer. */
  static StatusType m_StatusChanging;
  
  /** Special status value which indicates a pixel is on the boundary of the
   *  image */
  static StatusType m_StatusBoundaryPixel;
  
  /** This image is a copy of the input with m_IsoSurfaceValue subtracted from
   * each pixel.  This way we only need to consider the zero level set in our
   * calculations.  Makes the implementation easier and more efficient. */
  typename OutputImageType::Pointer m_ShiftedImage;
  
  /** An array which contains all of the layers needed in the sparse
   * field. Layers are organized as follows: m_Layer[0] = active layer, 
   * m_Layer[i:odd] = inside layer (i+1)/2, m_Layer[i:even] = outside layer i/2
   */
  LayerListType m_Layers;
  
  /** The number of layers to use in the sparse field.  Sparse field will
   * consist of m_NumberOfLayers layers on both sides of a single active layer.
   * This active layer is the interface of interest, i.e. the zero level set.*/
  StatusType m_NumberOfLayers;
  
  /** An image of status values used internally by the algorithm. */
  typename StatusImageType::Pointer m_StatusImage;
  typename OutputImageType::Pointer m_OutputImage;
  
  //
  typename StatusImageType::Pointer m_StatusImageNew;
  typename OutputImageType::Pointer m_OutputImageNew;
  
  /** Storage for layer node objects. */
  typename LayerNodeStorageType::Pointer m_LayerNodeStore;
  
  /** The value in the input which represents the isosurface of interest. */
  ValueType m_IsoSurfaceValue;
  
  /** The RMS change calculated from each update.  Can be used by a subclass to
   *  determine halting criteria.  Valid only for the previous iteration, not
   *  during the current iteration.  Calculated in ApplyUpdate. */
  ValueType m_RMSChange;
  
  /** Reimplement the GenerateData() function from FiniteDifferenceImageFilter
   *  for more effective multiprocessing */
  virtual void GenerateData();
  
  /** Copies the input to the output image.  Processing occurs on the output
   * image, so the data type of the output image determines the precision of
   * the calculations (i.e. double or float).  This method overrides the
   * parent class method to do some additional processing. */
  void CopyInputToOutput(); 
  
  /** Reserves memory in the update buffer */
  void AllocateUpdateBuffer() {}
  
  /** Constructs the sparse field layers and initializes their values. Also
   *  creates data structures that are NOT local to a thread. */
  void Initialize();
  
  /** Constructs the active layer and initialize the first layers inside and
   *  outside of the active layer.  The active layer defines the position of the
   *  zero level set by its values, which are constrained within a range around
   *  zero. */
  void ConstructActiveLayer();
  
  /** Initializes the values of the active layer set. */
  void InitializeActiveLayerValues();
  
  /** Initializes a layer of the sparse field using a previously initialized
   * layer. Builds the list of nodes in m_Layer[to] using m_Layer[from].
   * Marks values in the m_StatusImage. */
  void ConstructLayer(StatusType from, StatusType to);
  
  /** */
  void ProcessStatusList(LayerType *InputList, StatusType ChangeToStatus,
                          StatusType SearchForStatus, int ThreadId);
  
  /** Adjusts the values associated with all the index layers of the sparse
   * field by propagating out one layer at a time from the active set. This
   * method also takes care of deleting nodes from the layers which have been
   * marked in the status image as having been moved to other layers.*/
  void PropagateAllLayerValues();
  
  /** Adjusts the values in a single layer "to" using values in a neighboring
   *  layer "from".  The list of indicies in "to" are traversed and assigned
   *  new values appropriately. Any indicies in "to" without neighbors in
   *  "from" are moved into the "promote" layer (or deleted if "promote" is
   *  greater than the number of layers). "InOrOut" == 1 indicates this
   *  propagation is inwards (more negative).  "InOrOut" == 0 indicates this
   *  propagation is outwards (more positive). */   
  void PropagateLayerValues(StatusType from, StatusType to, StatusType promote,
                             int InOrOut);
  
  /**This method pre-processes pixels inside and outside the sparse field
   * layers.  The default is to set them to positive and negative values,
   * respectively. This is not necessary as part of the calculations, but
   * produces a more intuitive output for the user. */
  virtual void InitializeBackgroundPixels();
  
  /** Each thread allocates and initializes the data it will use by itself.
   *  This maintains the memory locality of data w.r.t. the thread in s shared
   *  memory environment.*/
  void ThreadedAllocateData(int ThreadId);
  void ThreadedInitializeData(int ThreadId, const ThreadRegionType & ThreadRegion);
  
  /** This performs the initial load distribution among the threads.  Every
   *  thread gets a slab of the data to work on. The slabs created along a specific
   *  dimension.  Load balancing is performed along the greatest numbered dimension
   *  (i.e. the 3rd dimension in the 3D case and the 2nd dimension in the 2D case).
   *  During the initializing of the sparse field layer an histogram is computed
   *  that stores the number of nodes in the active set for each index along the
   *  chosen dimension.  This histogram is used to divide the work "equally" among
   *  threads so that each thread approximately get the same number of nodes to
   *  process. */  
  void ComputeInitialThreadBoundaries();
  
  /** Find the thread to which a pixel belongs  */
  int GetThreadNumber(int splitAxisValue);
  
  /** Obtain a thread's region split as per the load balancing is done. */
  void GetThreadRegionSplitByBoundary(int ThreadId, ThreadRegionType& ThreadRegion);
  
  /** Delete the data and synchronization primitives used by the threads during
   *  iterations. */
  void DeallocateData();

  /** Structure for managing thread-specific data */
  struct ParallelSparseFieldLevelSetThreadStruct
  {
    ParallelSparseFieldLevelSetImageFilter* Filter;
    TimeStepType* TimeStepList;
    bool* ValidTimeStepList;
    TimeStepType TimeStep;
  };
  
  /** This method calculates the change and does the update, i.e. one iteration
   *  of this iterative solver.  A barrier class is used to synchronize
   *  execution and keep the CalculateChange and ApplyUpdate sections from
   *  executing simultaneously.  */
  void Iterate();
  static ITK_THREAD_RETURN_TYPE IterateThreaderCallback(void * arg);
  
  /** This method allows a subclass to override the way in which updates to
   * output values are applied during each iteration.  The default simply
   * follows the standard finite difference scheme of scaling the change by the
   * timestep and adding to the value of the previous iteration.*/
  inline virtual ValueType CalculateUpdateValue(const IndexType &idx,
                                                 const TimeStepType &dt,
                             const ValueType &value, const ValueType &change)
    {
      return (value + dt * change);
    }
  
  /** This method is not implemented or necessary for this solver */
  void ApplyUpdate(TimeStepType dt) {}

  /** Does the actual work of updating the output from the UpdateContainer over
   *  an output region supplied by the multithreading mechanism.  */
  virtual void ThreadedApplyUpdate(TimeStepType dt, int ThreadId);
  
  /** This method is not implemented or necessary for this solver */
  TimeStepType CalculateChange()
  {
    return NumericTraits<TimeStepType>::Zero;
  }
  
  /** This method does the actual work of calculating change over a region
   * supplied by the multithreading mechanism.  */
  virtual TimeStepType ThreadedCalculateChange(int ThreadId);

  /** */
  void ThreadedUpdateActiveLayerValues(TimeStepType dt, LayerType *StatusUpList,
                                       LayerType *StatusDownList, int ThreadId);
  
  /** */
  void CopyInsertList(int ThreadId, LayerPointerType FromListPtr,
                      LayerPointerType ToListPtr);

  /** */
  void ClearList(int ThreadId, LayerPointerType ListPtr);

  /** */
  void CopyInsertInterNeighborNodeTransferBufferLayers(int ThreadId,
                 LayerPointerType InputList, int InOrOut, int BufferLayerNumber);

  /** */
  void ClearInterNeighborNodeTransferBufferLayers(int ThreadId, int InOrOut,
                                                  int BufferLayerNumber);
  
  /** */
  void ThreadedProcessFirstLayerStatusLists(int OnputLayerNumber,
                         int OutputLayerNumber, StatusType SearchForStatus,
                             int InOrOut, int BufferLayerNumber, int ThreadId);

  /** */
  void ThreadedProcessStatusList(int InputLayerNumber, int OutputLayerNumber,
           StatusType ChangeToStatus, StatusType SearchForStatus, int InOrOut,
                                 int BufferLayerNumber, int ThreadId);

  /** */
  void ThreadedProcessOutsideList(int InputLayerNumber, StatusType ChangeToStatus,
               int InOrOut, int BufferLayerNumber, int ThreadId);
  
  /** */
  void ThreadedPropagateLayerValues (StatusType from, StatusType to, StatusType promote,
                                     int InorOut, int ThreadId);

  /** Split the volume uniformly along the chosen dimension for post processing
   *  the output. */
  void GetThreadRegionSplitUniformly  (int ThreadId, ThreadRegionType& ThreadRegion);

  /** */
  void ThreadedPostProcessOutput (int ThreadId, const ThreadRegionType & regionToProcess);
  
  /** Check if the load is fairly balanced among the threads. */
  virtual void CheckLoadBalance();

  /** Redistribute an unbalanced load among the threads */
  virtual void ThreadedLoadBalance (int ThreadId);
  
  /** Thread synchronization method. */
  void WaitForAll();
  void SignalNeighborsAndWait (int ThreadId);
  void SignalNeighbor  (int SemaphoreArrayNumber, int ThreadId);
  void WaitForNeighbor (int SemaphoreArrayNumber, int ThreadId);
  
  /**  For debugging.  Writes the active layer set (points closest to evolving
   *  interface) to a file. */
  //  void WriteActivePointsToFile ();
  
  /** The number of threads to use. */
  int m_NumOfThreads;
  
  /** The dimension along which to distribute the load. */
  int m_SplitAxis;
  
  /** The length of the dimension along which to split the load. */
  int m_ZSize;

  /** */
  bool m_BoundaryChanged;

  /** */
  int * m_Boundary;

  /** */
  int * m_GlobalZHistogram;

  /** */
  int * m_MapZToThreadNumber;

  /** */
  int * m_ZCumulativeFrequency;

  /** */
  typename Barrier::Pointer m_Barrier;

  /** */
  struct ThreadData
  {
    char pad1 [128];
    
    TimeStepType TimeStep;
    ThreadRegionType ThreadRegion;
    ValueType m_RMSChange;
    int m_Count;
    
    /** The layers */
    LayerListType m_Layers;

    /** Used to transfer data between m_Layers during load balancing */
    LayerListType * m_LoadTransferBufferLayers;
    
    /** Node memory pool */
    typename LayerNodeStorageType::Pointer m_LayerNodeStore;
    
    LayerPointerType UpList[2];
    LayerPointerType DownList[2];
    
    /** Used to transfer data between UpList and DownList across thread
     *  boundaries */
    LayerPointerType** m_InterNeighborNodeTransferBufferLayers[2];
    
    void * globalData;
    
    /** Local histogram with each thread */
    int * m_ZHistogram;
    
    typename Semaphore::Pointer m_Semaphore[2];
    
    /** Indicates whether to use m_SemaphoreArray[0] or m_SemaphoreArray[1] */
    int m_SemaphoreArrayNumber;
    
    char pad2 [128];
  };
  
  /** An array storing the individual structures for each thread. */
  ThreadData *m_Data;
  
  /** Used to check if there are too few pixels remaining. If yes, then we can
   *  stop iterating. */
  bool m_Stop;
  
private:
  
  ParallelSparseFieldLevelSetImageFilter(const Self&); // purposely not implemented
  void operator=(const Self&);                         // purposely not implemented
  
  /** This flag is true when methods need to check boundary conditions and
   *  false when methods do not need to check for boundary conditions. */
  bool m_BoundsCheckingActive;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkParallelSparseFieldLevelSetImageFilter.txx"
#endif

#endif
