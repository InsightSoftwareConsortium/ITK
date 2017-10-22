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
#ifndef itkParallelSparseFieldLevelSetImageFilter_h
#define itkParallelSparseFieldLevelSetImageFilter_h

#include <vector>
#include "itkFiniteDifferenceImageFilter.h"
#include "itkSparseFieldLayer.h"
#include "itkObjectStore.h"
#include "itkNeighborhoodIterator.h"
#include "itkMultiThreader.h"
#include "itkBarrier.h"

namespace itk
{
/** \class ParallelSparseFieldLevelSetNode
 * A data structure used in the ParallelSparsefieldlevelsetimagefilter to construct
 * lists of indices and other values.
 * \ingroup ITKLevelSets
 */
template< typename TNodeIndexType >
class ITK_TEMPLATE_EXPORT ParallelSparseFieldLevelSetNode
{
public:
  TNodeIndexType                   m_Index;
  float                            m_Value;
  ParallelSparseFieldLevelSetNode *Next;
  ParallelSparseFieldLevelSetNode *Previous;
};

/**
 * \class ParallelSparseFieldCityBlockNeighborList
 *
 * \brief A convenience class for storing indices which reference neighbor
 * pixels within a neighborhood.
 *
 * \par
 * This class creates and stores indices for use in finding neighbors within
 * an itk::NeighborhoodIterator object.  Both an array of unsigned integer
 * indices and an array of N dimensional offsets (from the center of the
 * neighborhood) are created and stored.  The indices and offsets correspond
 * to the "city-block" neighbors, that is, 4-neighbors in 2d, 6-neighbors in
 * 3d, etc.
 *
 * \par
 * Order of reference is lowest index to highest index in the neighborhood.
 * For example, for 4 connectivity, the indices refer to the following
 * neighbors:
 * \code
 *
 *  * 1 *
 *  2 * 3
 *  * 4 *
 *
 * \endcode
 *
 * \ingroup ITKLevelSets
 */
template< typename TNeighborhoodType >
class ITK_TEMPLATE_EXPORT ParallelSparseFieldCityBlockNeighborList
{
public:
  typedef TNeighborhoodType                     NeighborhoodType;
  typedef typename NeighborhoodType::OffsetType OffsetType;
  typedef typename NeighborhoodType::RadiusType RadiusType;

  itkStaticConstMacro(Dimension, unsigned int, NeighborhoodType::Dimension);

  const RadiusType & GetRadius() const
  {
    return m_Radius;
  }

  const unsigned int & GetArrayIndex(unsigned int i) const
  {
    return m_ArrayIndex[i];
  }

  const OffsetType & GetNeighborhoodOffset(unsigned int i) const
  {
    return m_NeighborhoodOffset[i];
  }

  const unsigned int & GetSize() const
  {
    return m_Size;
  }

  unsigned int GetStride(unsigned int i)
  {
    return m_StrideTable[i];
  }

  ParallelSparseFieldCityBlockNeighborList();

  ~ParallelSparseFieldCityBlockNeighborList()
  {
    m_ArrayIndex.clear();
    m_NeighborhoodOffset.clear();
  }

  void Print(std::ostream & os) const;

private:
  char                        m_Pad1[128];
  unsigned int                m_Size;
  RadiusType                  m_Radius;
  std::vector< unsigned int > m_ArrayIndex;
  std::vector< OffsetType >   m_NeighborhoodOffset;

  /** An internal table for keeping track of stride lengths in a neighborhood,
   *  i.e. the memory offsets between pixels along each dimensional axis. */
  unsigned int m_StrideTable[Dimension];
  char         m_Pad2[128];
};

/**
 *  \class ParallelSparseFieldLevelSetImageFilter
 *
 *  \brief This class implements a finite difference partial differential
 *  equation solver for evolving surfaces embedded in volumes as level-sets.
 *
 *  \par
 *  The "sparse field" approach to the level-set model is a logical extension
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
 *  The sparse field algorithm works by constructing a linked list of indices
 *  that are adjacent to the \f$k\f$-level set.  These indices are called the
 *  "active set".  The values at these active set indices define the
 *  position of the \f$k\f$-level curve.  The active set indices are shifted
 *  to follow the distance transform embedding of the \f$k\f$-level curve as
 *  their values move in and out of a fixed numerical range about \f$k\f$. In
 *  this way, the active set is maintained as only those pixels adjacent to the
 *  evolving surface.  Calculations are then done only at indices contained in
 *  the active set.
 *
 * \par
 *  The city-block neighborhoods of the active set indices are maintained as
 *  separate lists called "layers".  At each iteration, the values at the
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
 *  value and redefine the active set indices and those of its layers based on
 *  any value changes which have moved outside of the numerical range allowed
 *  for the active set.
 *
 *  3. Starting with the first layers adjacent to the active set and moving
 *  outwards, reconstruct the distance transform by setting values in the
 *  layers according to their neighbors.  At the very outer layers, add or
 *  remove indices which have come into or moved out of the sparse field.
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
 * transform only holds for those indices in layers around the active layer.
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
 * \ingroup ITKLevelSets
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT ParallelSparseFieldLevelSetImageFilter:
  public FiniteDifferenceImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs */
  typedef ParallelSparseFieldLevelSetImageFilter                   Self;
  typedef FiniteDifferenceImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                     Pointer;
  typedef SmartPointer< const Self >                               ConstPointer;

  /**Typedefs from the superclass */
  typedef typename Superclass::TimeStepType                 TimeStepType;
  typedef typename Superclass::FiniteDifferenceFunctionType FiniteDifferenceFunctionType;
  typedef typename Superclass::RadiusType                   RadiusType;
  typedef typename Superclass::NeighborhoodScalesType       NeighborhoodScalesType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ParallelSparseFieldLevelSetImageFilter, FiniteDifferenceImageFilter);

  /** Information derived from the image types. */
  typedef TInputImage                         InputImageType;
  typedef TOutputImage                        OutputImageType;
  typedef typename OutputImageType::IndexType IndexType;

  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  typedef typename OutputImageType::PixelType PixelType;

  typedef typename OutputImageType::RegionType ThreadRegionType;

  /** The data type used in numerical computations.  Derived from the output
   *  image type. */
  typedef typename OutputImageType::ValueType ValueType;

  /** Node type used in parallel sparse field layer lists. */
  typedef ParallelSparseFieldLevelSetNode< IndexType > LayerNodeType;

  /** A list type used in the algorithm. */
  typedef SparseFieldLayer< LayerNodeType > LayerType;
  typedef typename LayerType::Pointer       LayerPointerType;

  /** A type for a list of LayerPointerTypes */
  typedef std::vector< LayerPointerType > LayerListType;

  /** Type used for storing status information */
  typedef signed char StatusType;

  /** The type of the image used to index status information.  Necessary for
   *  the internals of the algorithm. */
  typedef Image< StatusType, itkGetStaticConstMacro(ImageDimension) > StatusImageType;

  /** Memory pre-allocator used to manage layer nodes in a multithreaded
   *  environment. */
  typedef ObjectStore< LayerNodeType > LayerNodeStorageType;

  typedef Offset< itkGetStaticConstMacro(ImageDimension) > OffsetType;

  /** Set/Get the number of layers to use in the sparse field.  Argument is the
   *  number of layers on ONE side of the active layer, so the total layers in
   *   the sparse field is 2 * NumberOfLayers + 1 */
  itkSetMacro(NumberOfLayers, StatusType);
  itkGetConstMacro(NumberOfLayers, StatusType);

  /** Set/Get the value of the isosurface to use in the input image. */
  itkSetMacro(IsoSurfaceValue, ValueType);
  itkGetConstMacro(IsoSurfaceValue, ValueType);

  LayerPointerType GetActiveListForIndex(const IndexType index)
  {
    // get the 'z' value for the index
    const unsigned int indexZ = index[m_SplitAxis];
    // get the thread in whose region the index lies
    const unsigned int ThreadNum = this->GetThreadNumber (indexZ);

    // get the active list for that thread
    return m_Data[ThreadNum].m_Layers[0];
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputEqualityComparableCheck,
                   ( Concept::EqualityComparable< PixelType > ) );
  itkConceptMacro( DoubleConvertibleToOutputCheck,
                   ( Concept::Convertible< double, PixelType > ) );
  itkConceptMacro( OutputOStreamWritableCheck,
                   ( Concept::OStreamWritable< PixelType > ) );
  // End concept checking
#endif

protected:
  ParallelSparseFieldLevelSetImageFilter();
  ~ParallelSparseFieldLevelSetImageFilter() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Connectivity information for examining neighbor pixels.   */
  ParallelSparseFieldCityBlockNeighborList< NeighborhoodIterator< OutputImageType > >
  m_NeighborList;

  /** The constant gradient to maintain between isosurfaces in the
      spare-field of the level-set image.  This value defaults to 1.0 */
  double m_ConstantGradientValue;

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

  /** Special status value used as a default for indices which have no
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
   * calculations.  Makes the implementation easier and more efficient.
   * This is used only during the initialization of the level set. */
  typename OutputImageType::Pointer m_ShiftedImage;

  /** An array which contains all of the layers needed in the sparse
   * field. Layers are organized as follows: m_Layer[0] = active layer,
   * m_Layer[i:odd] = inside layer (i+1)/2, m_Layer[i:even] = outside layer i/2.
   * This is used only during the initialization of the level set. */
  LayerListType m_Layers;

  /** The number of layers to use in the sparse field.  Sparse field will
   * consist of m_NumberOfLayers layers on both sides of a single active layer.
   * This active layer is the interface of interest, i.e. the zero
   * level set. */
  StatusType m_NumberOfLayers;

  /** An image of status values used internally by the algorithm. */
  typename StatusImageType::Pointer m_StatusImage;
  typename OutputImageType::Pointer m_OutputImage;

  /** Images used temporarily during the initialization of the thread data
    structures. */
  typename StatusImageType::Pointer m_StatusImageTemp;
  typename OutputImageType::Pointer m_OutputImageTemp;

  /** Storage for layer node objects. */
  typename LayerNodeStorageType::Pointer m_LayerNodeStore;

  /** The value in the input which represents the isosurface of interest. */
  ValueType m_IsoSurfaceValue;

  /** The RMS change calculated from each update.  Can be used by a subclass to
   *  determine halting criteria.  Valid only for the previous iteration, not
   *  during the current iteration.  Calculated in ApplyUpdate. */
  //  ValueType m_RMSChange;

  /** Reimplement the GenerateData() function from FiniteDifferenceImageFilter
   *  for more effective multithreading */
  virtual void GenerateData() ITK_OVERRIDE;

  /** Copies the input to the output image.  Processing occurs on the output
   * image, so the data type of the output image determines the precision of
   * the calculations (i.e. double or float).  This method overrides the
   * parent class method to do some additional processing. */
  void CopyInputToOutput() ITK_OVERRIDE;

  /** Reserves memory in the update buffer */
  void AllocateUpdateBuffer() ITK_OVERRIDE {}

  /** Constructs the sparse field layers and initializes their values. Also
   *  creates data structures that are NOT local to a thread. */
  void Initialize() ITK_OVERRIDE;

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
  void ConstructLayer(const StatusType& from, const StatusType& to);

  /** */
  void ProcessStatusList(LayerType *InputList, const StatusType& ChangeToStatus,
                         const StatusType& SearchForStatus, ThreadIdType ThreadId);

  /** Adjusts the values associated with all the index layers of the sparse
   * field by propagating out one layer at a time from the active set. This
   * method also takes care of deleting nodes from the layers which have been
   * marked in the status image as having been moved to other layers. */
  void PropagateAllLayerValues();

  /** Adjusts the values in a single layer "to" using values in a neighboring
   *  layer "from".  The list of indices in "to" are traversed and assigned
   *  new values appropriately. Any indices in "to" without neighbors in
   *  "from" are moved into the "promote" layer (or deleted if "promote" is
   *  greater than the number of layers). "InOrOut" == 1 indicates this
   *  propagation is inwards (more negative).  "InOrOut" == 0 indicates this
   *  propagation is outwards (more positive). */
  void PropagateLayerValues(const StatusType& from, const StatusType& to,
                            const StatusType& promote, unsigned int InOrOut);

  /**This method pre-processes pixels inside and outside the sparse field
   * layers.  The default is to set them to positive and negative values,
   * respectively. This is not necessary as part of the calculations, but
   * produces a more intuitive output for the user. */
  virtual void InitializeBackgroundPixels();

  /** Each thread allocates and initializes the data it will use by itself.
   *  This maintains the memory locality of data w.r.t. the thread in a shared
   *  memory environment. */
  void ThreadedAllocateData(ThreadIdType ThreadId);

  void ThreadedInitializeData(ThreadIdType ThreadId, const ThreadRegionType & ThreadRegion);

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
  unsigned int GetThreadNumber(unsigned int splitAxisValue);

  /** Obtain a thread's region split as per the load balancing is done. */
  void GetThreadRegionSplitByBoundary(ThreadIdType ThreadId, ThreadRegionType & ThreadRegion);

  /** Delete the data and synchronization primitives used by the threads during
   *  iterations. */
  void DeallocateData();

  /** Structure for managing thread-specific data */
  struct ParallelSparseFieldLevelSetThreadStruct {
    ParallelSparseFieldLevelSetImageFilter *Filter;
    std::vector< TimeStepType > TimeStepList;
    std::vector< bool > ValidTimeStepList;
    TimeStepType TimeStep;
  };

  /** This method calculates the change and does the update, i.e. one iteration
   *  of this iterative solver.  A barrier class is used to synchronize
   *  execution and keep the CalculateChange and ApplyUpdate sections from
   *  executing simultaneously.  */
  void Iterate();

  static ITK_THREAD_RETURN_TYPE IterateThreaderCallback(void *arg);

  /** This method allows a subclass to override the way in which updates to
   * output values are applied during each iteration.  The default simply
   * follows the standard finite difference scheme of scaling the change by the
   * timestep and adding to the value of the previous iteration. */
  inline virtual ValueType ThreadedCalculateUpdateValue(const ThreadIdType itkNotUsed(ThreadId),
                                                        const IndexType itkNotUsed(index),
                                                        const TimeStepType & dt,
                                                        const ValueType & value,
                                                        const ValueType & change)
  {
    return ( value + static_cast< ValueType >( dt ) * change );
  }

  // This method can be overridden in derived classes.
  // The pixel at 'index' is entering the active layer for thread 'ThreadId'.
  // The outputimage at 'index' will have the value as given by the
  // 'value' parameter.
  virtual void ThreadedProcessPixelEnteringActiveLayer( const IndexType& itkNotUsed(index),
                                                        const ValueType& itkNotUsed(value),
                                                        ThreadIdType itkNotUsed(ThreadId) );

  /** This method is not implemented or necessary for this solver */
  void ApplyUpdate(const TimeStepType&) ITK_OVERRIDE {}

  /** Does the actual work of updating the output from the UpdateContainer over
   *  an output region supplied by the multithreading mechanism.  */
  virtual void ThreadedApplyUpdate(const TimeStepType& dt, ThreadIdType ThreadId);

  /** This method is not implemented or necessary for this solver */
  TimeStepType CalculateChange() ITK_OVERRIDE
  {
    return NumericTraits< TimeStepType >::ZeroValue();
  }

  /** This method does the actual work of calculating change over a region
   *  supplied by the multithreading mechanism.  */
  virtual TimeStepType ThreadedCalculateChange(ThreadIdType ThreadId);

  /** 1. Updates the values (in the output-image) of the nodes in the active layer
   *  that are moving OUT of the active layer. These values are used in the
   *  ThreadedProcessFirstLayerStatusLists() method to assign values for new nodes
   *  that are moving IN the active layer.
   *  2. This function also constructs the up/down lists for nodes that are moving
   *  out of the active layer. */
  void ThreadedUpdateActiveLayerValues( const TimeStepType& dt,
    LayerType   *StatusUpList,
    LayerType   *StatusDownList,
    ThreadIdType ThreadId);

  /** Make a copy of the nodes in the FromList and insert them into the ToList.
    */
  void CopyInsertList( ThreadIdType ThreadId,
    LayerPointerType FromListPtr,
    LayerPointerType ToListPtr);

  /** Delete all nodes in the List */
  void ClearList(ThreadIdType ThreadId, LayerPointerType ListPtr);

  /** Make a copy of the nodes given to one thread by its neighbors to process
   *  and insert them into the thread's own list. */
  void CopyInsertInterNeighborNodeTransferBufferLayers(
    ThreadIdType ThreadId,
    LayerPointerType InputList,
    unsigned int InOrOut,
    unsigned int BufferLayerNumber);

  /** Delete all nodes in a thread's own lists which its used to transfer nodes
   *  to neighboring threads. */
  void ClearInterNeighborNodeTransferBufferLayers(
    ThreadIdType ThreadId, unsigned int InOrOut,
    unsigned int BufferLayerNumber);

  /** Performs two tasks. The situation here is that ThreadedProcessStatusList
   *  has been called just once after the active layer values have been updated and the
   *  UpLists and DownLists formed. Some nodes are now moving into the active layer.
   *  The two tasks performed are:
   *  1. modify the status-image like it is performed by the ThreadedProcessStatusList.
   *  2. Update the values in the output-image for those nodes that are moving IN the
   *  active layer. */
  void ThreadedProcessFirstLayerStatusLists(
    unsigned int InputLayerNumber,
    unsigned int OutputLayerNumber,
    const StatusType& SearchForStatus,
    unsigned int InOrOut,
    unsigned int BufferLayerNumber,
    ThreadIdType ThreadId);

  /** Push each index in the input list into its appropriate status layer
   *  (ChangeToStatus) and update the status image value at that index.
   *  Also examine the neighbors of the index, (with status SearchForStatus) to determine
   *  which need to go onto the output list.
   */
  void ThreadedProcessStatusList(
    unsigned int InputLayerNumber,
    unsigned int OutputLayerNumber,
    const StatusType& ChangeToStatus,
    const StatusType& SearchForStatus,
    unsigned int InOrOut,
    unsigned int BufferLayerNumber,
    ThreadIdType ThreadId);

  /** Push each index in the input list into its appropriate status layer
   *  (ChangeToStatus) and ... ... update the status image value at that index
   */
  void ThreadedProcessOutsideList(
    unsigned int InputLayerNumber,
    const StatusType& ChangeToStatus,
    unsigned int InOrOut,
    unsigned int BufferLayerNumber,
    ThreadIdType ThreadId);

  /** */
  void ThreadedPropagateLayerValues(
    const StatusType& from,
    const StatusType& to,
    const StatusType& promote,
    unsigned int InorOut,
    ThreadIdType ThreadId);

  /** Split the volume uniformly along the chosen dimension for post processing
   *  the output. */
  void GetThreadRegionSplitUniformly(
    ThreadIdType ThreadId, ThreadRegionType & ThreadRegion);

  /** Assign background pixels INSIDE the sparse field layers to a new level set
   *  with value less than the innermost layer.  Assign background pixels
   *  OUTSIDE the sparse field layers to a new level set with value greater than
   *  the outermost layer.
   */
  void ThreadedPostProcessOutput(const ThreadRegionType & regionToProcess);

  /** Check if the load is fairly balanced among the threads.
   *  This is performed by just one thread while all other threads wait.
   *  This need NOT be performed every iteration because the level-set surface moves slowly
   *  and it is correct to believe that during an iteration the movement is small enough that
   *  the small gain obtained by load balancing (if any) does not warrant the overhead for
   *  calling this method.
   *  How often this is done is controlled by a parameter LOAD_BALANCE_ITERATION_FREQUENCY
   *  which is defined in the IterateThreaderCallback() function.
   *  A parameter that defines a degree of unbalancedness of the load among threads is
   *  MAX_PIXEL_DIFFERENCE_PERCENT which is defined in CheckLoadBalance(). */
  virtual void CheckLoadBalance();

  /** Redistribute an load among the threads to obtain a more balanced load distribution.
   *  This is performed in parallel by all the threads. */
  virtual void ThreadedLoadBalance(ThreadIdType ThreadId);

  /** Thread synchronization methods. */
  void WaitForAll();

  void SignalNeighborsAndWait(ThreadIdType ThreadId);

  void SignalNeighbor(unsigned int SemaphoreArrayNumber, ThreadIdType ThreadId);

  void WaitForNeighbor(unsigned int SemaphoreArrayNumber, ThreadIdType ThreadId);

  /** If child classes need an entry point to the start of every iteration step
   * they can override this method. This method is defined but empty in this class. */
  virtual void ThreadedInitializeIteration(ThreadIdType ThreadId);

  /**  For debugging.  Writes the active layer set (grid-points closest to evolving
   *  interface) to a file. */
  //  void WriteActivePointsToFile ();

  /** The number of threads to use. */
  ThreadIdType m_NumOfThreads;

  /** The dimension along which to distribute the load. */
  unsigned int m_SplitAxis;

  /** The length of the dimension along which to distribute the load. */
  unsigned int m_ZSize;

  /** A boolean variable stating if the boundaries had been changed during
   *  CheckLoadBalance() */
  bool m_BoundaryChanged;

  /** The boundaries defining thread regions */
  unsigned int *m_Boundary;

  /** Histogram of number of pixels in each Z plane for the entire 3D volume */
  int *m_GlobalZHistogram;

  /** The mapping from a z-value to the thread in whose region the z-value lies
    */
  unsigned int *m_MapZToThreadNumber;

  /** Cumulative frequency of number of pixels in each Z plane for the entire 3D
   *  volume  */
  int *m_ZCumulativeFrequency;

  /** A global barrier used for synchronization between all threads. */
  typename Barrier::Pointer m_Barrier;

  /** Local data for each individual thread. */
  struct ThreadData {
    char pad1[128];

    TimeStepType TimeStep;
    ThreadRegionType ThreadRegion;
    ValueType m_RMSChange;
    unsigned int m_Count;

    /** The layers */
    LayerListType m_Layers;

    /** Used to transfer data between m_Layers during load balancing */
    LayerListType *m_LoadTransferBufferLayers;

    /** Node memory pool */
    typename LayerNodeStorageType::Pointer m_LayerNodeStore;

    LayerPointerType UpList[2];
    LayerPointerType DownList[2];

    /** Used to transfer data between UpList and DownList across thread
     *  boundaries */
    LayerPointerType **m_InterNeighborNodeTransferBufferLayers[2];

    /** A pointer to the GlobalData struct obtained from the difference function.
     *  Every thread has its own copy of the struct */
    void *globalData;

    /** Local histogram with each thread */
    int *m_ZHistogram;

    /** pseudo-Semaphores used for signalling and waiting neighbor
     *  threads. Strictly speaking the semaphores are NOT just
     *  accessed by the thread that owns them
     *  BUT also by the thread's neighbors. So they are NOT truly "local" data. */
    int m_Semaphore[2];

    SimpleMutexLock            m_Lock[2];
    ConditionVariable::Pointer m_Condition[2];

    /** Indicates whether to use m_Semaphore[0] or m_Semaphore[1] for
      signalling/waiting */
    unsigned int m_SemaphoreArrayNumber;

    char m_Pad2[128];
  };

  /** An array storing the individual (local) data structures for each thread.
    */
  ThreadData *m_Data;

  /** Used to check if there are too few pixels remaining. If yes, then we can
   *  stop iterating. */
  bool m_Stop;

  /** This flag tells the solver whether or not to interpolate for the actual
      surface location when calculating change at each active layer node.  By
      default this is turned on. Subclasses which do not sample propagation
      (speed), advection, or curvature terms should turn this flag off. */
  bool m_InterpolateSurfaceLocation;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(ParallelSparseFieldLevelSetImageFilter);

  /** This flag is true when methods need to check boundary conditions and
   *  false when methods do not need to check for boundary conditions. */
  bool m_BoundsCheckingActive;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkParallelSparseFieldLevelSetImageFilter.hxx"
#endif

#endif
