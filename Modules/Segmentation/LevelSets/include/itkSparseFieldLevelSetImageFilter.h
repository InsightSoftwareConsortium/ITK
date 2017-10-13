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
#ifndef itkSparseFieldLevelSetImageFilter_h
#define itkSparseFieldLevelSetImageFilter_h

#include "itkFiniteDifferenceImageFilter.h"
#include "itkMultiThreader.h"
#include "itkSparseFieldLayer.h"
#include "itkObjectStore.h"
#include <vector>
#include "itkNeighborhoodIterator.h"

namespace itk
{
/** \class  SparseFieldLevelSetNode
 * A data structure used in the SparseFieldLevelSetImageFilter to construct
 * lists of indices and other values.
 * \ingroup ITKLevelSets
 */
template< typename TValue >
class ITK_TEMPLATE_EXPORT SparseFieldLevelSetNode
{
public:
  TValue               m_Value;
  SparseFieldLevelSetNode *Next;
  SparseFieldLevelSetNode *Previous;
};

/**
 * \class SparseFieldCityBlockNeighborList
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
class ITK_TEMPLATE_EXPORT SparseFieldCityBlockNeighborList
{
public:
  typedef TNeighborhoodType                     NeighborhoodType;
  typedef typename NeighborhoodType::OffsetType OffsetType;
  typedef typename NeighborhoodType::RadiusType RadiusType;
  itkStaticConstMacro(Dimension, unsigned int,
                      NeighborhoodType::Dimension);

  const RadiusType & GetRadius() const
  { return m_Radius; }

  const unsigned int & GetArrayIndex(unsigned int i) const
  { return m_ArrayIndex[i]; }

  const OffsetType & GetNeighborhoodOffset(unsigned int i) const
  { return m_NeighborhoodOffset[i]; }

  const unsigned int & GetSize() const
  { return m_Size; }

  int GetStride(unsigned int i)
  { return m_StrideTable[i]; }

  SparseFieldCityBlockNeighborList();
  ~SparseFieldCityBlockNeighborList() {}

  void Print(std::ostream & os) const;

private:
  unsigned int                m_Size;
  RadiusType                  m_Radius;
  std::vector< unsigned int > m_ArrayIndex;
  std::vector< OffsetType >   m_NeighborhoodOffset;

  /** An internal table for keeping track of stride lengths in a neighborhood,
      i.e. the memory offsets between pixels along each dimensional axis. */
  unsigned m_StrideTable[Dimension];
};

/**
 *  \class SparseFieldLevelSetImageFilter
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
 * isosurface as the zero level set.  Values INSIDE the surface will be
 * NEGATIVE and values OUTSIDE the surface will be POSITIVE.  The distance
 * transform only holds for those indices in layers around the active layer.
 * Elsewhere, the values are a fixed positive or negative that is one greater
 * than the layer of greatest magnitude.  In other words, if there are three
 * layers, then inside values reach a minimum of -4.0 and outside values a
 * maximum of 4.0.
 *
 * \par PARAMETERS
 * The NumberOfLayers parameter controls the number of layers inside and
 * outside of the active set (see description above).  The sparse field will
 * contain 2*NumberOfLayers+1 lists of indices: the active set and city block
 * neighbors inside and outside the active set. It is important to
 * specify enough layers to cover the footprint of your calculations.
 * Curvature calculations require a minimum of 2 layers. Higher order
 * derivatives and other geometrical measures may require more layers.
 * If too few layers are specified, then the calculations will pull values
 * from the background, which may consist of arbitrary or random values.
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
class ITK_TEMPLATE_EXPORT SparseFieldLevelSetImageFilter:
  public FiniteDifferenceImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs */
  typedef SparseFieldLevelSetImageFilter                           Self;
  typedef FiniteDifferenceImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                     Pointer;
  typedef SmartPointer< const Self >                               ConstPointer;

  /**Typedefs from the superclass */
  typedef typename Superclass::TimeStepType           TimeStepType;
  typedef typename Superclass::RadiusType             RadiusType;
  typedef typename Superclass::NeighborhoodScalesType NeighborhoodScalesType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SparseFieldLevelSetImageFilter, FiniteDifferenceImageFilter);

  /** Information derived from the image types. */
  typedef TInputImage                         InputImageType;
  typedef TOutputImage                        OutputImageType;
  typedef typename OutputImageType::IndexType IndexType;
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** The data type used in numerical computations.  Derived from the output
   *  image type. */
  typedef typename OutputImageType::ValueType ValueType;

  /** Node type used in sparse field layer lists. */
  typedef SparseFieldLevelSetNode< IndexType > LayerNodeType;

  /** A list type used in the algorithm. */
  typedef SparseFieldLayer< LayerNodeType > LayerType;
  typedef typename LayerType::Pointer       LayerPointerType;

  /** A type for a list of LayerPointerTypes */
  typedef std::vector< LayerPointerType > LayerListType;

  /** Type used for storing status information */
  typedef signed char StatusType;

  /** The type of the image used to index status information.  Necessary for
   *  the internals of the algorithm. */
  typedef Image< StatusType, itkGetStaticConstMacro(ImageDimension) >
  StatusImageType;

  /** Memory pre-allocator used to manage layer nodes in a multi-threaded
   *  environment. */
  typedef ObjectStore< LayerNodeType > LayerNodeStorageType;

  /** Container type used to store updates to the active layer. */
  typedef std::vector< ValueType > UpdateBufferType;

  /** Set/Get the number of layers to use in the sparse field.  Argument is the
   *  number of layers on ONE side of the active layer, so the total layers in
   *   the sparse field is 2 * NumberOfLayers +1 */
  itkSetMacro(NumberOfLayers, unsigned int);
  itkGetConstMacro(NumberOfLayers, unsigned int);

  /** Set/Get the value of the isosurface to use in the input image. */
  itkSetMacro(IsoSurfaceValue, ValueType);
  itkGetConstMacro(IsoSurfaceValue, ValueType);

  /** Get the RMS change calculated in the PREVIOUS iteration.  This value is
   *  the square root of the average square of the change value of all pixels
   *  updated during the previous iteration. */
  //  itkGetConstMacro(RMSChange, ValueType);

  /** Get/Set the value of the InterpolateSurfaceLocation flag.  This flag
      tells the solver whether or not to interpolate for the surface location
      when calculating change at a voxel location.  Turned on by default.  Some
      applications may not use this value and can safely turn the flag
      off. */
  itkSetMacro(InterpolateSurfaceLocation, bool);
  itkGetConstMacro(InterpolateSurfaceLocation, bool);

  /** See Get/SetInterpolateSurfaceLocation */
  void InterpolateSurfaceLocationOn()
  { this->SetInterpolateSurfaceLocation(true); }
  void InterpolateSurfaceLocationOff()
  { this->SetInterpolateSurfaceLocation(false); }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputEqualityComparableCheck,
                   ( Concept::EqualityComparable< typename TOutputImage::PixelType > ) );
  itkConceptMacro( DoubleConvertibleToOutputCheck,
                   ( Concept::Convertible< double, typename TOutputImage::PixelType > ) );
  itkConceptMacro( OutputOStreamWritableCheck,
                   ( Concept::OStreamWritable< typename TOutputImage::PixelType > ) );
  // End concept checking
#endif

protected:
  SparseFieldLevelSetImageFilter();
  ~SparseFieldLevelSetImageFilter() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /**This function allows a subclass to override the way in which updates to
   * output values are applied during each iteration.  The default simply
   * follows the standard finite difference scheme of scaling the change by the
   * timestep and adding to the value of the previous iteration. */
  inline virtual ValueType CalculateUpdateValue(
    const IndexType & itkNotUsed(idx),
    const TimeStepType & dt,
    const ValueType & value,
    const ValueType & change)
  { return ( value + dt * change ); }

  /**This method packages the output(s) into a consistent format.  The default
   * implementation produces a volume with the final solution values in the
   * sparse field, and inside and outside values elsewhere as appropriate. */
  virtual void PostProcessOutput() ITK_OVERRIDE;

  /**This method pre-processes pixels inside and outside the sparse field
   * layers.  The default is to set them to positive and negative values,
   * respectively. This is not necessary as part of the calculations, but
   * produces a more intuitive output for the user. */
  virtual void InitializeBackgroundPixels();

  /** Constructs the sparse field layers and initializes their values. */
  void Initialize() ITK_OVERRIDE;

  /** Copies the input to the output image.  Processing occurs on the output
   * image, so the data type of the output image determines the precision of
   * the calculations (i.e. double or float).  This method overrides the
   * parent class method to do some additional processing. */
  void CopyInputToOutput() ITK_OVERRIDE;

  /** Reserves memory in the update buffer. Called before each iteration. */
  void AllocateUpdateBuffer() ITK_OVERRIDE;

  /** Applies the update buffer values to the active layer and reconstructs the
   *  sparse field layers for the next iteration. */
  void ApplyUpdate(const TimeStepType& dt) ITK_OVERRIDE;

  /** Traverses the active layer list and calculates the change at these
   *  indices to be applied in the current iteration. */
  TimeStepType CalculateChange() ITK_OVERRIDE;

  /** Initializes a layer of the sparse field using a previously initialized
   * layer. Builds the list of nodes in m_Layer[to] using m_Layer[from].
   * Marks values in the m_StatusImage. */
  void ConstructLayer(StatusType from, StatusType to);

  /** Constructs the active layer and initialize the first layers inside and
   * outside of the active layer.  The active layer defines the position of the
   * zero level set by its values, which are constrained within a range around
   *  zero. */
  void ConstructActiveLayer();

  /** Initializes the values of the active layer set. */
  void InitializeActiveLayerValues();

  /** Adjusts the values in a single layer "to" using values in a neighboring
   *  layer "from".  The list of indices in "to" are traversed and assigned
   *  new values appropriately. Any indices in "to" without neighbors in
   *  "from" are moved into the "promote" layer (or deleted if "promote" is
   *  greater than the number of layers). "InOrOut" == 1 indicates this
   *  propagation is inwards (more negative).  "InOrOut" == 2 indicates this
   *  propagation is outwards (more positive). */
  void PropagateLayerValues(StatusType from, StatusType to,
                            StatusType promote, int InOrOut);

  /** Adjusts the values associated with all the index layers of the sparse
   * field by propagating out one layer at a time from the active set. This
   * method also takes care of deleting nodes from the layers which have been
   * marked in the status image as having been moved to other layers. */
  void PropagateAllLayerValues();

  /** Updates the active layer values using m_UpdateBuffer. Also creates an
   *  "up" and "down" list for promotion/demotion of indices leaving the
   *  active set. */
  void UpdateActiveLayerValues(TimeStepType dt, LayerType *StatusUpList,
                               LayerType *StatusDownList);

  /** */
  void ProcessStatusList(LayerType *InputList, LayerType *OutputList,
                         StatusType ChangeToStatus, StatusType SearchForStatus);

  /** */
  void ProcessOutsideList(LayerType *OutsideList, StatusType ChangeToStatus);

  itkGetConstMacro(ValueZero, ValueType);
  itkGetConstMacro(ValueOne, ValueType);

  /** Connectivity information for examining neighbor pixels.   */
  SparseFieldCityBlockNeighborList< NeighborhoodIterator< OutputImageType > >
  m_NeighborList;

  /** The constant gradient to maintain between isosurfaces in the
      sparse-field of the level-set image.  This value defaults to 1.0 */
  double m_ConstantGradientValue;

  /** Multiplicative identity of the ValueType. */
  static ValueType m_ValueOne;

  /** Additive identity of the ValueType. */
  static ValueType m_ValueZero;

  /** Special status value which indicates pending change to another sparse
   *  field layer. */
  static StatusType m_StatusChanging;

  /** Special status value which indicates a pending change to a more positive
   *  sparse field. */
  static StatusType m_StatusActiveChangingUp;

  /** Special status value which indicates a pending change to a more negative
   *  sparse field. */
  static StatusType m_StatusActiveChangingDown;

  /** Special status value which indicates a pixel is on the boundary of the
   *  image */
  static StatusType m_StatusBoundaryPixel;

  /** Special status value used as a default for indices which have no
      meaningful status. */
  static StatusType m_StatusNull;

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
   * This active layer is the interface of interest, i.e. the zero
   * level set. */
  unsigned int m_NumberOfLayers;

  /** An image of status values used internally by the algorithm. */
  typename StatusImageType::Pointer m_StatusImage;

  /** Storage for layer node objects. */
  typename LayerNodeStorageType::Pointer m_LayerNodeStore;

  /** The value in the input which represents the isosurface of interest. */
  ValueType m_IsoSurfaceValue;

  /** The update buffer used to store change values computed in
   *  CalculateChange. */
  UpdateBufferType m_UpdateBuffer;

  /** The RMS change calculated from each update.  Can be used by a subclass to
   *  determine halting criteria.  Valid only for the previous iteration, not
   *  during the current iteration.  Calculated in ApplyUpdate. */
  //  ValueType m_RMSChange;

  /** This flag tells the solver whether or not to interpolate for the actual
      surface location when calculating change at each active layer node.  By
      default this is turned on. Subclasses which do not sample propagation
      (speed), advection, or curvature terms should turn this flag off. */
  bool m_InterpolateSurfaceLocation;

  const InputImageType *m_InputImage;
  OutputImageType      *m_OutputImage;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SparseFieldLevelSetImageFilter);

  /** This flag is true when methods need to check boundary conditions and
      false when methods do not need to check for boundary conditions. */
  bool m_BoundsCheckingActive;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSparseFieldLevelSetImageFilter.hxx"
#endif

#endif
