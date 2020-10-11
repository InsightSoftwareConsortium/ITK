/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkNarrowBandImageFilterBase_h
#define itkNarrowBandImageFilterBase_h

#include "itkFiniteDifferenceImageFilter.h"
#include "itkMultiThreaderBase.h"
#include "itkNarrowBand.h"
#include "itkObjectStore.h"
#include "itkPlatformMultiThreader.h"


namespace itk
{
/**
 * \class NarrowBandImageFilterBase
 *
 * \brief This class implements a multi-threaded finite difference
 *  image to image solver that can be applied to an arbitrary list of
 *  pixels.
 *
 * \par
 *  This class is intended as a common base class for classical narrowband
 *  solvers and manifold solvers. This base class implements a common
 *  memory management and multi-threaded architecture for applying a
 *  finite difference function to a list of pixels in an image. The specifics
 *  of narrowband solvers such as re-initialization and the use of land-mines
 *  are not implemented.
 *
 * \par INPUTS
 *  This filter takes an itk::Image as input.  The appropriate type of input
 *  image is entirely determined by the application.  As a rule, however, the
 *  input type is immediately converted to the output type before processing.
 *  This is because the input is not assumed to be a real value type and must be
 *  converted to signed, real values for the calculations.
 *
 * \par OUTPUTS
 *  The output is an itk::Image and is the solution of the pde.  The embedding of
 *  the interface may vary with the application, but the usual ITK convention is
 *  that it is the zero level set in the output image.
 *
 * \par IMPORTANT!
 *  Read the documentation for FiniteDifferenceImageFilter before attempting to
 *  use this filter.  The solver requires that you specify a
 *  FiniteDifferenceFunction to use for calculations.  This is set using the
 *  method SetDifferenceFunction in the parent class.
 *
 * \par REFERENCES
 * Sethian, J.A. Level Set Methods. Cambridge University Press. 1996.
 *
 * \ingroup ITKNarrowBand
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT NarrowBandImageFilterBase : public FiniteDifferenceImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(NarrowBandImageFilterBase);

  /** Standard class type aliases */
  using Self = NarrowBandImageFilterBase;
  using Superclass = FiniteDifferenceImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(NarrowBandImageFilterBase, ImageToImageFilter);

  /**Typedefs from the superclass */
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;
  using FiniteDifferenceFunctionType = typename Superclass::FiniteDifferenceFunctionType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** The pixel type of the output image will be used in computations.
   * Inherited from the superclass. */
  using PixelType = typename Superclass::PixelType;

  /** The value type of a time step.  Inherited from the superclass. */
  using TimeStepType = typename Superclass::TimeStepType;

  /** The index type for the output image. */
  using IndexType = typename OutputImageType::IndexType;

  /** The data type used in numerical computations.  Derived from the output
   *  image type. */
  using ValueType = typename OutputImageType::ValueType;

  /** This is the storage type for the nodes on the narrow band. */
  using BandNodeType = BandNode<IndexType, PixelType>;

  /** The list type for storing the narrow band. */
  using NarrowBandType = NarrowBand<BandNodeType>;
  using NarrowBandPointer = typename NarrowBandType::Pointer;
  using RegionType = typename NarrowBandType::RegionType;
  using NarrowBandIterator = typename NarrowBandType::Iterator;

  /** Set/Get IsoSurfaceValue to use in the input image */
  itkSetMacro(IsoSurfaceValue, ValueType);
  itkGetConstMacro(IsoSurfaceValue, ValueType);

  /** Root Mean Square Change between successive iterations */
  //  itkGetConstMacro( RMSChange, ValueType);

  /** This function is used to insert a pixel index into the narrow band  The
   *   entire narrow band can be constructed using this method.  Usually,
   *   however, the narrow band is initialized and reinitialized automatically
   *   by the subclass. */
  void
  InsertNarrowBandNode(const BandNodeType & node)
  {
    m_NarrowBand->PushBack(node); // add new node
    this->Modified();
  }

  void
  InsertNarrowBandNode(const IndexType & index)
  {
    BandNodeType tmpnode;

    tmpnode.m_Index = index;
    m_NarrowBand->PushBack(tmpnode);
    this->Modified();
  }

  void
  InsertNarrowBandNode(const IndexType & index, const PixelType & value, const signed char & nodestate)
  {
    BandNodeType tmpnode;

    tmpnode.m_Data = value;
    tmpnode.m_Index = index;
    tmpnode.m_NodeState = nodestate;

    m_NarrowBand->PushBack(tmpnode);
    this->Modified();
  }

  /** Set the narrow band total radius. The narrow band width will be
   * twice this value (positive and negative distance to the zero level
   * set). The default value is 3. */
  void
  SetNarrowBandTotalRadius(const float & val)
  {
    if (m_NarrowBand->GetTotalRadius() != val)
    {
      m_NarrowBand->SetTotalRadius(val);
      this->Modified();
    }
  }

  /** Get the narrow band total radius. */
  float
  GetNarrowBandTotalRadius() const
  {
    return m_NarrowBand->GetTotalRadius();
  }

  /** Set the narrow band inner radius. The inner radius is the safe
   * are where the level set can be computed. The default value is 1. */
  void
  SetNarrowBandInnerRadius(const float & val)
  {
    if (m_NarrowBand->GetInnerRadius() != val)
    {
      m_NarrowBand->SetInnerRadius(val);
      this->Modified();
    }
  }

  /** Get the narrow band inner radius. */
  float
  GetNarrowBandInnerRadius() const
  {
    return m_NarrowBand->GetInnerRadius();
  }

  /** This is the virtual method called by Initialize to set the band of operation.
   *  It is left to the subclasses to define this functionality.
   *  This function can make use of above InsertNarrowBandNode function to create a
   *  band.
   */
  virtual void
  CreateNarrowBand()
  {}

  virtual void
  SetNarrowBand(NarrowBandType * ptr)
  {
    if (m_NarrowBand != ptr)
    {
      m_NarrowBand = ptr;
      this->Modified();
    }
  }

  void
  CopyInputToOutput() override;

protected:
  NarrowBandImageFilterBase()
  {
    m_NarrowBand = NarrowBandType::New();
    m_NarrowBand->SetTotalRadius(4);
    m_NarrowBand->SetInnerRadius(2);
    m_ReinitializationFrequency = 6;
    m_IsoSurfaceValue = 0.0;
    m_Step = 0;
    m_Touched = false;
  }

  ~NarrowBandImageFilterBase() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  NarrowBandPointer m_NarrowBand;

  /** \struct ThreadRegionType
  The type of region used in multithreading.
  Defines a subregion of the narrowband. */
  struct ThreadRegionType
  {
    /** this is the actual first element */
    NarrowBandIterator first;

    /** this is one past the actual last element */
    NarrowBandIterator last;
  };

  /** A list of subregions of the narrowband which are passed to each thread
   * for parallel processing. */
  std::vector<RegionType> m_RegionList;

  /** This function returns a single region (of the narrow band list) for use
      in multi-threading */
  void
  GetSplitRegion(const size_t & i, ThreadRegionType & splitRegion);

  /** This function clears the existing narrow band, calls CreateNarrowBand to create
   *  a band, and calls the SplitRegions function of NarrowBand to pre-partition
   *  the band for multi-threading.
   */
  void
  Initialize() override;

  /** This method check the narrow band state each iteration and reinitialize
      the narrow band if it is appropriate calling CreateNarrowBand and
      SplitRegions to pre-partition the band for multi-threading.
  */
  void
  InitializeIteration() override;

  /** This method allows deallocation of data and further post processing
   */
  void
  PostProcessOutput() override;

  /* This function clears all pixels from the narrow band */
  void
  ClearNarrowBand();

  /** This is the default, high-level algorithm for calculating finite
   * difference solutions.  It calls virtual methods in its subclasses
   * to implement the major steps of the algorithm.
   *
   * This method is a thread implementation of the iterative scheme implemented
   * in itkFiniteDifferenceImageFilter::GenerateData. It relies on ThreadedApplyUpdate
   * and ThreadedCalculateChange to update the solution at every iteration. */
  void
  GenerateData() override;

  /* Variables to control reinitialization */
  IdentifierType m_ReinitializationFrequency;
  IdentifierType m_Step;

  bool m_Touched;

  std::vector<bool> m_TouchedForThread;

  ValueType m_IsoSurfaceValue;

private:
  /* This class does not use AllocateUpdateBuffer to allocate memory for its
   * narrow band. This is taken care of in SetNarrowBand, and InsertNarrowBandNode
   * functions. This function is here for compatibility with the
   * FiniteDifferenceSolver framework.
   */
  void
  AllocateUpdateBuffer() override
  {}

  /** This method applies changes from the m_NarrowBand to the output using
   * the ThreadedApplyUpdate() method and a multithreading mechanism.  "dt" is
   * the time step to use for the update of each pixel. */
  virtual void
  ThreadedApplyUpdate(const TimeStepType & dt, const ThreadRegionType & regionToProcess, ThreadIdType threadId);

  void
  ApplyUpdate(const TimeStepType &) override
  {}

  /** This method populates m_NarrowBand with changes for each pixel in the
   * output using the ThreadedCalculateChange() method and a multithreading
   * mechanism. Returns value is a time step to be used for the update. */
  virtual TimeStepType
  ThreadedCalculateChange(const ThreadRegionType & regionToProcess);

  TimeStepType
  CalculateChange() override
  {
    return 0;
  }
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNarrowBandImageFilterBase.hxx"
#endif

#endif
