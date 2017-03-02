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
#ifndef itkMultiphaseFiniteDifferenceImageFilter_h
#define itkMultiphaseFiniteDifferenceImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkFiniteDifferenceFunction.h"
#include "vnl/vnl_vector.h"
#include "itkImageRegionIterator.h"

#include "itkListSample.h"
#include "itkKdTreeGenerator.h"

namespace itk
{
/**
 * \class MultiphaseFiniteDifferenceImageFilter
 *
 * \par The Finite Difference Solver Hierarchy
 *
 * This is an alternate version of the ITK finite difference solver (FDS)
 * framework, supporting the solution of multiple functions, simultaneously.
 * The FDS framework is a set of classes for creating filters to solve partial
 * differential equations on images using an iterative, finite difference
 * update scheme.
 *
 * \par
 * The high-level algorithm implemented by the framework can be described by
 * the following pseudocode.
 *
 * \code
 *  WHILE NOT convergence:
 *     FOR ALL pixels i
 *      FOR ALL functions f
 *      min_time_step = min(min_time_step, calculate_change(f, i))
 *      FOR ALL functions f
 *          update(f, i, time_step)
 * \endcode
 *
 * \par
 * The following equation describes update \f$n+1\f$ at pixel \f$i\f$ on
 * discrete image \f$ u \f$ :
 *
 * \par
 * \f$u_{\mathbf{i}}^{n+1}=u^n_{\mathbf{i}}+\Delta u^n_{\mathbf{i}}\Delta t\f$
 *
 * \par Component objects
 * The FDS hierarchy is comprised of two component object types, variations of
 * which are designed to be plugged together to create filters for different
 * applications.  At the process level are the "solver" objects, which are
 * subclasses of MultiphaseFiniteDifferenceImageFilter.  Solver objects are filters that
 * take image inputs and produce image outputs.  Solver objects require a
 * "finite difference function" object to perform the calculation at each
 * image pixel during iteration.  These specialized function objects are
 * subclasses of FiniteDifferenceFunction. FiniteDifferenceFunctions take a
 * neighborhood of pixels as input (in the form of an
 * itk::NeighborhoodIterator) and produce a scalar valued result.
 *
 * \par
 * Filters for different applications are created by defining a function object
 * to handle the numerical calculations and choosing (or creating) a solver
 * object that reflects the requirements and constraints of the application.
 * For example, anisotropic diffusion filters are created by plugging
 * anisotropic diffusion functions into the DenseFiniteDifferenceImageFilter2.
 * The separation between function object and solver object allows us to
 * create, for example, sparse-field, dense-field, and narrow-band
 * implementations of a level-set surface evolution filter can all be
 * constructed by plugging the same function object into three different,
 * specialized solvers.
 *
 * \par Creating new filters in this hierarchy
 * The procedure for creating a filter within the FDS hierarchy is to identify
 * all the virtual methods that need to be defined for your particular
 * application.  In the simplest case, a filter needs only to instantiate a
 * specific function object and define some halting criteria.  For more
 * complicated applications, you may need to define a specialized type of
 * iteration scheme or updating procedure in a higher-level solver object.
 *
 * \par
 * Some simple examples are the specific subclasses of
 * AnisotropicDiffusionImageFilter.  The leaves of the anisotropic diffusion
 * filter tree only define the function object they use for their particular
 * flavor of diffusion.  See CurvatureAnisotropicDiffusionImageFilter and
 * GradientAnisotropicDiffusionImageFilter for details.
 *
 * \par FiniteDifferenceImageFilter2
 * This class defines the generic solver API at the top level of the FDS
 * framework. FiniteDifferenceImageFilter2 is an abstract class that implements
 * the generic, high-level algorithm (described above).
 *
 * \par Inputs and Outputs
 * This filter is an Image to Image filter.  Depending on the specific
 * subclass implementation, finite difference image filters may process a
 * variety of image types.  The input to the filter is the initial
 * value of \f$ u \f$ and the output of the filter is the solution to the
 * p.d.e.
 *
 * \par How to use this class
 * GenerateData() relies on several virtual methods that must be defined by a
 * subclass.  Specifically: \em AllocateUpdateBuffer \em ApplyUpdate
 * \em CalculateChange and \em Halt.  To create a finite difference solver,
 * implement a subclass to define these methods.
 *
 * \par
 * Note that there is no fixed container type for the buffer used to hold the
 * update \f$ \Delta \f$.  The container might be another image, or simply a
 * list of values.  AllocateUpdateBuffer is responsible for creating the \f$
 * \Delta \f$ container.  CalculateChange populates this buffer and ApplyUpdate
 * adds the buffer values to the output image (solution).  The boolean Halt()
 * (or ThreadedHalt) method returns a true value to stop iteration.
 *
 *
 * Based on the paper:
 *
 *        "An active contour model without edges"
 *         T. Chan and L. Vese.
 *         In Scale-Space Theories in Computer Vision, pages 141-151, 1999.
 *
 * \author Mosaliganti K., Smith B., Gelas A., Gouaillard A., Megason S.
 *
 *  This code was taken from the Insight Journal paper:
 *
 *      "Cell Tracking using Coupled Active Surfaces for Nuclei and Membranes"
 *      http://www.insight-journal.org/browse/publication/642
 *      https://hdl.handle.net/10380/3055
 *
 *  That is based on the papers:
 *
 *      "Level Set Segmentation: Active Contours without edge"
 *      http://www.insight-journal.org/browse/publication/322
 *      https://hdl.handle.net/1926/1532
 *
 *      and
 *
 *      "Level set segmentation using coupled active surfaces"
 *      http://www.insight-journal.org/browse/publication/323
 *      https://hdl.handle.net/1926/1533
 *
 *
 * \ingroup ImageFilter
 * \ingroup LevelSetSegmentation
 * \sa DenseFiniteDifferenceImageFilter2
 * \ingroup ITKReview
 */
template< typename TInputImage,
          typename TFeatureImage,
          typename TOutputImage,
          typename TFiniteDifferenceFunction = FiniteDifferenceFunction< TOutputImage >,
          typename TIdCell = unsigned int >
class ITK_TEMPLATE_EXPORT MultiphaseFiniteDifferenceImageFilter:
  public InPlaceImageFilter< TFeatureImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef MultiphaseFiniteDifferenceImageFilter             Self;
  typedef InPlaceImageFilter< TFeatureImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                              Pointer;
  typedef SmartPointer< const Self >                        ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(MultiphaseFiniteDifferenceImageFilter, InPlaceImageFilter);

  /** Dimensionality of input and output data is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** Input and output image types. */
  typedef TInputImage                               InputImageType;
  typedef typename InputImageType::Pointer          InputImagePointer;
  typedef typename InputImageType::PointType        InputPointType;
  typedef typename InputPointType::CoordRepType     InputCoordRepType;
  typedef typename InputImageType::IndexType        InputIndexType;
  typedef typename InputIndexType::IndexValueType   InputIndexValueType;
  typedef typename InputImageType::SizeType         InputSizeType;
  typedef typename InputSizeType::SizeValueType     InputSizeValueType;
  typedef typename InputImageType::RegionType       InputRegionType;
  typedef typename InputImageType::PixelType        InputPixelType;
  typedef typename InputImageType::SpacingType      InputSpacingType;
  typedef typename InputImageType::OffsetValueType  InputOffsetValueType;

  typedef TFeatureImage                          FeatureImageType;
  typedef typename FeatureImageType::Pointer     FeatureImagePointer;
  typedef typename FeatureImageType::RegionType  FeatureRegionType;
  typedef typename FeatureImageType::SizeType    FeatureSizeType;
  typedef typename FeatureImageType::SpacingType FeatureSpacingType;
  typedef typename FeatureImageType::PointType   FeaturePointType;
  typedef typename FeatureImageType::PixelType   FeaturePixelType;

  typedef TOutputImage                             OutputImageType;
  typedef typename OutputImageType::Pointer        OutputImagePointer;
  typedef typename OutputImageType::PixelType      OutputPixelType;
  typedef typename OutputImageType::RegionType     OutputRegionType;
  typedef typename OutputImageType::SizeType       OutputSizeType;
  typedef typename OutputImageType::SizeValueType  OutputSizeValueType;
  typedef typename OutputImageType::IndexType      OutputIndexType;
  typedef typename OutputImageType::IndexValueType OutputIndexValueType;

  typedef TIdCell                   IdCellType;
  typedef std::vector< IdCellType > VectorIdCellType;

  /** The value type of the time step.  This is distinct from PixelType
   * because PixelType may often be a vector value, while the TimeStep is
   * a scalar value. */
  typedef TFiniteDifferenceFunction                           FiniteDifferenceFunctionType;
  typedef typename FiniteDifferenceFunctionType::Pointer      FiniteDifferenceFunctionPointer;
  typedef typename FiniteDifferenceFunctionType::TimeStepType TimeStepType;
  typedef typename std::vector< TimeStepType >                TimeStepVectorType;
  typedef typename FiniteDifferenceFunctionType::RadiusType   RadiusType;

  typedef Vector< float, itkGetStaticConstMacro(ImageDimension) >
  CentroidVectorType;
  typedef Statistics::ListSample< CentroidVectorType > SampleType;
  typedef Statistics::KdTreeGenerator< SampleType >    KdTreeGeneratorType;
  typedef typename KdTreeGeneratorType::Pointer        KdTreeGeneratorPointer;
  typedef typename KdTreeGeneratorType::KdTreeType     KdTreeType;
  typedef typename KdTreeType::Pointer                 KdTreePointer;

  /** This method returns a pointer to a FiniteDifferenceFunction object that
   * will be used by the filter to calculate updates at image pixels.
   * \param functionIndex Index of difference function to return.
   * \returns A FiniteDifferenceObject pointer. */
  virtual const FiniteDifferenceFunctionPointer GetDifferenceFunction(
    const IdCellType & functionIndex) const
  {
    if ( functionIndex < m_FunctionCount )
      {
      return ( this->m_DifferenceFunctions[functionIndex] );
      }
    else
      {
      return ITK_NULLPTR;
      }
  }

  /** This method sets the pointer to a FiniteDifferenceFunction object that
   * will be used by the filter to calculate updates at image pixels.
   * \param functionIndex Index of difference function to set.
   * \param function Pointer to difference function to set. */
  virtual void SetDifferenceFunction(const IdCellType & functionIndex,
                                     FiniteDifferenceFunctionPointer function)
  {
    if ( functionIndex < m_FunctionCount )
      {
      this->m_DifferenceFunctions[functionIndex] = function;
      }
  }

  /** Set/Get the number of iterations that the filter will run. */
  itkSetMacro(NumberOfIterations, unsigned int);
  itkGetConstReferenceMacro(NumberOfIterations, unsigned int);

  /** Use the image spacing information in calculations. Use this option if you
   *  want derivatives in physical space. Default is UseImageSpacingOn. */
  itkSetMacro(UseImageSpacing, bool);
  itkBooleanMacro(UseImageSpacing);
  itkGetConstReferenceMacro(UseImageSpacing, bool);

  /** Set/Get the maximum error allowed in the solution.  This may not be
   * defined for all solvers and its meaning may change with the application. */
  itkSetMacro(MaximumRMSError, double);
  itkGetConstReferenceMacro(MaximumRMSError, double);

  /** Set/Get the root mean squared change of the previous iteration. May not
      be used by all solvers. */
  itkSetMacro(RMSChange, double);
  itkGetConstReferenceMacro(RMSChange, double);

  /** Set/Get the state of the filter. */
  itkSetMacro(InitializedState, bool);
  itkGetConstReferenceMacro(InitializedState, bool);
  itkBooleanMacro(InitializedState);

  /** Require the filter to be manually reinitialized (by calling
      SetInitializedStateOff() */
  itkSetMacro(ManualReinitialization, bool);
  itkGetConstReferenceMacro(ManualReinitialization, bool);
  itkBooleanMacro(ManualReinitialization);

  /** Set the number of elapsed iterations of the filter. */
  itkSetMacro(ElapsedIterations, unsigned int);

  /** Get the number of elapsed iterations of the filter. */
  itkGetConstReferenceMacro(ElapsedIterations, unsigned int);

  void SetLevelSet(const IdCellType & i, const InputImageType *levelSet)
  {
    m_LevelSet[i] = InputImageType::New();
    m_LevelSet[i]->SetRequestedRegion( levelSet->GetRequestedRegion() );
    m_LevelSet[i]->SetBufferedRegion( levelSet->GetBufferedRegion() );
    m_LevelSet[i]->SetLargestPossibleRegion( levelSet->GetLargestPossibleRegion() );
    m_LevelSet[i]->Allocate();
    m_LevelSet[i]->CopyInformation(levelSet);

    ImageRegionConstIterator< InputImageType > in ( levelSet, levelSet->GetBufferedRegion() );
    ImageRegionIterator< InputImageType >      cp ( m_LevelSet[i], levelSet->GetBufferedRegion() );

    in.GoToBegin();
    cp.GoToBegin();

    while ( !in.IsAtEnd() )
      {
      cp.Set( in.Get() );
      ++in;
      ++cp;
      }
  }

  InputImagePointer GetLevelSet(const IdCellType & i)
  {
    if ( i >= m_FunctionCount )
      {
      itkExceptionMacro("Request for level set #" << i
                                                  << " but there are only " << m_FunctionCount);
      }
    else
      {
      return m_LevelSet[i];
      }
  }

  void SetLookup(VectorIdCellType lookup)
  {
    this->m_Lookup = lookup;
  }

  void SetKdTree(KdTreeType *kdtree)
  {
    this->m_KdTree = kdtree;
  }

  void SetFunctionCount(const IdCellType & n)
  {
    m_FunctionCount = n;

    m_DifferenceFunctions.resize(m_FunctionCount, ITK_NULLPTR);

    RadiusType radius;
    radius.Fill(1);

    for ( unsigned int i = 0; i < this->m_FunctionCount; i++ )
      {
      this->m_DifferenceFunctions[i] = FiniteDifferenceFunctionType::New();
      this->m_DifferenceFunctions[i]->Initialize(radius);
      }

    // Initialize the images
    m_LevelSet.resize(m_FunctionCount, ITK_NULLPTR);

    // Initialize the lookup table
    this->m_Lookup.resize(m_FunctionCount);

    IdCellType k = 1;

    typedef typename std::vector< IdCellType >::iterator VectorIteratorType;

    VectorIteratorType it = this->m_Lookup.begin();

    while ( it != this->m_Lookup.end() )
      {
      *it = k;
      ++it;
      ++k;
      }
  }

protected:
  MultiphaseFiniteDifferenceImageFilter()
  {
    this->m_KdTree = ITK_NULLPTR;
    this->m_ElapsedIterations = 0;
    this->m_MaximumRMSError = itk::Math::eps;
    this->m_RMSChange = NumericTraits< double >::max();
    this->m_UseImageSpacing = true;
    this->m_ManualReinitialization = false;
    this->m_InitializedState = false;
    this->m_NumberOfIterations = NumericTraits< unsigned int >::max();
    this->m_FunctionCount = 0;
    this->InPlaceOff();
  }

  ~MultiphaseFiniteDifferenceImageFilter(){}

  IdCellType                       m_FunctionCount;
  std::vector< InputImagePointer > m_LevelSet;
  VectorIdCellType                 m_Lookup;
  KdTreePointer                    m_KdTree;

  unsigned int m_ElapsedIterations;
  double       m_MaximumRMSError;
  double       m_RMSChange;
  unsigned int m_NumberOfIterations;

  /** The function that will be used in calculating updates for each pixel. */
  std::vector< FiniteDifferenceFunctionPointer > m_DifferenceFunctions;

  /** Control whether derivatives use spacing of the input image in its
   * calculation. */
  bool m_UseImageSpacing;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** This method allocates a temporary update container in the subclass. */
  virtual void AllocateUpdateBuffer() = 0;

  /** This method is defined by a subclass to apply changes to the output
   * from an update buffer and a time step value "dt".
   * \param dt Time step value. */
  virtual void ApplyUpdate(TimeStepType dt) = 0;

  /** This method is defined by a subclass to populate an update buffer
   * with changes for the pixels in the output.  It returns a time
   * step value to be used for the update.
   * \returns A time step to use in updating the output with the changes
   * calculated from this method. */
  virtual TimeStepType CalculateChange() = 0;

  /** This method can be defined in subclasses as needed to copy the input
   * to the output. See DenseFiniteDifferenceImageFilter2 for an
   * implementation. */
  virtual void CopyInputToOutput() = 0;

  /** This is the default, high-level algorithm for calculating finite
   * difference solutions.  It calls virtual methods in its subclasses
   * to implement the major steps of the algorithm. */
  virtual void GenerateData() ITK_OVERRIDE;

  /** FiniteDifferenceImageFilter2 needs a larger input requested region than
   * the output requested region.  As such, we need to provide
   * an implementation for GenerateInputRequestedRegion() in order to inform
   * the pipeline execution model.
   *
   * \par
   * The filter will ask for a padded region to perform its neighborhood
   * calculations.  If no such region is available, the boundaries will be
   * handled as described in the FiniteDifferenceFunction defined by the
   * subclass.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** This method returns true when the current iterative solution of the
   * equation has met the criteria to stop solving.  Defined by a subclass. */
  virtual bool Halt();

  /** This method is similar to Halt(), and its default implementation in this
   * class is simply to call Halt(). However, this method takes as a parameter a
   * void pointer to the MultiThreader::ThreadInfoStruct structure. If you
   * override this method instead of overriding Halt, you will be able to get the
   * current thread ID and handle the Halt method accordingly. This is useful if
   * you are doing a lot of processing in Halt that you don't want parallelized.
   * Notice that ThreadedHalt is only called by the multithreaded filters, so you
   * still should implement Halt, just in case a non-threaded filter is used.
   */
  virtual bool ThreadedHalt( void *itkNotUsed(threadInfo) )
  {
    return this->Halt();
  }

  /** This method is optionally defined by a subclass and is called before
   * the loop of iterations of calculate_change & upate. It does the global
   * initialization, i.e. in the SparseFieldLevelSetImageFilter, initialize
   * the list of layers.
   * */
  virtual void Initialize() {}

  /** This method is optionally defined by a subclass and is called immediately
   * prior to each iterative CalculateChange-ApplyUpdate cycle.  It can be
   * used to set global variables needed for the next iteration (ie. average
   * gradient magnitude of the image in anisotropic diffusion functions), or
   * otherwise prepare for the next iteration.
   * */
  virtual void InitializeIteration()
  {
    for ( IdCellType i = 0; i < this->m_FunctionCount; i++ )
      {
      this->m_DifferenceFunctions[i]->InitializeIteration();
      }
  }

  /** Virtual method for resolving a single time step from a set of time steps
   * returned from processing threads.
   * \return Time step (dt) for the iteration update based on a list
   * of time steps generated from the threaded calculated change method (one
   * for each region processed).
   *
   * \param timeStepList The set of time changes compiled from all the threaded
   *        calls to ThreadedGenerateData.
   *
   * \param valid The set of flags indicating which of "list" elements are valid
   *
   * The default is to return the minimum value in the list. */
  inline TimeStepType ResolveTimeStep(const TimeStepVectorType & timeStepList,
                                      const std::vector< bool > & valid);

  /** This method is called after the solution has been generated to allow
   * subclasses to apply some further processing to the output. */
  virtual void PostProcessOutput() {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MultiphaseFiniteDifferenceImageFilter);

  /** Indicates whether the filter automatically resets to UNINITIALIZED state
      after completing, or whether filter must be manually reset */
  bool m_ManualReinitialization;

  /** State that the filter is in, i.e. UNINITIALIZED or INITIALIZED */
  bool m_InitializedState;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultiphaseFiniteDifferenceImageFilter.hxx"
#endif

#endif
