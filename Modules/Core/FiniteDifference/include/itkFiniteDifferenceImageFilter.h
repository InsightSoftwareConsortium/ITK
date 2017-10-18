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
#ifndef itkFiniteDifferenceImageFilter_h
#define itkFiniteDifferenceImageFilter_h

#include "itkInPlaceImageFilter.h"
#include "itkFiniteDifferenceFunction.h"

namespace itk
{
/**
 * \class FiniteDifferenceImageFilter
 *
 * \par The Finite Difference Solver Hierarchy
 *
 * \par
 * This is an overview of the Finite Difference Solver (FDS) framework. The
 * FDS framework is a set of classes for creating filters to solve partial
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
 *        time_step = calculate_change(i)
 *        update(i, time_step)
 * \endcode
 *
 * \par
 * The following equation describes update \f$ n+1 \f$ at pixel \f$ i \f$ on
 * discrete image \f$ u \f$ :
 *
 * \f[ u_{\mathbf{i}}^{n+1}=u^n_{\mathbf{i}}+\Delta u^n_{\mathbf{i}}\Delta t \f]
 *
 * \par Component objects
 * The FDS hierarchy is comprised of two component object types, variations of
 * which are designed to be plugged together to create filters for different
 * applications.  At the process level are the "solver" objects, which are
 * subclasses of FiniteDifferenceImageFilter. Solver objects are filters that
 * take image inputs and produce image outputs.  Solver objects require a
 * FiniteDifferenceFunction object to perform the calculation at each
 * image pixel during iteration. These specialized function objects are
 * subclasses of FiniteDifferenceFunction. FiniteDifferenceFunction take a
 * neighborhood of pixels as input (in the form of an
 * itk::NeighborhoodIterator) and produce a scalar valued result.
 *
 * \par
 * Filters for different applications are created by defining a function object
 * to handle the numerical calculations and choosing (or creating) a solver
 * object that reflects the requirements and constraints of the application.
 *
 * For example, anisotropic diffusion filters are created by plugging
 * anisotropic diffusion functions into the DenseFiniteDifferenceImageFilter.
 *
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
 * specific function object and define some halting criteria. For more
 * complicated applications, you may need to define a specialized type of
 * iteration scheme or updating procedure in a higher-level solver object.
 *
 * \par
 * Some simple examples are the specific subclasses of
 * AnisotropicDiffusionImageFilter. The leaves of the anisotropic diffusion
 * filter tree only define the function object they use for their particular
 * flavor of diffusion. See CurvatureAnisotropicDiffusionImageFilter and
 * GradientAnisotropicDiffusionImageFilter for details.
 *
 * \par FiniteDifferenceImageFilter
 * This class defines the generic solver API at the top level of the FDS
 * framework. FiniteDifferenceImageFilter is an abstract class that implements
 * the generic, high-level algorithm (described above).
 *
 * \par Inputs and Outputs
 * This filter is an ImageToImage filter. Depending on the specific
 * subclass implementation, finite difference image filters may process a
 * variety of image types. The input to the filter is the initial
 * value of \f$ u \f$ and the output of the filter is the solution to the
 * P.D.E.
 *
 * \par How to use this class
 * GenerateData() relies on several virtual methods that must be defined by a
 * subclass.
 * Specifically:
 * \li \em AllocateUpdateBuffer()
 * \li \em ApplyUpdate()
 * \li \em CalculateChange()
 * \li \em Halt().
 * To create a finite difference solver, implement a subclass to define these methods.
 *
 * \par
 * Note that there is no fixed container type for the buffer used to hold
 * the update \f$ \Delta \f$. The container might be another image, or simply
 * a list of values.  AllocateUpdateBuffer is responsible for creating the
 * \f$ \Delta \f$ container.  CalculateChange populates this buffer and
 * ApplyUpdate adds the buffer values to the output image (solution). The
 * boolean Halt() (or ThreadedHalt()) method returns a true value to stop iteration.
 *
 * \ingroup ImageFilter
 * \ingroup LevelSetSegmentation
 *
 * \sa DenseFiniteDifferenceImageFilter
 * \ingroup ITKFiniteDifference
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT FiniteDifferenceImageFilter:
  public InPlaceImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef FiniteDifferenceImageFilter                     Self;
  typedef InPlaceImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(FiniteDifferenceImageFilter, InPlaceImageFilter);

  /** Input and output image types. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** Dimensionality of input and output data is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int, OutputImageType::ImageDimension);

  /** The pixel type of the output image will be used in computations. */
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TInputImage::PixelType  InputPixelType;
  typedef OutputPixelType                  PixelType;

  /** Extract value type in case the pixel is of vector type */
  typedef typename NumericTraits< OutputPixelType >::ValueType OutputPixelValueType;
  typedef typename NumericTraits< InputPixelType >::ValueType  InputPixelValueType;

  /** The value type of the time step.  This is distinct from PixelType
   * because PixelType may often be a vector value, while the TimeStep is
   * a scalar value. */
  typedef FiniteDifferenceFunction< TOutputImage >            FiniteDifferenceFunctionType;
  typedef typename FiniteDifferenceFunctionType::TimeStepType TimeStepType;

  typedef typename FiniteDifferenceFunctionType::RadiusType             RadiusType;
  typedef typename FiniteDifferenceFunctionType::NeighborhoodScalesType NeighborhoodScalesType;

  /** Get the number of elapsed iterations of the filter. */
  itkGetConstReferenceMacro(ElapsedIterations, IdentifierType);

  /** This method returns a pointer to a FiniteDifferenceFunction object that
   * will be used by the filter to calculate updates at image pixels.
   * \returns A FiniteDifferenceObject pointer. */
  itkGetConstReferenceObjectMacro(DifferenceFunction,
                                  FiniteDifferenceFunctionType);

  /** This method sets the pointer to a FiniteDifferenceFunction object that
   * will be used by the filter to calculate updates at image pixels.
   * \returns A FiniteDifferenceObject pointer. */
  itkSetObjectMacro(DifferenceFunction, FiniteDifferenceFunctionType);

  /** Set/Get the number of iterations that the filter will run. */
  itkSetMacro(NumberOfIterations, IdentifierType);
  itkGetConstReferenceMacro(NumberOfIterations, IdentifierType);

  /** Use the image spacing information in calculations. Use this option if you
   *  want derivatives in physical space. Default is UseImageSpacingOn. */
  itkSetMacro(UseImageSpacing, bool);
  itkBooleanMacro(UseImageSpacing);
  itkGetConstReferenceMacro(UseImageSpacing, bool);

  /** Set/Get the maximum error allowed in the solution.  This may not be
      defined for all solvers and its meaning may change with the application. */
  itkSetMacro(MaximumRMSError, double);
  itkGetConstReferenceMacro(MaximumRMSError, double);

  /** Set/Get the root mean squared change of the previous iteration. May not
      be used by all solvers. */
  itkSetMacro(RMSChange, double);
  itkGetConstReferenceMacro(RMSChange, double);

  /** Require the filter to be manually reinitialized (by calling
      SetStateToUninitialized() */
  itkSetMacro(ManualReinitialization, bool);
  itkGetConstReferenceMacro(ManualReinitialization, bool);
  itkBooleanMacro(ManualReinitialization);

  itkSetMacro( IsInitialized, bool );
  itkGetMacro( IsInitialized, bool );

  void SetStateToUninitialized() { this->SetIsInitialized( false ); }
  void SetStateToInitialized() { this->SetIsInitialized( true ); }

#ifdef ITK_USE_STRICT_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputPixelIsFloatingPointCheck,
                   ( Concept::IsFloatingPoint< OutputPixelValueType > ) );
  // End concept checking
#endif

protected:

  FiniteDifferenceImageFilter();
  virtual ~FiniteDifferenceImageFilter() ITK_OVERRIDE;

  /** State that the filter is in, i.e. UNINITIALIZED or INITIALIZED */
  bool m_IsInitialized;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** This method allocates a temporary update container in the subclass. */
  virtual void AllocateUpdateBuffer() = 0;

  /** This method is defined by a subclass to apply changes to the output
   * from an update buffer and a time step value "dt".
   * \param dt Time step value. */
  virtual void ApplyUpdate(const TimeStepType& dt) = 0;

  /** This method is defined by a subclass to populate an update buffer
   * with changes for the pixels in the output.  It returns a time
   * step value to be used for the update.
   * \returns A time step to use in updating the output with the changes
   * calculated from this method. */
  virtual TimeStepType CalculateChange() = 0;

  /** This method can be defined in subclasses as needed to copy the input
   * to the output. See DenseFiniteDifferenceImageFilter for an
   * implementation. */
  virtual void CopyInputToOutput() = 0;

  /** This is the default, high-level algorithm for calculating finite
   * difference solutions.  It calls virtual methods in its subclasses
   * to implement the major steps of the algorithm. */
  virtual void GenerateData() ITK_OVERRIDE;

  /** FiniteDifferenceImageFilter needs a larger input requested region than
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
   * class is simply to call Halt(). However, this method takes as a parameter
   * a void pointer to the MultiThreader::ThreadInfoStruct structure. If you
   * override this method instead of overriding Halt, you will be able to get
   * the current thread ID and handle the Halt method accordingly. This is useful
   * if you are doing a lot of processing in Halt that you don't want parallelized.
   * Notice that ThreadedHalt is only called by the multithreaded filters, so you
   * still should implement Halt, just in case a non-threaded filter is used.
   */
  virtual bool ThreadedHalt( void *itkNotUsed(threadInfo) ) { return this->Halt(); }

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
  { m_DifferenceFunction->InitializeIteration(); }

  /** Virtual method for resolving a single time step from a set of time steps
   * returned from processing threads.
   * \return Time step (dt) for the iteration update based on a list
   * of time steps generated from the threaded calculated change method (one
   * for each region processed).
   *
   * \param timeStepList The set of time changes compiled from all the threaded calls
   * to ThreadedGenerateData.
   * \param valid The set of flags indicating which of "list" elements are
   *  valid
   *
   * The default is to return the minimum value in the list. */
  virtual TimeStepType ResolveTimeStep(const std::vector< TimeStepType >& timeStepList,
                                       const std::vector< bool >& valid ) const;

  /** Set the number of elapsed iterations of the filter. */
  itkSetMacro(ElapsedIterations, IdentifierType);

  /** This method is called after the solution has been generated to allow
   * subclasses to apply some further processing to the output. */
  virtual void PostProcessOutput() {}

  /** The maximum number of iterations this filter will run */
  IdentifierType m_NumberOfIterations;

  /** A counter for keeping track of the number of elapsed
      iterations during filtering. */
  IdentifierType m_ElapsedIterations;

  /** Indicates whether the filter automatically resets to UNINITIALIZED state
      after completing, or whether filter must be manually reset */
  bool m_ManualReinitialization;

  double m_RMSChange;
  double m_MaximumRMSError;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FiniteDifferenceImageFilter);

  /** Initialize the values of the Function coefficients. This function will
   * also take care of checking whether the image spacing should be taken into
   * account or not. */
  void InitializeFunctionCoefficients();

  /** Control whether derivatives use spacing of the input image in
      its calculation. */
  bool m_UseImageSpacing;

  /** The function that will be used in calculating updates for each pixel. */
  typename FiniteDifferenceFunctionType::Pointer m_DifferenceFunction;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFiniteDifferenceImageFilter.hxx"
#endif

#endif
