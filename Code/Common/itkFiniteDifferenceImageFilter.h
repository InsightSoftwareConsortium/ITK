/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFiniteDifferenceImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFiniteDifferenceImageFilter_h_
#define __itkFiniteDifferenceImageFilter_h_

#include "itkImageToImageFilter.h"
#include "itkFiniteDifferenceFunction.h"

namespace itk {

/**
 * \class FiniteDifferenceImageFilter
 *
 * \par The Finite Difference Solver Hierarchy
 *
 * This is an overview of the finite difference solver (FDS) framework.  The
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
 * The following equation describes update \f$n+1\f$ at pixel \f$i\f$ on
 * discrete image \f$ u \f$ : 
 *
 * \par
 * \f$u_{\mathbf{i}}^{n+1}=u^n_{\mathbf{i}}+\Delta u^n_{\mathbf{i}}\Delta t\f$
 *
 * \par Component objects
 * The FDS hierarchy is comprised of two component object types, variations of
 * which are designed to be plugged together to create filters for different
 * applications.  At the process level are the ``solver'' objects, which are
 * subclasses of FiniteDifferenceImageFilter.  Solver objects are filters that
 * take image inputs and produce image outputs.  Solver objects require a
 * ``finite difference function'' object to perform the calculation at each
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
 * anisotropic diffusion functions into the DenseFiniteDifferenceImageFilter.
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
 * \par FiniteDifferenceImageFilter
 * This class defines the generic solver API at the top level of the FDS
 * framework. FiniteDifferenceImageFilter is an abstract class that implements
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
 * Note that there is no fixed container type for the buffer used to hold
 * the update \f$ \Delta \f$.  The container might be another image, or simply
 * a list of values.  AllocateUpdateBuffer is responsible for creating the
 * \f$ \Delta \f$ container.  CalculateChange populates this buffer and
 * ApplyUpdate adds the buffer values to the output image (solution).  The
 * boolean Halt() method returns a true value to stop iteration.
 *
 * \ingroup ImageFilter
 * \ingroup LevelSetSegmentation
 * \sa DenseFiniteDifferenceImageFilter */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT FiniteDifferenceImageFilter  
  : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef FiniteDifferenceImageFilter Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods) */
  itkTypeMacro(FiniteDifferenceImageFilter, ImageToImageFilter );
  
  /** Input and output image types. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;
  
  /** Dimensionality of input and output data is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      OutputImageType::ImageDimension);

  /** The pixel type of the output image will be used in computations. */
  typedef typename TOutputImage::PixelType PixelType;

  /** The value type of the time step.  This is distinct from PixelType
   * because PixelType may often be a vector value, while the TimeStep is
   * a scalar value. */
  typedef FiniteDifferenceFunction<TOutputImage> FiniteDifferenceFunctionType;
  typedef typename FiniteDifferenceFunctionType::TimeStepType TimeStepType;
    
  /** Get the number of elapsed iterations of the filter. */
  itkGetConstMacro(ElapsedIterations, unsigned int);

  /** This method returns a pointer to a FiniteDifferenceFunction object that
   * will be used by the filter to calculate updates at image pixels.
   * \returns A FiniteDifferenceObject pointer. */
  itkGetConstReferenceObjectMacro(DifferenceFunction,
                                  FiniteDifferenceFunctionType );

  /** This method sets the pointer to a FiniteDifferenceFunction object that
   * will be used by the filter to calculate updates at image pixels.
   * \returns A FiniteDifferenceObject pointer. */
  itkSetObjectMacro(DifferenceFunction, FiniteDifferenceFunctionType );

protected:
  FiniteDifferenceImageFilter() : m_ElapsedIterations(0) {m_DifferenceFunction = 0;}
  ~FiniteDifferenceImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

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

  /** A simple method to copy the data from the input to the output.  ( Supports
   * "read-only" image adaptors in the case where the input image type converts
   * to a different output image type. )  */
  virtual void CopyInputToOutput();
  
  /** This is the default, high-level algorithm for calculating finite
   * difference solutions.  It calls virtual methods in its subclasses
   * to implement the major steps of the algorithm. */
  virtual void GenerateData();

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
  virtual void GenerateInputRequestedRegion();
  
  /** This method returns true when the current iterative solution of the
   * equation has met the criteria to stop solving.  Defined by a subclass. */
  virtual bool Halt() = 0;
  /** This method is optionally defined by a subclass and is called before
   * the loop of iterations of calculate_change & upate. It does the global
   * initialization, i.e. in the SparseFieldLevelSetImageFilter, initialize 
   * the list of layers. 
   * */
  virtual void Initialize() { };

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
   * \param list The set of time changes compiled from all the threaded calls
   * to ThreadedGenerateData.
   * \param valid The set of flags indicating which of "list" elements are
   *  valid
   * \param size The size of "list" and "valid"
   *
   * The default is to return the minimum value in the list. */
  virtual TimeStepType ResolveTimeStep(const TimeStepType* list, const bool* valid,
                                    int size);

  /** Set the number of elapsed iterations of the filter. */
  itkSetMacro(ElapsedIterations, unsigned int);

  /** This method is called after the solution has been generated to allow
   * subclasses to apply some further processing to the output.*/
  virtual void PostProcessOutput() {}
  
private:
  FiniteDifferenceImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** A counter for keeping track of the number of elapsed 
      iterations during filtering. */
  unsigned int m_ElapsedIterations;

  /** The function that will be used in calculating updates for each pixel. */
  typename FiniteDifferenceFunctionType::Pointer m_DifferenceFunction;
};
  
}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFiniteDifferenceImageFilter.txx"
#endif

#endif

