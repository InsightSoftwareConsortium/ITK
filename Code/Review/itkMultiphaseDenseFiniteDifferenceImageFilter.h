/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiphaseDenseFiniteDifferenceImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMultiphaseDenseFiniteDifferenceImageFilter_h
#define __itkMultiphaseDenseFiniteDifferenceImageFilter_h

#include "itkMultiphaseFiniteDifferenceImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkNeighborhoodAlgorithm.h"

#include <list>


namespace itk {

/**
 * \class MultiphaseDenseFiniteDifferenceImageFilter
 *
 * This filter implements a layer of the finite difference solver hierarchy that
 * performs ``dense'' iteration, ie. iteration over all pixels in the input and
 * output at each change calculation and update step. Dense iteration is in
 * contrast to a ``sparse'' iteration over a subset of the pixels.  See
 * documentation for FiniteDifferenceImageFilter for an overview of the
 * iterative finite difference algorithm:
 *
 * \par
 * \f$u_{\mathbf{i}}^{n+1}=u^n_{\mathbf{i}}+\Delta u^n_{\mathbf{i}}\Delta t\f$
 *
 * \par
 * The generic code for performing iterations and updates at each time
 * step is inherited from the parent class.  This class defines an update
 * buffer for \f$ \Delta \f$ and the methods CalculateChange() and
 * ApplyUpdate(). These methods are designed to automatically thread their
 * execution.  \f$ \Delta \f$ is defined as an image of identical size and type
 * as the output image.
 *
 * \par
 * As we descend through each layer in the hierarchy, we know more and more
 * about the specific application of our filter.  At this level, we
 * have committed to iteration over each pixel in an image. We take advantage
 * of that knowledge to multithread the iteration and update methods.
 *
 * \par Inputs and Outputs
 * This is an image to image filter.  The specific types of the images are not
 * fixed at this level in the hierarchy.
 *
 * \par How to use this class
 * This filter is only one layer in a branch the finite difference solver
 * hierarchy.  It does not define the function used in the CalculateChange() and
 * it does not define the stopping criteria (Halt method).  To use this class,
 * subclass it to a specific instance that supplies a function and Halt()
 * method.
 *
 * Based on the paper:
 *
 *        "An active contour model without edges"
 *         T. Chan and L. Vese. 
 *         In Scale-Space Theories in Computer Vision, pages 141–151, 1999.
 * 
 * \author Mosaliganti K., Smith B., Gelas A., Gouaillard A., Megason S.
 *
 *  This code was taken from the Insight Journal paper:
 *
 *      "Cell Tracking using Coupled Active Surfaces for Nuclei and Membranes"
 *      http://www.insight-journal.org/browse/publication/642
 *      http://hdl.handle.net/10380/3055
 *
 *  That is based on the papers:
 *
 *      "Level Set Segmentation: Active Contours without edge"
 *      http://www.insight-journal.org/browse/publication/322
 *      http://hdl.handle.net/1926/1532
 *
 *      and
 *
 *      "Level set segmentation using coupled active surfaces"
 *      http://www.insight-journal.org/browse/publication/323
 *      http://hdl.handle.net/1926/1533
 *
 *
 * \ingroup ImageFilters
 * \sa FiniteDifferenceImageFilter */
template < class TInputImage, class TOutputImage, class TFunction >
class ITK_EXPORT MultiphaseDenseFiniteDifferenceImageFilter
  : public MultiphaseFiniteDifferenceImageFilter<TInputImage,
  TOutputImage,
  TFunction >
{
public:
  /** Standard class typedefs */
  typedef MultiphaseDenseFiniteDifferenceImageFilter        Self;
  typedef MultiphaseFiniteDifferenceImageFilter< 
    TInputImage, TOutputImage, TFunction >                  Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro( MultiphaseDenseFiniteDifferenceImageFilter, ImageToImageFilter );

  /** Convenient typedefs */
  typedef typename Superclass::InputImageType           InputImageType;
  typedef typename Superclass::InputSizeType            InputSizeType;
  typedef typename Superclass::InputImagePointer        InputImagePointer;
  typedef typename Superclass::InputRegionType          InputRegionType;
  typedef typename Superclass::InputSpacingType         InputSpacingType;
  typedef typename Superclass::InputPointType           InputPointType;

  typedef typename Superclass::OutputImageType          OutputImageType;
  typedef typename Superclass::OutputImagePointer       OutputImagePointer;
  typedef typename Superclass::OutputRegionType         OutputRegionType;
  typedef typename Superclass::OutputSizeType           OutputSizeType;
  typedef typename Superclass::OutputSizeValueType      SizeValueType;
  typedef typename Superclass::OutputIndexType          OutputIndexType;
  typedef typename Superclass::OutputIndexValueType     OutputIndexValueType;
  typedef typename OutputImageType::PixelType           OutputPixelType;
  typedef OutputImageType                               UpdateBufferType;

  typedef ImageRegionIterator<UpdateBufferType> UpdateIteratorType;

  typedef typename Superclass::FiniteDifferenceFunctionType
    FiniteDifferenceFunctionType;

  /** Dimensionality of input and output data is assumed to be the same.
   * It is inherited from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,Superclass::ImageDimension);

  /** The pixel type of the output image will be used in computations.
   * Inherited from the superclass. */
  typedef typename Superclass::PixelType PixelType;

  /** The value type of a time step.  Inherited from the superclass. */
  typedef typename Superclass::TimeStepType TimeStepType;

  /** The container type for the update buffer. */


  typedef typename FiniteDifferenceFunctionType::NeighborhoodType NeighborhoodIteratorType;

  typedef NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< OutputImageType >  FaceCalculatorType;
  typedef typename FaceCalculatorType::FaceListType                               FaceListType;

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(OutputTimesDoubleCheck,
    (Concept::MultiplyOperator<PixelType, double>));
  itkConceptMacro(OutputAdditiveOperatorsCheck,
    (Concept::AdditiveOperators<PixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck,
    (Concept::Convertible<typename TInputImage::PixelType, PixelType>));
  /** End concept checking */
#endif

protected:
  MultiphaseDenseFiniteDifferenceImageFilter()
    {
    this->m_ReinitializeCounter = 1;  // FIXME: Should this be a bool ?
    this->m_UpdateCounter = 0;        // FIXME: Should this be a bool ?
    }

  ~MultiphaseDenseFiniteDifferenceImageFilter()
    {
    delete [] this->m_UpdateBuffers;
    }

  void SetFunctionCount( unsigned int n )
    {
    this->Superclass::SetFunctionCount( n );

    this->m_UpdateBuffers = new UpdateBufferPointer[n];

    for( unsigned int i = 0; i < this->m_FunctionCount; i++ )
      {
      this->m_UpdateBuffers[i] = UpdateBufferType::New();
      }
    }

  /** A simple method to copy the data from the input to the output.  ( Supports
   * "read-only" image adaptors in the case where the input image type converts
   * to a different output image type. )  */
  virtual void CopyInputToOutput();
  virtual void PostProcessOutput();

  /** This method applies changes from the m_UpdateBuffer to the output using
   * the ThreadedApplyUpdate() method and a multithreading mechanism.  "dt" is
   * the time step to use for the update of each pixel. */
  virtual void ApplyUpdate(TimeStepType dt);

  unsigned int m_ReinitializeCounter;  // FIXME: Should this be a boolean ?
  unsigned int m_UpdateCounter;        // FIXME: Should this be a boolean ?

private:
  MultiphaseDenseFiniteDifferenceImageFilter(const Self&);
  void operator=(const Self&); //purposely not implemented

  /** The type of region used for multithreading */
  typedef typename UpdateBufferType::RegionType ThreadRegionType;

  /** This method allocates storage in m_UpdateBuffer.  It is called from
   * Superclass::GenerateData(). */
  virtual void AllocateUpdateBuffer();

  /** This method populates an update buffer with changes for each pixel in the
   * output using the ThreadedCalculateChange() method and a multithreading
   * mechanism. Returns value is a time step to be used for the update. */
  virtual TimeStepType CalculateChange();

  /** The buffer that holds the updates for an iteration of the algorithm. */
  typedef typename UpdateBufferType::Pointer UpdateBufferPointer;

  typedef BinaryThresholdImageFilter< OutputImageType, OutputImageType >          ThresholdFilterType;
  typedef SignedMaurerDistanceMapImageFilter< OutputImageType, OutputImageType >  MaurerType;

  UpdateBufferPointer * m_UpdateBuffers;
};


}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultiphaseDenseFiniteDifferenceImageFilter.txx"
#endif

#endif
