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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkBSplineInterpolateImageFunction_h
#define itkBSplineInterpolateImageFunction_h

#include <vector>

#include "itkInterpolateImageFunction.h"
#include "vnl/vnl_matrix.h"

#include "itkBSplineDecompositionImageFilter.h"
#include "itkConceptChecking.h"
#include "itkCovariantVector.h"

namespace itk
{
/** \class BSplineInterpolateImageFunction
 * \brief Evaluates the B-Spline interpolation of an image.  Spline order may be from 0 to 5.
 *
 * This class defines N-Dimension B-Spline transformation.
 * It is based on:
 *    [1] M. Unser,
 *       "Splines: A Perfect Fit for Signal and Image Processing,"
 *        IEEE Signal Processing Magazine, vol. 16, no. 6, pp. 22-38,
 *        November 1999.
 *    [2] M. Unser, A. Aldroubi and M. Eden,
 *        "B-Spline Signal Processing: Part I--Theory,"
 *        IEEE Transactions on Signal Processing, vol. 41, no. 2, pp. 821-832,
 *        February 1993.
 *    [3] M. Unser, A. Aldroubi and M. Eden,
 *        "B-Spline Signal Processing: Part II--Efficient Design and Applications,"
 *        IEEE Transactions on Signal Processing, vol. 41, no. 2, pp. 834-848,
 *        February 1993.
 * And code obtained from bigwww.epfl.ch by Philippe Thevenaz
 *
 * The B spline coefficients are calculated through the
 * BSplineDecompositionImageFilter
 *
 * Limitations:  Spline order must be between 0 and 5.
 *               Spline order must be set before setting the image.
 *               Uses mirror boundary conditions.
 *               Requires the same order of Spline for each dimension.
 *               Spline is determined in all dimensions, cannot selectively
 *                  pick dimension for calculating spline.
 *
 * \sa BSplineDecompositionImageFilter
 *
 * \ingroup ImageFunctions
 * \ingroup ITKImageFunction
 *
 * \wiki
 * \wikiexample{ImageProcessing/Upsampling,Upsampling an image}
 * \endwiki
 */
template<
  typename TImageType,
  typename TCoordRep = double,
  typename TCoefficientType = double >
class ITK_TEMPLATE_EXPORT BSplineInterpolateImageFunction:
  public InterpolateImageFunction< TImageType, TCoordRep >
{
public:
  /** Standard class typedefs. */
  typedef BSplineInterpolateImageFunction                   Self;
  typedef InterpolateImageFunction< TImageType, TCoordRep > Superclass;
  typedef SmartPointer< Self >                              Pointer;
  typedef SmartPointer< const Self >                        ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BSplineInterpolateImageFunction, InterpolateImageFunction);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputImageType typedef support. */
  typedef typename Superclass::InputImageType InputImageType;

  /** Dimension underlying input image. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Index typedef support. */
  typedef typename Superclass::IndexType IndexType;

  /** ContinuousIndex typedef support. */
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** PointType typedef support */
  typedef typename Superclass::PointType PointType;

  /** Iterator typedef support */
  typedef ImageLinearIteratorWithIndex< TImageType > Iterator;

  /** Internal Coefficient typedef support */
  typedef TCoefficientType CoefficientDataType;
  typedef Image< CoefficientDataType,
                 itkGetStaticConstMacro(ImageDimension) >
  CoefficientImageType;

  /** Define filter for calculating the BSpline coefficients */
  typedef BSplineDecompositionImageFilter< TImageType, CoefficientImageType > CoefficientFilter;
  typedef typename CoefficientFilter::Pointer                                 CoefficientFilterPointer;

  /** Derivative typedef support */
  typedef CovariantVector< OutputType,
                           itkGetStaticConstMacro(ImageDimension) >
  CovariantVectorType;

  /** Evaluate the function at a ContinuousIndex position.
   *
   * Returns the B-Spline interpolated image intensity at a
   * specified point position. No bounds checking is done.
   * The point is assume to lie within the image buffer.
   *
   * ImageFunction::IsInsideBuffer() can be used to check bounds before
   * calling the method. */
  virtual OutputType Evaluate(const PointType & point) const ITK_OVERRIDE
  {
    ContinuousIndexType index;

    this->GetInputImage()->TransformPhysicalPointToContinuousIndex(point,
                                                                   index);
    // No thread info passed in, so call method that doesn't need thread ID.
    return ( this->EvaluateAtContinuousIndex(index) );
  }

  virtual OutputType Evaluate(const PointType & point,
                              ThreadIdType threadId) const
  {
    ContinuousIndexType index;

    this->GetInputImage()->TransformPhysicalPointToContinuousIndex(point,
                                                                   index);
    return ( this->EvaluateAtContinuousIndex(index, threadId) );
  }

  virtual OutputType EvaluateAtContinuousIndex(const ContinuousIndexType &
                                               index) const ITK_OVERRIDE
  {
    // Don't know thread information, make evaluateIndex, weights on the stack.
    // Slower, but safer.
    vnl_matrix< long >   evaluateIndex( ImageDimension, ( m_SplineOrder + 1 ) );
    vnl_matrix< double > weights( ImageDimension, ( m_SplineOrder + 1 ) );

    // Pass evaluateIndex, weights by reference. They're only good as long
    // as this method is in scope.
    return this->EvaluateAtContinuousIndexInternal(index,
                                                   evaluateIndex,
                                                   weights);
  }

  virtual OutputType EvaluateAtContinuousIndex(const ContinuousIndexType &
                                               index,
                                               ThreadIdType threadId) const;

  CovariantVectorType EvaluateDerivative(const PointType & point) const
  {
    ContinuousIndexType index;

    this->GetInputImage()->TransformPhysicalPointToContinuousIndex(point,
                                                                   index);
    // No thread info passed in, so call method that doesn't need thread ID.
    return ( this->EvaluateDerivativeAtContinuousIndex(index) );
  }

  CovariantVectorType EvaluateDerivative(const PointType & point,
                                         ThreadIdType threadId) const
  {
    ContinuousIndexType index;

    this->GetInputImage()->TransformPhysicalPointToContinuousIndex(point,
                                                                   index);
    return ( this->EvaluateDerivativeAtContinuousIndex(index, threadId) );
  }

  CovariantVectorType EvaluateDerivativeAtContinuousIndex(
    const ContinuousIndexType & x) const
  {
    // Don't know thread information, make evaluateIndex, weights,
    // weightsDerivative
    // on the stack.
    // Slower, but safer.
    vnl_matrix< long >   evaluateIndex( ImageDimension, ( m_SplineOrder + 1 ) );
    vnl_matrix< double > weights( ImageDimension, ( m_SplineOrder + 1 ) );
    vnl_matrix< double > weightsDerivative( ImageDimension, ( m_SplineOrder + 1 ) );

    // Pass evaluateIndex, weights, weightsDerivative by reference. They're only
    // good
    // as long as this method is in scope.
    return this->EvaluateDerivativeAtContinuousIndexInternal(x,
                                                             evaluateIndex,
                                                             weights,
                                                             weightsDerivative);
  }

  CovariantVectorType EvaluateDerivativeAtContinuousIndex(
    const ContinuousIndexType & x,
    ThreadIdType threadId) const;

  void EvaluateValueAndDerivative(const PointType & point,
                                  OutputType & value,
                                  CovariantVectorType & deriv) const
  {
    ContinuousIndexType index;

    this->GetInputImage()->TransformPhysicalPointToContinuousIndex(point,
                                                                   index);

    // No thread info passed in, so call method that doesn't need thread ID.
    this->EvaluateValueAndDerivativeAtContinuousIndex(index,
                                                      value,
                                                      deriv);
  }

  void EvaluateValueAndDerivative(const PointType & point,
                                  OutputType & value,
                                  CovariantVectorType & deriv,
                                  ThreadIdType threadId) const
  {
    ContinuousIndexType index;

    this->GetInputImage()->TransformPhysicalPointToContinuousIndex(point,
                                                                   index);
    this->EvaluateValueAndDerivativeAtContinuousIndex(index,
                                                      value,
                                                      deriv,
                                                      threadId);
  }

  void EvaluateValueAndDerivativeAtContinuousIndex(
    const ContinuousIndexType & x,
    OutputType & value,
    CovariantVectorType & deriv
    ) const
  {
    // Don't know thread information, make evaluateIndex, weights,
    // weightsDerivative
    // on the stack.
    // Slower, but safer.
    vnl_matrix< long >   evaluateIndex( ImageDimension, ( m_SplineOrder + 1 ) );
    vnl_matrix< double > weights( ImageDimension, ( m_SplineOrder + 1 ) );
    vnl_matrix< double > weightsDerivative( ImageDimension, ( m_SplineOrder + 1 ) );

    // Pass evaluateIndex, weights, weightsDerivative by reference. They're only
    // good
    // as long as this method is in scope.
    this->EvaluateValueAndDerivativeAtContinuousIndexInternal(x,
                                                              value,
                                                              deriv,
                                                              evaluateIndex,
                                                              weights,
                                                              weightsDerivative);
  }

  void EvaluateValueAndDerivativeAtContinuousIndex(
    const ContinuousIndexType & x,
    OutputType & value,
    CovariantVectorType & deriv,
    ThreadIdType threadId) const;

  /** Get/Sets the Spline Order, supports 0th - 5th order splines. The default
   *  is a 3rd order spline. */
  void SetSplineOrder(unsigned int SplineOrder);

  itkGetConstMacro(SplineOrder, int);

  void SetNumberOfThreads(ThreadIdType numThreads);

  itkGetConstMacro(NumberOfThreads, ThreadIdType);

  /** Set the input image.  This must be set by the user. */
  virtual void SetInputImage(const TImageType *inputData) ITK_OVERRIDE;

  /** The UseImageDirection flag determines whether image derivatives are
   * computed with respect to the image grid or with respect to the physical
   * space. When this flag is ON the derivatives are computed with respect to
   * the coordinate system of physical space. The difference is whether we take
   * into account the image Direction or not. The flag ON will take into
   * account the image direction and will result in an extra matrix
   * multiplication compared to the amount of computation performed when the
   * flag is OFF.
   * The default value of this flag is On.
   */
  itkSetMacro(UseImageDirection, bool);
  itkGetConstMacro(UseImageDirection, bool);
  itkBooleanMacro(UseImageDirection);

protected:

  /** The following methods take working space (evaluateIndex, weights, weightsDerivative)
   *  that is managed by the caller. If threadId is known, the working variables are looked
   *  up in the thread indexed arrays. If threadId is not known, working variables are made
   *  on the stack and passed to these methods. The stack allocation should be ok since
   *  these methods do not store the working variables, i.e. they are not expected to
   *  be available beyond the scope of the function call.
   *
   *  This was done to allow for two types of re-entrancy. The first is when a threaded
   *  filter, e.g. InterpolateImagePointsFilter calls EvaluateAtContinuousIndex from multiple
   *  threads without passing a threadId. So, EvaluateAtContinuousIndex must be thread safe.
   *  This is handled with the stack-based allocation of the working space.
   *
   *  The second form of re-entrancy involves methods that call EvaluateAtContinuousIndex
   *  from multiple threads, but pass a threadId. In this case, we can gain a little efficiency
   *  (hopefully) by looking up pre-allocated working space in arrays that are indexed by thread.
   *  The efficiency gain is likely dependent on the size of the working variables, which are
   *  in-turn dependent on the dimensionality of the image and the order of the spline.
   */
  virtual OutputType EvaluateAtContinuousIndexInternal(const ContinuousIndexType & index,
                                                       vnl_matrix< long > & evaluateIndex,
                                                       vnl_matrix< double > & weights) const;

  virtual void EvaluateValueAndDerivativeAtContinuousIndexInternal(const ContinuousIndexType & x,
                                                                   OutputType & value,
                                                                   CovariantVectorType & derivativeValue,
                                                                   vnl_matrix< long > & evaluateIndex,
                                                                   vnl_matrix< double > & weights,
                                                                   vnl_matrix< double > & weightsDerivative
                                                                   ) const;

  virtual CovariantVectorType EvaluateDerivativeAtContinuousIndexInternal(const ContinuousIndexType & x,
                                                                          vnl_matrix< long > & evaluateIndex,
                                                                          vnl_matrix< double > & weights,
                                                                          vnl_matrix< double > & weightsDerivative
                                                                          ) const;

  BSplineInterpolateImageFunction();
  ~BSplineInterpolateImageFunction() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  // These are needed by the smoothing spline routine.
  // temp storage for processing of Coefficients
  std::vector< CoefficientDataType >    m_Scratch;
  // Image size
  typename TImageType::SizeType m_DataLength;
  // User specified spline order (3rd or cubic is the default)
  unsigned int m_SplineOrder;

  // Spline coefficients
  typename CoefficientImageType::ConstPointer m_Coefficients;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BSplineInterpolateImageFunction);

  /** Determines the weights for interpolation of the value x */
  void SetInterpolationWeights(const ContinuousIndexType & x,
                               const vnl_matrix< long > & EvaluateIndex,
                               vnl_matrix< double > & weights,
                               unsigned int splineOrder) const;

  /** Determines the weights for the derivative portion of the value x */
  void SetDerivativeWeights(const ContinuousIndexType & x,
                            const vnl_matrix< long > & EvaluateIndex,
                            vnl_matrix< double > & weights,
                            unsigned int splineOrder) const;

  /** Precomputation for converting the 1D index of the interpolation
   *  neighborhood to an N-dimensional index. */
  void GeneratePointsToIndex();

  /** Determines the indices to use give the splines region of support */
  void DetermineRegionOfSupport(vnl_matrix< long > & evaluateIndex,
                                const ContinuousIndexType & x,
                                unsigned int splineOrder) const;

  /** Set the indices in evaluateIndex at the boundaries based on mirror
    * boundary conditions. */
  void ApplyMirrorBoundaryConditions(vnl_matrix< long > & evaluateIndex,
                                     unsigned int splineOrder) const;

  Iterator m_CIterator;                                    // Iterator for
                                                           // traversing spline
                                                           // coefficients.
  unsigned long m_MaxNumberInterpolationPoints;            // number of
                                                           // neighborhood
                                                           // points used for
                                                           // interpolation
  std::vector< IndexType > m_PointsToIndex;                // Preallocation of
                                                           // interpolation
                                                           // neighborhood
                                                           // indices

  CoefficientFilterPointer m_CoefficientFilter;

  // flag to take or not the image direction into account when computing the
  // derivatives.
  bool m_UseImageDirection;

  ThreadIdType          m_NumberOfThreads;
  vnl_matrix< long > *  m_ThreadedEvaluateIndex;
  vnl_matrix< double > *m_ThreadedWeights;
  vnl_matrix< double > *m_ThreadedWeightsDerivative;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBSplineInterpolateImageFunction.hxx"
#endif

#endif
