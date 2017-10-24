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
#ifndef itkVectorGradientMagnitudeImageFilter_h
#define itkVectorGradientMagnitudeImageFilter_h

#include "itkNeighborhoodIterator.h"
#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkVector.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/algo/vnl_symmetric_eigensystem.h"
#include "itkMath.h"

namespace itk
{
/** \class VectorGradientMagnitudeImageFilter
 *
 * \brief Computes a scalar, gradient magnitude image from a multiple channel
 * (pixels are vectors) input.
 *
 * \par Overview
 * This filter has two calculation modes.  The first (default) mode calculates
 * gradient magnitude as the difference between the largest two eigenvalues in a
 * principle component analysis of the partial derivatives [1].  The
 * gradient is then based on the direction of maximal change, and is a
 * characterization of how "elongated" the point-spread of the analysis is
 * found to be.
 *
 * The second, more heuristic, calculation mode finds gradient magnitude as the
 * square-root of the sum of the * individual weighted vector component
 * derivative sums squared. That is, * \f$ \mathbf{magnitude} = \left(
 * \sum_{i=0}^n \sum_{j=0}^m \frac{\delta * \phi_j}{\delta \mathbf{x}_{i}}^2
 * \right)^{\frac{1}{2}} \f$, where \f$\phi_j\f$ * is the \f$j^{\mathbf{th}}\f$
 * channel of vector image \f$\phi\f$ of dimension \f$n\f$. * Weighting terms
 * are applied to each vector component.
 *
 * The second mode is computationally much faster than the first and has the
 * advantage that it is automatically multi-threaded (some vnl functions used
 * in the first mode are not thread-safe).  The first mode, however, tends to
 * give intuitively better results with less (or no) parameter tuning.
 *
 * \par Template Parameters (Input and Output)
 * This filter has one required template parameter which defines the input
 * image type.  The pixel type of the input image is assumed to be a vector
 * (e.g., itk::Vector, itk::RGBPixel, itk::FixedArray).  The scalar type of the
 * vector components must be castable to floating point.  Instantiating with an
 * image of RGBPixel<unsigned short>, for example, is allowed, but the filter
 * will convert it to an image of Vector<float,3> for processing.
 *
 * The second template parameter, TRealType, can be optionally specified to define the
 * scalar numerical type used in calculations.  This is the component type of
 * the output image, which will be of itk::Vector<TRealType, N>, where N is the
 * number of channels in the multiple component input image.  The default type
 * of TRealType is float.  For extra precision, you may safely change this
 * parameter to double.
 *
 * The third template parameter is the output image type.  The third parameter
 * will be automatically constructed from the first and second parameters, so
 * it is not necessary (or advisable) to set this parameter explicitly.  Given
 * an M-channel input image with dimensionality N, and a numerical type
 * specified as TRealType, the output image will be of type
 * itk::Image<itk::Vector<TRealType, M>, N>.
 *
 * \par Filter Parameters
 * The methods Set/GetUsePrincipleComponents and
 * SetUsePrincipleComponentsOn/Off determine controls the calculation mode that
 * is used.
 *
 * The method SetUseImageSpacingOn will cause derivatives in the image to be
 * scaled (inversely) with the pixel size of the input image, effectively
 * taking derivatives in world coordinates (versus isotropic image
 * space). SetUseImageSpacingOff turns this functionality off.  Default is
 * UseImageSpacingOn.  The parameter UseImageSpacing can be set
 * directly with the method SetUseImageSpacing(bool).
 *
 * Weights can be applied to the derivatives directly using the
 * SetDerivativeWeights method.  Note that if UseImageSpacing is set to TRUE
 * (ON), then these weights will be overridden by weights derived from the
 * image spacing when the filter is updated.  The argument to this method is a
 * C array of TRealValue type.
 *
 * Weights
 * can be applied to each vector component of the image when the component
 * derivative values are summed during computation.  Specify these weights
 * using the SetComponentWeights method.  The argument to this method is a C
 * array of TRealValue type.

 * \par Constraints
 * The filter requires an image with at least two dimensions and a vector
 * length of at least 2.  The theory supports extension to scalar images, but
 * the implementation of the itk vector classes do not
 *
 * The template parameter TRealType must be floating point (float or double) or
 * a user-defined "real" numerical type with arithmetic operations defined
 * sufficient to compute derivatives.
 *
 * \par Performance
 * This filter will automatically multithread if run with
 * SetUsePrincipleComponents=Off or on 3D data in UsePrincipleComponents=On
 * mode.   Unfortunately the ND eigen solver used is not thread safe (a special
 * 3D solver is), so it cannot multithread for data other than 3D in
 * UsePrincipleComponents=On mode.
 *
 * \par References
 *
 * [1] G. Sapiro and D. Ringach, "Anisotropic Diffusion of Multivalued Images
 * with Application to Color Filtering," IEEE Transactions on Image Processing,
 * Vol 5, No. 11 pp. 1582-1586, 1996

 * \ingroup GradientFilters
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * \ingroup ITKImageGradient
 */
template< typename TInputImage,
          typename TRealType = float,
          typename TOutputImage = Image< TRealType,
                                         TInputImage::ImageDimension >
          >
class ITK_TEMPLATE_EXPORT VectorGradientMagnitudeImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef VectorGradientMagnitudeImageFilter              Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(VectorGradientMagnitudeImageFilter, ImageToImageFilter);

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TInputImage::PixelType  InputPixelType;

  /** Image typedef support */
  typedef TInputImage                       InputImageType;
  typedef TOutputImage                      OutputImageType;
  typedef typename InputImageType::Pointer  InputImagePointer;
  typedef typename OutputImageType::Pointer OutputImagePointer;

  /** The dimensionality of the input and output images. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Length of the vector pixel type of the input image. */
  itkStaticConstMacro(VectorDimension, unsigned int,
                      InputPixelType::Dimension);

  /** Define the data type and the vector of data type used in calculations. */
  typedef TRealType                                                                         RealType;
  typedef Vector< TRealType, InputPixelType::Dimension >
                      RealVectorType;
  typedef Image< RealVectorType, TInputImage::ImageDimension >
                      RealVectorImageType;

  /** Type of the iterator that will be used to move through the image.  Also
      the type which will be passed to the evaluate function */
  typedef ConstNeighborhoodIterator< RealVectorImageType >   ConstNeighborhoodIteratorType;
  typedef typename ConstNeighborhoodIteratorType::RadiusType RadiusType;

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** VectorGradientMagnitudeImageFilter needs a larger input requested
   * region than the output requested region (larger by the kernel
   * size to calculate derivatives).  As such,
   * VectorGradientMagnitudeImageFilter needs to provide an implementation
   * for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Set the derivative weights according to the spacing of the input image
      (1/spacing). Use this option if you want to calculate the gradient in the
      space in which the data was acquired. Default is
      ImageSpacingOn. */
  void SetUseImageSpacingOn()
  { this->SetUseImageSpacing(true); }

  /** Reset the derivative weights to ignore image spacing.  Use this option if
      you want to calculate the gradient in the image space.  Default is
      ImageSpacingOn. */
  void SetUseImageSpacingOff()
  { this->SetUseImageSpacing(false); }

  /** Set/Get whether or not the filter will use the spacing of the input
      image in its calculations */
  void SetUseImageSpacing(bool);

  itkGetConstMacro(UseImageSpacing, bool);

  typedef FixedArray< TRealType, VectorDimension > WeightsType;

  /** Directly Set/Get the array of weights used in the gradient calculations.
      Note that calling UseImageSpacingOn will clobber these values. */
  itkSetMacro(DerivativeWeights, WeightsType);
  itkGetConstReferenceMacro(DerivativeWeights, WeightsType);

  /** Set/Get the array of weightings for the different components of the
      vector.  Default values are 1.0. */
  itkSetMacro(ComponentWeights, WeightsType);
  itkGetConstReferenceMacro(ComponentWeights, WeightsType);

#ifndef ITK_LEGACY_REMOVE
  /** Get the neighborhood radius - always 1 */
  virtual const RadiusType GetNeighborhoodRadius() const
  {
    RadiusType r1;
    r1.Fill(1);
    return r1;
  }

  /** Set the neighborhood radius - ignored */
  virtual void SetNeighborhoodRadius(const RadiusType) {}
#endif

  /** Set/Get principle components calculation mode.  When this is set to TRUE/ON,
      the gradient calculation will involve a priniciple component analysis of
      the partial derivatives of the color components.  When this value is set
      to FALSE/OFF, the calculation is done as a square root of weighted sum of the
      derivatives squared.  Default is UsePrincipleComponents = true. */
  itkSetMacro(UsePrincipleComponents, bool);
  itkGetConstMacro(UsePrincipleComponents, bool);
  void SetUsePrincipleComponentsOn()
  {
    this->SetUsePrincipleComponents(true);
  }

  void SetUsePrincipleComponentsOff()
  {
    this->SetUsePrincipleComponents(false);
  }

  /** A specialized solver for finding the roots of a cubic polynomial.
   *  Necessary to multi-thread the 3D case */
  static int CubicSolver(double *, double *);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename InputPixelType::ValueType > ) );
  itkConceptMacro( RealTypeHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< RealType > ) );
  // End concept checking
#endif

protected:
  VectorGradientMagnitudeImageFilter();
  virtual ~VectorGradientMagnitudeImageFilter() ITK_OVERRIDE {}

  /** Do any necessary casting/copying of the input data.  Input pixel types
     whose value types are not real number types must be cast to real number
     types. */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  /** VectorGradientMagnitudeImageFilter can be implemented as a
   * multithreaded filter.  Therefore, this implementation provides a
   * ThreadedGenerateData() routine which is called for each
   * processing thread. The output image data is allocated
   * automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to
   * the portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  typedef typename InputImageType::Superclass ImageBaseType;

  /** Get access to the input image casted as real pixel values */
  itkGetConstObjectMacro(RealValuedInputImage, ImageBaseType);

  TRealType NonPCEvaluateAtNeighborhood(const ConstNeighborhoodIteratorType & it) const
  {
    unsigned  i, j;
    TRealType dx, sum, accum;

    accum = NumericTraits< TRealType >::ZeroValue();
    for ( i = 0; i < ImageDimension; ++i )
      {
      sum = NumericTraits< TRealType >::ZeroValue();
      for ( j = 0; j < VectorDimension; ++j )
        {
        dx =  m_DerivativeWeights[i] * m_SqrtComponentWeights[j]
             * 0.5 * ( it.GetNext(i)[j] - it.GetPrevious(i)[j] );
        sum += dx * dx;
        }
      accum += sum;
      }
    return std::sqrt(accum);
  }

  TRealType EvaluateAtNeighborhood3D(const ConstNeighborhoodIteratorType & it) const
  {
    // WARNING:  ONLY CALL THIS METHOD WHEN PROCESSING A 3D IMAGE
    unsigned int i, j;
    double       Lambda[3];
    double       CharEqn[3];
    double       ans;

    vnl_matrix< TRealType > g(ImageDimension, ImageDimension);
    vnl_vector_fixed< TRealType, VectorDimension >
    d_phi_du[TInputImage::ImageDimension];

    // Calculate the directional derivatives for each vector component using
    // central differences.
    for ( i = 0; i < ImageDimension; i++ )
      {
      for ( j = 0; j < VectorDimension; j++ )
        {
        d_phi_du[i][j] = m_DerivativeWeights[i] * m_SqrtComponentWeights[j]
                         * 0.5 * ( it.GetNext(i)[j] - it.GetPrevious(i)[j] );
        }
      }

    // Calculate the symmetric metric tensor g
    for ( i = 0; i < ImageDimension; i++ )
      {
      for ( j = i; j < ImageDimension; j++ )
        {
        g[j][i] = g[i][j] = dot_product(d_phi_du[i], d_phi_du[j]);
        }
      }

    // Find the coefficients of the characteristic equation det(g - lambda I)=0
    //    CharEqn[3] = 1.0;

    CharEqn[2] = -( g[0][0] + g[1][1] + g[2][2] );

    CharEqn[1] = ( g[0][0] * g[1][1] + g[0][0] * g[2][2] + g[1][1] * g[2][2] )
                 - ( g[0][1] * g[1][0] + g[0][2] * g[2][0] + g[1][2] * g[2][1] );

    CharEqn[0] = g[0][0] * ( g[1][2] * g[2][1] -  g[1][1] * g[2][2]  )
                 + g[1][0] * (  g[2][2] * g[0][1] - g[0][2] * g[2][1] )
                 + g[2][0] * ( g[1][1] * g[0][2] - g[0][1] * g[1][2] );

    // Find the eigenvalues of g
    int numberOfDistinctRoots =  this->CubicSolver(CharEqn, Lambda);

    // Define gradient magnitude as the difference between two largest
    // eigenvalues.  Other definitions may be appropriate here as well.
    if ( numberOfDistinctRoots == 3 ) // By far the most common case
      {
      if ( Lambda[0] > Lambda[1] )
        {
        if ( Lambda[1] > Lambda[2] )  // Most common, guaranteed?
          {
          ans = Lambda[0] - Lambda[1];
          }
        else
          {
          if ( Lambda[0] > Lambda[2] )
            {
            ans = Lambda[0] - Lambda[2];
            }
          else
            {
            ans = Lambda[2] - Lambda[0];
            }
          }
        }
      else
        {
        if ( Lambda[0] > Lambda[2] )
          {
          ans = Lambda[1] - Lambda[0];
          }
        else
          {
          if ( Lambda[1] > Lambda[2] )
            {
            ans = Lambda[1] - Lambda[2];
            }
          else
            {
            ans = Lambda[2] - Lambda[1];
            }
          }
        }
      }
    else if ( numberOfDistinctRoots == 2 )
      {
      if ( Lambda[0] > Lambda[1] )
        {
        ans = Lambda[0] - Lambda[1];
        }
      else
        {
        ans = Lambda[1] - Lambda[0];
        }
      }
    else if ( numberOfDistinctRoots == 1 )
      {
      ans = 0.0;
      }
    else
      {
      itkExceptionMacro(<< "Undefined condition. Cubic root solver returned "
                        << numberOfDistinctRoots << " distinct roots.");
      }

    return ans;
  }

  // Function is defined here because the templating confuses gcc 2.96 when
  // defined
  // in .hxx file.  jc 1/29/03
  TRealType EvaluateAtNeighborhood(const ConstNeighborhoodIteratorType & it) const
  {
    unsigned int i, j;

    vnl_matrix< TRealType > g(ImageDimension, ImageDimension);
    vnl_vector_fixed< TRealType, VectorDimension >
    d_phi_du[TInputImage::ImageDimension];

    // Calculate the directional derivatives for each vector component using
    // central differences.
    for ( i = 0; i < ImageDimension; i++ )
      {
      for ( j = 0; j < VectorDimension; j++ )
        {
        d_phi_du[i][j] = m_DerivativeWeights[i] * m_SqrtComponentWeights[j]
                         * 0.5 * ( it.GetNext(i)[j] - it.GetPrevious(i)[j] );
        }
      }

    // Calculate the symmetric metric tensor g
    for ( i = 0; i < ImageDimension; i++ )
      {
      for ( j = i; j < ImageDimension; j++ )
        {
        g[j][i] = g[i][j] = dot_product(d_phi_du[i], d_phi_du[j]);
        }
      }

    // Find the eigenvalues of g
    vnl_symmetric_eigensystem< TRealType > E(g);

    // Return the difference in length between the first two principle axes.
    // Note that other edge strength metrics may be appropriate here instead..
    return ( E.get_eigenvalue(ImageDimension - 1) - E.get_eigenvalue(ImageDimension - 2) );
  }

  /** The weights used to scale derivatives during processing */
  WeightsType m_DerivativeWeights;

  /** These weights are used to scale
      vector component values when they are combined to produce  a scalar.  The
      square root */
  WeightsType m_ComponentWeights;
  WeightsType m_SqrtComponentWeights;

private:
  bool m_UseImageSpacing;
  bool m_UsePrincipleComponents;

  ThreadIdType m_RequestedNumberOfThreads;

  typename ImageBaseType::ConstPointer m_RealValuedInputImage;

  ITK_DISALLOW_COPY_AND_ASSIGN(VectorGradientMagnitudeImageFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorGradientMagnitudeImageFilter.hxx"
#endif

#endif
