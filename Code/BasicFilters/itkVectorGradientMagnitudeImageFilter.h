/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorGradientMagnitudeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorGradientMagnitudeImageFilter_h
#define __itkVectorGradientMagnitudeImageFilter_h

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodIterator.h"
#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkVector.h"
#include "vnl/vnl_matrix.h"
#include "vnl/algo/vnl_symmetric_eigensystem.h"
#include "vnl/vnl_math.h"

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
 * UseImageSpacingOff (all weights are 1.0).  The parameter UseImageSpacing can
 * be set directly with the method SetUseImageSpacing(bool).
 * 
 * Weights can be applied to the derivatives directly using the
 * SetDerivativeWeights method.  Note that if UseImageSpacing is set to TRUE
 * (ON), then these weights will be overridden by weights derived from the
 * image spacing when the filter is updated.  The argument to this method is a
 * C array of TRealValue type.
 *
 * If using the second calculation mode (SetUsePrincipleComponentsOff), weights
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
 */
template < typename TInputImage,
           typename TRealType = float,
           typename TOutputImage = Image< TRealType,
                            ::itk::GetImageDimension<TInputImage>::ImageDimension >
>
class ITK_EXPORT VectorGradientMagnitudeImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef VectorGradientMagnitudeImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(VectorGradientMagnitudeImageFilter, ImageToImageFilter);
  
  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TInputImage::PixelType  InputPixelType;

  /** Image typedef support */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;
  typedef typename InputImageType::Pointer  InputImagePointer;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  
  /** The dimensionality of the input and output images. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  
  /** Length of the vector pixel type of the input image. */
  itkStaticConstMacro(VectorDimension, unsigned int,
                      InputPixelType::Dimension);

  /** Define the data type and the vector of data type used in calculations. */
  typedef TRealType RealType;
  typedef Vector<TRealType, ::itk::GetVectorDimension<InputPixelType>::VectorDimension> RealVectorType;
  typedef Image<RealVectorType, ::itk::GetImageDimension<TInputImage>::ImageDimension>  RealVectorImageType;
  

  /** Type of the iterator that will be used to move through the image.  Also
      the type which will be passed to the evaluate function */
  typedef ConstNeighborhoodIterator<RealVectorImageType> ConstNeighborhoodIteratorType;
  
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
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

  /** Set the derivative weights according to the spacing of the input image
      (1/spacing). Use this option if you want to calculate the gradient in the
      space in which the data was acquired.*/
  void SetUseImageSpacingOn()
  { this->SetUseImageSpacing(true); }

  /** Reset the derivative weights to ignore image spacing.  Use this option if
      you want to calculate the gradient in the image space.  Default is
      ImageSpacingOff. */
  void SetUseImageSpacingOff()
  { this->SetUseImageSpacing(false); }

  /** Set/Get whether or not the filter will use the spacing of the input
      image in its calculations */
  void SetUseImageSpacing(bool);                         
  itkGetMacro(UseImageSpacing, bool);

  /** Directly Set/Get the array of weights used in the gradient calculations.
      Note that calling UseImageSpacingOn will clobber these values.*/
  void SetDerivativeWeights(TRealType data[]);
  itkGetVectorMacro(DerivativeWeights, const TRealType, ImageDimension);

  /** Set/Get the array of weightings for the different components of the
      vector.  Default values are 1.0.  When UsePrincipleComponents = true, then
      these weights are not used */
  itkSetVectorMacro(ComponentWeights, TRealType, ImageDimension);
  itkGetVectorMacro(ComponentWeights, const TRealType, ImageDimension);
  
  /** Set/Get principle components calculation mode.  When this is set to TRUE/ON,
      the gradient calculation will involve a priniciple component analysis of
      the partial derivatives of the color components.  When this value is set
      to FALSE/OFF, the calculation is done as a square root of weighted sum of the
      derivatives squared.  Default is UsePrincipleComponents = true. */
  itkSetMacro(UsePrincipleComponents, bool);
  itkGetMacro(UsePrincipleComponents, bool);
  void SetUsePrincipleComponentsOn()
  {
    this->SetUsePrincipleComponents(true);
  }
  void SetUsePrincipleComponentsOff()
  {
    this->SetUsePrincipleComponents(false);
  }
  
protected:
  VectorGradientMagnitudeImageFilter();
  virtual ~VectorGradientMagnitudeImageFilter() {}

 /** Do any necessary casting/copying of the input data.  Input pixel types
     whose value types are not real number types must be cast to real number
     types.*/
  void BeforeThreadedGenerateData ();

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
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

  void PrintSelf(std::ostream& os, Indent indent) const;

  // Function is defined here because the templating confuses gcc 2.96 when defined
  // in .txx file. jc 1/29/03
  TRealType NonPCEvaluateAtNeighborhood(const ConstNeighborhoodIteratorType &it) const
  {
    unsigned i, j;
    TRealType dx, sum, accum;
    accum = NumericTraits<TRealType>::Zero;
    for (i = 0; i < ImageDimension; ++i)
      {
        sum = NumericTraits<TRealType>::Zero;
        for (j = 0; j < VectorDimension; ++j)
          {
            dx = m_DerivativeWeights[i]
              * (0.5 * it.GetNext(i)[j] - 0.5 * it.GetPrevious(i)[j]);
            sum += m_ComponentWeights[j] * (dx * dx);
          }
        accum += sum;
      }
    return ::vnl_math_sqrt(accum);
  }

  // Function is defined here because the templating confuses gcc 2.96 when defined
  // in .txx file.  jc 1/29/03
  TRealType EvaluateAtNeighborhood(const ConstNeighborhoodIteratorType &it) const
  {
    unsigned int i, j;
    vnl_matrix<TRealType> g(ImageDimension, ImageDimension);
    vnl_vector_fixed<TRealType, VectorDimension> d_phi_du[ImageDimension];
    
    // Calculate the directional derivatives for each vector component using
    // central differences.
    for (i = 0; i < ImageDimension; i++)
      {
        for (j = 0; j < VectorDimension; j++)
          {  d_phi_du[i][j] = m_DerivativeWeights[i]
               * (it.GetNext(i)[j] * 0.5 - it.GetPrevious(i)[j] * 0.5); }
      }
    
    // Calculate the symmetric metric tensor g
    for (i = 0; i < ImageDimension; i++)
      {
        for (j = i; j < ImageDimension; j++)
          {
            g[j][i] = g[i][j] = dot_product(d_phi_du[i], d_phi_du[j]);
          }
      }
    
    // Find the eigenvalues of g
    vnl_symmetric_eigensystem<TRealType> E(g);
    
    // Return the difference in length between the first two principle axes.
    // Note that other edge strength metrics may be appropriate here instead..
    return ( E.get_eigenvalue(ImageDimension - 1)
             - E.get_eigenvalue(ImageDimension - 2) ); 
  }
  
  /** The weights used to scale derivatives during processing */
  TRealType m_DerivativeWeights[ImageDimension];

  /** If UsePrincipleComponents = off, then these weights are used to scale
      vector component values when they are combined to produce  a scalar.  */
  TRealType m_ComponentWeights[VectorDimension];
  
private:
  bool m_UseImageSpacing;
  bool m_UsePrincipleComponents;
  int m_RequestedNumberOfThreads;

  typename ImageBase<ImageDimension>::ConstPointer m_RealValuedInputImage;
  
  VectorGradientMagnitudeImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  typename ConstNeighborhoodIteratorType::RadiusType m_NeighborhoodRadius;  
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorGradientMagnitudeImageFilter.txx"
#endif

#endif
