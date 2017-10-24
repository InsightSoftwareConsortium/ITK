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
#ifndef itkDisplacementFieldJacobianDeterminantFilter_h
#define itkDisplacementFieldJacobianDeterminantFilter_h

#include "itkNeighborhoodIterator.h"
#include "itkImageToImageFilter.h"
#include "itkVector.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_det.h"

namespace itk
{
/** \class DisplacementFieldJacobianDeterminantFilter
 *
 * \brief Computes a scalar image from a vector image (e.g., deformation field)
 * input, where each output scalar at each pixel is the Jacobian determinant
 * of the vector field at that location.  This calculation is correct in the
 * case where the vector image is a "displacement" from the current location.
 * The computation for the jacobian determinant is: det[ dT/dx ] = det[ I + du/dx ].
 *
 * \par Overview
 * This filter is based on itkVectorGradientMagnitudeImageFilter and supports
 * the m_DerivativeWeights weights for partial derivatives.
 *
 * Note that the determinant of a zero vector field is also zero, whereas
 * the Jacobian determinant of the corresponding identity warp transformation
 * is 1.0.  In order to compute the effective deformation Jacobian determinant
 * 1.0 must be added to the diagonal elements of Jacobian prior to taking the derivative.
 * i.e. det([ (1.0+dx/dx)  dx/dy dx/dz ; dy/dx (1.0+dy/dy) dy/dz; dz/dx dz/dy (1.0+dz/dz) ])
 *
 * \par Template Parameters (Input and Output)
 * This filter has one required template parameter which defines the input
 * image type.  The pixel type of the input image is assumed to be a vector
 * (e.g., itk::Vector, itk::RGBPixel, itk::FixedArray).  The scalar type of the
 * vector components must be castable to floating point.  Instantiating with an
 * image of RGBPixel<unsigned short>, for example, is allowed, but the filter
 * will convert it to an image of Vector<float,3> for processing.
 *
 * The second template parameter, TRealType, can be optionally specified to
 * define the scalar numerical type used in calculations.  This is the
 * component type of the output image, which will be of
 * itk::Vector<TRealType, N>, where N is the number of channels in the multiple
 * component input image.  The default type of TRealType is float.  For extra
 * precision, you may safely change this parameter to double.
 *
 * The third template parameter is the output image type.  The third parameter
 * will be automatically constructed from the first and second parameters, so
 * it is not necessary (or advisable) to set this parameter explicitly.  Given
 * an M-channel input image with dimensionality N, and a numerical type
 * specified as TRealType, the output image will be of type
 * itk::Image<TRealType, N>.
 *
 * \par Filter Parameters
 * The method SetUseImageSpacingOn will cause derivatives in the image to be
 * scaled (inversely) with the pixel size of the input image, effectively
 * taking derivatives in world coordinates (versus isotropic image
 * space). SetUseImageSpacingOff turns this functionality off.  Default is
 * UseImageSpacingOn.  The parameter UseImageSpacing can
 * be set directly with the method SetUseImageSpacing(bool).
 *
 * Weights can be applied to the derivatives directly using the
 * SetDerivativeWeights method.  Note that if UseImageSpacing is set to TRUE
 * (ON), then these weights will be overridden by weights derived from the
 * image spacing when the filter is updated.  The argument to this method is a
 * C array of TRealValue type.
 *
 * \par Constraints
 * We use vnl_det for determinent computation, which only supports square
 * matrices. So the vector dimension of the input image values must be equal
 * to the image dimensions, which is trivially true for a deformation field
 * that maps an n-dimensional space onto itself.

 * Currently, dimensions up to and including 4 are supported. This
 * limitation comes from the presence of vnl_det() functions for matrices of
 * dimension up to 4x4.
 *
 * The template parameter TRealType must be floating point (float or double) or
 * a user-defined "real" numerical type with arithmetic operations defined
 * sufficient to compute derivatives.
 *
 * \ingroup GradientFilters
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 *
 * \note This class was adapted by
 * \author Hans J. Johnson, The University of Iowa
 * from code provided by
 * \author Tom Vercauteren, INRIA & Mauna Kea Technologies
 * \author Torsten Rohlfing, Neuroscience Program, SRI International.
 * \ingroup ITKDisplacementField
 */
template< typename TInputImage,
          typename TRealType = float,
          typename TOutputImage = Image< TRealType,
                                         TInputImage::ImageDimension >
          >
class ITK_TEMPLATE_EXPORT DisplacementFieldJacobianDeterminantFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef DisplacementFieldJacobianDeterminantFilter      Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(DisplacementFieldJacobianDeterminantFilter, ImageToImageFilter);

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
  typedef TRealType RealType;
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

  /** DisplacementFieldJacobianDeterminantFilter needs a larger input requested
   * region than the output requested region (larger by the kernel
   * size to calculate derivatives).  As such,
   * DisplacementFieldJacobianDeterminantFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Set the derivative weights according to the spacing of the input image
      (1/spacing). Use this option if you want to calculate the Jacobian
      determinant in the space in which the data was acquired. Default
      is ImageSpacingOn. */
  void SetUseImageSpacingOn()
  { this->SetUseImageSpacing(true); }

  /** Reset the derivative weights to ignore image spacing.  Use this option if
      you want to calculate the Jacobian determinant in the image space.
      Default is ImageSpacingOn. */
  void SetUseImageSpacingOff()
  { this->SetUseImageSpacing(false); }

  /** Set/Get whether or not the filter will use the spacing of the input
      image in its calculations */
  void SetUseImageSpacing(bool);

  itkGetConstMacro(UseImageSpacing, bool);

  typedef FixedArray< TRealType, ImageDimension > WeightsType;

  /** Directly Set/Get the array of weights used in the gradient calculations.
      Note that calling UseImageSpacingOn will clobber these values. */
  void SetDerivativeWeights(const WeightsType &);
  itkGetConstReferenceMacro(DerivativeWeights, WeightsType);

protected:
  DisplacementFieldJacobianDeterminantFilter();
  virtual ~DisplacementFieldJacobianDeterminantFilter() ITK_OVERRIDE {}

  /** Do any necessary casting/copying of the input data.  Input pixel types
     whose value types are not real number types must be cast to real number
     types. */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  /** DisplacementFieldJacobianDeterminantFilter can be implemented as a
   * multithreaded filter (we're only using vnl_det(), which is trivially
   * thread safe).  Therefore, this implementation provides a
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

  /** Get/Set the neighborhood radius used for gradient computation */
  itkGetConstReferenceMacro(NeighborhoodRadius, RadiusType);
  itkSetMacro(NeighborhoodRadius, RadiusType);

  virtual TRealType EvaluateAtNeighborhood(const ConstNeighborhoodIteratorType & it) const;

  /** The weights used to scale partial derivatives during processing */
  WeightsType m_DerivativeWeights;
  /** Pre-compute 0.5*m_DerivativeWeights since that is the only thing used in
    the computations. */
  WeightsType m_HalfDerivativeWeights;

private:
  bool m_UseImageSpacing;

  ThreadIdType m_RequestedNumberOfThreads;

  typename ImageBaseType::ConstPointer m_RealValuedInputImage;

  ITK_DISALLOW_COPY_AND_ASSIGN(DisplacementFieldJacobianDeterminantFilter);

  RadiusType m_NeighborhoodRadius;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDisplacementFieldJacobianDeterminantFilter.hxx"
#endif

#endif
