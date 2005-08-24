/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformationFieldJacobianDeterminantFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDeformationFieldJacobianDeterminantFilter_h
#define __itkDeformationFieldJacobianDeterminantFilter_h

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodIterator.h"
#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkVector.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_det.h"

namespace itk
{
/** \class DeformationFieldJacobianDeterminantFilter
 *
 * \brief Computes a scalar image from a vector image (e.g., deformation field)
 * input, where each output scalar at each pixel is the Jacobian determinant
 * of the vector field at that location.
 * 
 * \par Overview
 * This filter is based on itkVectorGradientMagnitudeImageFilter and supports
 * the m_DerivativeWeights weights for partial derivatives.
 *
 * Note that the determinant of a zero vector field is also zero, whereas
 * the Jacobian determinant of the corresponding identity warp transformation
 * should be 1.0. In general, in order to obtain the effective deformation
 * Jacobian determinant, 1.0 must be added to each Jacobian value in the 
 * output.
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
 * UseImageSpacingOff (all weights are 1.0).  The parameter UseImageSpacing can
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
 *
 * \note This class was developed with funding from:
 *
 * "CNS Deficits: Interaction of Age and Alcoholism"
 * NIAAA AA05965, PI: A. Pfefferbaum
 *
 * "INIA: Imaging Core"
 * NIAAA AA13521, PI: A. Pfefferbaum
 *
 * 
 * \author Torsten Rohlfing, Neuroscience Program, SRI International.
 */
template < typename TInputImage,
           typename TRealType = float,
           typename TOutputImage = Image< TRealType,
                                          ::itk::GetImageDimension<TInputImage>::ImageDimension >
>
class ITK_EXPORT DeformationFieldJacobianDeterminantFilter :
    public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef DeformationFieldJacobianDeterminantFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(DeformationFieldJacobianDeterminantFilter, ImageToImageFilter);
  
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
  typedef typename ConstNeighborhoodIteratorType::RadiusType RadiusType;  
  
  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** DeformationFieldJacobianDeterminantFilter needs a larger input requested
   * region than the output requested region (larger by the kernel
   * size to calculate derivatives).  As such, 
   * DeformationFieldJacobianDeterminantFilter needs to provide an 
   * implementation for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

  /** Set the derivative weights according to the spacing of the input image
      (1/spacing). Use this option if you want to calculate the Jacobian
      determinant in the space in which the data was acquired.*/
  void SetUseImageSpacingOn()
  { this->SetUseImageSpacing(true); }

  /** Reset the derivative weights to ignore image spacing.  Use this option if
      you want to calculate the Jacobian determinant in the image space.  
      Default is ImageSpacingOff. */
  void SetUseImageSpacingOff()
  { this->SetUseImageSpacing(false); }

  /** Set/Get whether or not the filter will use the spacing of the input
      image in its calculations */
  void SetUseImageSpacing(bool);                         
  itkGetMacro(UseImageSpacing, bool);

  /** Directly Set/Get the array of weights used in the gradient calculations.
      Note that calling UseImageSpacingOn will clobber these values.*/
  void SetDerivativeWeights(TRealType data[]);
  itkGetVectorMacro(DerivativeWeights, const TRealType, itk::GetImageDimension<TInputImage>::ImageDimension);

protected:
  DeformationFieldJacobianDeterminantFilter();
  virtual ~DeformationFieldJacobianDeterminantFilter() {}

  /** Do any necessary casting/copying of the input data.  Input pixel types
     whose value types are not real number types must be cast to real number
     types.*/
  void BeforeThreadedGenerateData ();

  /** DeformationFieldJacobianDeterminantFilter can be implemented as a
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
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

  void PrintSelf(std::ostream& os, Indent indent) const;

  typedef typename InputImageType::Superclass ImageBaseType;

  /** Get access to the input image casted as real pixel values */
  itkGetConstObjectMacro( RealValuedInputImage, ImageBaseType );

  /** Get/Set the neighborhood radius used for gradient computation */
  itkGetConstReferenceMacro( NeighborhoodRadius, RadiusType );
  itkSetMacro( NeighborhoodRadius, RadiusType );
  

  TRealType EvaluateAtNeighborhood
  (const ConstNeighborhoodIteratorType &it) const
  {
    unsigned i, j;
    vnl_matrix_fixed<TRealType,ImageDimension,VectorDimension> J;

    for (i = 0; i < ImageDimension; ++i)
      {
      for (j = 0; j < VectorDimension; ++j)
        {
        J[i][j] = m_DerivativeWeights[i]
                  * 0.5 * (it.GetNext(i)[j] - it.GetPrevious(i)[j]);
        }
      }
    
    return vnl_det(J);
  }

  /** The weights used to scale partial derivatives during processing */
  TRealType m_DerivativeWeights[itk::GetImageDimension<TInputImage>::ImageDimension];

private:
  bool m_UseImageSpacing;
  int m_RequestedNumberOfThreads;

  typename ImageBaseType::ConstPointer m_RealValuedInputImage;
  
  DeformationFieldJacobianDeterminantFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  RadiusType    m_NeighborhoodRadius;  
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformationFieldJacobianDeterminantFilter.txx"
#endif

#endif
