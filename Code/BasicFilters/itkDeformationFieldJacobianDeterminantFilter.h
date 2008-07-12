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
 * of the vector field at that location.  This calculation is only correct if the
 * the vector field has values that are the absolute locations from which to get
 * the new values are to be taken.  This implies that the identity vector field (VF)
 * mapping would have values at each location (x) equal to the location itself.
 * VF(x)=x.  THIS IS VERY UNUSUAL.  The DeformationFieldJacobianDeterminantFilter
 * computes the proper Jacobian Determinant for a vector field described this way
 * as det[ dT/dx ] = det[ du/dx ].
 *
 * In most cases deformation field mappings are represented as displacements from
 * the current location so that an identity mapping is represented by an all zero vector field.
 * In that more common case, one should use the DisplacementFieldJacobianDeterminantFilter which
 * computes the Jacobian Determinant properly as: det[ dT/dx ] = det[ I + du/dx ].
 * This class is a specialization of the DisplacementFieldJacobianDeterminantFilter, further
 * details regarding it's implementation should be review in 
 * itkDisplacementFieldJacobianDeterminantFilter.h.
 *
 * \ingroup GradientFilters
 *
 * \sa DisplacementFieldJacobianDeterminantFilter
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
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


  virtual TRealType EvaluateAtNeighborhood(const ConstNeighborhoodIteratorType &it) const;

  /** The weights used to scale partial derivatives during processing */
  TRealType m_DerivativeWeights[itk::GetImageDimension<TInputImage>::ImageDimension];
  /** Pre-compute 0.5*m_DerivativeWeights since that is the only thing used in the computations. */
  TRealType m_HalfDerivativeWeights[itk::GetImageDimension<TInputImage>::ImageDimension];

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
