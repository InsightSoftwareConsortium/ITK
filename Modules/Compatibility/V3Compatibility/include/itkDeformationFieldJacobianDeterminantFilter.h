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
#ifndef itkDeformationFieldJacobianDeterminantFilter_h
#define itkDeformationFieldJacobianDeterminantFilter_h

#include "itkImage.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_det.h"

#ifndef ITKV3_COMPATIBILITY
#error "This file is only valid when ITKV3_COMPATIBILITY is turned on. Users are encouraged to convert to itk::DisplacementFieldJacobianDeterminantFilter in ITKv4"
#endif

//This class now inherits from itkDisplacementFieldJacobianDeterminantFilter
//and simply overrides the EvaluateAtNeighborhood function.
#include "itkDisplacementFieldJacobianDeterminantFilter.h"

namespace itk
{
/** \class DeformationFieldJacobianDeterminantFilter
 *
 * \brief Computes a scalar image from a vector image (e.g., deformation field)
 * input, where each output scalar at each pixel is the Jacobian determinant
 * of the vector field at that location.  This calculation is only correct if the
 * the vector field has values that are the absolute locations from which to get
 * the new values.  This implies that the identity vector field (VF)
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
 * \deprecated
 * \ingroup GradientFilters
 * \ingroup ITKV3Compatibility
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
 * \ingroup ITKDisplacementField
 */
template< typename TInputImage,
          typename TRealType = float,
          typename TOutputImage = Image< TRealType,
                                         TInputImage::ImageDimension >
          >
class ITK_TEMPLATE_EXPORT DeformationFieldJacobianDeterminantFilter:
  public DisplacementFieldJacobianDeterminantFilter< TInputImage, TRealType, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef DeformationFieldJacobianDeterminantFilter Self;
  typedef DisplacementFieldJacobianDeterminantFilter<
    TInputImage, TRealType, TOutputImage > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(DeformationFieldJacobianDeterminantFilter, DisplacementFieldJacobianDeterminantFilter);

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
  typedef Image<
    RealVectorType, TInputImage::ImageDimension >
                    RealVectorImageType;

  /** Type of the iterator that will be used to move through the image.  Also
      the type which will be passed to the evaluate function */
  typedef ConstNeighborhoodIterator< RealVectorImageType >   ConstNeighborhoodIteratorType;
  typedef typename ConstNeighborhoodIteratorType::RadiusType RadiusType;

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual TRealType EvaluateAtNeighborhood(const ConstNeighborhoodIteratorType & it) const ITK_OVERRIDE;

protected:
  DeformationFieldJacobianDeterminantFilter();
  virtual ~DeformationFieldJacobianDeterminantFilter() {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DeformationFieldJacobianDeterminantFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformationFieldJacobianDeterminantFilter.hxx"
#endif

#endif
