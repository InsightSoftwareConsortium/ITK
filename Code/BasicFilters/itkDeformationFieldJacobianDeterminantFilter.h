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
template< typename TInputImage,
          typename TRealType = float,
          typename TOutputImage = Image< TRealType,
                                         ::itk::GetImageDimension< TInputImage >::ImageDimension >
          >
class ITK_EXPORT DeformationFieldJacobianDeterminantFilter:
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
  typedef Vector<
    TRealType, ::itk::GetVectorDimension< InputPixelType >::VectorDimension >
  RealVectorType;
  typedef Image<
    RealVectorType, ::itk::GetImageDimension< TInputImage >::ImageDimension >
  RealVectorImageType;

  /** Type of the iterator that will be used to move through the image.  Also
      the type which will be passed to the evaluate function */
  typedef ConstNeighborhoodIterator< RealVectorImageType >   ConstNeighborhoodIteratorType;
  typedef typename ConstNeighborhoodIteratorType::RadiusType RadiusType;

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  void PrintSelf(std::ostream & os, Indent indent) const;

  virtual TRealType EvaluateAtNeighborhood(const ConstNeighborhoodIteratorType & it) const;

protected:
  DeformationFieldJacobianDeterminantFilter();
  virtual ~DeformationFieldJacobianDeterminantFilter() {}
private:
  DeformationFieldJacobianDeterminantFilter(const Self &); //purposely not
                                                           // implemented
  void operator=(const Self &);                            //purposely not

  // implemented
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDeformationFieldJacobianDeterminantFilter.txx"
#endif

#endif
