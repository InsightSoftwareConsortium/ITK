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
#ifndef itkBayesianClassifierInitializationImageFilter_h
#define itkBayesianClassifierInitializationImageFilter_h

#include "itkVectorImage.h"
#include "itkVectorContainer.h"
#include "itkImageToImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkMembershipFunctionBase.h"

namespace itk
{
/** \class BayesianClassifierInitializationImageFilter
 *
 * \brief This filter is intended to be used as a helper class to
 * initialize the BayesianClassifierImageFilter.
 *
 * \par
 * The goal of this filter is to generate a membership image that indicates
 * the membership of each pixel to each class. These membership images are fed
 * as input to the Bayesian classfier filter.
 *
 * \par Parameters
 * Number of classes: This defines the number of classes, which will determine
 * the number of membership images that will be generated. The user must specify
 * this.
 *
 * \par
 * Membership functions: The user can optionally plugin in any membership function.
 * The number of membership functions plugged in should be the
 * same as the number of classes. If the user does not supply membership
 * functions, the filter will generate membership functions for you. These
 * functions are Gaussian density functions centered around 'n' pixel intensity
 * values, \f$ I_k \f$. These 'n' values are obtained by running K-means on the
 * image. In other words, the default behaviour of the filter is to generate
 * Gaussian mixture model for the input image.
 *
 * \par Inputs and Outputs
 * The filter takes a scalar Image as input and generates a VectorImage, each
 * component \f$ c \f$ of which represents memberships of each pixel to the
 * class \f$ c \f$.
 *
 * \par Template parameters
 * This filter is templated over the input image type and the data type used
 * to represent the probabilities (defaults to float).
 *
 * \author John Melonakos, Georgia Tech
 *
 * \note
 * This work is part of the National Alliance for Medical Image Computing
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 *
 * \sa BayesianClassifierImageFilter
 * \sa VectorImage
 * \ingroup ClassificationFilters
 * \ingroup ITKClassifiers
 */
template< typename TInputImage, typename TProbabilityPrecisionType = float >
class ITK_TEMPLATE_EXPORT BayesianClassifierInitializationImageFilter:
  public
  ImageToImageFilter< TInputImage, VectorImage< TProbabilityPrecisionType,
                                                TInputImage::ImageDimension > >
{
public:
  /** Standard class typedefs. */
  typedef BayesianClassifierInitializationImageFilter Self;
  typedef TInputImage                                 InputImageType;
  typedef TProbabilityPrecisionType                   ProbabilityPrecisionType;

  /** Dimension of the input image */
  itkStaticConstMacro(Dimension, unsigned int,
                       InputImageType ::ImageDimension);

  typedef VectorImage< ProbabilityPrecisionType,
                       itkGetStaticConstMacro(Dimension) >                   OutputImageType;
  typedef ImageToImageFilter< InputImageType, OutputImageType > Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BayesianClassifierInitializationImageFilter, ImageToImageFilter);

  /** Input image iterators */
  typedef ImageRegionConstIterator< InputImageType > InputImageIteratorType;

  /** Pixel types. */
  typedef typename InputImageType::PixelType  InputPixelType;
  typedef typename OutputImageType::PixelType OutputPixelType;

  /** Image Type and Pixel type for the images representing the membership of a
   *  pixel to a particular class. This image has arrays as pixels, the number of
   *  elements in the array is the same as the number of classes to be used.    */
  typedef VectorImage< ProbabilityPrecisionType,
                       itkGetStaticConstMacro(Dimension) >             MembershipImageType;
  typedef typename MembershipImageType::PixelType    MembershipPixelType;
  typedef typename MembershipImageType::Pointer      MembershipImagePointer;
  typedef ImageRegionIterator< MembershipImageType > MembershipImageIteratorType;

  /** Type of the Measurement */
  typedef Vector< InputPixelType, 1 > MeasurementVectorType;

  /** Type of the density functions */
  typedef Statistics::MembershipFunctionBase< MeasurementVectorType > MembershipFunctionType;
  typedef typename MembershipFunctionType::Pointer                    MembershipFunctionPointer;

  /** Membership function container */
  typedef VectorContainer< unsigned int,
                           MembershipFunctionPointer >  MembershipFunctionContainerType;
  typedef typename MembershipFunctionContainerType::Pointer
  MembershipFunctionContainerPointer;

  /** Method to set/get the density functions. Here you can set a vector
   * container of density functions. If no density functions are specified,
   * the filter will create ones for you. These default density functions
   * are Gaussian density functions centered around the K-means of the
   * input image.  */
  virtual void SetMembershipFunctions(MembershipFunctionContainerType
                                      *densityFunctionContainer);
  itkGetModifiableObjectMacro(MembershipFunctionContainer, MembershipFunctionContainerType);

  /** Set/Get methods for the number of classes. The user must supply this. */
  itkSetMacro(NumberOfClasses, unsigned int);
  itkGetConstMacro(NumberOfClasses, unsigned int);

  virtual void GenerateOutputInformation() ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputMultiplyOperatorCheck,
                   ( Concept::MultiplyOperator< InputPixelType > ) );
  itkConceptMacro( DoubleConvertibleToProbabilityCheck,
                   ( Concept::Convertible< double, TProbabilityPrecisionType > ) );
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputPixelType > ) );
  itkConceptMacro( ProbabilityHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< TProbabilityPrecisionType > ) );
  itkConceptMacro( DoublePlusInputCheck,
                   ( Concept::AdditiveOperators< double, InputPixelType > ) );
  // End concept checking
#endif

protected:
  BayesianClassifierInitializationImageFilter();
  virtual ~BayesianClassifierInitializationImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Initialize the membership functions. This will be called only if the membership
   * function hasn't already been set. This method initializes membership functions
   * using Gaussian density functions centered around the means computed using
   * Kmeans.
   */
  virtual void InitializeMembershipFunctions();

  /** Here is where the prior and membership probability vector images are
    created.*/
  virtual void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BayesianClassifierInitializationImageFilter);

  bool         m_UserSuppliesMembershipFunctions;
  unsigned int m_NumberOfClasses;

  typename MembershipFunctionContainerType::Pointer m_MembershipFunctionContainer;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBayesianClassifierInitializationImageFilter.hxx"
#endif

#endif
