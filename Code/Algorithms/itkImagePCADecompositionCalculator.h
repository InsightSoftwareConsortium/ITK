/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImagePCADecompositionCalculator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkImagePCADecompositionCalculator_h
#define __itkImagePCADecompositionCalculator_h

#include "itkObject.h"
#include "itkImagePCAShapeModelEstimator.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"

namespace itk
{

/** \class ImagePCADecompositionCalculator
 * \brief Decomposes an image into directions along basis components.
 * 
 * This calculator computes the projection of an image into a subspace specified
 * by some orthonormal basis.
 * Typically, this basis will be the principal components of an image data set,
 * as calculated by an ImagePCAShapeModelEstimator. The output of the calculator
 * is a vnl_vector containing the coefficients along each dimension of the
 * provided basis set.
 * To use this calculator, first set each basis image with the SetBasisImage 
 * method. In the PCA case, the basis images are the outputs of the 
 * ImagePCAShapeModelEstimator (except the zeroth output, which is the average 
 * image).
 * SetBasisFromModel is a convenience method to set all of this information from
 * a given ImagePCAShapeModelEstimator instance.
 *  
 * This class is templated over the input image type and the type of images
 * used to describe the basis.
 *
 * \warning This method assumes that the input image consists of scalar pixel
 * types.
 *
 * \warning All images (input and basis) must be the same size. This is not 
 * checked at runtime. 
 *
 * \author Zachary Pincus
 *
 * \ingroup Operators
 */
template <class TInputImage, 
    class TBasisImage = Image<double, ::itk::GetImageDimension<TInputImage>::ImageDimension> >
class ITK_EXPORT ImagePCADecompositionCalculator : public Object 
{
public:
  /** Standard class typedefs. */
  typedef ImagePCADecompositionCalculator Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImagePCADecompositionCalculator, Object);

  /** Type definitions for the input images. */
  typedef TInputImage  InputImageType;
  typedef TBasisImage  BasisImageType;
  
  /** Pointer types for the image. */
  typedef typename TInputImage::Pointer  InputImagePointer;
  typedef typename TBasisImage::Pointer  BasisImagePointer;
  
  /** Const Pointer type for the image. */
  typedef typename TInputImage::ConstPointer InputImageConstPointer;
  
  /** Basis image pixel type: this is also the type of the optput vector */
  typedef typename TBasisImage::PixelType BasisPixelType;
  /** Input Image dimension */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension); 

  /** Basis Image dimension */
  itkStaticConstMacro(BasisImageDimension, unsigned int,
                      TBasisImage::ImageDimension); 
  
  
  /** Vector of basis image pointers. */
  typedef std::vector< BasisImagePointer > BasisImagePointerVector;
  
  /** Type definitions for internal vectors and matrices */
  typedef vnl_matrix<BasisPixelType> BasisMatrixType;
  typedef vnl_vector<BasisPixelType> BasisVectorType;
  
  /** Set the input image. */
  itkSetConstObjectMacro(Image,InputImageType);
  
  /** Set the basis images. */
  void SetBasisImages(const BasisImagePointerVector _arg); 
  
  /** Get the basis images. */
  BasisImagePointerVector& GetBasisImages(void) {return m_BasisImages;}
  
  /** Type definition of a compatible ImagePCAShapeModelEstimator */
  typedef typename ImagePCAShapeModelEstimator<TInputImage,
    TBasisImage>::Pointer ModelPointerType;
  /** Set the basis images from a ImagePCAShapeModelEstimator */
  void SetBasisFromModel(ModelPointerType model);
  
  /** Compute the PCA decomposition of the input image. */
  void Compute(void);

  /** Return the projection of the image. */
  itkGetMacro(Projection,BasisVectorType);
  

protected:
  ImagePCADecompositionCalculator();
  virtual ~ImagePCADecompositionCalculator() {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  void CalculateBasisMatrix(void);
  void CalculateImageAsVector(void);
  
private:
  typedef typename BasisImageType::SizeType BasisSizeType;
    
  ImagePCADecompositionCalculator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  BasisVectorType m_Projection;
  BasisVectorType m_ImageAsVector;
  BasisImagePointerVector  m_BasisImages;
  BasisSizeType m_Size;
  InputImageConstPointer  m_Image;
  BasisMatrixType  m_BasisMatrix;
  bool m_BasisMatrixCalculated;
  unsigned long m_NumPixels;
};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImagePCADecompositionCalculator.txx"
#endif

#endif
