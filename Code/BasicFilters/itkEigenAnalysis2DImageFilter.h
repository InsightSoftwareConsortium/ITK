/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEigenAnalysis2DImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkEigenAnalysis2DImageFilter_h
#define __itkEigenAnalysis2DImageFilter_h

#include "itkImageToImageFilter.h"


/** \class EigenAnalysis2DImageFilter
 * \brief Computes pixel-wise the eigen values and eigen vectors 
 *        of a 2D symmetrical matrix.
 *
 * The filter expects three inputs images { A, B, C } representing
 * the component of the matrix
 *
 *                    | A  B |
 *                    | B  c |
 *
 * The eigen values are stored in two output images, and the eigen
 * vector associated with the maximum eigenvalue is stored in an 
 * image using vector as pixel type.
 * 
 * \ingroup ShouldBeThreaded IntensityImageFilters
 */
namespace itk {

template <class TInputImage, class TEigenValueImage, class TEigenVectorImage >
class ITK_EXPORT EigenAnalysis2DImageFilter:
    public ImageToImageFilter<TInputImage,TEigenValueImage>
{
public:
  /** Standard class typedefs. */
  typedef EigenAnalysis2DImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TEigenValueImage> Superclass;
  typedef SmartPointer<Self>                   Pointer;
  typedef SmartPointer<const Self>        ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Typedef for the vector type representing the eigen vectors */
  typedef typename TEigenVectorImage::PixelType     EigenVectorType;
  typedef typename EigenVectorType::ValueType   VectorComponentType;

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** Some convenient typedefs. */
  typedef TEigenValueImage EigenValueImageType;
  typedef typename EigenValueImageType::Pointer     EigenValueImagePointer;
  typedef typename EigenValueImageType::RegionType  EigenValueImageRegionType; 
  typedef typename EigenValueImageType::PixelType   EigenValueImagePixelType; 
  typedef TEigenVectorImage                         EigenVectorImageType;
  typedef typename EigenVectorImageType::Pointer    EigenVectorImagePointer;
  typedef typename EigenVectorImageType::RegionType EigenVectorImageRegionType;
  typedef typename EigenVectorImageType::PixelType  EigenVectorImagePixelType;

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Connect the image containting the elements [0,0]
   * of the input 2D matrix */
  void SetInput1( TInputImage * image1);

  /** Connect the image containting the elements [0,1]
   * of the input 2D matrix. This is the same [1,0]
   * element given that the input matrix is expected
   * to be symmetric */
  void SetInput2( TInputImage * image2);

  /** Connect the image containting the elements [1,1]
   * of the input 2D matrix */
  void SetInput3( TInputImage * image3);

  /** Get the Output image with the greatest eigenvalue */
  EigenValueImageType * GetMaxEigenValue( void );

  /** Get the Output image with the smallest eigenvalue */
  EigenValueImageType * GetMinEigenValue( void );

  /** Get the Output image with the eigen vector associated with
   * the greatest eigen value */
  EigenVectorImageType * GetMaxEigenVector( void );

  /**  Create the Output */
  DataObject::Pointer MakeOutput(unsigned int idx);

protected:
  EigenAnalysis2DImageFilter();
  virtual ~EigenAnalysis2DImageFilter() {};
  
  void GenerateData( void );

private:
  EigenAnalysis2DImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEigenAnalysis2DImageFilter.txx"
#endif

#endif




