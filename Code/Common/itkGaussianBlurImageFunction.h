/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianBlurImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGaussianBlurImageFunction_h
#define _itkGaussianBlurImageFunction_h

#include "itkNeighborhoodOperatorImageFunction.h"
#include "itkGaussianOperator.h"
#include "itkGaussianSpatialFunction.h"
#include "itkImageFunction.h"

namespace itk
{

/**
 * \class GaussianBlurImageFunction
 * \brief Compute the convolution of a neighborhood operator with the image
 *        at a specific location in space, i.e. point, index or continuous
 *        index.
 * This class is templated over the input image type.
 * \sa NeighborhoodOperator
 * \sa ImageFunction
 */
template <class TInputImage,class TOutput=double>
class ITK_EXPORT GaussianBlurImageFunction :
  public ImageFunction< TInputImage, TOutput >
{
public:

  /**Standard "Self" typedef */
  typedef GaussianBlurImageFunction Self;

  /** Standard "Superclass" typedef*/
  typedef ImageFunction<TInputImage, TOutput> Superclass;

  /** Smart pointer typedef support. */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory.*/
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( GaussianBlurImageFunction, ImageFunction );

  /** InputImageType typedef support.*/
  typedef TInputImage                                 InputImageType;
  typedef typename InputImageType::PixelType          InputPixelType;
  typedef typename Superclass::IndexType              IndexType;
  typedef typename Superclass::ContinuousIndexType    ContinuousIndexType;
  typedef NeighborhoodOperatorImageFunction<InputImageType,
                                             TOutput> OperatorImageFunctionType;
  typedef typename OperatorImageFunctionType::Pointer OperatorImageFunctionPointer;

  /** Dimension of the underlying image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      InputImageType::ImageDimension);
  
  typedef GaussianOperator<TOutput,
                           itkGetStaticConstMacro(ImageDimension)> 
                                                      GaussianOperatorType;
  typedef Neighborhood<TOutput, itkGetStaticConstMacro(ImageDimension)> NeighborhoodType;
  typedef FixedArray<NeighborhoodType,itkGetStaticConstMacro(ImageDimension)> OperatorArrayType;
  
  typedef GaussianSpatialFunction<TOutput,1>  GaussianFunctionType;
  typedef typename GaussianFunctionType::Pointer GaussianFunctionPointer;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;
  
  /** Evalutate the  in the given dimension at specified point */
   virtual TOutput Evaluate(const PointType& point) const;


  /** Evaluate the function at specified Index position*/
  virtual TOutput EvaluateAtIndex( const IndexType & index ) const;

  /** Evaluate the function at specified ContinousIndex position.*/
  virtual TOutput EvaluateAtContinuousIndex( 
    const ContinuousIndexType & index ) const;

  /** The variance for the discrete Gaussian kernel.  Sets the variance
   * independently for each dimension, but 
   * see also SetVariance(const double v). The default is 0.0 in each
   * dimension. If UseImageSpacing is true, the units are the physical units
   * of your image.  If UseImageSpacing is false then the units are pixels.*/
  void SetSigma( const double sigma[itkGetStaticConstMacro(ImageDimension)] );
  void SetSigma( const float sigma[itkGetStaticConstMacro(ImageDimension)] );
  void SetSigma( const double sigma);
  const double* GetSigma() const {return m_Sigma;}
  
  /** Set the input image.
   * \warning this method caches BufferedRegion information.
   * If the BufferedRegion has changed, user must call
   * SetInputImage again to update cached values. */
  virtual void SetInputImage( const InputImageType * ptr );

  void SetExtent( const double extent[itkGetStaticConstMacro(ImageDimension)] );
  void SetExtent( const double extent);
  const double* GetExtent() const {return m_Extent;}

protected:
  GaussianBlurImageFunction();
  GaussianBlurImageFunction( const Self& ){};

  ~GaussianBlurImageFunction(){};

  void operator=( const Self& ){};
  void PrintSelf(std::ostream& os, Indent indent) const;

  void RecomputeGaussianKernel();
  void RecomputeContinuousGaussianKernel(
           const double offset[itkGetStaticConstMacro(ImageDimension)] ) const;

private:
  
  double                        m_Sigma[itkGetStaticConstMacro(ImageDimension)];
  OperatorImageFunctionPointer  m_OperatorImageFunction;
  mutable OperatorArrayType     m_OperatorArray;
  mutable OperatorArrayType     m_ContinuousOperatorArray;

  /** The maximum error of the gaussian blurring kernel in each dimensional
   * direction. For definition of maximum error, see GaussianOperator.
   * \sa GaussianOperator */
  double m_MaximumError[itkGetStaticConstMacro(ImageDimension)];
  double m_Extent[itkGetStaticConstMacro(ImageDimension)];

  /** Maximum allowed kernel width for any dimension of the discrete Gaussian
      approximation */
  int m_MaximumKernelWidth;

  /** Number of dimensions to process. Default is all dimensions */
  unsigned int m_FilterDimensionality;

  /** Flag to indicate whether to use image spacing */
  bool m_UseImageSpacing;

  /** Neighborhood Image Function */
  GaussianFunctionPointer m_GaussianFunction;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianBlurImageFunction.txx"
#endif

#endif

