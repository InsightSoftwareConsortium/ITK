#ifndef __itkHybridFilter_h
#define __itkHybridFilter_h

#include "itkBalloonForceFilter.h"
#include "itkGibbsPriorFilter.h"
#include "itkSimpleImageRegionIterator.h"

namespace itk
{

/** \class HybridFilter
 * \brief Computes the gradient of an image by convolution
 *        with the first derivative of a Gaussian.
 * 
 * This filter is implemented using the recursive gaussian
 * filters
 *
 */
template <class TInputImage, class TOutputImage, class TInputMesh, 
	class TOutputMesh>
class ITK_EXPORT HybridFilter:
  public ImageToImageFilter<TInputImage,TOutputImage>
{

public:

  /**
   * Standard "Self" typedef.
   */
  typedef HybridFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageToImageFilter<TInputImage,TOutputImage> Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>                Pointer;
  typedef SmartPointer<const Self>			ConstPointer;


  /** 
   *  Smoothing filter type
   */
  typedef BalloonForceFilter<
							TInputMesh,
                            TOutputMesh>	BalloonForceFilterType;


  /** 
   *  Derivative along one dimension filter type
   */
  typedef GibbsPriorFilter<
							TInputImage,
                            TOutputImage>	GibbsPriorFilterType;

  /** 
   *  Pointer to a balloon force filter 
   */
  typedef typename BalloonForceFilterType::Pointer	BalloonForceFilterPointer;


  /** 
   *  Pointer to a gibbs prior filter 
   */
  typedef typename GibbsPriorFilterType::Pointer	GibbsPriorFilterPointer;                                  
                                  
  typedef
    SimpleImageRegionIterator< TOutputImage > OutputImageIterator;
  
  /** 
   * Image Dimension
   */
  enum { ImageDimension = TInputImage::ImageDimension };

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**

   * Set potential of the balloon force filter 
   
   * using the output of gibbs prior filter

   */

  void SetPotential( void );

  /**

   * Sent object region labelled by the deformable 
   
   * model to the gibbs prior model for parameter update 

   */

  void SetObjectRegion( void );

  /**

   * Set the balloon force filter and gibbs prior filter

   */
  void SetBalloonForceFilter(BalloonForceFilterPointer	bffilter);
  void SetGibbsPriorFilter(GibbsPriorFilterPointer	gpfilter);

  void Advance();
  void SetGibbsInput();

protected:

  HybridFilter();
  
  virtual ~HybridFilter() {};
  
  HybridFilter(const Self&) {}
  
  void operator=(const Self&) {}
  
  /**

   * Generate Data

   */
  virtual void GenerateData();

private:
  
  BalloonForceFilterPointer		m_BalloonForceFilter;
  GibbsPriorFilterPointer		m_GibbsPriorFilter;
  int m_IterNum;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
//#include "itkHybridFilter.txx"
#endif

#endif
