/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInPlaceImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkInPlaceImageFilter_h
#define __itkInPlaceImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
  
/** \class InPlaceImageFilter
 * \brief Base class for filters that take an image as input and overwrite that image as the output
 *
 * InPlaceImageFilter is the base class for all process objects whose
 * output image data is constructed by overwriting the input image
 * data. In other words, the output bulk data is the same block of
 * memory as the input bulk data.  This filter provides the mechanisms
 * for in place image processing while maintaining general pipeline
 * mechanics. InPlaceImageFilters use less memory than standard
 * ImageToImageFilters because the input buffer is reused as the
 * output buffer.  However, this benefit does not come without a cost.
 * Since the filter overwrites its input, the ownership of the bulk
 * data is transitioned from the input data object to the output data
 * object.  When a data object has multiple consumers with one
 * of the consumers being an in place filter, the in place filter
 * effectively destroys the bulk data for the data object. Upstream
 * filters will then have to re-execute to regenerate the data object's
 * bulk data for the remaining consumers.
 *
 * Since an InPlaceImageFilter reuses the input bulk data memory for the
 * output bulk data memory, the input image type must match the output
 * image type.  Therefore, this class is only templated on the input image
 * type.
 *
 * \ingroup ImageFilters
 */
template <class TInputImage>
class ITK_EXPORT InPlaceImageFilter : public ImageToImageFilter<TInputImage, TInputImage>
{
public:
  /** Standard class typedefs. */
  typedef InPlaceImageFilter  Self;
  typedef ImageToImageFilter<TInputImage, TInputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(InPlaceImageFilter,ImageToImageFilter);

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::Pointer        InputImagePointer;
  typedef typename InputImageType::ConstPointer   InputImageConstPointer;
  typedef typename InputImageType::RegionType     InputImageRegionType; 
  typedef typename InputImageType::PixelType      InputImagePixelType; 
  
  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TInputImage::ImageDimension);


 protected:
  InPlaceImageFilter();
  ~InPlaceImageFilter();

  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  /** The GenerateData method normally allocates the buffers for all
   * of the outputs of a filter. Since InPlaceImageFilter's use an
   * overwritten version of input for its output, the output buffer
   * should not be allocated. Instead, we graft the input to the
   * filter to the output.  If an InPlaceFilter has multiple outputs,
   * then it would need to override this method to graft one of its
   * outputs and allocate the remaining. If a filter is threaded
   * (i.e. it provides an implementation of ThreadedGenerateData()),
   * this method is called automatically. If an InPlaceFilter is not
   * threaded (i.e. it provides an implementation of GenerateData()),
   * then this method (or equivalent) must be called in
   * GenerateData(). */
  virtual void AllocateOutputs();

  /** InPlaceImageFilter will transfer ownership of the input bulk
   * data to the output object.  Once the output object owns the bulk
   * data (done in AllocateOutputs()), the input object must release
   * its hold on the bulk data.   ProcessObject::ReleaseInputs()
   * only releases the input bulk data when the user has set the
   * ReleaseDataFlag.  InPlaceImageFilter::ReleaseInputs() also
   * releases the input that it has overwritten.
   *
   * \sa ProcessObject::ReleaseInputs() */
  virtual void ReleaseInputs(); 

private:
  InPlaceImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkInPlaceImageFilter.txx"
#endif

#endif
