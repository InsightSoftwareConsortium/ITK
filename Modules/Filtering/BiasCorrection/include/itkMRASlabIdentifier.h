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
#ifndef itkMRASlabIdentifier_h
#define itkMRASlabIdentifier_h

#include "itkObject.h"
#include "itkImage.h"
#include <vector>

namespace itk
{
/** \class MRASlabIdentifier
 * \brief identifies slab in MR images comparing minimum intensity averages
 *
 * This class is templated over the type of image.
 * In many cases, a 3D MR image is constructed by merging smaller 3D
 * blocks (slabs) which were acquired with different settings such as magnetic
 * settings and patient positions. Therefore, stripe like patterns with slabs
 * can be present in the resulting image. Such artifacts are called "slab
 * boundary" artifacts or "venetian blind" artifacts.
 *
 * With the slab boundary artifacts in an image, even a same tissue class's
 * intensity values might vary significantly along the borders of slabs.
 * Such rough value changes are not appropriate for some image processing
 * methods. For example, MRIBiasFieldCorrectionFilter assumes a smooth bias
 * field. However, with the slab boundary artifacts, the bias field estimation
 * scheme that MRIBiasFieldCorrectionFilter uses might not adopt well.
 * So, the MRIBiasFieldCorrectionFilter creates regions for slabs using the
 * MRASlabIdentifier and then apply its bias correction scheme to each slab.
 *
 * For this identifier, a slice means 2D image data which is extracted from
 * the input image along one of three axes (x, y, z). Users can specify
 * the slicing axis using the SetSlicingDirection(int dimension) member.
 * (0 - x, 1 - y, 2 - z).
 *
 * The identification scheme used here is very simple.
 * 1) Users should specify how many pixels per slice the identifier
 *    will sample.
 * 2) For each slice, the identifier searches the specified number of pixels
 *    of which intensity values are greater than 0 and less than those
 *    of the other pixels in the slice
 * 3) The identifier calculates the average for each slice and the overall
 *    average using the search results.
 * 4) For each slice, it subtracts the overall average from the slice average.
 *    If the sign of the subtraction result changes, then it assumes that a
 *    slab ends and another slab begins.
 * \ingroup ITKBiasCorrection
 */
template< typename TInputImage >
class ITK_TEMPLATE_EXPORT MRASlabIdentifier:public Object
{
public:
  /** Standard class typedefs. */
  typedef MRASlabIdentifier          Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MRASlabIdentifier, Object);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Type definition for the input image. */
  typedef TInputImage ImageType;

  /** Pointer type for the image. */
  typedef typename TInputImage::Pointer ImagePointer;

  /** Const Pointer type for the image. */
  typedef typename TInputImage::ConstPointer ImageConstPointer;

  /** Type definition for the input image pixel type. */
  typedef typename TInputImage::PixelType  ImagePixelType;
  typedef typename TInputImage::IndexType  ImageIndexType;
  typedef typename TInputImage::SizeType   ImageSizeType;
  typedef typename TInputImage::RegionType ImageRegionType;
  typedef std::vector< ImageRegionType >   SlabRegionVectorType;

  /** Set/Get the input image. */
  itkSetConstObjectMacro(Image, ImageType);
  itkGetConstObjectMacro(Image, ImageType);

  /** Set/Get the number of minimum intensity pixels per slice. */
  itkSetMacro(NumberOfSamples, unsigned int);
  itkGetConstReferenceMacro(NumberOfSamples, unsigned int);

  /** Set/Get the minimum threshold value for the background pixels */
  itkSetMacro(BackgroundMinimumThreshold, ImagePixelType);
  itkGetConstReferenceMacro(BackgroundMinimumThreshold, ImagePixelType);

  /** Set/Get the tolerance value. */
  itkSetMacro(Tolerance, double);
  itkGetConstReferenceMacro(Tolerance, double);

  /** Set/Get the direction of slicing/
   * 0 - x axis, 1 - y axis, 2 - z axis */
  itkSetMacro(SlicingDirection, int);
  itkGetConstReferenceMacro(SlicingDirection, int);

  /** Compute the average values of miminum intensity pixels for each slice and
   * compare the average values with overall averages. */
  void GenerateSlabRegions();

  /** Get slab regions. */
  SlabRegionVectorType GetSlabRegionVector();

protected:
  MRASlabIdentifier();
  virtual ~MRASlabIdentifier() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MRASlabIdentifier);

  /** Target image pointer that MRASlabIdentifier will use. */
  ImageConstPointer m_Image;

  /** The number of pixels per slice which will be included
   * for average calculation. In a sense, it's sampling size per slice. */
  unsigned int m_NumberOfSamples;

  int                  m_SlicingDirection;
  ImagePixelType       m_BackgroundMinimumThreshold;
  double               m_Tolerance;
  SlabRegionVectorType m_Slabs;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMRASlabIdentifier.hxx"
#endif

#endif /* itkMRASlabIdentifier_h */
