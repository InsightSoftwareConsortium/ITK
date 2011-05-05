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
#ifndef __itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter_h
#define __itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkBloxBoundaryProfileItem.h"
#include "itkBloxBoundaryProfileImage.h"
#include "itkBloxCoreAtomImage.h"

namespace itk
{
/** \class BloxBoundaryProfileImageToBloxCoreAtomImageFilter
 * \brief Converts a blox boundary profile image to an image of core atoms.
 * \ingroup ITK-Blox
 */
template< typename TInputImage, typename TOutputImage, typename TSourceImage >
class ITK_EXPORT BloxBoundaryProfileImageToBloxCoreAtomImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef BloxBoundaryProfileImageToBloxCoreAtomImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage >   Superclass;
  typedef SmartPointer< Self >                              Pointer;
  typedef SmartPointer< const Self >                        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BloxBoundaryProfileImageToBloxCoreAtomImageFilter, ImageToImageFilter);

  /** Number of dimensions */
  itkStaticConstMacro(NDimensions, unsigned int, TSourceImage::ImageDimension);

  /** Typedefs */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;

  typedef TOutputImage                           OutputImageType;
  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;
  typedef typename OutputImageType::PixelType    OutputImagePixelType;
  typedef typename OutputImageType::ConstPointer OutputImageConstPointer;
  typedef typename OutputImageType::IndexType    OutputImageIndexType;

  typedef TSourceImage                           SourceImageType;
  typedef typename SourceImageType::Pointer      SourceImagePointer;
  typedef typename SourceImageType::RegionType   SourceImageRegionType;
  typedef typename SourceImageType::PixelType    SourceImagePixelType;
  typedef typename SourceImageType::ConstPointer SourceImageConstPointer;
  typedef typename SourceImageType::IndexType    SourceImageIndexType;

  /** Image size typedef */
  typedef Size< itkGetStaticConstMacro(NDimensions) > SizeType;

  /** The type of Point used to convert between physical and blox space */
  typedef Point< double, itkGetStaticConstMacro(NDimensions) > PositionType;

  /** The vector between two points */
  typedef typename PositionType::VectorType VectorType;

  /** How we represent gradients. */
  typedef CovariantVector< double, itkGetStaticConstMacro(NDimensions) > GradientType;

  /** Walk the input image, find core atoms, store them.  */
  void FindCoreAtoms();

  /** Find core atoms given a specific boundary point. */
  typedef BloxBoundaryProfileItem< itkGetStaticConstMacro(NDimensions) > BloxProfileItemType;
  void FindCoreAtomsAtBoundaryPoint(BloxProfileItemType *pItem);

  /** Gets and sets for member variables. */
  itkSetMacro(DistanceMin, double);
  itkSetMacro(DistanceMax, double);
  itkSetMacro(Epsilon, double);
  itkSetMacro(Polarity, bool);

  /** Overload of base class function to generate input region */
  void GenerateInputRequestedRegion();

  /** Set the two input images */
  void SetInput1(const SourceImageType *image1);

  void SetInput2(const InputImageType *image2);

protected:
  BloxBoundaryProfileImageToBloxCoreAtomImageFilter();
  virtual ~BloxBoundaryProfileImageToBloxCoreAtomImageFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Method for forming the BloxBoundaryPointImage. */
  void GenerateData();

private:
  //purposely not implemented
  BloxBoundaryProfileImageToBloxCoreAtomImageFilter(const Self &);
  void operator=(const Self &);

  /** Pointers to input and output images */
  InputImageConstPointer  m_BoundaryProfileImagePtr;
  OutputImagePointer      m_OutputPtr;
  SourceImageConstPointer m_SourceImagePtr;

  /** Parameters used to establish conic shell iterator regions.
   * See the documentation for itkConicShellInteriorExteriorSpatialFunction
   * for how these affect the iterator. */
  double       m_DistanceMin;
  double       m_DistanceMax;
  double       m_Epsilon;
  bool         m_Polarity;
  bool         m_IntensityFlag;
  unsigned int m_IntensityThreshold;
  bool         m_CreateCoreAtom;
  int          m_CoreAtomsCreated;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxBoundaryProfileImageToBloxCoreAtomImageFilter.txx"
#endif

#endif
