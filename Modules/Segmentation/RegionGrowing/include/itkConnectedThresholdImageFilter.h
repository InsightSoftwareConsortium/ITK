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
#ifndef itkConnectedThresholdImageFilter_h
#define itkConnectedThresholdImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
/** \class ConnectedThresholdImageFilter
 * \brief Label pixels that are connected to a seed and lie within a range of values
 *
 * ConnectedThresholdImageFilter labels pixels with ReplaceValue that are
 * connected to an initial Seed AND lie within a Lower and Upper
 * threshold range.
 *
 * \ingroup RegionGrowingSegmentation
 * \ingroup ITKRegionGrowing
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT ConnectedThresholdImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ConnectedThresholdImageFilter                   Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ConnectedThresholdImageFilter,
               ImageToImageFilter);

  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;
  typedef typename InputImageType::IndexType    IndexType;
  typedef typename std::vector<IndexType>       SeedContainerType;
  typedef typename InputImageType::SizeType     SizeType;

  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::Pointer    OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename OutputImageType::PixelType  OutputImagePixelType;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Set seed point. */
  void SetSeed(const IndexType & seed);

  void AddSeed(const IndexType & seed);

  /** Clear the seed list. */
  void ClearSeeds();

  /** Method to access seed container. */
  virtual const SeedContainerType &GetSeeds() const;

  /** Set/Get value to replace thresholded pixels. Pixels that lie *
   *  within Lower and Upper (inclusive) will be replaced with this
   *  value. The default is 1. */
  itkSetMacro(ReplaceValue, OutputImagePixelType);
  itkGetConstMacro(ReplaceValue, OutputImagePixelType);

  /** Type of DataObjects to use for scalar inputs */
  typedef SimpleDataObjectDecorator< InputImagePixelType > InputPixelObjectType;

  /** Set Upper and Lower Threshold inputs as values */
  virtual void SetUpper(InputImagePixelType);
  virtual void SetLower(InputImagePixelType);

  /** Set Threshold inputs that are connected to the pipeline */
  virtual void SetUpperInput(const InputPixelObjectType *);
  virtual void SetLowerInput(const InputPixelObjectType *);

  /** Get Upper and Lower Threshold inputs as values. */
  virtual InputImagePixelType GetUpper() const;
  virtual InputImagePixelType GetLower() const;

  /** Get Threshold inputs that are connected to the pipeline. */
  virtual InputPixelObjectType * GetUpperInput();
  virtual InputPixelObjectType * GetLowerInput();

  /** Image dimension constants. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputEqualityComparableCheck,
                   ( Concept::EqualityComparable< OutputImagePixelType > ) );
  itkConceptMacro( InputEqualityComparableCheck,
                   ( Concept::EqualityComparable< InputImagePixelType > ) );
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< InputImagePixelType, OutputImagePixelType > ) );
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< InputImageDimension, OutputImageDimension > ) );
  itkConceptMacro( IntConvertibleToInputCheck,
                   ( Concept::Convertible< int, InputImagePixelType > ) );
  itkConceptMacro( OutputOStreamWritableCheck,
                   ( Concept::OStreamWritable< OutputImagePixelType > ) );
  // End concept checking
#endif

  /** Face connectivity is 4 connected in 2D, 6  connected in 3D, 2*n   in ND
   *  Full connectivity is 8 connected in 2D, 26 connected in 3D, 3^n-1 in ND
   *  Default is to use FaceConnectivity. */
  typedef enum {
    FaceConnectivity,
    FullConnectivity
  } ConnectivityEnumType;

  /** Type of connectivity to use (fully connected OR 4(2D), 6(3D),
   * 2*N(ND) connectivity). */
  itkSetEnumMacro(Connectivity, ConnectivityEnumType);
  itkGetEnumMacro(Connectivity, ConnectivityEnumType);

protected:
  ConnectedThresholdImageFilter();
  ~ConnectedThresholdImageFilter() ITK_OVERRIDE {}

  // Override since the filter needs all the data for the algorithm.
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  // Override since the filter produces the entire dataset.
  void EnlargeOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ConnectedThresholdImageFilter);

  SeedContainerType     m_Seeds;

  OutputImagePixelType  m_ReplaceValue;

  ConnectivityEnumType  m_Connectivity;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConnectedThresholdImageFilter.hxx"
#endif

#endif
