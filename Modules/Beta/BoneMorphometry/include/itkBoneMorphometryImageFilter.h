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
#ifndef itkBoneMorphometryImageFilter_h
#define itkBoneMorphometryImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"
#include "itkSimpleDataObjectDecorator.h"
#include "itksys/hash_map.hxx"
#include "itkHistogram.h"
#include <vector>
#include "itkConstNeighborhoodIterator.h"

namespace itk
{
/** \class BoneMorphometryImageFilter
 * \brief Compute the percent bone volume [BVTV], trabecular thickness [TbTh], trabecular separation [TbSp] trabecular number [TbN] and Bone Surface to Bone Volume ration [BSBV]
 *
 * BoneMorphometryImageFilter computes bone morphometry features such as the percent bone volume [BVTV], the trabecular thickness [TbTh],
 * the trabecular separation [TbSp], the trabecular number [TbN], or the Bone Surface to Bone Volume ration [BSBV].
 * To do so, the filter needs a 3D input scan and a threshold. All voxels with an intensity higher than the threshold will
 * be considered as part of the bone.
 * A mask can also be specified in order to have more precise results (the morphometry will be computed only for the mask's voxels with a value of 1)
 *
 * BoneMorphometryImageFilter behaves as a filter with an input and output. Thus it can be inserted
 * in a pipeline with other filters and the metrics will only be
 * recomputed if a downstream filter changes.
 *
 * The filter passes its input through unmodified.  The filter is
 * threaded. It computes metrics in each thread then combines them in
 * its AfterThreadedGenerate method.
 *
 * \author: Jean-Baptiste Vimort
 * \ingroup BoneMorphometry
 *
 */
template< typename TInputImage>
class ITK_TEMPLATE_EXPORT BoneMorphometryImageFilter:
public ImageToImageFilter< TInputImage, TInputImage >
{
public:
  /** Standard Self typedef */
  typedef BoneMorphometryImageFilter                      Self;
  typedef ImageToImageFilter< TInputImage, TInputImage >  Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BoneMorphometryImageFilter, ImageToImageFilter);

  /** Image related typedefs. */
  typedef typename TInputImage::Pointer    InputImagePointer;
  typedef typename TInputImage::RegionType RegionType;
  typedef typename TInputImage::SizeType   SizeType;
  typedef typename TInputImage::IndexType  IndexType;
  typedef typename TInputImage::PixelType  PixelType;

  /** NeighborhoodIterator typedef */
  typedef typename itk::ConstNeighborhoodIterator< TInputImage >    NeighborhoodIteratorType;
  typedef typename NeighborhoodIteratorType::RadiusType             NeighborhoodRadiusType;
  typedef typename NeighborhoodIteratorType::OffsetType             NeighborhoodOffsetType;

  /** Type to use for computations. */
  typedef typename NumericTraits< PixelType >::RealType RealType;

  /** Methods to set/get the mask image */
  itkSetInputMacro(MaskImage, TInputImage);
  itkGetInputMacro(MaskImage, TInputImage);

  /** Methods to set/get the mask image */
  itkSetMacro(Threshold, RealType);
  itkGetMacro(Threshold, RealType);

  /** Methods to get the mask different outputs */

  RealType GetBVTV() { return m_Pp; }

  RealType GetTbN() { return m_Pl; }

  RealType GetTbTh() { return m_Pp/m_Pl; }

  RealType GetTbSp() { return (1.0 - m_Pp) / m_Pl; }

  RealType GetBSBV() { return 2.0 * (m_Pl / m_Pp); }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputPixelDimensionCheck,
                   ( Concept::SameDimension<TInputImage::ImageDimension, 3u>) );
  // End concept checking
#endif

protected:
  BoneMorphometryImageFilter();
  virtual ~BoneMorphometryImageFilter() {}


  /** Pass the input through unmodified. Do this by Grafting in the
  *  AllocateOutputs method.
  */
  void AllocateOutputs() ITK_OVERRIDE;

  /** Override since the filter needs all the data for the algorithm */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** Override since the filter produces all of its output */
  void EnlargeOutputRequestedRegion(DataObject *data) ITK_OVERRIDE;

  /** Initialize some accumulators before the threads run. */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  /** Do final mean and variance computation from data accumulated in threads.
    */
  void AfterThreadedGenerateData() ITK_OVERRIDE;

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData(const RegionType &
                             outputRegionForThread,
                             ThreadIdType threadId) ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  //Inputs
  RealType m_Threshold;

  // Internal computation
  double m_Pp;
  double m_Pl;
  double m_PlX;
  double m_PlY;
  double m_PlZ;

  itk::Array<long> m_NumVoxels;
  itk::Array<long> m_NumBoneVoxels;
  itk::Array<long> m_NumX;
  itk::Array<long> m_NumY;
  itk::Array<long> m_NumZ;
  itk::Array<long> m_NumXO;
  itk::Array<long> m_NumYO;
  itk::Array<long> m_NumZO;

}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBoneMorphometryImageFilter.hxx"
#endif

#endif // itkBoneMorphometryImageFilter_h
