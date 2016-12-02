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
#ifndef itkShapedFloodFilledFunctionConditionalConstIterator_h
#define itkShapedFloodFilledFunctionConditionalConstIterator_h

#include <queue>
#include <vector>

#include "itkSize.h"
#include "itkConditionalConstIterator.h"
#include "itkConnectedComponentAlgorithm.h"

namespace itk
{
/**
 * \class ShapedFloodFilledFunctionConditionalConstIterator
 * \brief Iterates over a flood-filled spatial function with read-only access
 *        to pixels.
 *
 * Contributed as a paper to the Insight Journal:
 *  https://hdl.handle.net/1926/1320
 *
 * \ingroup ImageIterators
 *
 * \ingroup ITKCommon
 */
template< typename TImage, typename TFunction >
class ITK_TEMPLATE_EXPORT ShapedFloodFilledFunctionConditionalConstIterator:
  public ConditionalConstIterator< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ShapedFloodFilledFunctionConditionalConstIterator Self;

  /** Type of function */
  typedef TFunction FunctionType;

  /** Type of vector used to store location info in the spatial function */
  typedef typename TFunction::InputType FunctionInputType;

  /** Index typedef support. */
  typedef typename TImage::IndexType IndexType;

  /** Index Container Type */
  typedef typename std::vector< IndexType > SeedsContainerType;

  /** Offset typedef support. */
  typedef typename TImage::OffsetType OffsetType;

  /** Size typedef support. */
  typedef typename TImage::SizeType SizeType;

  /** Region typedef support */
  typedef typename TImage::RegionType RegionType;

  /** Image typedef support. */
  typedef TImage ImageType;

  /** Internal Pixel Type */
  typedef typename TImage::InternalPixelType InternalPixelType;

  /** External Pixel Type */
  typedef typename TImage::PixelType PixelType;

  /** Internal Neighborhood Iterator Type */
  typedef typename itk::ShapedNeighborhoodIterator< ImageType > NeighborhoodIteratorType;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * that functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  itkStaticConstMacro(NDimensions, unsigned int, TImage::ImageDimension);

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor uses
   * an explicit seed pixel for the flood fill, the "startIndex" */
  ShapedFloodFilledFunctionConditionalConstIterator(const ImageType *imagePtr,
                                                    FunctionType *fnPtr,
                                                    IndexType startIndex);

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor uses
   * a list of seed pixels for the flood fill */
  ShapedFloodFilledFunctionConditionalConstIterator(const ImageType *imagePtr,
                                                    FunctionType *fnPtr,
                                                    std::vector< IndexType > & startIndices);

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor
   * should be used when the seed pixel is unknown */
  ShapedFloodFilledFunctionConditionalConstIterator(const ImageType *imagePtr,
                                                    FunctionType *fnPtr);

  /** Automatically find a seed pixel and set m_StartIndex. Does nothing
   * if a seed pixel isn't found. A seed pixel is determined by
   * traversing the input image's LargestPossibleRegion and
   * applying the IsPixelIncluded() test. */
  void FindSeedPixel();

  /** Automatically find all seed pixels. */
  void FindSeedPixels();

  /** Initializes the iterator, called from constructor */
  void InitializeIterator();

  /** Default Destructor. */
  virtual ~ShapedFloodFilledFunctionConditionalConstIterator() {}

  /** Compute whether the index of interest should be included in the flood */
  virtual bool IsPixelIncluded(const IndexType & index) const = 0;

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self & operator=(const Self & it)
  {
    this->m_Image = it.m_Image;     // copy the smart pointer
    this->m_Region = it.m_Region;
    return *this;
  }

  /** Get the dimension (size) of the index. */
  static unsigned int GetIteratorDimension()
  { return TImage::ImageDimension; }

  /** Get the index. This provides a read only reference to the index.
   * This causes the index to be calculated from pointer arithmetic and is
   * therefore an expensive operation.
   * \sa SetIndex */
  const IndexType GetIndex()
  { return m_IndexStack.front(); }

  /** Get the pixel value */
  const PixelType Get(void) const
  { return this->m_Image->GetPixel( m_IndexStack.front() ); }

  /** Is the iterator at the end of the region? */
  bool IsAtEnd()
  { return this->m_IsAtEnd; }

  /** Put more seeds on the list */
  void AddSeed(const IndexType seed)
  {
    m_Seeds.push_back (seed);
  }

  /** Clear all the seeds */
  void ClearSeeds()
  {
    m_Seeds.clear();
  }

  /** Move an iterator to the beginning of the region. "Begin" is
   * defined as the first pixel in the region. */
  void GoToBegin()
  {
    // Clear the queue
    while ( !m_IndexStack.empty() )
      {
      m_IndexStack.pop();
      }

    this->m_IsAtEnd = true;
    // Initialize the temporary image
    m_TempPtr->FillBuffer(
      NumericTraits< typename TTempImage::PixelType >::ZeroValue()
      );

    for ( unsigned int i = 0; i < m_Seeds.size(); i++ )
      {
      if ( this->m_Image->GetBufferedRegion().IsInside (m_Seeds[i])
           && this->IsPixelIncluded(m_Seeds[i]) )
        {
        // Push the seed onto the queue
        m_IndexStack.push(m_Seeds[i]);

        // Obviously, we're at the beginning
        this->m_IsAtEnd = false;

        // Mark the start index in the temp image as inside the
        // function, neighbor check incomplete
        m_TempPtr->SetPixel(m_Seeds[i], 2);
        }
      }
  }

  /** Walk forward one index */
  void operator++()
  { this->DoFloodStep(); }

  void DoFloodStep();

  virtual SmartPointer< FunctionType > GetFunction() const
  {
    return m_Function;
  }

  /** When m_FullyConnected is set to true, the neighborhood
   * iterator will inspect an 8 respectively 26 neighborhood.
   * When the value is set to false, the neighborhood will be
   * 4 in 2D and 6 in 3D. */
  void SetFullyConnected(const bool _arg);

  bool GetFullyConnected() const;

  itkBooleanMacro(FullyConnected);

  virtual const SeedsContainerType &GetSeeds() const
  {
    return m_Seeds;
  }

protected: //made protected so other iterators can access
  /** Smart pointer to the function we're evaluating */
  SmartPointer< FunctionType > m_Function;

  /** A temporary image used for storing info about indices
   * 0 = pixel has not yet been processed
   * 1 = pixel is not inside the function
   * 2 = pixel is inside the function, neighbor check incomplete
   * 3 = pixel is inside the function, neighbor check complete */
  typedef Image< unsigned char, itkGetStaticConstMacro(NDimensions) > TTempImage;

  typename TTempImage::Pointer m_TempPtr;

  /** A list of locations to start the recursive fill */
  SeedsContainerType m_Seeds;

  /** The origin of the source image */
  typename ImageType::PointType m_ImageOrigin;

  /** The spacing of the source image */
  typename ImageType::SpacingType m_ImageSpacing;

  /** The neighborhood iterator */
  NeighborhoodIteratorType m_NeighborhoodIterator;

  /** Region of the source image */
  RegionType m_ImageRegion;

  /** Stack used to hold the path of the iterator through the image */
  std::queue< IndexType > m_IndexStack;

  /** Location vector used in the flood algorithm */
  FunctionInputType m_LocationVector;

  /** Indicates whether or not we've found a neighbor that needs to be
    * checked.  */
  bool m_FoundUncheckedNeighbor;

  /** Indicates whether or not an index is valid (inside an image)/ */
  bool m_IsValidIndex;

  /** Defines the connectivity of the neighborhood iterator.
   * In case of 2D the default connectivity is 4 (6 in 3D) and
   * when m_FullyConnected is set to true the connectivity is
   * 8 (26 in 3D).
   */
  bool m_FullyConnected;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShapedFloodFilledFunctionConditionalConstIterator.hxx"
#endif

#endif
