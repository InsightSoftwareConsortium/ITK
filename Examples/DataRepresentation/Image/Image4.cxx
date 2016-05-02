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

// Software Guide : BeginLatex
//
// Even though \href{https://www.itk.org}{ITK} can be used to perform
// general image processing tasks, the primary purpose of the toolkit is the
// processing of medical image data. In that respect, additional
// information about the images is considered mandatory. In particular the
// information associated with the physical spacing between pixels and the
// position of the image in space with respect to some world coordinate
// system are extremely important.
//
// Image origin, voxel directions (i.e. orientation), and spacing are fundamental to many
// applications. Registration, for example, is performed in physical
// coordinates. Improperly defined spacing, direction, and origins will result in
// inconsistent results in such processes. Medical images with no spatial
// information should not be used for medical diagnosis, image analysis,
// feature extraction, assisted radiation therapy or image guided surgery. In
// other words, medical images lacking spatial information are not only
// useless but also hazardous.
//
// \begin{figure} \center
// \includegraphics[width=\textwidth]{ImageOriginAndSpacing}
// \itkcaption[ITK Image Geometrical Concepts]{Geometrical concepts associated
// with the ITK image.}
// \label{fig:ImageOriginAndSpacing}
// \end{figure}
//
// Figure \ref{fig:ImageOriginAndSpacing} illustrates the main geometrical
// concepts associated with the \doxygen{Image}.
// In this figure, circles are
// used to represent the center of pixels. The value of the pixel is assumed
// to exist as a Dirac delta function located at the pixel center. Pixel
// spacing is measured between the pixel centers and can be different along
// each dimension. The image origin is associated with the coordinates of the
// first pixel in the image.
// For this simplified example, the voxel lattice is perfectly aligned with physical
// space orientation, and the image direction is therefore an identity mapping. If the
// voxel lattice samples were rotated with respect to physical space, then the image direction
// would contain a rotation matrix.
//
// A \emph{pixel} is considered to be the
// rectangular region surrounding the pixel center holding the data
// value. This can be viewed as the Voronoi region of the image grid, as
// illustrated in the right side of the figure. Linear interpolation of
// image values is performed inside the Delaunay region whose corners
// are pixel centers.
//
// Software Guide : EndLatex

#include "itkImage.h"

// Function to simulate getting mouse click from an image
static itk::Image< unsigned short, 3 >::IndexType GetIndexFromMouseClick()
{
  itk::Image< unsigned short, 3 >::IndexType LeftEyeIndex;
  LeftEyeIndex[0]=60;
  LeftEyeIndex[1]=127;
  LeftEyeIndex[2]=93;
  return LeftEyeIndex;
}

int main(int, char *[])
{
  const unsigned int Dimension=3;
  typedef itk::Image< unsigned short, Dimension > ImageType;
  ImageType::Pointer image = ImageType::New();

  const ImageType::SizeType  size  = {{ 200, 200, 200}}; //Size along {X,Y,Z}
  const ImageType::IndexType start = {{ 0, 0, 0 }}; // First index on {X,Y,Z}

  ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( start );

  image->SetRegions( region );
  image->Allocate(true); // initialize buffer to zero

  // Software Guide : BeginLatex
  //
  // Image spacing is represented in a \code{FixedArray}
  // whose size matches the dimension of the image. In order to manually set
  // the spacing of the image, an array of the corresponding type must be
  // created.  The elements of the array should then be initialized with the
  // spacing between the centers of adjacent pixels. The following code
  // illustrates the methods available in the \doxygen{Image} class for dealing
  // with spacing and origin.
  //
  // \index{itk::Image!Spacing}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ImageType::SpacingType spacing;

  // Units (e.g., mm, inches, etc.) are defined by the application.
  spacing[0] = 0.33; // spacing along X
  spacing[1] = 0.33; // spacing along Y
  spacing[2] = 1.20; // spacing along Z
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The array can be assigned to the image using
  // the \code{SetSpacing()} method.
  //
  // \index{itk::Image!SetSpacing()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  image->SetSpacing( spacing );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  // The spacing information can be retrieved from an image by using the
  // \code{GetSpacing()} method. This method returns a reference to a
  // \code{FixedArray}. The returned object can then be used to read the
  // contents of the array. Note the use of the \code{const} keyword to indicate
  // that the array will not be modified.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const ImageType::SpacingType& sp = image->GetSpacing();

  std::cout << "Spacing = ";
  std::cout << sp[0] << ", " << sp[1] << ", " << sp[2] << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The image origin is managed in a similar way to the spacing. A
  // \code{Point} of the appropriate dimension must first be
  // allocated.  The coordinates of the origin can then be assigned to
  // every component.  These coordinates correspond to the position of
  // the first pixel of the image with respect to an arbitrary
  // reference system in physical space. It is the user's
  // responsibility to make sure that multiple images used in the same
  // application are using a consistent reference system. This is
  // extremely important in image registration applications.
  //
  // The following code illustrates the creation and assignment of a variable
  // suitable for initializing the image origin.
  //
  // \index{itk::Image!origin}
  // \index{itk::Image!SetOrigin()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  // coordinates of the center of the first pixel in N-D
  ImageType::PointType newOrigin;
  newOrigin.Fill(0.0);
  image->SetOrigin( newOrigin );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The origin can also be retrieved from an image by using the
  //  \code{GetOrigin()} method. This will return a reference to a
  //  \code{Point}. The reference can be used to read the contents of
  //  the array. Note again the use of the \code{const} keyword to indicate
  //  that the array contents will not be modified.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const ImageType::PointType & origin = image->GetOrigin();

  std::cout << "Origin = ";
  std::cout << origin[0] << ", "
            << origin[1] << ", "
            << origin[2] << std::endl;

  // Software Guide : EndCodeSnippet

  //TODO: This example should really be written for a more complicated direction cosine. i.e.
  //As the first index element increases, the 1st physical space decreases.

  //  Software Guide : BeginLatex
  //
  // The image direction matrix represents the orientation relationships between
  // the image samples and physical space coordinate systems. The image direction
  // matrix is an orthonormal matrix that describes the possible permutation of image index
  // values and the rotational aspects that are needed to properly reconcile image index
  // organization with physical space axis.
  // The image directions is a $N x N$ matrix where $N$ is the dimension of the image. An
  // identity image direction indicates that increasing values of the 1st, 2nd, 3rd index
  // element corresponds to increasing values of the 1st, 2nd and 3rd physical space axis
  // respectively, and that the voxel samples are perfectly aligned with the physical space axis.
  //
  // The following code illustrates the creation and assignment of a variable
  // suitable for initializing the image direction with an identity.
  //
  // \index{itk::Image!direction}
  // \index{itk::Image!SetDirection()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  // coordinates of the center of the first pixel in N-D
  ImageType::DirectionType direction;
  direction.SetIdentity();
  image->SetDirection( direction );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  The direction can also be retrieved from an image by using the
  //  \code{GetDirection()} method. This will return a reference to a
  //  \code{Matrix}. The reference can be used to read the contents of
  //  the array. Note again the use of the \code{const} keyword to indicate
  //  that the matrix contents can not be modified.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const ImageType::DirectionType& direct = image->GetDirection();

  std::cout << "Direction = " << std::endl;
  std::cout << direct << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Once the spacing, origin, and direction of the image samples have been initialized, the image
  // will correctly map pixel indices to and from physical space
  // coordinates. The following code illustrates how a point in physical
  // space can be mapped into an image index for the purpose of reading the
  // content of the closest pixel.
  //
  // First, a \doxygen{Point} type must be declared. The point type is
  // templated over the type used to represent coordinates and over the
  // dimension of the space. In this particular case, the dimension of the
  // point must match the dimension of the image.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Point< double, ImageType::ImageDimension > PointType;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \doxygen{Point} class, like an \doxygen{Index}, is a relatively
  // small and simple object. This means that no \doxygen{SmartPointer}
  // is used here and the objects are simply declared as instances,
  // like any other C++ class. Once the point is declared, its
  // components can be accessed using traditional array notation. In
  // particular, the \code{[]} operator is available. For efficiency reasons,
  // no bounds checking is performed on the index used to access a particular
  // point component. It is the user's responsibility to make sure that the
  // index is in the range $\{0,Dimension-1\}$.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  PointType point;
  point[0] = 1.45;    // x coordinate
  point[1] = 7.21;    // y coordinate
  point[2] = 9.28;    // z coordinate
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The image will map the point to an index using the values of the
  // current spacing and origin. An index object must be provided to
  // receive the results of the mapping. The index object can be
  // instantiated by using the \code{IndexType} defined in the image
  // type.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ImageType::IndexType pixelIndex;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \code{TransformPhysicalPointToIndex()} method of the image class
  // will compute the pixel index closest to the point provided. The method
  // checks for this index to be contained inside the current buffered pixel
  // data. The method returns a boolean indicating whether the resulting
  // index falls inside the buffered region or not. The output index should
  // not be used when the returned value of the method is \code{false}.
  //
  // The following lines illustrate the point to index mapping and the
  // subsequent use of the pixel index for accessing pixel data from the
  // image.
  //
  // \index{itk::Image!TransformPhysicalPointToIndex()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const bool isInside =
    image->TransformPhysicalPointToIndex( point, pixelIndex );
  if ( isInside )
    {
    ImageType::PixelType pixelValue = image->GetPixel( pixelIndex );
    pixelValue += 5;
    image->SetPixel( pixelIndex, pixelValue );
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Remember that \code{GetPixel()} and \code{SetPixel()} are very
  // inefficient methods for accessing pixel data. Image iterators should be
  // used when massive access to pixel data is required.
  //
  // Software Guide : EndLatex
  //

  //  Software Guide : BeginLatex
  //
  //  The following example illustrates the mathematical relationships between
  //  image index locations and its corresponding physical point representation
  //  for a given Image.
  //
  // \index{itk::Image!PhysicalPoint}
  // \index{itk::Image!Index}
  //
  // Let us imagine that a graphical user interface exists
  // where the end user manually selects the voxel index location
  // of the left eye in a volume with a mouse interface.  We need to
  // convert that index location to a physical location so that
  // laser guided surgery can be accurately performed. The
  // \code{TransformIndexToPhysicalPoint} method can be used for this.
  //
  // SoftwareGuide : EndLatex

  // Software Guide : BeginCodeSnippet
  const ImageType::IndexType LeftEyeIndex = GetIndexFromMouseClick();
  ImageType::PointType LeftEyePoint;
  image->TransformIndexToPhysicalPoint(LeftEyeIndex,LeftEyePoint);
  // Software Guide : EndCodeSnippet

  std::cout << "===========================================" << std::endl;
  std::cout << "The Left Eye Location is " << LeftEyePoint << std::endl;

  // Software Guide : BeginLatex
  //
  // For a given index $I_{3X1}$, the physical location $P_{3X1}$ is calculated
  // as following:
  //
  // \begin{equation}
  //   P_{3X1} = O_{3X1} + D_{3X3} * diag( S_{3X1} )_{3x3} * I_{3X1}
  // \end{equation}
  // where $D$ is an orthonormal direction cosines matrix and
  // $S$ is the image spacing diagonal matrix.
  //
  // In matlab syntax the conversions are:
  //
  // \begin{verbatim}
  // % Non-identity Spacing and Direction
  // spacing=diag( [0.9375, 0.9375, 1.5] );
  // direction=[0.998189, 0.0569345, -0.0194113;
  //            0.0194429, -7.38061e-08, 0.999811;
  //            0.0569237, -0.998378, -0.00110704];
  // point = origin + direction * spacing * LeftEyeIndex
  // \end{verbatim}
  //
  // A corresponding mathematical expansion of the C/C++ code is:
  // SoftwareGuide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Matrix<double, Dimension, Dimension> MatrixType;
  MatrixType SpacingMatrix;
  SpacingMatrix.Fill( 0.0F );

  const ImageType::SpacingType & ImageSpacing = image->GetSpacing();
  SpacingMatrix( 0,0 ) = ImageSpacing[0];
  SpacingMatrix( 1,1 ) = ImageSpacing[1];
  SpacingMatrix( 2,2 ) = ImageSpacing[2];

  const ImageType::DirectionType & ImageDirectionCosines =
    image->GetDirection();
  const ImageType::PointType &ImageOrigin = image->GetOrigin();

  typedef itk::Vector< double, Dimension > VectorType;
  VectorType LeftEyeIndexVector;
  LeftEyeIndexVector[0]= LeftEyeIndex[0];
  LeftEyeIndexVector[1]= LeftEyeIndex[1];
  LeftEyeIndexVector[2]= LeftEyeIndex[2];

  ImageType::PointType LeftEyePointByHand =
     ImageOrigin + ImageDirectionCosines * SpacingMatrix * LeftEyeIndexVector;
  // Software Guide : EndCodeSnippet

  std::cout << "===========================================" << std::endl;
  std::cout << "Spacing:: " << std::endl << SpacingMatrix << std::endl;
  std::cout << "===========================================" << std::endl;
  std::cout << "DirectionCosines:: " << std::endl << ImageDirectionCosines << std::endl;
  std::cout << "===========================================" << std::endl;
  std::cout << "Origin:: " << std::endl << ImageOrigin << std::endl;
  std::cout << "===========================================" << std::endl;
  std::cout << "The Left Eye Location is " << LeftEyePointByHand << std::endl;

  //
  // Check if two results are identical
  //
  if ( (LeftEyePointByHand - LeftEyePoint).GetNorm() < 0.01F )
  {
    std::cout << "===========================================" << std::endl;
    std::cout << "Two results are identical as expected!" << std::endl;
    std::cout << "The Left Eye from TransformIndexToPhysicalPoint is " << LeftEyePoint << std::endl;
    std::cout << "The Left Eye from Math is " << LeftEyePointByHand << std::endl;
  }

  return EXIT_SUCCESS;
}
