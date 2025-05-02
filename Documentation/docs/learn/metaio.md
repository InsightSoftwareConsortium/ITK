# MetaIO

## Abstract

MetaImage is the text-based tagged file format for medical images that resulted.
We have now extended that file format to support a variety of objects that occur
in medicine such a tubes (for vessels, needles, etc.), blobs (for arbitrary
shaped objects), cubes, spheres, etc. The complete library is known at MetaIO.

The central code of MetaImage/MetaIO is quite stable. MetaImage has been in use
for several years by a wide range of research at UNC, Chapel Hill. New features
are occasionally added, but backward compatibility will always be maintained.

## Introduction and Installation

### Obtaining MetaIO

1. The [upstream MetaIO Git repository](https://github.com/Kitware/MetaIO) is
   available on GitHub.

MetaIO is being distributed with the following packages:

-   National Library of Medicine’s Insight Toolkit (ITK) for medical image
    segmentation and registration.
-   Kitware's Visualization Toolkit (VTK).

### Installing The MetaIO Package

MetaIO is a hierarchy of C++ classes and functions. We have yet to find a modern
C++ compiler that does not compile MetaIO. Supported compilers include
[ITK's supported compilers](../supported_compilers.md).

MetaIO is built as part of the standard ITK and VTK installations. It is
also quite easy to use MetaIO from within these toolkits without using the rest
of the toolkit.

#### ITK MetaIO

The hierarchy of the software in the stand-alone MetaIO package is as follows:

-   MetaIO/
    -   doc/
    -   tests/

The top level contains the source files, the header files, and the
`CMakeLists.txt` file that is used by the CMake program to compile MetaIO. This
document and the MetaObjects www pages are in the `doc` directory. A sequence of
simple tests is available in the `tests` directory.

The hierarchy of the software in the Insight and InsightApplications
distributions is as follows:

-   ITK
    -   Modules/ThirdParty/MetaIO/src/MetaIO/src
        -   doc/
        -   tests/

Routines that wrap MetaIO for ITK's image IO object factory are in:

-   ITK/Modules/IO/Meta
    -   `itkMetaImageIO`

Routines that wrap MetaIO for reading and writing ITKS's Spatial Objects (tubes,
blobs, ellipses, meshes, etc.) are in:

-   ITK/Modules/Core/SpatialObjects

#### Stand Alone MetaIO

MetaIO can also be compiled outside of these toolkits. This is left as an
exercise to the user (hint: requires the kwsys and zlib libraries). Instead of a
stand-alone installation, we highly recommend using the distribution in ITK - if
you build ITK, you get MetaIO for free!

## Quick Start

### Data conversion via MetaHeaders

This section assumes that you have data that you wish to process using an
application that reads MetaImages. This section gives examples on how “convert”
your data to the MetaImage format.

For uncompressed data, “conversion” to MetaImage is actually just a matter of
specifying a MetaImage Headerfile (a “MetaHeader”) that describes and points to
the file(s) containing your data.

-   Uncompressed data is data stored in a raw format, possibly with a header, as
    is often the case for DICOM, BMP, and PNG formatted images.

For compressed data, you must first convert your data to a non-compressed
format. One of the most robust image conversion software packages is
ImageMagick. It has an application called “convert” that handles most of the
popular 2D image formats.

-   Compressed data is includes JPEG or GIF formats as well as select PNG and
    TIFF images.

### Using MetaImageImporter

`MetaImageImporter` asks a series of questions about your data and then produces
a MetaImage header file that points to your data and allows the MetaIO library
to read your data.

`MetaImageImporter` is part of the InsightApplications repository. See the ITK
website for information on downloading and installing InsightApplications - the
companion to the Insight repository.

`MetaImageImporter` now has a QT graphical user interface. Please see the
documentation.

Otherwise, the following two sub-sections will step you through the conversion
process. The first sub-section applies if all of your data is in one file, i.e.,
is a "brick-of-bytes". The second sub-section applies if your data is spread
across files, e.g., is dicom or a tiff sequence.

### Reading a Brick-of-Bytes (an N-Dimensional volume in a single file)

A “brick of bytes” is a volume of image data stored in a single file possibly
with preceding and trailing non-image data. A volume can be of any dimension (1
dimensional to N dimensional).

To correctly load these images, the minimal information that you need to know
is:

-   Number of dimensions
-   Size of each dimension
-   Data type
-   Name of the data file

For example, let’s say the data was 3 dimensional, had 256 x 256 x 64 voxels,
used an unsigned short to represent the value at each voxel, and was stored in
the file “image.raw”. The resulting MetaHeader (our naming convention would call
this file “image.mhd”) file would read:

```

ObjectType = Image
NDims = 3
DimSize = 256 256 64
ElementType = MET_USHORT
ElementDataFile = image.raw

```
(this tag must be last in a MetaImageHeader)

That’s it, but this assumes quite a bit about the image data. Specifically, it
assumes:

-   There are not any non-image data bytes (header data) at the beginning of the
    image data file “image.raw”.
-   The voxels are cubes – the distance spanned by and between a voxel in each
    coordinate direction is 1 “unit”, e.g., 1x1x1mm voxel size and voxel
    spacing.
-   The byte-order of the data in `image.raw` matches the byte ordering native
    to the machine the application is running on (e.g., PC’s use LSB ordering
    and Suns/Macs use MSB ordering).

If these assumptions are false, the data will not be loaded correctly by the
application. To fix these problems, MetaIO allows you to specify additional
tag/value pairs in the header:

-   To skip the header bytes in the image data file, use:
    ```
    HeaderSize = X
    ```
    where `X` is the number of bytes to skip at the beginning of the file before
    reading image data. If you know there are no trailing bytes (extra bytes at
    the end of the file) you can specify:
    ```
    HeaderSize = -1
    ```
    and MetaImage will automatically calculate the number of extra bytes in the
    data file, assume those bytes are at the head of the data file, and
    automatically skip them before beginning to read the image data.

-   To specify the spacing of the voxels, use:
    ```
    ElementSpacing = X Y Z
    ```
    where `X` is the distance between the centers of the voxels along the
    x-dimension, `Y` is the spacing in the y-dimension, and `Z` is the spacing
    in the z-dimension. Therefore, to specify a 1x1x3mm voxel spacing, use:
    ```
    ElementSpacing = 1 1 3
    ```
    NOTE: If `ElementSpacing` is not specified, it is assumed to be equal to
    `ElementSize`. If neither is specified, both are assumed to be 1.

-   To specify a voxel size, use:
    ```
    ElementSize = X Y Z
    ```
    where `X Y Z` represent the size in the x, y, and z-dimensions respectively.
    NOTE: If `ElementSize` is not specified, it is assumed to be equal to
    `ElementSpacing`. If neither is specified, both are assumed to be 1.

-   To specify a particular byte ordering, use:
    ```
    ElementByteOrderMSB = True
    ```
    or
    ```
    ElementByteOrderMSB = False
    ```
    MSB (aka big-endian) ordering is common to SPARC and Motorola processors
    (e.g., Macintoshes). LSB (aka little-endian) ordering is common to Intel
    processors (e.g., PC compatibles).

Putting it all together, to “convert” a file containing the image data in a
continuous block at the end of the file, specify the header:

```

ObjectType = Image
NDims = 3
DimSize = 256 256 64
ElementType = MET_USHORT
HeaderSize = -1
ElementSize = 1 1 3
ElementSpacing = 1 1 1
ElementByteOrderMSB = False
ElementDataFile = image.raw

```

### Reading DICOM and Other One-Slice-Per-File Data Formats

If the data is split into one slice per file, as is done with most DICOM object
files, only the `ElementDataFile` tag’s option needs to change. Note that 3D
DICOM object files are becoming popular, and some such DICOM files can be read
using the above, volume, technique.

Since the MetaLibrary cannot directly parse DICOM headers, those headers must be
skipped and the user must specify the image dimensions and other essential image
information. For DICOM files, the MetaLibrary must automatically calculate the
header size of each file (luckily for almost every DICOM object the image data
is stored at the end of the file). For this reason, this method only works for
uncompressed files.

To specify which files comprise the volume, they can be specified as an ordered
list in the MetaHeader using the `ElementDataFile=LIST` option. The filenames
should be listed at the end of the MetaHeader, after the `ElementDataFile`
option, and the filenames should be separated by whitespace:

```

ObjectType = Image
NDims = 3
DimSize = 512 512 100
ElementType = MET_USHORT
HeaderSize = -1
ElementSize = 1 1 3
ElementSpacing = 1 1 1
ElementByteOrderMSB = False
ElementDataFile = LIST
filenameOfSlice1
filenameOfSlice2
filenameOfSlice3
filenameOfSlice4
.
. (one hundred filenames must be specified to specify the 100 slices in the volume)
.

```

This method works even if there are spaces in the file paths and file names.

Notice that this method can become tedious if a large number of files need to be
read. To alleviate this, a second way of specifying a series of files can be
used if the filenames are numerically distinguished. That is, the file names
should be able to be specified using a numeric substitution into a c-style
printf-string, for a range of values. In pseudo-code:

```

for i=numBegin to numEnd step numStep
sprintf(sliceName, “baseName.%03d”, i);
end

```

The parameters of this system are `numBegin`, `numEnd`, `numStep`, and the
c-style printf string (e.g., `“baseName.%03d”`). The begin, end, and step
parameters appear in order after the c-style printf string:

```

ObjectType = Image
NDims = 3
DimSize = 512 512 100
ElementType = MET_USHORT
HeaderSize = -1
ElementSize = 1 1 3
ElementSpacing = 1 1 1
ElementByteOrderMSB = False
ElementDataFile = baseName.%03d 1 100 1

```

The above MetaImage header will cause the files “baseName.001” to “baseName.100”
to be read to create a 100-slice volume. This method works even if there are
spaces in the file paths and file names. However, when spaces are present in the
file path and/or file name, all three parameters (begin, end, and step) need to
be specified as the last parameters. The remaining parameters (initially parsed
based on spaces) are then joined back together (including spaces) to generate
the file name.

In some cases, it may be helpful to skip slices in the volume. Changing the
slice spacing and the `ElementDataFileNumStep` enacts this…

```

ObjectType = Image
NDims = 3
DimSize = 512 512 50
ElementType = MET_USHORT
HeaderSize = -1
ElementSize = 1 1 3
ElementSpacing = 1 1 2
ElementByteOrderMSB = False
ElementDataFile = baseName.%03d 1 100 2

```

The complete set of MetaImage Tags are given in the Reference section of this
document. The next section discusses how to use the MetaImage Library for image
reading and writing in your own programs.

## MetaIO Library Architecture

The base class of the MetaIO library is the `MetaObject` class. It defines a
base set of tags that are common to all metaObjects such as `MetaImages`,
`MetaTubes`, etc.

The tags are defined using the protected member functions `SetupReadFields` and
`SetupWriteFields`. These functions create a list of `MetaFieldRecords` to
define the name, type, interdependence, and necessity of each tag. Helper
functions for defining the fields are in `MetaUtils.cxx`. The types are defined
via enums in `MetaTypes.h`.

The derived classes add tags to the list via their own `SetupReadFields` and
`SetupWriteFields` member functions. The `MetaImage` subclass also re-implements
the `Read` and `Write` methods since non tag data (i.e., the pixel values) must
also be read. Compare the derived classes for `MetaCube` and `MetaImage`.

### MetaObjects

In this section we describe the metaObjects which have been implemented already.
If you want to implement other objects, you can easily derive these classes.
`metaObject` is the base class for metaIO. `metaScene` and `metaGroup` are also
a useful objects that support multiple metaObjects. All these objects are
described in details next.

**Constructors**

-   Simple constructor: `MetaObject(void);`
-   Read a metafile and store the result in the current object:
    `MetaObject(const char * _fileName);`
-   Define the dimension of the object at construction time:
    `MetaObject(unsigned int dim);`

**Member functions**

-   Specify the filename to read (Optional):
    -   `void FileName(const char *_fileName);`
    -   `const char * FileName(void) const;`
-   Read a MetaFile: `bool Read(const char * _fileName=NULL);`
-   Write a MetaFile:
    -   `bool Write(const char * _fileName=NULL);`
    -   `virtual bool Append(const char *_headName=NULL);`
-   Print the info about the metaObject: `virtual void PrintInfo(void) const;`
-   Clear the information as well as the data of the metObject: `virtual void
    Clear(void);`

**Field descriptions**

-   Name:
    -   `void Name(const char *_Name);`
    -   `const char * Name(void) const;`
-   Color:
    -   `const float * Color(void) const;`
    -   `void Color(float _r, float _g, float _b, float _a);`
    -   `void Color(const float * _color);`
-   ID: ID number of the current metaObject
    -   `void ID(int _id);`
    -   `int ID(void) const;`
-   Parent ID: ID number of the parent metaObject
    -   `void ParentID(int _parentId);`
    -   `int ParentID(void) const;`
-   Binary Data: Specify if the data is binary or not
    -   `void BinaryData(bool _binaryData);`
    -   `bool BinaryData(void) const;`
-   Binary Data Byte Order:
    -   `void BinaryDataByteOrderMSB(bool _binaryDataByteOrderMSB);`
    -   `bool BinaryDataByteOrderMSB(void) const;`
-   Comments:
    -   `const char * Comment(void) const;`
    -   `void Comment(const char * _comment);`
-   Object Typename and optional subtype (i.e. the type of the object)
    -   `const char * ObjectTypeName(void) const;`
    -   `void ObjectTypeName(const char * _objectTypeName);`
    -   `const char * ObjectSubTypeName(void) const;`
    -   `void ObjectSubTypeName(const char * _objectSubTypeName);`

**Associated transformations**

Physical location (in millimeters and with respect to machine coordinate system
or the patient) of the first element in the image. Physical orientation of the
object is defined as an NDims x NDims matrix that is serialized in a
column-major format in MetaIO files.

-   Offset: (equiv. to position and origin)
    -   `const float * Offset(void) const;`
    -   `float Offset(int _i) const;`
    -   `void Offset(const float * _position);`
    -   `void Offset(int _i, float _value);`
-   Position: (equiv. to offset and origin)
    -   `const float * Position(void) const;`
    -   `float Position(int _i) const;`
    -   `void Position(const float * _position);`
    -   `void Position(int _i, float _value);`
-   Origin: (equiv. to offset and position)
    -   `const float * Origin(void) const;`
    -   `float Origin(int _i) const;`
    -   `void Origin(const float * _position);`
    -   `void Origin(int _i, float _value);`
-   Rotation: (equiv. to orientation and transformMatrix)
    -   `const float * Rotation(void) const;`
    -   `float Rotation(int _i, int _j) const;`
    -   `void Rotation(const float * _orientation);`
    -   `void Rotation(int _i, int _j, float _value);`
-   Orientation: (equiv. to rotation and transformMatrix)
    -   `const float * Orientation(void) const;`
    -   `float Orientation(int _i, int _j) const;`
    -   `void Orientation(const float * _orientation);`
    -   `void Orientation(int _i, int _j, float _value);`
-   TransformMatrix: (equiv. to rotation and orientation)
    -   `const float * TransformMatrix(void) const;`
    -   `float TransformMatrix(int _i, int _j) const;`
    -   `void TransformMatrix(const float * _transformMatrix);`
    -   `void TransformMatrix(int _i, int _j, float _value);`
-   Center of rotation of the object:
    -   `const float * CenterOfRotation(void) const;`
    -   `float CenterOfRotation(int _i) const;`
    -   `void CenterOfRotation(const float * _position);`
    -   `void CenterOfRotation(int _i, float _value);`
-   Anatomical Orientation:
    -   `const char * AnatomicalOrientationAcronym(void) const;`
    -   `const MET_OrientationEnumType * AnatomicalOrientation(void) const;`
    -   `MET_OrientationEnumType AnatomicalOrientation(int _dim) const;`
    -   `void AnatomicalOrientation(const char *_ao);`
    -   `void AnatomicalOrientation(const MET_OrientationEnumType *_ao);`
    -   `void AnatomicalOrientation(int _dim, MET_OrientationEnumType _ao);`
    -   `void AnatomicalOrientation(int _dim, char ao);`
-   Element Spacing: Physical Spacing (in same units as position)
    -   `const float * ElementSpacing(void) const;`
    -   `float ElementSpacing(int _i) const;`
    -   `void ElementSpacing(const float * _elementSpacing);`
    -   `void ElementSpacing(int _i, float _value);`

For simplicity, some dynamic functions have been recently added. They allow the
user to add fields dynamically.

The function `AddUserField` is defined by:
```

template <class T>
bool AddUserField(const char* _fieldName, MET_ValueEnumType _type, int _length, T *_v, bool _required=true, int _dependsOn=-1 )

```

The user may also want to clear the fields created by using `ClearUserFields()`.

To determine the value of a field: `void* GetUserField(const char* _name);`

Note: When using `GetUserField()` function, the user is responsible for the
deletion of the pointer created. See the following example for details.

**Example**

```

/** We create a simple 3D metaObject with some properties */
MetaObject tObj(3);                      // Create a 3D metaObject
tObj.FileName("testObject.txt");         // Define the name of the file
tObj.Comment("TestObject");              // Add some comments
tObj.ObjectTypeName("Object");           // Define the type of the object
tObj.ObjectSubTypeName("MinorObject");   // and the subtype as well

/** We now define the position and the orientation as well as the spacing of the created object */
// The position part
tObj.Position(0, 1);
tObj.Position(1, 2);
tObj.Position(2, 3);

// The orientation part
float orient;
int i;
for(i=0; i<9; i++)
{
orient[i] = 0;
}
orient = 1;
orient = 1;
orient = 1;
tObj.Orientation(orient);

// The element spacing part
tObj.ElementSpacing(0, 1);
tObj.ElementSpacing(1, 2);
tObj.ElementSpacing(2, 1);

/** Add user's defined fields */
tObj.AddUserField("MyName", MET_STRING, strlen("JulienAndStephen"), "JulienAndStephen");

/** Write the object */
tObj.Write();

/** Clear completely the object */
tObj.Clear();
tObj.ClearUserFields();

/** Specify that we want to read the field ‘MyName’ */
tObj.AddUserField("MyName", MET_STRING);

/** Read the object */
tObj.Read("testObject.txt");

/** Print the object */
tObj.PrintInfo();

/** Get the name in the file */
char* name = static_cast<char*>(tObj.GetUserField("MyName"));
std::cout << name << std::endl;

/** delete the allocated pointer */
delete [] name;

```

## Types of MetaObjects

All of the following objects derive from `metaObject`.

### MetaBlob

A blob is defined by a list of points that describe the object. The points can
be inside the object (if obtained by connected-component for instance) or only
on the surface. Note that a color (RGBA) can be associated which each point.

The required fields are:

-   The number of points defining the object: `NPoints(int npnt);`
-   How the position of the points is stored in the file. By default the
    configuration is x y z red green blue alpha: `PointDim(const char*
    pointDim);`

To access the internal list of points user should use the `GetPoints()` function
which returns the internal list by reference. Note that the list is a list of
pointers to point and is deleted automatically by the object itself so the user
does not need to free the allocated memory.

**Example**

```

/** Create a 3D blob */
MetaBlob blob(3);
blob.ID(0);             // define the ID of the blob

/** Add 10 points to the blob */
BlobPnt* pnt;
unsigned int i;
for(i=0;i<10;i++)
{
pnt = new BlobPnt(3);
pnt->m_X=(float)0.2;
pnt->m_X[^1]=i;
pnt->m_X=i;
blob.GetPoints().push_back(pnt); // push the created point into the list of points
}

/** Write the blob in binary format */
blob.BinaryData(true);
blob.ElementType(MET_FLOAT);
blob.Write("myBlob.meta");

/** Read the file */
blob.Read("myBlob.meta");
blob.PrintInfo();

/** Access the list of points */
std::cout << "Accessing pointlist..." << std::endl;
MetaBlob::PointListType plist = blob.GetPoints();
MetaBlob::PointListType::const_iterator it = plist.begin();
while(it != plist.end())
{
for(unsigned int d = 0; d < 3; d++)
{
std::cout << (*it)->m_X[d] << " ";
}
std::cout << std::endl;
it++;
}

```

### MetaEllipse

`MetaEllipse` is an N-Dimensional object to define ellipsoids like circles,
spheres or even hyper-ellipsoids.

The only field you need to provide is the Radius.

There are several ways to input the radius:

-   As an array of floats: `void Radius(const float* radius);`
-   As a single value which means that we are defining an hyper-sphere: `void
    Radius(float radius);`
-   A convenient way to define a 2D ellipse: `void Radius(float r1,float r2);`
-   A convenient way to define a 3D ellipse: `void Radius(float r1,float r2,
    float r3);`

**Example**

```

/** Create a sphere */
MetaEllipse myEllipse (3);
myEllipse ->Radius(3); // radius of 3

```

### MetaGroup

`MetaGroup` does not have added functionalities compared to `metaObject`. It
allows to group object in a metafile.

### MetaImage

**Constructors**

-   Simple constructor by specifying the filename: `MetaImage(const char
    *_headerName);`
-   Constructor by shared memory: `MetaImage(MetaImage *_im);`
-   Other constructors:
    -   `MetaImage(int _nDims, const int * _dimSize, const float
        *_elementSpacing, MET_ValueEnumType _elementType, int
        _elementNumberOfChannels=1, void *_elementData=NULL);`
    -   `MetaImage(int _x, int _y, float _elementSpacingX, float
        _elementSpacingY, MET_ValueEnumType _elementType, int
        _elementNumberOfChannels=1, void *_elementData=NULL);`
    -   `MetaImage(int _x, int _y, int _z, float _elementSpacingX, float
        _elementSpacingY, float _elementSpacingZ, MET_ValueEnumType
        _elementType, int _elementNumberOfChannels=1, void *_elementData=NULL);`

**Member functions**

-   HeaderSize: Return the size of the header: `int HeaderSize(void) const;`
-   Quantity: Total number of elements in the image: `int Quantity(void) const;`
-   SubQuantity: Number of elements in image spanning sub-dimensions. E.g.,
    elements per line, 2D sub-image, 3D sub-volume.
    -   `const int * SubQuantity(void) const;`
    -   `int SubQuantity(int _i) const;`
-   ElementMin/Max: The default max returned is the largest allowed by
    ElemNBytes (12 bit uint16_t will give 4096 max). This may not represent the
    true max. Use `_reCalc=true` to force a calcuation of the actual max element
    value.
    -   `bool ElementMinMaxValid(void) const;`
    -   `void ElementMinMaxValid(bool _elementMinMaxValid);`
    -   `void ElementMinMaxRecalc(void);`
    -   `double ElementMin(void) const;`
    -   `void ElementMin(double _elementMin);`
    -   `double ElementMax(void) const;`
    -   `void ElementMax(double _elementMax);`
-   ElementByteOrderSwap: These functions are available only after
    `ReadImageData()` or if `_read_and_close=TRUE` when read.
    -   `void ElementByteOrderSwap(void);`
    -   `bool ElementByteOrderFix(void);`
-   ConverTo: Converts to a new data type. Rescales using Min and Max.
    -   `bool ConvertElementDataTo(MET_ValueEnumType _elementType=MET_UCHAR,
        double _toMin=0, double _toMax=0);`

**Field descriptions**

-   Modality: Specify the modality of the image
    -   `MET_ImageModalityEnumType Modality(void) const;`
    -   `void Modality(MET_ImageModalityEnumType _modality);`
-   Dimension size: Specify the size of the image in each dimension
    -   `void DimSize(const int * _dimSize);`
    -   `void DimSize(int _i, int _value);`
-   SequenceID: DICOM designation of this image relative to other images
    acquired at the same time
    -   `const float * SequenceID(void) const;`
    -   `float SequenceID(int _i) const;`
    -   `void SequenceID(const float * _sequenceID);`
    -   `void SequenceID(int _i, float _value);`
-   ElementSize: Optional Field. Physical size (in MM) of each element in the
    image (0 = xSize, 1 = ySize, 2 = zSize)
    -   `const float * ElementSize(void) const;`
    -   `float ElementSize(int i) const;`
    -   `void ElementSize(const float * _pointSize);`
    -   `void ElementSize(int _i, float _value);`
-   ElementType: Pixel type
    -   `MET_ValueEnumType ElementType(void) const;`
    -   `void ElementType(MET_ValueEnumType _elementType);`
-   ElementNumberOfChannels: Number of channels
    -   `int ElementNumberOfChannels(void) const;`
    -   `void ElementNumberOfChannels(int _elementNumberOfChannels);`
-   ElementData: Returns a pointer to the data.
    -   `void * ElementData(void);`
    -   `double ElementData(int _i) const;`
    -   `void ElementData(void * _data);`
    -   `bool ElementData(int _i, double _v);`
-   ElementDataFileName: Set/Get the filename
    -   `const char * ElementDataFileName(void) const;`
    -   `void ElementDataFileName(const char * _dataFileName);`

**Example**

```

ObjectType = Image
NDims = 2
BinaryData = True
BinaryDataByteOrderMSB = False
ElementSpacing = 1 2
DimSize = 8 8
ElementType = MET_CHAR
ElementDataFile = LOCAL
[Pixel Data]

```

### MetaLandmark

`MetaLandmark` is a simple list of landmarks.

The number of landmarks defining the object is set using the function
`NPoints(int npnt);`

How the position of the points is stored in the file: By default the
configuration is x y z red green blue alpha: `PointDim(const char* pointDim);`

To access the internal list of points user should use the `GetPoints()` function
which returns the internal list by reference. Note that the list is a list of
pointers to point and is deleted automatically by the object itself so the user
does not need to free the allocated memory.

**Example**

```

/** Create a 3D Landmark */
MetaLandmark Landmark(3);
Landmark.ID(0);
LandmarkPnt* pnt;

/** Add some landmarks to the list of landmark points*/
for(unsigned int i=0;i<10;i++)
{
pnt = new LandmarkPnt(3);
pnt->m_X=(float)0.2;
pnt->m_X[^1]=i;
pnt->m_X=i;
Landmark.GetPoints().push_back(pnt);
}

```

### MetaLine

A `metaLine` is actually a polyline defined by a list of connected points. A
point on the line has a given position, a normal and a color.

To set the position the local variable `m_X` should be filled in the point
structure. The variable `m_V` which is a double pointer to a float is used to
assess the normal. The normal has the dimension of the object minus one since a
metaLine in a 3D space will have two normals (a plane).

Note that the user does not need to allocate the memory for those variables,
this is done automatically in the constructor of the point.

To access the internal list of points user should use the `GetPoints()` function
which returns the internal list by reference. Note that the list is a list of
pointers to point and is deleted automatically by the object itself so the user
does not need to free the allocated memory.

**Example**

```

/** Create a 3D MetaLine */
MetaLine Line(3);
LinePnt* pnt;
for(unsigned int i=0;i<10;i++)
{
pnt = new LinePnt(3);
/** Define the position */
pnt->m_X=(float)0.2;
pnt->m_X[^1]=i;
pnt->m_X=i;

/** Define the normals */
pnt->m_V=(float)0.3;
pnt->m_V[^1]=i;
pnt->m_V=i;
pnt->m_V[^1]=(float)0.4;
pnt->m_V[^1][^1]=i+1;
pnt->m_V[^1]=i+1;
Line.GetPoints().push_back(pnt); // Corrected: Line, not Line->
}
/** Write the result */
Line.BinaryData(true);
Line.Write("myLine.meta");

```

### MetaSurface

The definition of a `metaSurface` is quite similar to the `metaLine`’s, except
for the normal which is only a NDim vector (i.e. an array of floats) where NDim
is the dimension of the metaObject.

To access the internal list of points user should use the `GetPoints()` function
which returns the internal list by reference. Note that the list is a list of
pointers to point and is deleted automatically by the object itself so the user
does not need to free the allocated memory.

**Example**

```

MetaSurface surface(3);
SurfacePnt* pnt;
for(unsigned int i=0;i<10;i++)
{
pnt = new SurfacePnt(3);
/** Position */
pnt->m_X=(float)0.2;
pnt->m_X[^1]=i;
pnt->m_X=i;
/* Normal */
pnt->m_V=(float)0.8;
pnt->m_V[^1]=i;
pnt->m_V=i;
surface.GetPoints().push_back(pnt); // Corrected: surface, not surface->
}

```

### MetaTube

A `metaTube` is a tubular structure defined by a list of connected points (like
a `metaLine`) but more fields have been added for a complete representation,
especially the one of blood vessels. To specify a point that belongs to the
tube, the user can define: the position, the radius at that point, the
normal(s), the tangent, the color, the Identification number, the medialness,
the branchness, the ridgeness, and three alpha values that represents the ratio
of the eigen values at that points.

Note that `metaTube` supports only 2D and 3D tubes.

Also for a `metaTube`, the ID of the root can be specified by the command
`Root(int rootID)` and the ID of the parent point can also be assessed using
`ParentPoint(int parentpoint)`.

To access the internal list of points user should use the `GetPoints()` function
which returns the internal list by reference. Note that the list is a list of
pointers to point and is deleted automatically by the object itself so the user
does not need to free the allocated memory.

**Example**

```

/** Create a 3D tube*/
MetaTube* tube1 = new MetaTube(3);
tube1->ID(0);
/** Add 10 points to the list of tubePoints */
TubePnt* pnt;
for(unsigned int i=0;i<10;i++)
{
pnt = new TubePnt(3);
pnt->m_X=i; // position
pnt->m_X[^1]=i;
pnt->m_X=i;
pnt->m_R=i; // radius
tube1->GetPoints().push_back(pnt);
}

```

### MetaScene

A `metaScene` is a metaObject that contains a flat list of metaObjects.

**Member functions**

-   Add an object to the scene: `void AddObject(MetaObject* object);`
-   Return the number of objects in the scene: `int NObjects(void) const;`
-   Get a list of objects present in the scene: `ObjectListType *
    GetObjectList(void) {return &amp; m_ObjectList;}`

**Example**

```

/** Define a 3D Scene */
MetaScene scene(3); // Corrected: was s->
MetaEllipse * e1 = new MetaEllipse(3);
e1->ID(0);
e1->Radius(3);
// MetaGroup g0; // Removed, appears unused/conflicting with g1 definition
MetaGroup * g1 = new MetaGroup(3);
g1->ID(2);
scene.AddObject(g1); // Corrected: scene, not s
scene.AddObject(e1); // Corrected: scene, not s
scene.Write("scene.scn"); // Corrected: scene, not s
scene.Clear();
scene.Read("scene.scn"); // Corrected: scene, not s

```

**Output File Example**

Here is the example of a metafile with a scene that contains metaObjects:

```

ObjectType = Scene
NDims = 3
NObjects = 3

ObjectType = Group
NDims = 3
ID = 2
EndGroup =

ObjectType = Ellipse
NDims = 3
ID = 0
ParentID = 2
Radius = 1 2 3

ObjectType = Line
NDims = 3
ID = 0
BinaryData = False
BinaryDataByteOrderMSB = False
ElementType = MET_FLOAT
PointDim = x y z v1x v1y v1z
NPoints = 3
Points =
1 2 3 0 0 0
1 2 3 0 0 0
1 2 3 0 0 0

ObjectType = Landmark
NDims = 3
ID = 0
BinaryData = True
BinaryDataByteOrderMSB = False
ElementType = MET_FLOAT
PointDim = x y z red green blue alpha
NPoints = 2
Points =
1 2 3 1.0 0.0 0.0 1.0
1 2 3 1.0 0.0 0.0 1.0
1 2 3 1.0 0.0 0.0 1.0

```

## Spatial Objects

MetaIO has also been chosen to support Spatial Objects IO. To obtain a complete
documentation of Spatial Objects and how to read/write them out please see the
Insight user’s manual available at the ITK website.

## Reference: Tags of MetaImage

### MetaObject Tags

The tags of `MetaObject` are:

-   `Comment`
    -   Type: `MET_STRING`
    -   Description: User defined - arbitrary
-   `ObjectType`
    -   Type: `MET_STRING`
    -   Description: Defined by derived objects – e.g., `Tube`, `Image`
-   `ObjectSubType`
    -   Type: `MET_STRING`
    -   Description: Defined by derived objects – currently not used
-   `TransformType`
    -   Type: `MET_STRING`
    -   Description: Defined by derived objects – e.g., `Rigid`
-   `NDims`
    -   Type: `MET_INT`
    -   Description: Defined at object instantiation
-   `Name`
    -   Type: `MET_STRING`
    -   Description: User defined
-   `ID`
    -   Type: `MET_INT`
    -   Description: User defined else -1
-   `ParentID`
    -   Type: `MET_INT`
    -   Description: User defined else -1
-   `BinaryData`
    -   Type: `MET_STRING` (`True`/`False`)
    -   Description: Are the data associated with this object stored as Binary
        or ASCII. Defined by derived objects.
-   `ElementByteOrderMSB` / `BinaryDataByteOrderMSB`
    -   Type: `MET_STRING` (`True`/`False`)
    -   Description: Specifies byte order for binary data (MSB=True, LSB=False).
-   `Color`
    -   Type: `MET_FLOAT_ARRAY[^4]`
    -   Description: R, G, B, alpha (opacity)
-   `Position` (also `Offset`, `Origin`)
    -   Type: `MET_FLOAT_ARRAY[NDims]`
    -   Description: X, Y, Z,… of real-world coordinate of 0,0,0 index of object
-   `Orientation` (also `Rotation`, `TransformMatrix`)
    -   Type: `MET_FLOAT_MATRIX[NDims][NDims]`
    -   Description:
        -   [^0][^0],[^0][^1],[^0][^2]... specify X, Y, Z… direction in
            real-world of X-axis of object
        -   [^1][^0],[^1][^1],[^1][^2]... specify X, Y, Z… direction in
            real-world of Y-axis of object, etc.
        -   Serialized column-major.
-   `AnatomicalOrientation`
    -   Type: `MET_STRING`
    -   Description: This is a convenience tag, to be used and maintained by
        applications. Changing this tag in a MetaIO file or via the MetaIO API
        will not cause the MetaIO library to resample or change the direction
        matrix of an image. It is up to an application to correctly read, write,
        and maintain this tag.
        -   History: Labels ("from" vs "to"), LPS/RAI conventions. DICOM and ITK
            use "to" labels (e.g., LPS). The direction matrix maps index space
            (i,j,k) to physical space (x,y,z). `AnatomicalOrientation`
            originally specified space (LPS/RAS), but later implementation
            (early 2000s, coinciding with ITK direction matrix) changed it to
            describe patient orientation in index space (assuming LPS), making
            it somewhat redundant with the direction matrix.
        -   Recommendation: Use the definition currently implemented by ITK,
            which represents the most common usage (a 3-letter code derived from
            the direction matrix, assuming an LPS space).
-   `ElementSpacing`
    -   Type: `MET_FLOAT_ARRAY[NDims]`
    -   Description: The distance between voxel centers (or equivalent element
        centers).

### Tags Added by MetaImage

In addition to the above tags, `MetaImage` provides the following tags:

-   `DimSize`
    -   Type: `MET_INT_ARRAY[NDims]`
    -   Description: Number of elements per axis in data.
-   `HeaderSize`
    -   Type: `MET_INT`
    -   Description: Number of Bytes to skip at the head of each data file.
        -   Specify –1 to have MetaImage calculate the header size based on the
            assumption that the data occurs at the end of the file.
        -   Specify 0 if the data occurs at the beginning of the file.
-   `Modality`
    -   Type: `MET_STRING`
    -   Description: One of enum type: `MET_MOD_CT`, `MET_MOD_MR`, `MET_MOD_US`…
        See `metaImageTypes.h`.
-   `SequenceID`
    -   Type: `MET_INT_ARRAY[^4]` (Note: Original doc says `MET_FLOAT_ARRAY`,
        but context suggests `INT`)
    -   Description: Four values comprising a DICOM sequence: Study, Series,
        Image numbers.
-   `ElementMin`
    -   Type: `MET_FLOAT`
    -   Description: Minimum value in the data.
-   `ElementMax`
    -   Type: `MET_FLOAT`
    -   Description: Maximum value in the data.
-   `ElementNumberOfChannels`
    -   Type: `MET_INT`
    -   Description: Number of values (of type `ElementType`) per voxel.
-   `ElementSize`
    -   Type: `MET_FLOAT_ARRAY[NDims]`
    -   Description: Physical size of each voxel.
-   `ElementType`
    -   Type: `MET_STRING`
    -   Description: One of enum type: `MET_UCHAR`, `MET_CHAR`… as described in the following table:

        | ElementType tag      | Description |
        | -------------------- | ----------- |
        | MET_NONE             | none |
        | MET_ASCII_CHAR       | ASCII character |
        | MET_CHAR             | 8-bit signed integer |
        | MET_UCHAR            | 8-bit unsigned integer |
        | MET_SHORT            | 16-bit signed integer |
        | MET_USHORT           | 16-bit unsigned integer |
        | MET_INT              | 32-bit signed integer |
        | MET_UINT             | 32-bit unsigned integer |
        | MET_LONG             | 32-bit signed integer |
        | MET_ULONG            | 32-bit unsigned integer |
        | MET_LONG_LONG        | 64-bit signed integer |
        | MET_ULONG_LONG       | 64-bit unsigned integer |
        | MET_FLOAT            | 32-bit single precision floating point |
        | MET_DOUBLE           | 64-bit double precision floating point |
        | MET_STRING           | string of 8-bit characters |
        | MET_CHAR_ARRAY       | array of 8-bit signed integers |
        | MET_UCHAR_ARRAY      | array of 8-bit unsigned integers |
        | MET_SHORT_ARRAY      | array of 16-bit signed integers |
        | MET_USHORT_ARRAY     | array of 16-bit unsigned integers |
        | MET_INT_ARRAY        | array of 32-bit signed integers |
        | MET_UINT_ARRAY       | array of 32-bit unsigned integers |
        | MET_LONG_ARRAY       | array of 32-bit signed integers |
        | MET_ULONG_ARRAY      | array of 32-bit unsigned integers |
        | MET_LONG_LONG_ARRAY  | array of 64-bit signed integers |
        | MET_ULONG_LONG_ARRAY | array of 64-bit unsigned integers |
        | MET_FLOAT_ARRAY      | array of 32-bit single precision floating points |
        | MET_DOUBLE_ARRAY     | array of 64-bit double precision floating points |
        | MET_FLOAT_MATRIX     | matrix of 32-bit single precision floating points |
        | MET_OTHER            | other |

        Note: this description (including the specified number of bits) applies
        to all supported platforms, independent of platform specific sizes of
        built-in C++ types (like `sizeof(long)`).

-   `ElementDataFile`
    -   Type: `MET_STRING`
    -   Description: Specifies the location of the pixel/voxel data. One of the
        following formats:
        -   `filename.raw`: Name of the single file containing the data (for
            "brick-of-bytes").
        -   `printf_format min max [step]`: A C-style printf format string
            followed by the minimum, maximum, and optional step integer values.
            Used to generate a list of filenames for data split across multiple
            files (e.g., slices). Each file must contain an (N-1)D block of
            data. Example: `slice.%03d.raw 1 100 1`.
        -   `LIST [Dims]`: Indicates that a list of filenames follows on
            subsequent lines (one filename per line). Each file contains a block
            of data. By default, each file is assumed to contain an (N-1)D
            block. An optional dimension specifier (e.g., `2D`) can be provided
            to indicate the dimensionality of data within each file. Example:
            `LIST 2D`.
        -   `LOCAL`: Indicates that the binary data begins immediately on the
            next line within the same `.mhd` file.
