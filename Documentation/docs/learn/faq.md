# Frequently asked questions (FAQ)

## General information and availability

### Do I need VTK to build ITK?

With the default configuration, ITK does not require VTK. However, some
optional modules (e.g. [ITKVTKGlue](https://github.com/InsightSoftwareConsortium/ITKVtkGlue)) do require VTK.

### Data formats

#### What 3D data file formats can ITK import and export?

The following table identifies the file formats that ITK can read/write
via a factory mechanism based on the file extension
([`itk::ImageIOFactory`](https://itk.org/Doxygen/html/classitk_1_1ImageIOFactory.html)). Some proprietary files format are only imported.

More information on supported file formats is available on the
[ITK-Wasm Image File Formats](https://wasm.itk.org/en/latest/introduction/file_formats/images.html)
site.

Other file formats not listed below, such as [OME-NGFF](https://ngff.openmicroscopy.org/0.4/),
may be supported by ITK's remote modules. See for example the
[ITKIOOMEZarrNGFF](https://github.com/InsightSoftwareConsortium/ITKIOOMEZarrNGFF) remote
module for [Zarr](https://zarr.dev/)-backed OME-NGFF read/write capabilities.

[itkwidgets](https://itkwidgets.readthedocs.io/) offers a Python
interface for interactive visualization on the web of all of these
images.

| File Format | Read/Write | Import |
| ----------- | ---------- | ------ |
| Analyze 7.5 | `itk::AnalyzeImageIO` | |
| [BioRad](https://www.bio-rad.com/) | | |
| [BMP](https://en.wikipedia.org/wiki/BMP_file_format) † | [`itk::BMPImageIO`](https://itk.org/Doxygen/html/classitk_1_1BMPImageIO.html) | |
| [DICOM](https://dicom.nema.org/) | [`itk::GDCMImageIO`](https://itk.org/Doxygen/html/classitk_1_1GDCMImageIO.html) | |
| [DICOM Series](https://dicom.nema.org/) | | |
| GE 4x | [`itk::GE4ImageIO`](https://itk.org/Doxygen/html/classitk_1_1GE4ImageIO.html) | |
| GE 5x | [`itk::GE5ImageIO`](https://itk.org/Doxygen/html/classitk_1_1GE5ImageIO.html) | |
| GE Advantage Windows | | [`itk::GEAdwImageIO`](https://itk.org/Doxygen/html/classitk_1_1GEAdwImageIO.html) |
| [GIPL](https://www.ncbi.nlm.nih.gov/pubmed/12956259) (Guy's Image Processing Lab) (.gipl) | [`itk::GiplImageIO`](https://itk.org/Doxygen/html/classitk_1_1GiplImageIO.html) | |
| IPLCommon | [`itk::IPLCommonImageIO`](https://itk.org/Doxygen/html/classitk_1_1IPLCommonImageIO.html) | |
| [ITK HDF5](https://support.hdfgroup.org/HDF5/) | | |
| [JPEG](https://en.wikipedia.org/wiki/JPEG_File_Interchange_Format) † | [`itk::JPEGImageIO`](https://itk.org/Doxygen/html/classitk_1_1JPEGImageIO.html) | |
| [LSM](https://www.openwetware.org/wiki/Dissecting_LSM_files) | [`itk::LSMImageIO`](https://itk.org/Doxygen/html/classitk_1_1LSMImageIO.html) | |
| [MetaImage](https://itk.org/Wiki/ITK/MetaIO/Documentation) (.mha/.mhd) | [`itk::MetaImageIO`](https://itk.org/Doxygen/html/classitk_1_1MetaImageIO.html) | |
| [MINC 2.0](https://en.wikibooks.org/wiki/MINC/SoftwareDevelopment/MINC2.0_File_Format_Reference) (Medical Image NetCDF) | [`itk::MINCImagIO`](https://itk.org/Doxygen/html/classitk_1_1MINCImageIO.html) | |
| [MGH](https://surfer.nmr.mgh.harvard.edu/fswiki/FsTutorial/MghFormat) | [`itk:MGHImageIO`](https://itk.org/Doxygen/html/classitk_1_1MGHImageIO.html) | |
| [MRC](http://www.ccpem.ac.uk/mrc_format/mrc_format.php) | [`itk::MRCImageIO`](https://itk.org/Doxygen/html/classitk_1_1MRCImageIO.html) | |
| [NIfTI](https://nifti.nimh.nih.gov/nifti-1) (.nii) | [`itk::NiftiImageIO`](https://itk.org/Doxygen/html/classitk_1_1NiftiImageIO.html) | |
| [NRRD](http://teem.sourceforge.net/nrrd/format.html) (.nhdr/.nrrd) | [`itk::NrrdImageIO`](https://itk.org/Doxygen/html/classitk_1_1NrrdImageIO.html) | |
| [PNG](https://en.wikipedia.org/wiki/Portable_Network_Graphics) † | [`itk::PNGImageIO`](https://itk.org/Doxygen/html/classitk_1_1PNGImageIO.html) | |
| Raw ‡ | [`itk::RawImageIO`](https://itk.org/Doxygen/html/classitk_1_1RawImageIO.html) | |
| [Scanco microCT volume file format](https://www.scanco.ch/en/support/customer-login/faq-customers/faq-customers-import-export.html) | | |
| Siemens Vision | | [`itk::SiemensVisionImageIO`](https://itk.org/Doxygen/html/classitk_1_1SiemensVisionImageIO.html) |
| Stimulate (spr/sdt) | [`itk::StimulateImageIO`](https://itk.org/Doxygen/html/classitk_1_1TIFFImageIO.html) | |
| [TIFF](https://en.wikipedia.org/wiki/TIFF) | [`itk::TIFFImageIO`]() | |
| [Varian FDF](https://github.com/InsightSoftwareConsortium/ITKIOFDF) | | |
| [VTK Structured Points](https://www.vtk.org/VTK/img/file-formats.pdf) | [`itk::VTKImageIO`](https://itk.org/Doxygen/html/classitk_1_1VTKImageIO.html) | |

† BMP, PNG and JPEG are not very good formats for 3D medical images.
They only support 2D images and a limited set of pixel types such as
unsigned char, and unsigned short. The great advantage of BMP, PNG and
JPEG is that you can view them with almost any image viewer. It is
possible to read/write 3D datasets as collections of 2D slices each one
in an independent BMP, PNG or JPEG file by using the
[`ImageSeriesReader`](https://itk.org/Doxygen/html/classitk_1_1ImageSeriesReader.html) and [`itk::ImageSeriesWriter`](https://itk.org/Doxygen/html/classitk_1_1ImageSeriesWriter.html).

‡ It is higly recommended not using this format. RAW is not a format, it
is insufficient, inconsistent and simply dangerous. A RAW file without a
header is useless, and once you add a header, it is not RAW anymore.

#### What if my file format is not supported by ITK?

If ITK doesn't have a specific file format reader at this point, you
may try converting this specific file format image to a format like PNG,
for which ITK has readers.

The [imageio](https://imageio.readthedocs.io/en/stable/) Python package
contains a good set of tools for performing such conversions.

You can also take advantage of the VTK readers/importers. All you need
is then use the VTKImageToImage adaptor in order to convert the
[`vtkImageData`](https://vtk.org/doc/nightly/html/classvtkImageData.html) into an [`itk::Image`](https://itk.org/Doxygen/html/classitk_1_1Image.html).

Supported VTK file formats can be found in https://docs.vtk.org/en/latest/supported_data_formats.html.

### What mesh and point set file formats can ITK import and export?

ITK supports reading and writing the following mesh and point set file
formats:

| File Format | Read/Write | Import |
| ----------- | ---------- | ------ |
| [BYU](http://www.eg-models.de/formats/Format_Byu.html) |
 [`itk::BYUMeshIO`](https://itk.org/Doxygen/html/classitk_1_1BYUMeshIO.html) | |
| [FreeSurfer surface, binary and ASCII](http://www.grahamwideman.com/gw/brain/fs/surfacefileformats.htm) | [`itk::FreeSurferBinaryMeshIO`](https://itk.org/Doxygen/html/classitk_1_1FreeSurferBinaryMeshIO.html), [`itk::FreeSurferAsciiMeshIO`](https://itk.org/Doxygen/html/classitk_1_1FreeSurferAsciiMeshIO.html) | |
| [OFF](https://en.wikipedia.org/wiki/OFF_%28file_format%29) | [`ìtk::OFFMeshIO`](https://itk.org/Doxygen/html/classitk_1_1OFFMeshIO.html) | |
| [STL](https://en.wikipedia.org/wiki/STL_%28file_format%29) | | |
| [SWC Neuron Morphology](https://swc-specification.readthedocs.io/en/latest/) | | |
| [OBJ](https://en.wikipedia.org/wiki/Wavefront_.obj_file) | [`ìtk::OBJMeshIO`](https://itk.org/Doxygen/html/classitk_1_1OBJMeshIO.html) | |
| [VTK legacy file format for vtkPolyData](https://www.vtk.org/wp-content/uploads/2015/04/file-formats.pdf) | [`itk::VTKPolyDataMeshIO`](https://itk.org/Doxygen/html/classitk_1_1VTKPolyDataMeshIO.html) | |

More information on supported mesh and point set file formats is available on the
[ITK-Wasm Mesh and Point Set File Formats](https://wasm.itk.org/en/latest/introduction/file_formats/meshes.html)
site.

### What transform file formats can ITK import and export?

ITK supports reading and writing the following transform file formats:

| File Format | Read/Write | Import |
| ----------- | ---------- | ------ |
| InsightLegacy | [`itk::TxtTransformIOTemplate`](https://itk.org/Doxygen/html/classitk_1_1TxtTransformIOTemplate.html) | |
| ITK HDF5 | [`itk::HDF5TransformIOTemplate`](https://itk.org/Doxygen/html/classitk_1_1HDF5TransformIOTemplate.html) | |
| MATLAB | [`itk::MatlabTransformIOTemplate`](https://itk.org/Doxygen/html/classitk_1_1MatlabTransformIOTemplate.html) | |
| MINC (Medical Image NetCDF) | `itk::MINCTransformIOTemplate` | |

#### DICOM data

##### How do I read a volume from a DICOM series?

The following are alternative options for reading DICOM series. The
first one in the list is the recommended option. The others are provided
in case of desperation.

1. Use the [itk-wasm dicom](https://itk-wasm-dicom-docs.on.fleek.co/#/),
   package, which is available from [here](https://itk-wasm-dicom-python-docs.on.fleek.co/).
   It has the broadest support and is the easiest to use.
1. Use the [`itk::ImageSeriesReader`](https://itk.org/Doxygen/html/classitk_1_1ImageSeriesReader.html) in combination with the
   `DicomSeriesFileNames`. For a full example on how to do this,
   please look at the [`DicomSeriesReadImageWrite2.cxx`](https://github.com/InsightSoftwareConsortium/ITK/blob/master/Examples/IO/DicomSeriesReadImageWrite2.cxx)
   code.
1. Write a MetaImage header. This is a small text file holding
   information about the image: spacing, dimensions, pixelt type, etc.
   This header can hold the list of DICOM files you want to read. The
   only restriction is that the files must be uncompressed. You can
   take an existing MetaImage header and modify it in order to fit
   your needs.
1. Use `MRIConvert`: Jolinda Smith from the Lewis Center for
   Neuroimaging at the University of Oregon developed a nice
   application that allows you to load DICOM series and export them in
   MetaImage and Analyze format (among others). She graciously has made
   this application publicly available at:
   https://darkwing.uoregon.edu/~jolinda/MRIConvert/.
1. Jeff Brubaker and Stephen R. Aylward, at the UNC  CADDLab
   developed a DICOM query/move application called "MIND"
   (with the motto: "MIND is not DICOM"). This application loads
   DICOM files over the network and export them in MetaImage format.
   This application is open source and it is available at:
   http://www.jeffro.net/mind/ and http://caddlab.rad.unc.edu/software/MIND/.

##### How do I write a volume as DICOM series?

Use ITK in combination with GDCM: http://gdcm.sourceforge.net/.

GDCM is an open source package developed by the Creatis team at
INSA-Lyon. It is distributed under a license similar to ITK: http://gdcm.sourceforge.net/Copyright.html.

GCDM uses CMake in order to configure its build process, so you will
find a familiar setup. Once you download, configure and buid GDCM, you
can reconfigure ITK by running CMake in your ITK build, going into the
advanced options and enabling `USE_GDCM`.

For a code example on how to use GDCM for reading and writing DICOM
series, please refer to the source code in [`DicomSeriesReadImageWrite2.cxx`](https://github.com/InsightSoftwareConsortium/ITK/blob/master/Examples/IO/DicomSeriesReadImageWrite2.cxx).

You can always use the latest and greatest of GDCM, simply use an
installed version of GDCM and link ITK to it using `ITK_USE_SYSTEM_GDCM`.

### Which interpreted languages are supported by ITK?

ITK supports a number of language bindings, including Python and
JavaScript, among others, through [SimpleITK](https://simpleitk.org/)
and [ITK-Wasm](https://wasm.itk.org/en/latest/).

### How do I cite the use of ITK in a publication?

The Insight software may be cited by referencing the paper, the books,
and/or the web site.

- **The papers**

```
McCormick M, Liu X, Jomier J, Marion C, Ibanez L. ITK: enabling
reproducible research and open science. Front Neuroinform. 2014;8:13.
Published 2014 Feb 20. <doi:10.3389/fninf.2014.00013>
```

```
T.S. Yoo, M. J. Ackerman, W. E. Lorensen, W. Schroeder, V. Chalana, S.
Aylward, D. Metaxas, R. Whitaker. Engineering and Algorithm Design for
an Image Processing API: A Technical Report on ITK - The Insight
Toolkit. In Proc. of Medicine Meets Virtual Reality, J. Westwood, ed.,
IOS Press Amsterdam pp 586-592 (2002).
```

- **The books**

```
"The ITK Software Guide: Design and Functionality"
 Fourth Edition
 Johnson, McCormick, Ibanez.
 published by Kitware Inc.
 2015
 ISBN: 9781-930934-28-3
```

```
"The ITK Software Guide: Introduction and Development Guidelines"
 Fourth Edition
 Johnson, McCormick, Ibanez.
 published by Kitware Inc.
 2015
 ISBN: 9781-930934-27-6
```

```
"Insight into Images"
 edited by Terry Yoo
 published by A.K. Peters
 2004
 ISBN: 1-56881-217-5
```

```
"The ITK Software Guide"
 Second Edition
 Ibanez, Schroeder, Ng, Cates.
 published by Kitware Inc.
 2005
 ISBN: 1-930934-15-7
```

```
"The ITK Software Guide"
 First Edition
 Ibanez, Schroeder, Ng, Cates.
 published by Kitware Inc.
 2003
 ISBN: 1-930934-10-6
```

```
"Mastering CMake, A Cross-Platform Build System"
 K. Martin and B. Hoffman
 published by Kitware Inc.
 ISBN:  1-930934-09-2
```

**BibTeX citations**

```
@manual{johnson2015itk1,
author = {Hans J. Johnson and Matthew M .McCormick and Luis Ib\'{a}{\~n}ez},
title = {The ITK Software Guide Book 1: Introduction and Development Guidelines - Volume 1},
publisher = {Kitware, Inc.},
year = {2015}
}
```

```
@manual{johnson2015itk2,
author = {Hans J. Johnson and Matthew M .McCormick and Luis Ib\'{a}{\~n}ez},
title = {The ITK Software Guide Book 2: Design and Functionality - Volume 2},
publisher = {Kitware, Inc.},
year = {2015}
}
```

```
@manual{ITKSoftwareGuide,
author = {Luis Ib\'{a}{\~n}ez and William Schroeder and Lydia Ng and Joshua Cates},
title = {The {ITK} {S}oftware {G}uide},
organization = {Kitware, Inc.},
edition = {First},
year = {2003},
note = {ISBN 1-930934-10-6},
url = {}
}
```

```
@manual{ITKSoftwareGuideSecondEdition,
author = {Luis Ib\'{a}{\~n}ez and William Schroeder and Lydia Ng and Joshua Cates},
title = {The {ITK} {S}oftware {G}uide},
organization = {Kitware, Inc.},
edition = {Second},
year = {2005},
note = {ISBN 1-930934-15-7},
url = {}
}
```

```
@manual{ITKSoftwareGuideThirdEdition,
author = {Hans J. Johnson and Matthew M .McCormick and Luis Ib\'{a}{\~n}ez and The Insight Software Consortium},
title = {The {ITK} {S}oftware {G}uide},
organization = {Kitware, Inc.},
edition = {Third},
year = {2013},
note = {},
url = {}
}
```

- **A specific software version**

See [https://zenodo.org/record/3592082](https://zenodo.org/record/3592082).

If you have a publication that used ITK, please create a pull request to
add it to [`ITKBibliography.bib`](https://github.com/InsightSoftwareConsortium/insightsoftwareconsortium.org/blob/master/static/citations-visualization/ITKBibliography.bib).

If you want to include ITK in an acknowledgment section, a phrase
similar to the following may be used:

```
"This work benefited from the use of the Insight Segmentation
 and Registration Toolkit (ITK), an open source software
 developed as an initiative of the U.S. National Library
 of Medicine and available at www.itk.org."
```

and

```
"The multi-platform configuration tool CMake was used for
 configuring ITK and facilitating its use from our project.
 CMake was partially funded by the U.S. National Library of
 Medicine as part of the Insight Toolkit project. CMake is an
 open source system and it is freely available at www.cmake.org."
```

### Is ITK FDA-Approved?

Given the fact that ITK is a software toolkit, it cannot be the subject
of FDA approval as a medical device. We have discussed this topic in
several occasions and received advice from FDA representatives, that can
be summarized as follows:

```
"ITK is to be considered as an off-the-shelf (OTS) product that
 is used for supporting a higher level medical application/product.
 The developer of such application/product will be responsible for
 performing the validation processes described in FDA published
 guidelines for the development of software-related medical devices."
```

For mode details see the page [FDA guidelines for software development](fda_sw_development_guidelines.md).

## Using ITK

### How do I configure Vim for ITK coding style?

ITK provides a [`clang-format` configuration file](https://github.com/InsightSoftwareConsortium/ITK/blob/master/.clang-format)
that ensures that the code being typed follows the ITK coding style.

It suffices to tell Vim to use such configuration file.

## Contribute

### How do I make code contributions to ITK?

Please see the [Contribute to ITK page](https://docs.itk.org/en/latest/contributing/index.html).

## Testing

### Is ITK tested?

Please see this page: [ITK/Testing](https://itk.org/Wiki/ITK/Testing).

## Working with image data

### How do I iterate through all the pixels in an image?

Please see the [`ImageRegionIterator.cxx`](https://github.com/InsightSoftwareConsortium/ITK/blob/master/Examples/Iterators/ImageRegionIterator.cxx)
example.

### What are Hounsfield Units?

CT imaging data are measured in Hounsfield Units (HU), which is a
quantitative scale for describing radiodensity.

Hounsfield units were defined by Sir Godfrey N. Hounsfield, one of the
pioneers of Computer Tomography for clinical applications.

The units represent the linear attenuation of X-Rays for a particular
material.

The units scale is defined in such a way that level for Water is 0 and
the level for Air is -1000.

The attenuation of any other material is mapped linearly into this
range.

The table below shows the HU of various human tissues.

| Tissue | HU |
| ------ | -- |
| Bone | 1000 |
| Liver | [40, 60] |
| White matter | 46 |
| Grey matter| 43 |
| Blood | 40 |
| Muscle | [10, 40] |
| Kidney | 30 |
| Cerebrospinal fluid | 15 |
| Water | 0 |
| Fat | [-100, -50] |
| Air | -1000 |

More details can be found in the Wikipedia article [Houndfield Units](https://en.wikipedia.org/wiki/Hounsfield_scale).

### What are MRI T1 / T2 units?

MRI images measure two types of relaxation times, T1 and T2.

The units are millisecons and the two relaxation concepts relate to how
long it takes for the molecules in the tissue to realign themselves with
a magentic field after being perturbed.

Details on the MRI units are available on the [Relaxation (NMR)](https://en.wikipedia.org/wiki/Relaxation_%28NMR%29)
Wikipedia article.

### DICOM: Bits Allocated, Bits Stored and High Bit

A question that is often asked on insight-users is: I am trying to
read/write a DICOM image, but some of the DICOM fields have changed
after I write it back on disk.

Here is what you have on disk:

```
Bits Allocated = 16
Bits Stored    = 12
High Bit       = 11

                  |<------------------ pixel ----------------->|
    ______________ ______________ ______________ ______________
   |XXXXXXXXXXXXXX|              |              |              |
   |______________|______________|______________|______________|
    15          12 11           8 7            4 3            0
```

Here is what you generally get after writing by GDCM:

```
Bits Allocated = 16
Bits Stored    = 16
High Bit       = 15

   |<------------------------- pixel ------------------------->|
    ______________ ______________ ______________ ______________
   |              |              |              |              |
   |______________|______________|______________|______________|
    15          12 11           8 7            4 3            0
```

Since DICOM V3, you **cannot** store any overlay in the unused bits of
the `Pixel Data` (`0x7fe0,0x0010`), thus it imply that the unused bits are
**zeros**. In ITK, there is no such thing as 12-bit pixel type, thus
when instanciating your reader you declare a 16-bit pixel type. This
buffer is then passed back to GDCM which sees an array of 16bits values
and then simply used the simpliest way to store this buffer back on
disk: `Bits Stored = 16` and `High Bit = 15`. In case you have not made any
change to your pixel buffer, implicitly the first 4 bits will be zero
again. The only difference being that they are not as clearly declared
as 12 bits.

### I'm working with DICOM images, where can I find more information?

See the [DICOM](dicom.md) page.

## Data display

### What imaging software is available for displaying medical image files?

Please see the [third party applications](third_party_applications.md) page for
visualization applications that used ITK to perform image
reading/writing.

### When I view the output of my program, all I see is a black image. What went wrong?

ITK is capable of reading and writing data in a variety of formats and
bit depths. However many "consumer" image programs only work with
8-bit data, so when you try to display a 16-bit data file the image will
appear black. The solution is to use an image display program that
correctly handles these types of files. A small selection is listed
above.

[ImageMagick](https://imagemagick.org/index.php) is a particularly useful set
of tools for image display, manipulation and conversion.

[itkwidgets](https://itkwidgets.readthedocs.io/) is capable of
correctly loading and displaying all file types supported by ITK
(including anisotropic images).

## Registration

### Can ITK be used for multi-modal registration? For example, CT-MRI, or MRI-fMRI?

ITK provides several method for performing multi-modality registration.
These methods are described in detail in the "Registration" chapter of
[The ITK Software Guide](https://itk.org/ItkSoftwareGuide.pdf). In
particular, you will find interesting the section where the mutual
information metric is described.

Examples on multimodality registration are available in
[Examples/RegistrationITKv4](https://github.com/InsightSoftwareConsortium/ITK/tree/master/Examples/RegistrationITKv4).

It is strongly recommended that you read the chapter on "Geometric
Transformations" section, where resampling is explained, before you get
involved with image registration methods. That will save you a lot of
misunderstandings that are common in new users.

You will also find useful the tutorial sessions, in particular [the
overview](http://www.itk.org/CourseWare/Training/RegistrationMethodsOverview.pdf).

## Common problems

### Why can't I read any image files? or Why am I getting the exception message: `There are no registered IO factories.`?

When trying to read or write a file using the [`itk::ImageFileReader`](https://itk.org/Doxygen/html/classitk_1_1ImageFileReader.html)
or the [`itk::ImageFileWriter`](https://itk.org/Doxygen/html/classitk_1_1ImageFileWriter.html) I got an exception with the following message:

```{bash}
itk::ImageFileReaderException (00F7F494)
Location: "Unknown"
File: C:\ITK\InsightToolkit-1.6.0\Code\IO\itkImageFileReader.hxx
Line: 101
Description: Could not create IO object for file c:\t1.mnc
```

This exception is thrown when ITK does not recognize the fileformat that
you are trying to read or write. When reading, ITK recognize file
formats by asking a set of internal readers to attempt to read your
file. If none of the registered readers accept your file, then the
exception above is thrown. When writing, ITK uses the extension of the
filename in order to chose the fileformat to be used. Make sure that you
use one of the filename extensions recognized by ITK, and that the
ImageIO class for that format has been registered.

Enable all the `IO` modules that you wish to support in your ITK CMake
configuration.

To register the `IO` factory:

- If you are using CMake: call `include(${ITK_USE_FILE})` *before*
  `add_executable` or `add_library`.
- If you are not using CMake: the factories should be registered with
  `itk::XXXImageIOFactory::RegisterOneFactory()` where `XXX` is the format.
- You may need to add something to the `REQUIRED` list of the
  `find_package` call that includes ITK from your project. For example,
  if you were trying to open a PNG file when you got this error,
  ensure your `CMakeLists.txt` contains `find_package(ITK REQUIRED ITKIOPNG)`.
