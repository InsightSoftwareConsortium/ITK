# DICOM

## Rescale Slope/Intercept

A question that is often asked is: I am trying to read/write a DICOM image,
but the pixel data has changed (the scalar range has changed from input file).

This surely comes from the fact that you did not used the proper
pixel type to instanciate your reader. You have to consider:

- `Bits Allocated`
- `Bits Stored`
- `Pixel Representation`
- `Samples per Pixel`

and:

- `Rescale Intercept`
- `Rescale Slope`

For instance you can use the `DicomImageReadPrintTags` example in the
ITK Software Guide, or even [gdcminfo](https://manpages.ubuntu.com/manpages/jammy/man1/gdcminfo.1.html).

```bash
$ DicomImageReadPrintTags image.dcm | grep "Slope\|Intercept\|Bits\|Pixel Rep"
(0028|0100) Bits Allocated = 16
(0028|0101) Bits Stored = 12
(0028|0103) Pixel Representation = 0
(0028|1052) Rescale Intercept = -1000
(0028|1053) Rescale Slope = 1
```

In this case, when you look only at how the pixel are stored on disk:
`Bits Allocated`, `Bits Stored` and `Pixel Representation`, you clearly
see that they are stored as unsigned short. However looking now at how
the pixel are mapped to real world value you need to apply an affine
transform:

$$RWV = slope * SP + intercept$$

where $RWV$ is the real world value, and $SP$ means `Samples per Pixel`.

In which case you'll get signed value. Therefore you will need a signed
short pixel to display the pixel value.

One might ask, how can I be sure that signed short will be enough for
loading the stored pixel value ? You have two clues: either look at
pixel stored: 12bits, therefore you know that signed short will be
enough. Or you can have a look sometimes at Smallest Image Pixel Value /
Largest Image Pixel Value. It will tell you what are the min/max in your
stored image:

```bash
$ DicomImageReadPrintTags image.dcm | grep "Image Pixel Value"
(0028|0106) Smallest Image Pixel Value = 0
(0028|0107) Largest Image Pixel Value = 4095
```

## Why did the header of my DICOM file change?

A common question that frequently arises after using
[`itk::GDCMImageIO`](https://itk.org/Doxygen/html/classitk_1_1GDCMImageIO.html)
to read, update and write out a DICOM file is: why did my header file
change during the update mechanism?

DICOM has a specific section on what a `DERIVED` image is (PS
3.3-2008, C.7.6.1.1.2 Image Type):

> If the pixel data of the derived Image is different from the pixel
> data of the source images and this difference is expected to affect
> professional interpretation of the image, the Derived Image shall have
> a UID different than all the source images.

Further, in the definition of Derivation Description there is a note
which says (PS 3.3-2008, C.7.6.1.1.3 Derivation Description):

> Examples of Derived Images which would normally be expected to affect
> professional interpretation and would thus have a new UID include:
>
> 1. Examples of Derived Images that would normally be expected to
>    affect professional interpretation and would thus have a new UID
>    include:
>
>    a. images resulting from image processing of another image (e.g. unsharp
>       masking),
>
>    b. a multiplanar reformatted CT image,
>
>    c. a DSA image derived by subtracting pixel values of one image from
>       another,
>
>    d. an image that has been decompressed after having been compressed with
>       a lossy compression algorithm. To ensure that the user has the
>       necessary information about the lossy compression, the approximate
>       compression ratio may be included in Derivation Description
>       (0008,2111).
>
> An example of a Derived Image that would normally not be expected to affect
> professional interpretation and thus would not require a new UID is an image
> that has been padded with additional rows and columns for more display
> purposes.

At a minimum you should expect the minimal set of changes:

- Assign a new `SOP Instance UID`.
- Reference the `SOP Instance UID` of the original in
 `Source Image Sequence`.
- Set the `Image Type` value 1 to `DERIVED` rather than `ORIGINAL`

The identical `SOP Instance UID` of two files mean that both files are
representations of the same objects and thus have identical content. It
is however allowed that both files uses different transfer syntax uid
(i.e. they have a different binary representation on disk)
(implicit/explicit, little/big endian). When a DICOM file is sent to a
PACS, it is up to the implementor to decide what to do in case of
duplicate `SOP Instance UID`. In most case you should expect that the
image is not resent, as standard garantee this is already the same
object.

> ⚠️ **Warning**
>
> Adding an overlay is considered to *affect professional
> interpretation* of the images. Thus new UIDs are required !
