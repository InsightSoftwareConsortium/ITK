ITK Data
========

This page documents how to add test data while developing ITK. See our
[CONTRIBUTING](index.md) and [Upload Binary Data] guides for more
information.

While these instructions assume that the required data will be contained in
binary files, the procedure (except that related to the content link file
generation) also applies to any other data contained in a text file that a
test may require, if any.

If you just want to browse and download the ITK testing images, browse the
[ITKTestingData repository]. Historical snapshots are also archived in
the [ITKData DataLad repository].

Setup
-----

The workflow below depends on local hooks to function properly. Follow the main
developer setup instructions before proceeding. In particular, run
`SetupForDevelopment.sh`:

```bash
./Utilities/SetupForDevelopment.sh
```

Workflow
--------

Our workflow for adding data integrates with our standard
[development process](index.md). Start by creating a topic. Return here
when you reach the "edit files" step.

These instructions follow a typical use case of adding a new test with a
baseline image.

### Create the content link

For the reasons stated in the [Discussion](#discussion) section, rather than
the binary files themselves, ITK and related projects use content link files
associated with these files.

Generate the *.cid* content link from your test data file, *MyTest.png* in
this example, with the [content-link-upload] web app. This app will
upload the data to IPFS and provide a *.cid* CMake ExternalData content link file
to download. This is the easiest and recommended way to upload new test data.

For advanced command line driven uploads, the upload script at
`Utilities/Maintenance/ExternalDataUpload/` can be used:

```bash
Utilities/Maintenance/ExternalDataUpload/ipfs-upload.sh \
    Modules/.../test/Baseline/MyTest.png
```

The script adds the file to your local IPFS node under the UnixFS v1 2025
profile, pins it on the `itk-filebase` remote pinning service (and on
`itk-pinata` when that service is configured — Pinata is optional because
its pin-by-CID endpoint requires a paid plan), and replaces the original
file with `MyTest.png.cid` containing the resulting CID. The CID and
source-tree path are also recorded in
`Testing/Data/content-links.manifest`.

For advanced CLI usage, first-time users must complete the one-time Kubo + pinning-service setup
documented in
[`Utilities/Maintenance/ExternalDataUpload/README.md`] before the script will
succeed. Contributors who cannot run a local Kubo daemon may instead use
the [content-link-upload] web app, which pins to [Filebase] and [Pinata] and returns
a `.cid` file directly — manifest and mirror updates must then be added by
hand.

For more details, see the description and procedures in [Upload Binary Data].

(add-data)=
### Add Data

Copy the data content link file into your local source tree.

```bash
mkdir -p Modules/.../test/Baseline
cp ~/MyTest.png.cid Modules/.../test/Baseline/MyTest.png.cid
```
(add-test)=
### Add Test

Edit the test `CMakeLists.txt` file and reference the data file in an
`itk_add_test` call. Specify the file inside `DATA{...}` using a path relative
to the test directory:

```bash
edit Modules/.../test/CMakeLists.txt

   itk_add_test(NAME MyTest COMMAND ... --compare DATA{Baseline/MyTest.png,:} ...)
```

  * Files in `Testing/Data` may be referenced as
    `DATA{${ITK_DATA_ROOT}/Input/MyInput.png}`.
  * If the data file references other data files, e.g. `.mhd -> .raw`, follow the
    link to the `ExternalData` module on the right and read the documentation on
    "associated" files.
  * Multiple baseline images and other series are handled automatically when the
    reference ends in the ",:" option; follow the link to the `ExternalData`
    module on the right for details.

(commit)=
### Commit

Continue to create the topic and edit other files as necessary. Add the content
link and commit it along with the other changes:

```bash
git add Modules/.../test/Baseline/MyTest.png.cid
git add Modules/.../test/CMakeLists.txt
git commit
```

Building
--------

### Download

For the test data to be downloaded and made available to the tests in your
build tree the `ITKData` target must be built. One may build the target
directly, e.g. `make ITKData`, to obtain the data without a complete build.
The output will be something like

```bash
   -- Fetching ".../ExternalData/CID/..."
   -- [download 100% complete]
   -- Downloaded object: "ITK-build/ExternalData/Objects/CID/..."
```

The downloaded files appear in `ITK-build/ExternalData` by default.

### Local Store

It is possible to configure one or more local ExternalData object stores shared
among multiple builds. Configure for each build the advanced cache entry
`ExternalData_OBJECT_STORES` to a directory on your local disk outside all
build trees, e.g. "`/home/user/.ExternalData`":

```bash
cmake -DExternalData_OBJECT_STORES=/home/user/.ExternalData ../ITK
```

The `ExternalData` module will store downloaded objects in the local store
instead of the build tree. Once an object has been downloaded by one build it
will persist in the local store for re-use by other builds without downloading
again.

(discussion)=
Discussion
----------

An ITK test data file is not stored in the main source tree under version
control. Instead the source tree contains a "content link" that refers to a
data object by a hash of its content. At build time the the
[`ExternalData.cmake`](https://github.com/InsightSoftwareConsortium/ITK/blob/main/CMake/ExternalData.cmake)
module fetches data needed by enabled tests. This allows arbitrarily large data
to be added and removed without bloating the version control history.

For more information, see
[CMake ExternalData: Using Large Files with Distributed Version Control] and
the [InterPlanetary File System (IPFS)].

[CMake ExternalData: Using Large Files with Distributed Version Control]: https://blog.kitware.com/cmake-externaldata-using-large-files-with-distributed-version-control/
[content-link-upload]: https://content-link-upload.itk.org
[InterPlanetary File System (IPFS)]: https://ipfs.tech/
[ITKData DataLad repository]: https://gin.g-node.org/InsightSoftwareConsortium/ITKData/src/main
[ITKTestingData repository]: https://github.com/InsightSoftwareConsortium/ITKTestingData
[Upload Binary Data]: upload_binary_data.md
[`Utilities/Maintenance/ExternalDataUpload/README.md`]: https://github.com/InsightSoftwareConsortium/ITK/blob/main/Utilities/Maintenance/ExternalDataUpload/README.md
[Filebase]: https://filebase.com/
[Pinata]: https://pinata.cloud/
