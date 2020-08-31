ITK Data
========

This page documents how to add test data while developing ITK. See our
[CONTRIBUTING](../CONTRIBUTING.md) and [UploadBinaryData] guides for more
information.

While these instructions assume that the required data will be contained in
binary files, the procedure (except that related to the content link file
generation) also applies to any other data contained in a text file that a
test may require, if any.

If you just want to browse and download the ITK testing images, browse the
data.kitware.com [ITK collection].

Setup
-----

The workflow below depends on local hooks to function properly. Follow the main
developer setup instructions before proceeding. In particular, run
`SetupForDevelopment.sh`:

```sh
   $ ./Utilities/SetupForDevelopment.sh
```

Workflow
--------

Our workflow for adding data integrates with our standard
[development process](CONTRIBUTING.md). Start by creating a topic. Return here
when you reach the "edit files" step.

These instructions follow a typical use case of adding a new test with a
baseline image.

### Add Data

Copy the data file into your local source tree.

```sh
   $ mkdir -p Modules/.../test/Baseline
   $ cp ~/MyTest.png Modules/.../test/Baseline/MyTest.png
```

### Add Test

Edit the test `CMakeLists.txt` file and reference the data file in an
`itk_add_test` call. Specify the file inside `DATA{...}` using a path relative
to the test directory:

```sh
   $ edit Modules/.../test/CMakeLists.txt

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

### Create content link

For the reasons stated in the [Discussion](#discussion) section, rather than
the binary files themselves, ITK and related projects use content link files
associated with these files.

To generate the content link file, use the procedure in [UploadBinaryData].


### Commit

Continue to create the topic and edit other files as necessary. Add the content
link and commit it along with the other changes:

```sh
   $ git add Modules/.../test/Baseline/MyTest.png.sha512
   $ git add Modules/.../test/CMakeLists.txt
   $ git commit
```

Building
--------

### Download

For the test data to be downloaded and made available to the tests in your
build tree the `ITKData` target must be built. One may build the target
directly, e.g. `make ITKData`, to obtain the data without a complete build.
The output will be something like

```sh
   -- Fetching ".../ExternalData/SHA512/..."
   -- [download 100% complete]
   -- Downloaded object: "ITK-build/ExternalData/Objects/SHA512/..."
```

The downloaded files appear in `ITK-build/ExternalData` by default.

### Local Store

It is possible to configure one or more local ExternalData object stores shared
among multiple builds. Configure for each build the advanced cache entry
`ExternalData_OBJECT_STORES` to a directory on your local disk outside all
build trees, e.g. "`/home/user/.ExternalData`":

```sh
   $ cmake -DExternalData_OBJECT_STORES=/home/user/.ExternalData ../ITK
```

The `ExternalData` module will store downloaded objects in the local store
instead of the build tree. Once an object has been downloaded by one build it
will persist in the local store for re-use by other builds without downloading
again.

Discussion
----------

An ITK test data file is not stored in the main source tree under version
control. Instead the source tree contains a "content link" that refers to a
data object by a hash of its content. At build time the the
[`ExternalData.cmake`](https://github.com/InsightSoftwareConsortium/ITK/blob/master/CMake/ExternalData.cmake)
module fetches data needed by enabled tests. This allows arbitrarily large data
to be added and removed without bloating the version control history.

For more information, see
[CMake ExternalData: Using Large Files with Distributed Version Control](https://blog.kitware.com/cmake-externaldata-using-large-files-with-distributed-version-control/).


[data.kitware.com]: https://data.kitware.com/
[Github]: https://github.com/InsightSoftwareConsortium/ITK
[UploadBinaryData]: UploadBinaryData.md
[ITK collection]: https://data.kitware.com/#collection/57b5c9e58d777f126827f5a1
