Uploading binary data in ITK
============================

Since every local [Git] repository contains a copy of the entire project
history, it is important to avoid adding large binary files directly to the
repository. Large binary files added and removed throughout a project's history
will cause the repository to become bloated, take up too much disk space,
require excessive time and bandwidth to download, etc.

A [solution to this problem] which has been adopted by ITK is to store binary
files, such as images, in a separate location outside the Git repository, then
download the files at build time with [CMake].

A "content link" file contains an identifying [SHA512 hash]. The content
link is stored in the [Git] repository at the path where the file would exist,
but with a `.sha512` extension appended to the file name. CMake will find these
content link files at **build** time, download them from a list of server
resources, and create symlinks or copies of the original files at the
corresponding location in the **build tree**.

See also our [Data](Data.md) guide for more information.

**Note**: for historical reasons, before [SHA512 hash] files were used in ITK,
[MD5 hash] content link files were used.

Adding images as input to ITK sources
-------------------------------------

[ITK examples] and ITK class tests (see Section 9.4 of the
[ITK Software Guide]) rely on **input** and **baseline** images (or data in
general) to demonstrate and check the features of a given class. Hence, when
developing an ITK example or test, images will need to be added to the [Git]
repository.

When using images for an ITK example or test images, the following principles
need to be followed:

  1. Images should be **small**.
     * The source tree is not an image database, but a source code repository.
     * Adding an image larger than 50 Kb should be justified by a discussion
      with the [ITK community].
  2. Regression (baseline) images should not use [Analyze format] unless the
     test is for the `AnalyzeImageIO` and related classes.
  3. Images should use non-trivial Metadata.
     * Origin should be different form zeros.
     * Spacing should be different from ones, and it should be anisotropic.
     * Direction should be different from identity.

Using Girder to get the SHA512 hash
-----------------------------------

### Prerequisites

The [data.kitware.com] server is an ITK community resource where any community
member can upload binary data files. There are two methods available to upload
data files:

  1. The [Girder web interface].
  2. The `girder-cli` command line executable that comes with the
     [girder-client] Python package.

Before uploading data, please visit [data.kitware.com] and register for an
account.

Once files have been uploaded to your account, they will be publicly available
and accessible since data is content addressed. At release time, the release
manager will upload and archive repository data references in the
[ITK collection] and other redundant storage locations.


### Upload via the web interface

  1. After logging in, you will be presented with the welcome page. Click on
     the *personal data space link*.
  2. Next, select the *Public* folder of your personal data space.
  3. Click the green upload button.
  4. Click the *Browse or drop files* to select the files to upload.
  5. Click *Start Upload* to upload the file to the server.
  6. Next, proceed to [Download the Content Link].

### Upload via Python script

A Python script to upload files from the command line, `girder-cli`, is
available with the [girder-client] Python package. To install it, type:

```sh
   $ python -m pip install girder-client
```

To upload files with the `girder-cli` script, we need to obtain an API key and a
parent folder id from the web interface.

  1. After logging in, select *My account* from the user drop down.
  2. Next, select the *API keys* tab.
  3. Create a new API key if one is not available by clickin on *Create new
     key*.
  4. The *show* link will show the key, which can be copied into the command
     line.
  5. Next, select *My Folders* from the user drop down.
  6. Next, select the *Public* folder of your personal data space.
  7. Click the *i* button for information about the folder.
  8. The *Unique ID* can be copied into the command line.

Use both the API key and the folder ID when calling `girder-cli`. For example,

```sh
   $ girder-cli \
       --api-key 12345ALongSetOfCharactersAndNumbers \
       --api-url https://data.kitware.com/api/v1 \
       upload \
       58becaee8d777f0aefede556 \
       /tmp/cthead1.png
```

Next, proceed to [Download the Content Link].

### Download the content link

  1. Click on the file that has been uploaded.
  2. Click on the *i* button for further information.
  3. Finally, click on the *Download key file* icon to download the key file.

Move the content link file to the **source tree** at the location where the
actual file is desired in the build tree. Stage the new file to your commit:

```sh
   $ git add -- path/to/file.sha512
```



[Download the Content Link]: https://itk.org/ITKExamples/Documentation/Contribute/UploadBinaryData.html#download-the-content-link
[CMake]: https://cmake.org/
[data.kitware.com]: https://data.kitware.com/
[girder-client]: https://girder.readthedocs.io/en/latest/python-client.html#the-command-line-interface
[Girder web interface]: https://girder.readthedocs.io/en/latest/user-guide.html
[Git]: https://git-scm.com/
[ITK collection]: https://data.kitware.com/#collection/57b5c9e58d777f126827f5a1
[ITK community]: https://discourse.itk.org/
[ITK Examples]: https://itk.org/ITKExamples/index.html
[ITK Software Guide]: https://itk.org/ItkSoftwareGuide.pdf
[solution to this problem]: https://blog.kitware.com/cmake-externaldata-using-large-files-with-distributed-version-control/

[Analyze format]: http://www.grahamwideman.com/gw/brain/analyze/formatdoc.htm
[MD5 hash]: https://en.wikipedia.org/wiki/MD5
[SHA512 hash]: https://en.wikipedia.org/wiki/SHA-2
