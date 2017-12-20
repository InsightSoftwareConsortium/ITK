Releasing ITK
=============

There are typically two feature releases a year, around June and December, and
one to three bug fix release between feature releases. Releasing ITK has many
steps. This document is a central location for all of the tribal knowledge
around it.

Current Maintainers
-------------------

The current ITK maintainers with a trusted GPG key are:

  * Matt McCormick (@thewtex) <matt.mccormick@kitware.com>
  * François Budin (@fbudin69500) <francois.budin@kitware.com>
  * Brad King (@bradking) <brad.king@kitware.com>
  * Jean-Christophe Fillion-Robin (@jcfr) <jchris.fillionr@kitware.com>

Release Life Cycle
------------------

The first release candidate (RC) is the initial branch point, so it does not have
special steps to create. However, as `master` moves fairly quickly, branches
need to be corralled into the `release` branch afterwards.

When releasing a new ITK version, the following steps are be taken:

  * **Before the RC announcement**: last period for adding classes and
    features.
    * New features and new methods can be added during this period.
  * **Feature freeze**
    * Increase code coverage
      * Address any **UNTESTED** files
      * Address files with code coverage lower than 80%
  * **Address run-time memory issues**
    * Purify reports
    * Valgrind reports
  * **RC process**: two to three release candidates are generally used before
    releasing a new version. A one-week notice on the [ITK discussion] should
    be sufficient as an initial deadline for RCs.
    * No new features should merged during the feature freeze, i.e. `ENH:`
    commits, although they can be prepared in Gerrit.
    * Release candidates (RC's) will be tagged weekly.
    * RC's will be tagged after [Dashboard] examination and discussion at the
    Friday ITK Confab.
    * The repository will be hard frozen to only allow merging by gatekeepers
    on Wednesday evening before the [Dashboard] builds start. The freeze will
    be released after tagging.
    * For the final RC, only gatekeeper merges will occur.
  * **Create the release**.
  * **Updating documentation, guides and websites**.
  * **Generating tarballs**
    * Tarballs are posted to [SourceForge].
    * Tarballs are linked from the ITK [download page].
  * Announcement

**Note**: ITK minor releases (patch) do not have a tagged release candidate.

Initial steps
-------------

Check the [ITK issue tracker] for critical bugs, or [gerrit] for critical fixes.

### Annoucements

Announcements should be sent to the community first and Linux distribution
maintainers.

#### ITK Discussion

Announce to the [community] that a release is being planned. Template:

    Hi,

    Does anyone have work in progress that should delay the branch point for
    $version?

    If so, please add the $version_rc1 milestone on any relevant merge requests
    and @mention $maintainers so they can be more easily tracked.

    We are hoping to get $version started by $date.

    Thanks,

    The ITK Maintenance Team

#### Distributions

Patches can accumulate over time in distributions as time goes on. An email
asking if anything needs to go into the next release should be sent to
maintainers of the packages.

This ITK [blog post] describes the Linux distributions to which ITK is
available.

Integrate bug fixes in the release branch
-----------------------------------------

Update master and release branches:

```sh
   $ git fetch
   $ git checkout master
   $ git reset --hard origin/master
   $ git checkout release
   $ git reset --hard origin/release
```

List differences between last release and current release branch:

```sh
   $ git shortlog --no-merges $release_version..release
```

Merge bug fix commits in release. The topic branch should be named
`<bug-name>-for-release`:
  * If topic branch was created from the `release` branch, `checkout` topic
    in new branch (see command lines on [gerrit]).
  * If topic branch was created on `master`, `cherry-pick` commit (see
    command line on [gerrit]) on a topic branch created off `release`. The
    commit will be visible twice in the history once release in merged into
    `master`.
  * Merge new branch on `release`:

```sh
   $ git merge <bug-name>-for-release --no-ff
```

   * Update the gerrit topic: write "Merged to release." in the merged topic.

Merge `release` into `master` (so that `master` keeps track of release history):

```sh
   $ git checkout master
   $ git pull
   $ git merge release
   $ git push origin master release
```

For patches that need to be merged to the `release-3.20` branch, they are first
merged to `release-3.20`, then `release-3.20` is merged to the release branch
with `git merge -s ours` to avoid a huge number of merge conflicts. Then,
`release` is merged into `master`.

Pre-tag activities
------------------

The following must be ensured before tagging the ITK repository:

  * Make sure to **update the versions** in the top level `CMakeLists.txt`.
    * For bugfix releases, this is done before the tag. For feature releases,
    this is done after the final tag.
  * Make sure **all new remote modules are built in by the Doxygen build**.
  * **Update** the `WikiExamples` and `SphinxExamples`` remote modules.

### Increment the version number

If the version number in `CMakeLists.txt` is not already set accordingly,
submit a merge request to update ITK's version number in the `master` branch to
what the new release is to be called by. Any point beyond that in the `master`
branch could serve as the start of the new release branch.

After creating the release branch, submit another merge request to update the
master branch's minor version number.

Tag the ITK repository
----------------------

Tagging the repository should only be done with the agreement of the
maintainers after an ITK Confab.

### Update the repository

Use the commmand:

```sh
   $ git checkout master
   $ git pull
```

to make sure that the source tree is updated. This must correspond to a source
tree that has been fully tested in the [Dashboard].

  * When tagging a **bugfix release** on the `release` branch, make sure to
    bump the `ITK_VERSION_PATCH` variable in the top level `CMakeLists.txt`
    before tagging.
  * When tagging a **feature release**, make sure to bump the
   `ITK_VERSION_MINOR` version on the `master` branch after tagging.

### Tag with a branch point reference

In the source tree that was just updated, use the command

```sh
   $ git tag -m "ITK $version" -s v$version $commit_hash_to_be_tagged
```

where, of course, `$version` must be changed for the for the correct release
number and $commit_hash_to_be_tagged` to the correct commit hash.

Push it to the repository

```sh
   $ git push origin v$version
```

Note that only trusted GPG key holders may do this step.

### Update the release branch

Update the `release` branch only during feature releases after the tag for the
release. Perform a `fast-forward` merge of `master` into release:

```sh
   $ git checkout $release_branch
   $ git reset --hard origin/$release_branch
   $ git merge --ff-only <version tag>
   $ git push origin $release_branch
   $ git checkout master
```

This will not create a new commit, only move the release branch to the tag,
i.e. it will be fast forwarded.

For minor releases, merge the release branch into `master` branch as for a
normal commit, and resolve conflicts (arising from mismatch in version number)
by keeping `master` branch versions.

Email the current maintainers with the `release_root` hash and version number
so that `kwrobot` may be updated to check if merge requests are eligible for
this release branch.

Announce to the [ITK discussion] that the release branch has been created.

### Remote modules

Add any new remote modules to nightly builds. Some builds may be difficult to add
due to third-party dependencies.

Archive ExternalData
--------------------

Set the environmental or CMake variable `ExternalData_OBJECT_STORES` to a
local directory. e.g.

```sh
   $ export ExternalData_OBJECT_STORES=${HOME}/data
```

Pre-populate the store with the contents of the 'InsightData' tarballs from a
 previous release. Once the tarball extracted, move the content of its
 subfolder called `.ExternalData` in your local `ExternalData_OBJECT_STORES`
 directory.

Then, from the ITK build directory, configure ITK enabling he flags:
  * `ITK_WRAP_PYTHON`
  * `ITK_LEGACY_SILENT`
  * `BUILD_TESTING`
  * `BUILD_EXAMPLES`

If you have previously enabled remote modules using the same ITK source
directory, either verify that they are enabled in your current build, or remove
their source directory that has been added inside ITK source directory
(i.e. `./Modules/Remote/$name_of_remote_module`).

Build the `ITKData` target

```sh
   $ make ITKData
```

This will download new testing data since the previous release.

Next, run the script from within the ITK source directory:

```sh
   $ ./Utilities/Maintenance/ContentLinkSynchronization.sh ${ExternalData_OBJECT_STORES}
```

Do not use `--cleanup` as for the purpose of the GitHub resource, it is
important to keep the older files: some are from older revisions of ITK, and
people continue to use the older versions of ITK and request the testing data.

This is will verify all contents, fully populate the `MD5/` and `SHA512/`
directories in the object store, and create any missing `.md5` or `.sha512`
content links. If any new content link files are created, commit the result.

Next, archive the data on data.kitware.com. Create a folder, e.g.
`$MAJOR_VERSION.$MINOR_VERSION`, in `ITK/ITKTestingData`, and run

```sh
   $ ./Utilities/Maintenance/ArchiveTestingDataOnGirder.py --object-store ${HOME}/data --parent-id <the-girder-id-of-the-folder-created> --api-key <your-girder-user-api-key>
````

This script requires the girder-client Python package install from Girder
master, November 2016 or later, (Girder > 2.0.0).

Run the scripts

```sh
   $ ./Utilities/Maintenance/ArchiveTestingDataOnAzure.py
   $ ./Utilities/Maintenance/ArchiveTestingDataOnGirder.py
   $ ./Utilities/Maintenance/ArchiveTestingDataOnMidas.py
```

to upload the testing data from a local store on the ITK
https://midas3.kitware.com/midas/ and http://slicer.kitware.com/midas3/
community. This script requires that pydas is installed.

Note: If you get the following error message:

```sh
   $ pydas.exceptions.InvalidPolicy: 'Request failed with HTTP status code 200, Midas Server error code -151, and response content {"stat":"fail","message":"Invalid policy or itemid","code":"-151"}'
```

make sure you have the permissions to write in the ITK collection on the
Midas server.

Archive the `InsightData` contents on ITK's file server at kitware:

```sh
   $ rsync -v -r /tmp/InsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION/.ExternalData/MD5/ public:/projects/Insight/WWW/InsightWeb/files/ExternalData/MD5/
```

Update the data archive at https://github.com/InsightSoftwareConsortium/ITKTestingData.

Create Tarballs
---------------

Tarballs need to be created and uploaded for each release. The source tarballs
should be generated in both `.tar.gz` format and `.zip` format. The `.zip`
files are for Windows users and the non-data files contain Windows newline
endings.

The `InsightData` tarballs are generated along with the source code tarballs.

Once the repository has been tagged, we use the following script in the
repository to create the tarballs:

### Unix

Run:

```sh
   $ ./Utilities/Maintenance/SourceTarball.bash --tgz
```

This will generate tarballs for the source and testing data.

### Windows

From a `git` bash shell with `wget` in `PATH`, run:

```sh
   $ ./Utilities/Maintenance/SourceTarball.bash --zip
``

This should be done on Windows so that the sources have Windows-style newline
endings.

**Note**: tarballs can be created from a specific commit. The user can manually
specify the version of ITK used to name the output files:

```sh
   $ ./Utilities/Maintenance/SourceTarball.bash -v $version $commit_hash_to_be_tagged
```
where, of course, `$version` must be changed for the for the correct release
number and $commit_hash_to_be_tagged` to the correct commit hash.

Alternatively,

```sh
   $ ./Utilities/Maintenance/SourceTarball.bash -v $version v$version
```
can be used to specify the version starting with `v`.

### Copy the files to SourceForge

This is to be done only by

  * Ken Martin (@martinken) <ken.martin@kitware.com>
  * Matt McCormick (@thewtex) <matt.mccormick@kitware.com>
  * François Budin (@fbudin69500) <francois.budin@kitware.com>
  * Brad King (@bradking) <brad.king@kitware.com>
  * Jean-Christophe Fillion-Robin (@jcfr) <jchris.fillionr@kitware.com>
  * Brad Lowekamp (@blowekamp) <blowekamp@mail.nih.gov>

Go to the site:

  https://sourceforge.net/downloads/itk/itk/

and provide user name and password

Then

  * Use the `Add Folder` button to create a folder for the release.
  * Click on the folder to open it
  * Use the `Add File` button to upload files. The interface allows to select
    multiple files for simultaneous upload.

Large files, like the Doxygen HTML tarball, need to be uploaded with scp, a la:

```sh
   $ scp InsightDoxygenDocHtml-$version.tar.gz mmmccormic@frs.sourceforge.net:/home/frs/project/itk/itk/$MAJOR_VERSION.$MINOR_VERSION/InsightDoxygenDocHtml-$version.tar.gz
```

After the tarballs have been uploaded to [SourceForge], their `MD5` sums must
be checked (the `i` info button on Sourceforge.net and locally with the
`md5sum` command). Also, tarballs must be downloaded from [SourceForge] and an
`Experimental` build run on Linux, macOS, and Windows, to check the that they
do not contain any configuration, build or test errors.

Generate Binaries
-----------------

`TODO`

### Linux


### macOS


### Windows


Generate Python Packages
------------------------

The [ITKPythonPackage](https://itkpythonpackage.readthedocs.io/en/latest/) website describes how to
[build ITK Python wheels](https://itkpythonpackage.readthedocs.io/en/latest/Build_ITK_Python_packages.html).

Python packages are currently generated nightly by the systems, `metroplex`,
`misty`, and `overload` at Kitware and uploaded to the [ITKPythonPackage
GitHub Release
page](https://github.com/InsightSoftwareConsortium/ITKPythonPackage/releases/tag/latest).

Additionally, external module wheels can also be generated. Please, visit the
[ITK module Python packages](https://itkpythonpackage.readthedocs.io/en/latest/Build_ITK_Module_Python_packages.html)
documentation for further information.

### Generate release ITK Python wheels

First, merge the
[ITKPythonPackage](https://github.com/InsightSoftwareConsortium/ITKPythonPackage)
`master` branch into the `release` branch.

Next, update the `VERSION` variable in *ITKPythonPackage/itkVersion.py* and
`ITK_GIT_TAG` in *ITKPythonPackage/CMakeLists.txt*.

Then [build the
wheels](https://itkpythonpackage.readthedocs.io/en/latest/Build_ITK_Python_packages.html)
from the `release` branch on Linux, macOS, and Windows.

### Upload the wheels to PyPI

Next, [upload the wheels to the Python Package Index
(PyPI)](https://itkpythonpackage.readthedocs.io/en/latest/Build_ITK_Module_Python_packages.html#upload-the-packages-to-pypi).

### Verify the binaries

Run `pip install itk` in a fresh virtualenv and run all the
[ITKExamples](https://github.com/InsightSoftwareConsortium/ITKExamples) Python
tests against this Python. For example,

```
virtualenv itk-venv
./itk-venv/bin/python -m pip install itk
git clone https://github.com/InsightSoftwareConsortium/ITKExamples
mkdir ITKExamples-build
cd ITKExamples-build
cmake -DITK_DIR=/path/to/ITK-build -DPYTHON_EXECUTABLE=../itk-venv/bin/python ../ITKExamples
ctest -R Python
```

### Upload the ITKPythonBuilds

Create a new GitHub Release in the
[ITKPythonPackage repository](https://github.com/InsightSoftwareConsortium/ITKPythonPackage/releases)
and upload the wheels there.

Also, create a corresponding GitHub Release in the
[ITKPythonBuilds](https://github.com/InsightSoftwareConsortium/ITKPythonBuilds)
repository. Upload builds tarballs created from the build trees with
scripts found in *ITKPythonPackage/scripts/*, i.e.
*ITKPythonPackage/scripts/macpython-build-tarball.sh*, etc..

Update the *ITKPythonPackage/scripts/*download-cache-and-build-module-wheels**
scripts to use the new version of *ITKPythonBuilds*.

### Verify external module GitHub CI builds

Re-run
[TravisCI](https://travis-ci.org/InsightSoftwareConsortium/ITKModuleTemplate),
[AppveyorCI](https://ci.appveyor.com/project/itkrobot/itkmoduletemplate), and
[CircleCI](https://circleci.com/gh/InsightSoftwareConsortium/ITKModuleTemplate) in the
[ITKModuleTemplate](https://github.com/InsightSoftwareConsortium/ITKModuleTemplate)
repository to ensure the new *ITKPythonBuilds* and external module package
build scripts are functioning properly.

### Update the conda-forge package

Create a PR to update
[conda-forge/itk-feedstock](https://github.com/conda-forge/itk-feedstock) to
the new version. This conda recipe downloads the wheel binary packages and
re-packages them as conda packages.

Generate Doxygen Documentation
------------------------------

On a machine with [Doxygen] installed, configure a build with
`BUILD_DOCUMENTATION=ON` and run the `DoxygenDoc` target. To create the
documentation tarball, run:

```sh
   $ tar -C Utilities/Doxygen/doc -czf vtkDocHtml-$version.tar.gz html
```

from the top of the build tree.


Note: links to the nightly generated Doxygen can be found in the footer of the
Doxygen HTML pages. Use the files to upload and create:

  * `InsightDoxygenDocTag-MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.gz`
  * `InsightDoxygenXml-MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar.gz`
  * `InsightDoxygenDocHtml-MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar.gz.s`

Prior to the release, new `Remote` modules should be enabled in the Doxygen
build's configuration.

Run CMake in the binary build and enable `BUILD_DOXYGEN`, configure and
generate, then:

```sh
   $ cd Binaries/ITK
   $ make Documentation
   $ cd Utilities
   $ mv old_doxygen_directory DoxygenInsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION
   $ tar -cf DoxygenInsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar DoxygenInsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION
   $ gzip -9 DoxygenInsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar
```

Historical note: Before ITK 3.8, the documentation used to be generated in a
directory called `Documentation/Doxygen`.

In `Public`, copy the documentation to `/projects/Insight/Doxygen`, and create
a subdirectory `Insight34-doxygen/Documentation/Doxygen`.

The final directory will look like
`/projects/Insight/Doxygen/Insight34-doxygen/Documentation/Doxygen`, and at that
level copy the `html` directory and the `InsightToolkit.tag` file.

Finally, create symbolic link at `/projects/Insight/WWW/InsightDocuments/Web`.

Update the ITK Software Guide
-----------------------------

The [ITK Sofware Guide] is available in both electronic and printed formats.
Every time a new ITK version is released, the following steps must be taken to
update the guide and make it available:

  * Add the necessary ITK contents (i.e. new modules, etc.).
  * Update the compiler and necessary tool (e.g. CMake minimum version)
    information if necessary.
  * Bump the ITK version in `Superbuild/ExternalITKv4.cmake`.
  * Set the `DRAFT_WATERMARK` CMake varable to `OFF` to remove the draft
    watermark.
  * Set the `PDF_QUALITY_LEVEL` CMake configuration option to `Screen` for the
    electronic version and `Printer` for the print version.
  * Turn on `GENERATE_HTML`.

To create `ItkSoftwareGuide.pdf` to put at itk.org/ItkSoftwareGuide.pdf from
`InsightSoftwareGuide-Book{1,2}-4.X.0.pdf`, use `pdftk`:

```
   $ pdftk ITKSoftwareGuide-Book1.pdf ITKSoftwareGuide-Book2.pdf cat output /tmp/ItkSoftwareGuide.pdf
```

### Prepare the print version

Set the `PDF_QUALITY_LEVEL` to `Printer`, and rebuild. Remove the junk initial
page, and then also one of the blank pages so pages fall on
*left-side*/*right-side* as formatted.

Then, run:

```sh
   $ pdftk ITKSoftwareGuide-Book1.pdf cat 2-3 5-end output /tmp/ITKSoftwareGuide-Book1.pdf
   $ pdftk ITKSoftwareGuide-Book2.pdf cat 2-3 5-end output /tmp/ITKSoftwareGuide-Book2.pdf
```

### Update the HTML pages

`rsync` the newly generated pages to the web server:

```sh
   $ rsync -rt html/ kitware@public:/projects/Insight/WWW/InsightWeb/ITKSoftwareGuide/html
```

Update ITK Sphinx Examples
--------------------------

In order to update the [ITK Sphinx examples], bump the `Superbuild` ITK
version in `Superbuild/External-ITK.cmake`.

Update the CMake minimum version in the example files if necessary.

Rendered versions (epub, pdf, html) can be downloaded from the download page
and rename them.

Set the prefix and tag:

```sh
   $ tag = $(git describe)
   $ prefix = InsightSphinxExamples-$version
```
where `$version` is the appropriate release number, e.g., `4.12.0`.

Generate the `.tar.gz` and `.zip` tarballs:

```sh
   $ git archive --format=tar --prefix=${prefix}/ --output=${prefix}.tar ${tag}
   $ gzip -9  ${prefix}.tar
   $ git archive --format=zip -9 --prefix=${prefix}/ --output=${prefix}.zip ${tag}
```

Update ITK Wiki Examples
------------------------

Update the CMake minimum version in the example files if necessary.

Download the latest examples from the [ITK wiki examples] (i.e. `git pull`
on the `ITKWikiExamples` repository), and run:

```sh
   $ wget -O ITKWikiExamples-master.zip https://github.com/InsightSoftwareConsortium/ITKWikiExamples/archive/master.zip
   $ unzip ITKWikiExamples-master.zip
   $ mv ITKWikiExamples-master InsightWikiExamples-${version}
   $ zip -r InsightWikiExamples-${version}.zip InsightWikiExamples-${version}
   $ tar cvf InsightWikiExamples-${version}.tar InsightWikiExamples-${version}
   $ gzip -9 InsightWikiExamples-${version}.tar
```

Upload to public.kitware.com
----------------------------

The generated files must be uploaded to the Kitware FTP directory at
[public.kitware.com]


```sh
   $ scp   InsightData-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar.gz                  kitware@public:/projects/Insight/WWW/InsightWeb/files/v$MAJOR_VERSION.$MINOR_VERSION
   $ scp   InsightData-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar.xz                  kitware@public:/projects/Insight/WWW/InsightWeb/files/v$MAJOR_VERSION.$MINOR_VERSION
   $ scp   InsightData-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.zip                     kitware@public:/projects/Insight/WWW/InsightWeb/files/v$MAJOR_VERSION.$MINOR_VERSION
   $ scp   InsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar.gz               kitware@public:/projects/Insight/WWW/InsightWeb/files/v$MAJOR_VERSION.$MINOR_VERSION
   $ scp   InsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar.xz               kitware@public:/projects/Insight/WWW/InsightWeb/files/v$MAJOR_VERSION.$MINOR_VERSION
   $ scp   InsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.zip                  kitware@public:/projects/Insight/WWW/InsightWeb/files/v$MAJOR_VERSION.$MINOR_VERSION
   $ scp   InsightDoxygenDocHtml-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar.gz        kitware@public:/projects/Insight/WWW/InsightWeb/files/v$MAJOR_VERSION.$MINOR_VERSION
   $ scp   InsightDoxygenDocTag-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar.gz         kitware@public:/projects/Insight/WWW/InsightWeb/files/v$MAJOR_VERSION.$MINOR_VERSION
   $ scp   InsightDoxygenDocXml-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar.gz         kitware@public:/projects/Insight/WWW/InsightWeb/files/v$MAJOR_VERSION.$MINOR_VERSION
   $ scp   ItkSoftwareGuide.pdf                                                             kitware@public:/projects/Insight/WWW/InsightWeb/files/v$MAJOR_VERSION.$MINOR_VERSION
   $ scp   InsightSoftwareGuide-Book1-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.pdf      kitware@public:/projects/Insight/WWW/InsightWeb/files/v$MAJOR_VERSION.$MINOR_VERSION
   $ scp   InsightSoftwareGuide-Book2-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.pdf      kitware@public:/projects/Insight/WWW/InsightWeb/files/v$MAJOR_VERSION.$MINOR_VERSION
   $ scp   InsightSoftwareGuideHtml-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar.gz     kitware@public:/projects/Insight/WWW/InsightWeb/files/v$MAJOR_VERSION.$MINOR_VERSION
   $ scp   InsightWikiExamples-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar.gz          kitware@public:/projects/Insight/WWW/InsightWeb/files/v$MAJOR_VERSION.$MINOR_VERSION
   $ scp   InsightWikiExamples-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.zip             kitware@public:/projects/Insight/WWW/InsightWeb/files/v$MAJOR_VERSION.$MINOR_VERSION
```

```sh
   $ ssh kitware@public
   $ cd /projects/Insight/WWW/InsightWeb
   $ rm ItkSoftwareGuide.pdf
   $ ln -s files/v$MAJOR_VERSION.$MINOR_VERSION/ItkSoftwareGuide.pdf ItkSoftwareGuide.pdf
```

Update the Website
------------------

The website is managed by Kitware folks. Access is currently granted to the ITK
maintainer group.

  * Add or modify the `Current Release` entries on the
    [download page](https://itk.org/ITK/resources/software.html).
  * Update the [documentation page](https://itk.org/ITK/help/documentation.html) with a
    link to the Doxygen files for the new release.
  * Verify that the links work !

Contact Communications at <comm@kitware.com> in order to update the above pages
and to produce a press release.

Update the Wiki
---------------

The [ITK wiki] hosts several pages that:

  * Store information about release versions
  * List API changes between two versions
  * Store the release schedule

Every time a new ITK version is released, the following steps must be taken to
keep the wiki information up-to-date:

  * **Add entry for next planned release**: delete the old entry and add a new
    entry in the [release schedule] page.
  * **Update the Releases page**: add the *What is new* and *What changed*
    entries to the [releases page].
  * **Update *What is new in release x.x* Wiki Page**: update this webpage with
    the message sent to the [ITK discussion] indicating the notable changes and
    other important information about the release.
  * **Update *What changed since x.x***: From a Linux system:

```sh
   $ cd ITK
   $ git shortlog --topo-order --no-merges v$old_version..v$new_version
```

Delete the `kwrobot` time stamp commits.

Update JIRA issue tracker
-------------------------

In the [ITK issue tracker] to create a new version to make possible for users
to report bug pertaining to that specific ITK release:

  * Create the next release target milestone with *Administration* -> *ITK* ->
    *Versions* -> *Add*.
  * Release the current release milestone with *Agile* -> *Classic* ... ->
    Right click the release on the left -> *Release*.

Further Testing
---------------

The purpose of this testing is to replicate the experience that a user may have
when trying the new release.

This means that a number of ITK developers should download the tarballs or do
Git checkouts with the release tag, and build the toolkit in as many
configurations as possible.

Release Notes Posts
-------------------

In `Utilities/Maintenance/release`, there are the following scripts:

  * `make-changelog.sh`
  * `prep-emails.py`
  * `send-emails.sh`

First, create the `changes.txt` file by giving a commit range to
`make-changelog.sh`:

```sh
   $ make-changelog.sh $previous_release $release
```

This file contains all of the changes in the commit range grouped by author.
It is then processed by `prep-emails.py`:

```sh
   $ ./prep-emails.py $version
```

It takes `changes.txt` and creates a file for each email address:
`user@domain.tld.txt`. You should delete any files which point to upstream or
the developer list which are used to preserve authorship for `ThirdParty`
updates or wide, sweeping changes.

Once that is complete, run `send-emails.sh`:

```sh
   $ mailer=mutt ./send-emails.sh $version
```

The `mailer` environment variable should support this command line:

```sh
   $ $mailer -s "$subject" $cc_list "$to" < "$body"
```

which is any `sendmail`-compatible program.

Announcing
----------

For the final release, the release notes produced should be used to

  * Post a message in the [ITK discussion]
  * Create a post in the [kitware blog]
  * Create a post in ITK project on [ResearchGate]

Finally, inform Communications at <comm@kitware.com>.

Send Contributor Momentos
-------------------------

This file:

https://github.com/thewtex/vtkGEB/blob/itk/itkgeb.stl

can be ordered from ShapeWays and sent to contributors. Deb Howell has generated
excellent packaging.



[kitware blog]: https://blog.kitware.com/
[blog post]: https://blog.kitware.com/itk-packages-in-linux-distributions/
[Dashboard]: https://open.cdash.org/index.php?project=Insight
[community]: https://discourse.itk.org/
[documentation page]: http://www.itk.org/ITK/help/documentation.html
[download page]: https://itk.org/ITK/resources/software.html
[gerrit]: http://review.source.kitware.com/
[ITKPythonPackage]: https://itkpythonpackage.readthedocs.io/en/latest/index.html
[ITK discussion]: https://discourse.itk.org/
[ITK issue tracker]: https://issues.itk.org/jira/secure/Dashboard.jspa
[ITK Sofware Guide]: https://itk.org/ItkSoftwareGuide.pdf
[ITK wiki]: https://itk.org/Wiki/ITK
[ITK wiki examples]: https://itk.org/Wiki/ITK/Examples
[ITK Sphinx examples]: https://itk.org/ITKExamples/
[releases page]: https://itk.org/Wiki/ITK/Releases
[release schedule]: https://itk.org/Wiki/ITK/Release_Schedule
[Software Guide]: http://itk.org/ItkSoftwareGuide.pdf

[kitware]: https://www.kitware.com/
[public.kitware.com]: public.kitware.com

[Doxygen]: http://www.stack.nl/~dimitri/doxygen/
[PyPi]: https://pypi.python.org/pypi
[ResearchGate]: https://www.researchgate.net/project/Insight-Toolkit-ITK
[SourceForge]: https://sourceforge.net/downloads/itk/itk/
