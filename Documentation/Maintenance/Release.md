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

### Bug fix release

  * **Before the release**: post a topic on the [ITK discussion] requesting
    additional bug fix patches that should be merged to the `release` branch.
  * **Create the release**.
    * Bump ITK's version
    * Tag the ITK repository
    * Bump ITKPythonPackage's version
  * **Generate tarballs**
    * Generate the *InsightToolkit* and *InsightData* tarballs.
    * Tarballs are tested locally.
    * Generate Python packages and ITKPythonBuilds.
    * Tarballs are posted to [ITK GitHub Releases].
    * Tarballs are archived on the [ITK data.kitware.com Releases].
    * Tarballs are linked from the ITK [download page].
    * Python packages are uploaded to PyPI
    * conda-forge libitk and itk Packages are updated
  * Announcement
    * GitHub Release
    * Discourse
    * Kitware Blog


### Feature release

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
  * **Generate tarballs**
    * Tarballs are posted to [ITK GitHub Releases].
    * Tarballs are linked from the ITK [download page].
  * Announcement

Initial steps
-------------

Check the [ITK issue tracking] for critical bugs, or [GitHub] for critical
fixes.

### Announcements

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

This ITK [blog post] describes the Linux distributions that package ITK.

Integrate bug fixes in the release branch
-----------------------------------------

Update `master` and `release` branches:

```sh
   git fetch upstream
   git checkout master
   git reset --hard upstream/master
   git checkout release
   git reset --hard upstream/release
```

List differences between last release and current release branch:

```sh
   git shortlog --no-merges $release_version..release
```

Merge bug fix commits in release. The topic branch should be named
`<bug-name>-for-release`:
  * If topic branch was created from the `release` branch, `checkout` topic
    in new branch.
  * If topic branch was created on `master`, `cherry-pick` commit (see
    command line on [GitHub]) on a topic branch created off `release`. The
    commit will be visible twice in the history once release in merged into
    `master`.
  * Merge new branch on `release`:

```sh
   $ git merge <bug-name>-for-release --no-ff
```

Merge `release-4.13` into `release` (so that `release` keeps track of release history):
Similarly for the `release-4.13` branch:

```sh
   git checkout release-4.13
   git pull upstream release-4.13
   git checkout release
   git merge release-4.13
   git push origin release release-4.13
```
Similarly for the `master` and `release` branches:

```sh
   git checkout master
   git pull
   git merge release
   git push origin master release
```

For patches that need to be merged to the `release-3.20` branch, they are first
merged to `release-3.20`, then `release-3.20` is merged to the release branch
with `git merge -s ours` to avoid a huge number of merge conflicts. Then,
`release` is merged into `master`.

Pre-tag activities
------------------

The following must be ensured before tagging the ITK repository:

  * Check the [Dashboard].
  * Make sure to **update the versions** in `ITK/CMake/itkVersion.cmake`.
    * For bugfix releases, this is done before the tag. For feature releases,
    this is done after the final tag.
  * Make sure **all new remote modules are built in by the Doxygen build**.
  * **Update** the `SphinxExamples` remote modules.

### Increment the version number

If the version number in `ITK/CMake/itkVersion.cmake` is not already set accordingly,
submit a pull request to update ITK's version number in the `master` branch to
what the new release is called. Any point beyond that in the `master` branch
could serve as the start of the new release branch.

After creating the release branch, submit another merge request to update the
master branch's minor version number.

Update Zenodo Citation Configuration
------------------------------------

Install the python packages:

```sh
  python -m pip install gitpython python-Levenshtein fuzzywuzzy
```

Run the update script:

```sh
  ./Utilities/Maintenance/UpdateZenodo.py
```

Commit the result:

```sh
  git add -- .zenodo.json
  git commit -m "DOC: Update .zenodo"
```

Archive ExternalData
--------------------

Set the environmental or CMake variable `ExternalData_OBJECT_STORES` to a
local directory. e.g.

```sh
   export ExternalData_OBJECT_STORES=${HOME}/data
```

Pre-populate the store with the contents of the 'InsightData' tarballs from a
previous release. Once the tarball extracted, move the content of its
subfolder called `.ExternalData` in your local `ExternalData_OBJECT_STORES`
directory.

Then, from the ITK build directory, configure ITK enabling the flags:
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
   make ITKData
```

This will download new testing data since the previous release.

Next, run the script from within the ITK source directory:

```sh
   ./Utilities/Maintenance/ContentLinkSynchronization.sh ${ExternalData_OBJECT_STORES}
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
   python -m pip install girder-client
   python ./Utilities/Maintenance/ArchiveTestingDataOnGirder.py --object-store ${ExternalData_OBJECT_STORES} --parent-id <the-girder-id-of-the-folder-created> --api-key <your-girder-user-api-key>
```

This script requires the girder-client Python package install from Girder
master, November 2016 or later, (Girder > 2.0.0).

Archive the `InsightData` contents on ITK's file server at Kitware:

```sh
   rsync -vrt ${ExternalData_OBJECT_STORES}/MD5/ kitware@public:ITKExternalData/MD5/
```

Update the data archive at https://github.com/InsightSoftwareConsortium/ITKTestingData.

Tag the ITK repository
----------------------

Tagging the repository should only be done with the agreement of the
maintainers after an ITK Confab.

### Update the repository

Use the commmand:

```sh
   git checkout master
   git pull
```

to make sure that the source tree is updated. This must correspond to a source
tree that has been fully tested in the [Dashboard].

  * When tagging a **bugfix release** on the `release` branch, make sure to
    bump the `ITK_VERSION_PATCH` variable in the `CMake/itkVersion.cmake`
    file before tagging.
  * When tagging a **feature release**, make sure to bump the
   `ITK_VERSION_MINOR` version on the `master` branch after tagging.

### Tag with a branch point reference

In the source tree that was just updated, use the command

```sh
   git tag -m "ITK $version" -s v$version $commit_hash_to_be_tagged
```

where, of course, `$version` must be changed for the for the correct release
number and $commit_hash_to_be_tagged` to the correct commit hash.

Push it to the repository

```sh
   git push upstream v$version
```

Note that only trusted GPG key holders may do this step.

### Update the release branch

Update the `release` branch only during feature releases after the tag for the
release. Perform a `fast-forward` merge of `master` into release:

```sh
   git checkout release
   git reset --hard upstream/release
   git merge --ff-only v$version
   git push upstream release
   git checkout master
```

This will not create a new commit, only move the release branch to the tag,
i.e. it will be fast forwarded.

For minor releases, merge the release branch into `master` branch as for a
normal commit, and resolve conflicts (arising from mismatch in version number)
by keeping `master` branch versions.

### Remote modules

Add any new remote modules to nightly builds. Some builds may be difficult to add
due to third-party dependencies.


Update Remote Modules
---------------------

In order to have the latest versions for all remote modules, and have them use
the latest ITK tag, the following steps should be performed:

1. Update the ITK tag used in the `azure-pipelines.yml` CI configuration and
the `setup.py` Python setup files, and update the remote module Python package
version to a new major version using the [UpdateRequiredITKVersionInRemoteModules.sh](https://github.com/InsightSoftwareConsortium/ITK/tree/master/Utilities/Maintenance/UpdateRequiredITKVersionInRemoteModules.sh)
script. This will involve merging a new pull request to each remote module
repository.

2. Upload the new remote module Python wheel to [PyPI].

3. Update the remote modules to their latest commits using the
[UpdateRemoteModules.sh](https://github.com/InsightSoftwareConsortium/ITK/tree/master/Utilities/Maintenance/UpdateRemoteModules.sh)
script.


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
   ./Utilities/Maintenance/SourceTarball.bash --tgz
```

This will generate tarballs for the source and testing data.

### Windows

From a Git Bash shell, run:

```sh
   ./Utilities/Maintenance/SourceTarball.bash --zip
```

This should be done on Windows so that the sources have Windows-style newline
endings.

**Note**: tarballs can be created from a specific commit. The user can manually
specify the version of ITK used to name the output files:

```sh
   ./Utilities/Maintenance/SourceTarball.bash -v $version $commit_hash_to_be_tagged
```
where, of course, `$version` must be changed for the for the correct release
number and `$commit_hash_to_be_tagged` to the correct commit hash.

Alternatively,

```sh
   ./Utilities/Maintenance/SourceTarball.bash -v $version v$version
```
can be used to specify the version starting with `v`.

Once all tarballs have been collected for upload to GitHub, create *MD5SUMS* and
*SHA512SUMS* checksum files. These checksums are used by clients downloading
the source tarballs to verify their contents, e.g. with `sha512sum -c
SHA512SUMS`.

```sh
   md5sum ./Insight* > MD5SUMS
   sha512sum ./Insight* > SHA512SUMS
```

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
`ITK_GIT_TAG` in *ITKPythonPackage/CMakeLists.txt*. Commit the update locally
to the release branch and push to GitHub `upstream`.

Then [build the
wheels](https://itkpythonpackage.readthedocs.io/en/latest/Build_ITK_Python_packages.html)
from the `release` branch locally.

Build the sdist and wheels for Linux:

```sh
ssh blaster
cd ~/Packaging/ITKPythonPackage
git reset --hard HEAD
git checkout release
git pull origin release
git clean -fdx
/home/kitware/Support/skbuild-venv/bin/python setup.py sdist --formats=gztar,zip
./scripts/dockcross-manylinux-build-wheels.sh
tar cvzf /tmp/dist-linux.tar.gz ./dist
rm dist/*
cd ..
./ITKPythonPackage/scripts/dockcross-manylinux-build-tarball.sh
```


Build the wheels for macOS:

```sh
ssh misty
cd ~/Dashboards/ITK/ITKPythonPackage
git reset --hard HEAD
git checkout release
git pull
git clean -fdx
./scripts/macpython-build-wheels.sh
tar cvzf /tmp/dist-macos.tar.gz ./dist
rm dist/*
cd ..
./ITKPythonPackage/scripts/macpython-build-tarball.sh
```

Build the wheels for Windows:

```sh
vncviewer overload # Open Git Bash shell
cd /c/P/IPP
git reset --hard HEAD
git checkout release
git pull
git clean -fdx
# Open a x64 Native Tools Command Prompt for VS 2019
cd C:\P\IPP
set PATH=C:\P\doxygen;%PATH%
C:\Python36-x64\python.exe ./scripts/windows_build_wheels.py
# Back in Git Bash...
tar cvzf /c/P/dist-windows.tar.gz ./dist
rm dist/*
cd ..
rm -f ./ITKPythonBuilds-windows.zip
powershell "IPP/scripts/windows-build-tarball.ps1"
```

Next, tag the release branch `HEAD` and push to GitHub:

```sh
git tag -m "ITKPythonPackage $version" -s v$version HEAD
git push upstream release v$version
```

### Upload the wheels to PyPI

Next, [upload the wheels to the Python Package Index
(PyPI)](https://itkpythonpackage.readthedocs.io/en/latest/Build_ITK_Module_Python_packages.html#upload-the-packages-to-pypi).

### Verify the binaries

Run `pip install itk` in a fresh virtualenv and run all the
[ITKExamples](https://github.com/InsightSoftwareConsortium/ITKExamples) Python
tests against this Python. For example,

```sh
virtualenv itk-venv
./itk-venv/bin/python -m pip install itk
git clone https://github.com/InsightSoftwareConsortium/ITKExamples
mkdir ITKExamples-build
cd ITKExamples-build
cmake -DITK_DIR=/path/to/ITK-build -DPython3_ROOT_DIR=../itk-venv/bin/python -DPython3_FIND_VIRTUALENV=ONLY ../ITKExamples
ctest -R Python
```

### Upload the ITKPythonBuilds

Create a new GitHub Release from the new git tag in the
[ITKPythonPackage repository](https://github.com/InsightSoftwareConsortium/ITKPythonPackage/releases),
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

Note: A `post1` wheel corresponds to bumping the *build.number* field in
*recipe/meta.yml*. `POST` variables in *bld.bat* and *build.sh* are available
to specify an optional wheel post version.

Generate Doxygen Documentation
------------------------------

Note: links to the nightly generated Doxygen can be found in the footer of the
Doxygen HTML pages. Use the files to upload and create:

  * `InsightDoxygenDocTag-MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.gz`
  * `InsightDoxygenXml-MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar.gz`
  * `InsightDoxygenDocHtml-MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar.gz.`

Prior to the release, new `Remote` modules should be enabled in the Doxygen
build's configuration.

Run CMake in the binary build and enable `BUILD_DOXYGEN`, configure and
generate, then:

```sh
   cd Binaries/ITK
   make Documentation
   cd Utilities
   mv old_doxygen_directory DoxygenInsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION
   tar -cf DoxygenInsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar DoxygenInsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION
   gzip -9 DoxygenInsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar
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
  * Bump the ITK version in `Superbuild/External_ITK.cmake`.
  * Set the `DRAFT_WATERMARK` CMake varable to `OFF` to remove the draft
    watermark.
  * Set the `PDF_QUALITY_LEVEL` CMake configuration option to `Screen` for the
    electronic version and `Printer` for the print version.
  * Generate the cover page for the concatenated, single PDF by exporting a
    PDF from *SoftwareGuide/Cover/ITKSoftwareGuideSinglePDFCoverPage.odt*.

To create `ItkSoftwareGuide.pdf` to deposit at itk.org/ItkSoftwareGuide.pdf from
`InsightSoftwareGuide-Book{1,2}-5.X.0.pdf`, use `pdftk`:

```
   pdftk ITKSoftwareGuideSinglePDFCoverPage.pdf ITKSoftwareGuide-Book1.pdf ITKSoftwareGuide-Book2.pdf cat output /tmp/ItkSoftwareGuide.pdf
```

Update *ItkSoftwareGuide.pdf* hosted at itk.org. Many links point at this resource.


```sh
   scp /tmp/ItkSoftwareGuide.pdf public.kitware.com:/tmp/
   cd /projects/Insight/WWW/InsightWeb
   rm ItkSoftwareGuide.pdf
   mv /tmp/ItkSoftwareGuide.pdf ./ItkSoftwareGuide.pdf
```
### Prepare the print version

Set the `PDF_QUALITY_LEVEL` to `Printer`, and rebuild. Remove the junk initial
page, and then also one of the blank pages so pages fall on
*left-side*/*right-side* as formatted.

Then, run:

```sh
   pdftk ITKSoftwareGuide-Book1.pdf cat 2-3 5-end output /tmp/ITKSoftwareGuide-Book1.pdf
   pdftk ITKSoftwareGuide-Book2.pdf cat 2-3 5-end output /tmp/ITKSoftwareGuide-Book2.pdf
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
   tag = $(git describe)
   prefix = InsightSphinxExamples-$version
```
where `$version` is the appropriate release number, e.g., `4.12.0`.

Generate the `.tar.gz` and `.zip` tarballs:

```sh
   git archive --format=tar --prefix=${prefix}/ --output=${prefix}.tar ${tag}
   gzip -9  ${prefix}.tar
   git archive --format=zip -9 --prefix=${prefix}/ --output=${prefix}.zip ${tag}
```

Upload the release artifacts to GitHub
--------------------------------------

[GitHub Releases](https://help.github.com/articles/creating-releases/) are how
we distribute project release artifacts from ITK 5 and onward. Prior to ITK 5,
ITK releases were [hosted on
Sourceforge.net](https://sourceforge.net/projects/itk/).

Visit the [ITK GitHub
Releases](https://github.com/InsightSoftwareConsortium/ITK/releases) page.
There will be a new release that was generated by pushing the Git tag. Click
the tag's link to start creating the GitHub Release.

Then, click the *Edit Tag* link.

Set the release title to "ITK $version", e.g. *ITK 5.0.0* or *ITK 5.0 Release
Candidate 1*.

Add the release notes (described below).

Upload the release artifacts. These include:

- InsightToolkit-$version.tar.gz
- InsightToolkit-$version.tar.zip
- InsightData-$version.tar.gz
- InsightData-$version.tar.zip
- MD5SUMS
- SHA512SUMS
- InsightDoxygenDocHtml-$version.tar.gz
- InsightDoxygenDocTag-$version.gz
- InsightDoxygenDocXml-$version.tar.gz
- InsightSoftwareGuide-Book1-$version.pdf
- InsightSoftwareGuide-Book2-$version.pdf

If this is an alpha, beta, or release candidate release, check the *This is a
pre-release* box.

Click *Update release*.


Upload the release artifacts to data.kitware.com
------------------------------------------------

Backup and archive the release artifacts in the [data.kitware.com ITK
Collection Releases
folder](https://data.kitware.com/#collection/57b5c9e58d777f126827f5a1/folder/5b1ec0378d777f2e622561e9).

This should include

1. GitHub Release artifacts
2. Python packages
3. Python builds

Update the testing data cache used for CI testing
-------------------------------------------------

In the *ITK/Testing/ContinuousIntegration* Azure Pipelines continuous
integration testing configuration script. Update the `ExternalDataVersion` to
point to data archive for the most recently created release. Commit and create
a PR.

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

Update Issue Tracker
--------------------

In the [ITK GitHub
Milestones](https://github.com/InsightSoftwareConsortium/ITK/milestones),
create a new milestone for the next release. Migrate issues to the new
milestone, and close the current release's milestone.

Delete the `kwrobot` time stamp commits.

Further Testing
---------------

The purpose of this testing is to replicate the experience that a user may have
when trying the new release.

This means that a number of ITK developers should download the tarballs or do
Git checkouts with the release tag, and build the toolkit in as many
configurations as possible.

Release Notes Posts
-------------------

To get started with the release notes, first use the download link
cookiecutter to generate Markdown and webpage Download page HTML:

```
pip install cookiecutter
cookiecutter ~/src/ITK/Utilities/Maintenance/DownloadLinksCookieCutter/
```

Start with the previous GitHub Release markdown content to produce the
release notes.

To generate the changelog by running

```sh
   cd ITK
   ./Utilities/Maintenance/AuthorsChangesSince.py $old_version
```

The log is generated at */tmp/AuthorsChangesSince/Changelog.md*.

The count of recent authors is found in the script output, and a list of new authors
are found at */tmp/AuthorsChangesSince/NewAuthors.txt*.


Announcing
----------

For the final release, the release notes produced should be used to

  * Provide the release notes in the [ITK GitHub Releases]
  * Post a message in the [ITK discussion]
  * Create a post in the [Kitware blog]
  * Add a release note doc in [ITK/Documentation/ReleaseNotes](https://github.com/InsightSoftwareConsortium/ITK/tree/master/Documentation/ReleaseNotes)
  * Create a post in ITK project on [ResearchGate]
  * Update [ITK's Wikipedia page](https://en.wikipedia.org/wiki/Insight_Segmentation_and_Registration_Toolkit).

Finally, inform Communications at <comm@kitware.com>.

Send Contributor Momentos
-------------------------

This file:

https://github.com/thewtex/vtkGEB/blob/itk/itkgeb.stl

can be ordered from ShapeWays and sent to contributors. Deb Howell has generated
excellent packaging.



[Kitware blog]: https://blog.kitware.com/
[blog post]: https://blog.kitware.com/itk-packages-in-linux-distributions/
[Dashboard]: https://open.cdash.org/index.php?project=Insight
[community]: https://discourse.itk.org/
[documentation page]: http://www.itk.org/ITK/help/documentation.html
[download page]: https://itk.org/ITK/resources/software.html
[GitHub]: http://github.com/InsightSoftwareConsortium/ITK
[ITKPythonPackage]: https://itkpythonpackage.readthedocs.io/en/latest/index.html
[ITK discussion]: https://discourse.itk.org/
[ITK issue tracking]: http://issues.itk.org/
[ITK Sofware Guide]: https://itk.org/ItkSoftwareGuide.pdf
[ITK wiki]: https://itk.org/Wiki/ITK
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
[ITK GitHub Releases]: https://github.com/InsightSoftwareConsortium/ITK/releases
[ITK data.kitware.com Releases]: https://data.kitware.com/#item/5b22a47f8d777f2e622564d8
[ITK GitHub Milestones]: https://github.com/InsightSoftwareConsortium/ITK/milestones
