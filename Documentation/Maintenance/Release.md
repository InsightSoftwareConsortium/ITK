There are typically two feature releases a year, around June and December, and
  IdentityFile=~/.ssh/id_git_itk
one to three bug fix release between feature releases. Releasing ITK has many
steps. This document is a central location for all of the tribal knowledge
around it.

Current Maintainers
-------------------

The current ITK maintainers with a trusted GPG key are:

  * Matt McCormick (@thewtex) <matt.mccormick@kitware.com>
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
    * Update content links
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

```bash
git fetch upstream
git checkout master
git reset --hard upstream/master
git checkout release
git reset --hard upstream/release
```

List differences between last release and current release branch:

```bash
git shortlog --no-merges $release_version..release
```

When creating a new major or minor release, a new branch should be pushed to
the InsightSoftwareConsortium/ITK repository on GitHub. For example, `5.4` for
the 5.4 release series. This is used by ReadTheDocs to build and deploy
versioned documentation for that release. After creating that branch,
[Activate it in the ReadTheDocs
configuration](https://readthedocs.org/projects/itk/versions/), then [trigger
a build for the branch](https://readthedocs.org/projects/itk/).

Merge bug fix commits in release. The topic branch should be named
`<bug-name>-for-release`:
  * If topic branch was created from the `release` branch, `checkout` topic
    in new branch.
  * If topic branch was created on `master`, `cherry-pick` commit (see
    command line on [GitHub]) on a topic branch created off `release`. The
    commit will be visible twice in the history once release in merged into
    `master`.
  * Merge new branch on `release`:

```bash
$ git merge <bug-name>-for-release --no-ff
```

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

```bash
python -m pip install gitpython python-Levenshtein fuzzywuzzy
```

Run the update script:

```bash
./Utilities/Maintenance/UpdateZenodo.py
```

Commit the result:

```bash
git add -- .zenodo.json
git commit -m "DOC: Update .zenodo"
```

Archive ExternalData
--------------------

More background on the testing data can be found in the
[Contributing Upload Binary Data][../docs/contributing/upload_binary_data.md) documentation.

The following steps archive data for release on various resources. Both
[datalad] and [@web3-storage/w3] should be installed locally. And the [kubo]
`ipfs` cli. It is recommended to install and run [ipfs-desktop] and symlink
the `ipfs` cli it comes with into your PATH.

### Fetch the latest ITKData datalad repository

Clone the ITKData datalad repository, if not already available.

```bash
cd ~/data/
datalad clone https://gin.g-node.org/InsightSoftwareConsortium/ITKData.git
cd ITKData
```

Make sure the datalad repository is up-to-date.

```bash
datalad update -r .
datalad get .
```

### Fetch new data locally

Checkout the tag which we are archiving.

```bash
cd ~/src/ITK
git checkout <version>
```

And fetch new data into the datalad repository.

```bash
cd ~/data/ITKData
./ContentLinkSynchronization.sh --create ~/src/ITK
```

Upload the tree to archival storage with:

```bash
w3 put . --no-wrap -n ITKData-pre-verify -H
```

Verify and possibly update CID's in the ITK repository with the CID output
from the previous step.

```bash
./ContentLinkSynchronization.sh --root-cid bafy<rest-of-the-cid> ~/src/ITK
datalad status
```

If there is new content, commit it with:

```bash
datalad save -m "ENH: Updates for ITK-v<itk-release-version>"
```

Upload the repository update to web3.storage:

```bash
w3 put . --no-wrap -n ITKData-v<itk-release-version> -H
```

Edit the *README.md* file with the new root CID and push.

```bash
datalad save -m "DOC: Update root CID for ITK-v<itk-release-version>"
datalad push
```

### Pin the CID on locally and on Pinata

If the [pinata] pinning service is not already available, create it:

```bash
ipfs pin remote service add pinata https://api.pinata.cloud/psa/ PINATA_JWT
```

Then pin the root CID locally and on Pinata:

```bash
ipfs pin add /ipfs/bafy<rest-of-cid>
ipfs pin remote add --service=pinata --name=ITKData-ITK-v<itk-release-version> /ipfs/bafy<rest-of-cid>
```

### Pin the CID on Kitware's ipfs server

Optionally, pin to Kitware's ipfs server:

```
ssh ipfs
export IPFS_PATH=/data/ipfs
ipfs pin add --progress /ipfs/bafy<rest-of-cid>
```

### Rsync the data to Kitware's Apache Server

Optionally, rsync the object to Kitware's Apache Server

```bash
rsync -vrtL ./Objects/CID kitware@web:ITKExternalData/
```

### Push the data to GitHub Pages

Push the data to the [ITKTestingData] `gh-pages` branch. GitHub restricts size
of files.

```
rsync -vrtL --max-size=45m ./Objects/CID ~/data/ITKTestingData/
cd ~/data/ITKTestingData
git add .
git commit -m "ENH: Updates for ITK <version>"
git push
```

Tag the ITK repository
----------------------

Tagging the repository should only be done with the agreement of the
maintainers after an ITK Confab.

### Update the repository

Use the command:

```bash
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

```bash
git tag -m "ITK $version" -s v$version $commit_hash_to_be_tagged
```

where, of course, `$version` must be changed for the for the correct release
number and $commit_hash_to_be_tagged` to the correct commit hash.

Push it to the repository

```bash
git push upstream v$version
```

Note that only trusted GPG key holders may do this step.

### Update the release branch

Update the `release` branch only during feature releases after the tag for the
release. Perform a `fast-forward` merge of `master` into release:

```bash
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

Merge the `release` branch into the current named version branch, e.g. `5.4`.

```bash
git checkout $version
git merge --ff-only release
git push upstream $version
```

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

Data is fetched from [IPFS]. An IPFS daemon must be running to fetch the data
- [ipfs-desktop] is recommended.

Once the repository has been tagged, we use the following script in the
repository to create the tarballs:

### Unix

Run:

```bash
./Utilities/Maintenance/SourceTarball.bash --tgz
```

This will generate tarballs for the source and testing data.

### Windows

From a Git Bash shell, run:

```bash
./Utilities/Maintenance/SourceTarball.bash --zip
```

This should be done on Windows so that the sources have Windows-style newline
endings.

**Note**: tarballs can be created from a specific commit. The user can manually
specify the version of ITK used to name the output files:

```bash
./Utilities/Maintenance/SourceTarball.bash -v $version $commit_hash_to_be_tagged
```
where, of course, `$version` must be changed for the for the correct release
number and `$commit_hash_to_be_tagged` to the correct commit hash.

Alternatively,

```bash
./Utilities/Maintenance/SourceTarball.bash -v $version v$version
```
can be used to specify the version starting with `v`.

Once all tarballs have been collected for upload to GitHub, create *MD5SUMS* and
*SHA512SUMS* checksum files. These checksums are used by clients downloading
the source tarballs to verify their contents, e.g. with `sha512sum -c
SHA512SUMS`.

```bash
md5sum ./Insight* > MD5SUMS
sha512sum ./Insight* > SHA512SUMS
```

Generate Python Packages
------------------------

The [ITKPythonPackage](https://itkpythonpackage.readthedocs.io/en/latest/) website describes how to
[build ITK Python wheels](https://itkpythonpackage.readthedocs.io/en/latest/Build_ITK_Python_packages.html).

Python packages are currently generated nightly by the systems, `blaster`,
`grax`, `misty`, and `overload` at Kitware and uploaded to the [ITKPythonPackage
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
and the build tarballs from the `release` branch locally.

  * Wheel archives are named `dist-<platform>.tar.gz` and contain loadable Python packages;
  * Build tarballs are named `ITKPythonBuilds-<platform>.tar.zst` and contain ITK dependencies,
      source and build trees, and scripts for building ITK remote module Python wheels.

Build the sdist and wheels for Linux (amd64):

```bash
ssh blaster
cd ~/Packaging/ITKPythonPackage

export MANYLINUX_VERSION=_2_28
git reset --hard HEAD
git checkout release
git pull origin release
sudo git clean -fdx
sudo rm -rf ITK-source
./scripts/dockcross-manylinux-build-wheels.sh
tar cvzf /tmp/dist-linux-manylinux_2_28.tar.gz ./dist
rm dist/*
cd ..
./ITKPythonPackage/scripts/dockcross-manylinux-build-tarball.sh
mv ITKPythonBuilds-linux.tar.zst ITKPythonBuilds-linux-manylinux_2_28.tar.zst

export MANYLINUX_VERSION=2014
cd ITKPythonPackage
git reset --hard HEAD
sudo git clean -fdx
git fetch origin manylinux2014
git cherry-pick origin/manylinux2014
./scripts/dockcross-manylinux-build-wheels.sh
# Create wheel archive
tar cvzf /tmp/dist-linux-manylinux2014.tar.gz ./dist
rm dist/*
# Create build tarball
cd ..
./ITKPythonPackage/scripts/dockcross-manylinux-build-tarball.sh
mv ITKPythonBuilds-linux.tar.zst ITKPythonBuilds-linux-manylinux2014.tar.zst
git reset --hard HEAD~1
```

For Linux ARM builds, the steps are similar, but the wheel build step is:

```bash
docker run --privileged --rm tonistiigi/binfmt --install all
docker run -it -v $(pwd):/work/ quay.io/pypa/manylinux_2_28_aarch64:latest bash
# In the container
cd /work
yum install sudo
./scripts/internal/manylinux-build-wheels.sh
```


Build the wheels for macOS (both amd64 and ARM):

```bash
ssh misty
cd /Users/svc-dashboard/D/P/ITKPythonPackage
git reset --hard HEAD
git checkout release
git pull
git clean -fdx
./scripts/macpython-build-wheels.sh
# Create wheel archive
tar cvzf /tmp/dist-macos.tar.gz ./dist
rm dist/*
# Create build tarball
cd ..
./ITKPythonPackage/scripts/macpython-build-tarball.sh
```

Build the wheels for Windows:

```bash
vncviewer overload # Open Git Bash shell
cd /c/P/IPP
git reset --hard HEAD
git checkout release
git pull
git clean -fdx
# Open a x64 Native Tools Command Prompt for VS 2022
cd C:\P\IPP
set PATH=C:\P\doxygen;%PATH%
C:\Python39-x64\python.exe ./scripts/windows_build_wheels.py
# Back in Git Bash...
# Create wheel archive
tar cvzf /c/P/dist-windows.tar.gz ./dist
rm dist/*
# Create build tarball
cd ..
rm -f ./ITKPythonBuilds-windows.zip
powershell "IPP/scripts/windows-build-tarball.ps1"
```

Next, tag the release branch `HEAD` and push to GitHub:

```bash
git tag -m "ITKPythonPackage $version" -s v$version HEAD
git push upstream release v$version
```

### Upload the wheels to PyPI

Next, [upload the wheels to the Python Package Index
(PyPI)](https://itkpythonpackage.readthedocs.io/en/latest/Build_ITK_Module_Python_packages.html#upload-the-packages-to-pypi).

### Verify the binaries

Run `pip install itk` in a fresh virtualenv and run all the
[ITKSphinxExamples](https://github.com/InsightSoftwareConsortium/ITKSphinxExamples) Python
tests against this Python. For example,

```bash
virtualenv itk-venv
./itk-venv/bin/python -m pip install itk
git clone https://github.com/InsightSoftwareConsortium/ITKSphinxExamples
mkdir ITKSphinxExamples-build
cd ITKSphinxExamples-build
cmake -DITK_DIR=/path/to/ITK-build -DPython3_ROOT_DIR=../itk-venv -DPython3_FIND_VIRTUALENV=ONLY -DPYTHON_EXECUTABLE=../itk-venv/bin/python3 ../ITKSphinxExamples
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

```bash
cd Binaries/ITK
make Documentation
cd Utilities
mv old_doxygen_directory DoxygenInsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION
tar -cf DoxygenInsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar DoxygenInsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION
gzip -9 DoxygenInsightToolkit-$MAJOR_VERSION.$MINOR_VERSION.$PATCH_VERSION.tar
```

Historical note: Before ITK 3.8, the documentation used to be generated in a
directory called `Documentation/Doxygen`.

Unpackage the Doxygen tarball, then upload to the Kitware web server to serve
at "https://itk.org/Doxygen$MAJOR_VERSION$MINOR_VERSION/html/". Contact
Kitware Sys-admin to setup a new rsync target. After the upload is complete,
verify that routing is set up so the path is not clobbered by the website /
blog.

Update the ITK Software Guide
-----------------------------

The [ITK Software Guide] is available in both electronic and printed formats.
Every time a new ITK version is released, the following steps must be taken to
update the guide and make it available:

  * Add the necessary ITK contents (i.e. new modules, etc.).
  * Update the compiler and necessary tool (e.g. CMake minimum version)
    information if necessary.
  * Bump the ITK version in `Superbuild/External_ITK.cmake`.
  * Set the `DRAFT_WATERMARK` CMake variable to `OFF` to remove the draft
    watermark.
  * Set the `PDF_QUALITY_LEVEL` CMake configuration option to `Screen` for the
    electronic version and `Printer` for the print version.
  * Generate the cover page for the concatenated, single PDF by exporting a
    PDF from *SoftwareGuide/Cover/ITKSoftwareGuideSinglePDFCoverPage.odt*.

To create `ItkSoftwareGuide.pdf` to deposit at itk.org/ItkSoftwareGuide.pdf from
`InsightSoftwareGuide-Book{1,2}-5.X.0.pdf`, use `pdftk`:

```bash
pdftk ITKSoftwareGuideSinglePDFCoverPage.pdf ITKSoftwareGuide-Book1.pdf ITKSoftwareGuide-Book2.pdf cat output /tmp/ItkSoftwareGuide.pdf
```

Update *ItkSoftwareGuide.pdf* hosted at itk.org. Many links point at this resource.


```bash
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

```bash
pdftk ITKSoftwareGuide-Book1.pdf cat 2-3 5-end output /tmp/ITKSoftwareGuide-Book1.pdf
pdftk ITKSoftwareGuide-Book2.pdf cat 2-3 5-end output /tmp/ITKSoftwareGuide-Book2.pdf
```

Update ITK Sphinx Examples
--------------------------

In order to update the [ITK Sphinx examples], bump the `Superbuild` ITK
version in `Superbuild/External-ITK.cmake`.

Update the CMake minimum version in the example files if necessary.

Rendered versions can be downloaded from the recent `build-test-publish`
[Documentation GitHub Artifact](https://github.com/InsightSoftwareConsortium/ITKSphinxExamples/actions/workflows/build-test-publish.yml?query=branch%3Amaster).
These should be added to a new GitHub Release on the ITKSphinxExamples
repository with the tag `v$version`.

These html tarballs are also renamed for distribution with the ITK release.

To generate source tarballs, set the prefix and tag:

```bash
tag = $(git describe)
prefix = InsightSphinxExamples-$version
```
where `$version` is the appropriate release number, e.g., `4.12.0`.

Generate the `.tar.gz` and `.zip` tarballs:

```bash
git archive --format=tar --prefix=${prefix}/ --output=${prefix}.tar ${tag}
gzip -9 ${prefix}.tar
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

Add the release notes (described below). Note: Do not publish the release
until the release notes and all artifacts have been added. The Zenodo citation
is created at the time of the release publication and will not include amended
information.

Upload the release artifacts. These include:

- InsightToolkit-$version.tar.gz
- InsightToolkit-$version.zip
- InsightData-$version.tar.gz
- InsightData-$version.zip
- MD5SUMS
- SHA512SUMS
- InsightDoxygenDocHtml-$version.tar.gz
- InsightDoxygenDocTag-$version.gz
- InsightDoxygenDocXml-$version.tar.gz
- InsightSoftwareGuide-Book1-$version.pdf
- InsightSoftwareGuide-Book2-$version.pdf
- InsightSphinxExamples-5.1.0.tar.gz
- InsightSphinxExamples-5.1.0.zip
- InsightSphinxExamplesHtml-5.1.0.zip


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
  * Update the API documentation with a new link. See [PR 3804](https://github.com/InsightSoftwareConsortium/ITK/pull/3804/commits/fb00e5b22ad0e5e838779965bfe7254edaae18d1) for an example change.
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
cookiecutter to generate [Download page](https://github.com/InsightSoftwareConsortium/ITK/blob/master/Documentation/docs/download.md) markdown:

```bash
pip install cookiecutter
cookiecutter ~/src/ITK/Utilities/Maintenance/DownloadLinksCookieCutter/
```

Start with the previous GitHub Release markdown content to produce the
release notes.

To generate the changelog by running

```bash
cd ITK
./Utilities/Maintenance/AuthorsChangesSince.py $old_version
```

The log is generated at */tmp/AuthorsChangesSince/Changelog.md*.

The count of recent authors is found in the script output, and a list of new authors
are found at */tmp/AuthorsChangesSince/NewAuthors.txt*.


Announcements
-------------

For the final release, the release notes produced should be used to

  * Provide the release notes in the [ITK GitHub Releases]
  * Post a message in the [ITK discussion]
  * Create a post in the [Kitware blog]
  * Add a release note doc in [ITK/Documentation/ReleaseNotes](https://github.com/InsightSoftwareConsortium/ITK/tree/master/Documentation/ReleaseNotes)
  * Update [ITK's Wikipedia page](https://en.wikipedia.org/wiki/Insight_Segmentation_and_Registration_Toolkit).
  * Send out a summary to Arliss <help@numfocus.org> at NumFOCUS to announce
    the release in the NumFOCUS project update monthly newsletter.
  * Post on the [ITK Open Collective page].
  * Post a message on the [Image.sc Forum].

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
[datalad]: https://www.datalad.org/
[community]: https://discourse.itk.org/
[documentation page]: https://www.itk.org/ITK/help/documentation.html
[download page]: https://itk.org/ITK/resources/software.html
[GitHub]: https://github.com/InsightSoftwareConsortium/ITK
[IPFS]: https://ipfs.tech/
[ipfs-desktop]: https://github.com/ipfs/ipfs-desktop/releases
[ITKPythonPackage]: https://itkpythonpackage.readthedocs.io/en/latest/index.html
[ITKTestingData]: https://github.com/InsightSoftwareConsortium/ITKTestingData
[ITK discussion]: https://discourse.itk.org/
[Image.sc Forum]: https://image.sc
[ITK Open Collective page]: https://opencollective.org/itk
[ITK issue tracking]: https://issues.itk.org/
[ITK Software Guide]: https://itk.org/ItkSoftwareGuide.pdf
[ITK wiki]: https://itk.org/Wiki/ITK
[ITK Sphinx examples]: https://itk.org/ITKExamples/
[kubo]: https://github.com/ipfs/kubo
[pinata]: https://pinata.cloud
[releases page]: https://itk.org/Wiki/ITK/Releases
[release schedule]: https://itk.org/Wiki/ITK/Release_Schedule
[Software Guide]: https://itk.org/ItkSoftwareGuide.pdf
[@web3-storage/w3]: https://www.npmjs.com/package/@web3-storage/w3

[kitware]: https://www.kitware.com/
[public.kitware.com]: public.kitware.com

[Doxygen]: https://www.stack.nl/~dimitri/doxygen/
[PyPi]: https://pypi.python.org/pypi
[ResearchGate]: https://www.researchgate.net/project/Insight-Toolkit-ITK
[SourceForge]: https://sourceforge.net/downloads/itk/itk/
[ITK GitHub Releases]: https://github.com/InsightSoftwareConsortium/ITK/releases
[ITK data.kitware.com Releases]: https://data.kitware.com/#item/5b22a47f8d777f2e622564d8
[ITK GitHub Milestones]: https://github.com/InsightSoftwareConsortium/ITK/milestones
