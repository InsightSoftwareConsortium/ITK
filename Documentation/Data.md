ITK Data
========

This page documents how to add test data while developing ITK. See our
[CONTRIBUTING](../CONTRIBUTING.md) and
[UploadBinaryData](UploadBinaryData.md) guides for more information.

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
    link to the ExternalData module on the right and read the documentation on
    "associated" files.
  * Multiple baseline images and other series are handled automatically when the
    reference ends in the ",:" option; follow the link to the `ExternalData`
    module on the right for details.

### Run CMake

CMake will move the original file. Keep your own copy if necessary.

Run cmake on the build tree:

```sh
    $ cd ../ITK-build
    $ cmake .
```
(*Or just run `make` to do a full configuration and build.*)

```sh
   $ cd ../ITK
```

During configuration CMake will display a message such as:

```sh
   Linked Modules/.../test/Baseline/MyTest.png.md5 to ExternalData MD5/...
```

This means that CMake converted the file into a data object referenced by a
"content link".


### Commit

Continue to create the topic and edit other files as necessary. Add the content
link and commit it along with the other changes:

```sh
   $ git add Modules/.../test/Baseline/MyTest.png.md5
   $ git add Modules/.../test/CMakeLists.txt
   $ git commit
```

The local `pre-commit` hook will display a message such as:

```sh
   Modules/.../test/Baseline/MyTest.png.md5: Added content to Git at refs/data/MD5/...
   Modules/.../test/Baseline/MyTest.png.md5: Added content to local store at .ExternalData/MD5/...
   Content link Modules/.../test/Baseline/MyTest.png.md5 -> .ExternalData/MD5/...
```

This means that the pre-commit hook recognized that the content link references
a new data object and [prepared it for upload](#pre-commit).

### Push

Follow the instructions to share the topic. When you push it to Gerrit for review using

```sh
   $ git gerrit-push
```

Part of the output will be of the form

```sh
   *       ...:refs/data/commits/...      [new branch]
   *       HEAD:refs/for/master/my-topic  [new branch]
   Pushed refs/data and removed local copy:
     MD5/...
```

This means that the `git gerrit-push` script pushed the topic and
[uploaded the data](#git-gerrit-push) it references.

Options for `gerrit-push`:

  * `--dry-run`: Report push that would occur without actually doing it
  * `--no-topic`: Push the data referenced by the topic but not the topic itself

Building
--------

### Download

For the test data to be downloaded and made available to the tests in your
build tree the `ITKData` target must be built. One may build the target
directly, e.g. `make ITKData`, to obtain the data without a complete build.
The output will be something like

```sh
   -- Fetching ".../ExternalData/MD5/..."
   -- [download 100% complete]
   -- Downloaded object: "ITK-build/ExternalData/Objects/MD5/..."
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

The ExternalData module will store downloaded objects in the local store
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

The above [#workflow] allows developers to add a new data file almost as if
committing it to the source tree. The following subsections discuss details of
the workflow implementation.

### ExternalData

While [#run-cmake] runs the `ExternalData` module evaluates `DATA{}`
references. ITK
[sets](https://github.com/InsightSoftwareConsortium/ITK/blob/master/CMake/ExternalData.cmake)
the `ExternalData_LINK_CONTENT` option to `MD5` to enable
automatic conversion of raw data files into content links. When the module
detects a real data file in the source tree it performs the following
transformation as specified in the module documentation:

  * Compute the MD5 hash of the file
  * Store the `${hash}` in a file with the original name plus `.md5`
  * Rename the original file to `.ExternalData_MD5_${hash}`

The real data now sit in a file that we
[tell Git to ignore](https://github.com/InsightSoftwareConsortium/ITK/blob/master/.gitignore).
For example:

```sh
   $ cat Modules/.../test/Baseline/.ExternalData_MD5_477e602800c18624d9bc7a32fa706b97 |md5sum
   477e602800c18624d9bc7a32fa706b97  -
   $ cat Modules/.../test/Baseline/MyTest.png.md5
   477e602800c18624d9bc7a32fa706b97
```

#### Recover Data File

To recover the original file after running CMake but before committing, undo
the operation:

```sh
   $ cd Modules/.../test/Baseline
   $ mv .ExternalData_MD5_$(cat MyTest.png.md5) MyTest.png
```

### pre-commit

While [committing](#commit) a new or modified content link the
[`pre-commit`](https://github.com/InsightSoftwareConsortium/ITK/blob/master/Utilities/Hooks/pre-commit)
hook moves the real data object from the `.ExternalData_MD5_${hash}` file left
by the `ExternalData` module to a local object repository stored in a
`.ExternalData` directory at the top of the source tree.

The hook also uses Git plumbing commands to store the data object as a blob in
the local Git repository. The blob is not referenced by the new commit but
instead by `refs/data/MD5/${hash}`. This keeps the blob alive in the local
repository but does not add it to the project history. For example:

```sh
   $ git for-each-ref --format="%(refname)" refs/data
   refs/data/MD5/477e602800c18624d9bc7a32fa706b97
   $ git cat-file blob refs/data/MD5/477e602800c18624d9bc7a32fa706b97 | md5sum
   477e602800c18624d9bc7a32fa706b97  -
```

### git gerrit-push

The `git gerrit-push` command is actually an alias for the
`Utilities/Git/git-gerrit-push` script. In addition to pushing the topic branch
to [Gerrit] the script also detects content links added or modified by the
commits in the topic. It reads the data object hashes from the content links
and looks for matching `refs/data/` entries in the local Git repository.

The script pushes the matching data objects to Gerrit inside a temporary commit object disjoint from the rest of history. For example:

```sh
   $ git gerrit-push --dry-run --no-topic
   *       f59717cfb68a7093010d18b84e8a9a90b6b42c11:refs/data/commits/f59717cfb68a7093010d18b84e8a9a90b6b42c11     [new branch]
   Pushed refs/data and removed local copy:
     MD5/477e602800c18624d9bc7a32fa706b97
   $ git ls-tree -r --name-only f59717cf
   MD5/477e602800c18624d9bc7a32fa706b97
   $ git log --oneline f59717cf
   f59717c data
```

A robot runs every few minutes to fetch the objects from individual
[data.kitware.com] accounts and uploads them to the ITK collection on
[data.kitware.com]. We tell `ExternalData`to search this locations and other
redundant data stores at build time.



[data.kitware.com]: https://data.kitware.com/
[Gerrit]: http://review.source.kitware.com/p/ITK
