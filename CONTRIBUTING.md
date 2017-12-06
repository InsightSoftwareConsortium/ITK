Contributing to ITK
===================

This page documents how to develop ITK using [Git].

**Note**: *Git is an extremely powerful version control tool that supports many
different "workflows" for indivudal development and collaboration. Here we
document procedures used by the ITK development community. In the interest of
simplicity and brevity we do not provide an explanation of why we use this
approach. Furthermore, this is not a Git tutorial. Please see our [GitHelp]
guide for third-party documentation.*

Setup
-----

Before you begin, perform initial setup:

  1. Register for [Gerrit access] and possibly for [Git push access].
  2. Optionally download our
     [one page PDF desk reference](https://itk.org/Wiki/images/1/10/GitITKCheatSheet.pdf).
  3. Follow the [download instructions] to create a local ITK clone:

```sh
   $ git clone git://itk.org/ITK.git
```

  4. Run the developer setup script [`SetupForDevelopment.sh`] to prepare your
     ITK work tree and create Git command aliases used below:

```sh
   $ ./Utilities/SetupForDevelopment.sh
```

Note that ITK defines some useful Git aliases, such as `pullall` or `prepush`,
through the [`SetupGitAliases.sh`] script for general Git tasks in ITK.

Note that if you answer `y` to the question "Do you want to test push access to
itk.org? \[y/N\]:", you will most likely receive the following error message:
"Permission denied (publickey). fatal: Could not read from remote repository.".
Only a few experienced contributors have push access. Having push access is
not necessary to contribute to ITK.

You may visit the *Pro Git: Setup* resource in [GitHelp] for further
information on setting up your local Git environment.

Workflow
--------

ITK development uses a
[branchy workflow](https://itk.org/Wiki/Git/Workflow/Topic) based on topic
branches. Our collaboration workflow consists of three main steps:

  1. Local Development
     * [Update](#update)
     * [Create a Topic](#create-a-topic)
  2. Code Review
     * [Share a Topic](#share-a-topic) (requires [Gerrit access])
     * [Revise a Topic](#revise-a-topic)
  3. Integrate Changes
     * [Merge a Topic](#merge-a-topic) (requires [Gerrit push access])
     * [Delete a Topic](#delete-a-topic)

Update
------

Update your local `master` branch:

```sh
   $ git checkout master
   $ git pullall
```

Create a Topic
--------------

All new work must be committed on topic branches. Name topics like you might
name functions: concise but precise. A reader should have a general idea of the
feature or fix to be developed given just the branch name.

To start a new topic branch:

```sh
   $ git fetch origin
```

For new development, start the topic from `origin/master`:

```sh
   $ git checkout -b my-topic origin/master
```

For release branch fixes, start the topic from `origin/release`, and by
convention use a topic name starting in `release-`:

```sh
   $ git checkout -b my-topic origin/release
```

(*You may visit the* Pro Git: Basic Branching *resource in [GitHelp] for
further information on working with branches.*)

Edit files and create commits (repeat as needed). Add a prefix to your commit
message (see below).

```sh
   $ edit file1 file2 file3
```
(*To add data follow [these instructions](Documentation/Data.md#add-data).*)

```sh
   $ git add file1 file2 file3
   $ git commit
```

(*You may visit the* Pro Git: Recording Changes *resource in [GitHelp] for
further information on making changes and committing snapshots.*)

**Note**: *If your change modifies any of the modules in the
`Modules/ThirdParty` directory, please read our
[UpdatingThirdParty](Documentation/UpdatingThirdParty.md) guide.*

Standard prefixes for ITK commit messages:

  * `BUG:` Fix for runtime crash or incorrect result
  * `COMP:` Compiler error or warning fix
  * `DOC:` Documentation change
  * `ENH:` New functionality
  * `PERF:` Performance improvement
  * `STYLE:` No logic impact (indentation, comments)
  * `WIP:` Work In Progress not ready for merge

Share a Topic
-------------

When a topic is ready for review and possible inclusion, share it by pushing
to Gerrit. Be sure you have registered for [Gerrit access][Access].

Checkout the topic if it is not your current branch:

```sh
   $ git checkout my-topic
```

Check what commits will be pushed to Gerrit for review:

```sh
   $ git prepush
```

Push commits in your topic branch for review by the community:

```sh
   $ git gerrit-push
```

(*If the topic adds data see [this note](Documentation/Data.md#push).*)

or if you started the topic from the release branch:

```sh
   $ git push gerrit HEAD:refs/for/release/my-topic
```

Find your change in the [ITK Gerrit] instance and add [ITK reviewers].

Revise a Topic
--------------

If a topic is approved during Gerrit review, skip to the
[next step](#merge-a-topic). Otherwise, revise the topic and push it back to
Gerrit for another review.

Checkout the topic if it is not your current branch:

```sh
   $ git checkout my-topic
```

To revise the most recent commit on the topic edit files and add changes
normally and then amend the commit:

```sh
   $ git commit --amend
```

(*You may visit the* Pro Git: Changing the Last Commit *resource in [GitHelp]
for further information on revising and rewriting your commit history.*)

To revise commits further back on the topic, say the `3`rd commit back:

```sh
   $ git rebase -i HEAD~3
```

(*Substitute the correct number of commits back, as low as `1`.*)

Follow Git's interactive instructions. Preserve the `Change-Id:` line at the
bottom of each commit message.

Return to the [Share a Topic](#share-a-topic) step to share the revised topic.

(*You may visit the* Pro Git: Changing Multiple Commits *resource in [GitHelp]
for further information on changing multiple commits -i.e. not only the last
one, but further back in your history-, and the* Pro Git: Rebasing *resource on
taking all the changes that were committed on one branch and replaying them on
another one.*)

Test a Topic
------------

When a patch is submitted, it is tested across the three major platforms before
being merged and tested on many platforms and configurations on the
[nightly dashboard](https://open.cdash.org/index.php?project=Insight).

If tests fail on a submitted topic, see the [Revise a Topic](#revise-a-topic)
step on how to submit a revised version. After a topic is merged, please check
the next day's nightly dashboard to ensure there are not any regressions. If
there are any new warnings or errors, submit a follow-up patch as soon as
possible.

When a patch is submitted, macOS-Clang, Windows-MSVC, and Linux-GCC builds will
start. Once they have finished, the build robots will make a comment on the
patch with a link to their results visualized in CDash and mark the patch set
as `Verified +1` or `Not Verified -1`. The results are submitted by the Kitware
Build Robot Gerrit user.

Builds can be spawned by adding the following comments to a patch set in Gerrit.

  * `request build: all`
  * `request build: osx`
  * `request build: linux`
  * `request build: windows`
  * `request build: python`
  * `request build: power8`
  * `request build: cpp11`
  * `request build: cpp14`

Merge a Topic
-------------

**Only authorized developers with [Git push access] to `itk.org` may perform
this step.**

After a feature topic has been reviewed and approved in Gerrit, merge it into
the upstream repository.

Checkout the topic if it is not your current branch:

```sh
   $ git checkout my-topic
```

Merge the topic, which is originally forked off the `master` branch, to
`master` branch:

```sh
   $ git gerrit-merge
```

(*If the merge conflicts follow the printed instructions to resolve them.*)

For bug fixes that are ready to be included in the next patch release, please
post a message in the [ITK discussion] for assistance.

Here are the recommended steps to merge a topic to both release and master
branches, assuming the topic branch is forked off the release branch:

```sh
   $ git checkout release
   $ git merge --no-ff my-topic
   $ git push origin release
```

and do:

```sh
   $ git checkout master
   $ git merge --no-ff release
   $ git push origin master
```

to merge the `release` branch back to `master`.

Delete a Topic
--------------

After a topic has been merged upstream, delete your local branch for the topic.

Checkout and update the `master` branch:

```sh
   $ git checkout master
   $ git pullall
```

Delete the local topic branch:

```sh
   $ git branch -d my-topic
```

The `branch -d` command works only when the topic branch has been correctly
merged. Use `-D` instead of `-d` to force the deletion of an unmerged topic
branch (**warning**: you could lose commits).



[README]: Documentation/README.md
[Access]: Documentation/Access.md
[download instructions]: Documentation/Download.md
[Gerrit access]: Documentation/Access.md
[Gerrit push access]: Documentation/Access.md
[GitHelp]: Documentation/GitHelp.md
[Git push access]: Documentation/Access.md
[UpdatingThirdParty]: Documentation/UpdatingThirdParty.md

[`SetupForDevelopment.sh`]: https://github.com/InsightSoftwareConsortium/ITK/blob/master/Utilities/SetupForDevelopment.sh
[`SetupGitAliases.sh`]: https://github.com/InsightSoftwareConsortium/ITK/blob/master/Utilities/DevelopmentSetupScripts/SetupGitAliases.sh

[ITK discussion]: https://discourse.itk.org/
[ITK Gerrit]: http://review.source.kitware.com/p/ITK
[ITK reviewers]: http://review.source.kitware.com/#/admin/groups/9

[Git]: http://git-scm.com
