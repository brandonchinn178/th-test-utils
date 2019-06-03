# Introduction

Thank you for considering contributing to this project! We're excited to build
exciting and helpful projects with you. As this project is open source, the
code is improved and maintained by volunteers -- as such, we ask that you
follow these guidelines to respect the time and energies of your fellow
developers. In return, they will respect *your* time and energies when
addressing your issues, answering your questions, or helping you finalize your
contributions. If everyone follows these guidelines, we will be a thriving
community building knowledge upon knowledge for the benefit of all.

As this project is maintained in developers' free time, any and all helpful
contributions are most welcome, no matter how big or small. This can include
things like improving documentation, bug triaging, or fixing typos. If you
notice anything that could be improved or fixed, feel free to open an issue
and/or submit a PR.

# Ground Rules

* Follow the [code of conduct](CODE_OF_CONDUCT.md).

* Be transparent. Create issues for major changes or enhancements and get
  community feedback before submitting a PR.

* Ensure that every non-trivial aspect in the codebase has associated tests
  that pass locally and on CI.

* Be helpful to your fellow contributors. This includes respectful discussions
  in comment threads, organizing your work in small and logical PRs that are
  easy to review, and commenting and documenting your work so that future
  developers may have context on why a certain decision was made.

# Your First Contribution

Interested in contributing but unsure where to begin?

* Issues labelled "good first issue" are a good place to start getting
  introduced to the codebase

* Check out the issues labelled "bug" and see if you can reproduce it. If so,
  see if you can trace the problem to a specific line or section of the code.
  Bugs are nice in that they arise from an assumption being made in the code
  that doesn't reflect reality, and finding the discrepancy between what a
  function returns and what the code says it should return is a worthwhile task
  (even if you don't know why or how to fix it).

* Look through the documentation and see if there's anything that you're
  confused about. As developers, we're usually decent coders but terrible
  writers, so any feedback about confusing docs is immensely helpful.

Is this your first time contributing to a code base? Here are some resources
that may be helpful:

* https://egghead.io/series/how-to-contribute-to-an-open-source-project-on-github
* https://try.github.io/

Don't be afraid to ask for help! If someone asks you to do something that you
don't know, ask them to explain what they want you to do.

# Getting started

1. Read [`DEVELOPER.md`](DEVELOPER.md)
1. Find an issue that you want to work on and assign it to yourself
1. Create a fork of the repo
    1. (optional) Create a branch on your fork. This will allow you to work on
        multiple issues in the same fork.
    1. Commit your changes on your fork
1. Make a PR from your fork to this repo
1. Get the PR reviewed and passing CI
   (see [Code review process](#code-review-process))
1. After approval, merge PR with merge bot (see below)

## Merge Bot

This repository is using the LeapYear merge bot. It's developed in house, and
is currently in a private repository. The workflow for using the merge bot is
fairly straightforward:

1. Create a PR
1. Go to the "Checks" tab in the PR
1. Click on "Bot Try" and click the "Run try" button
    * This will run CI on another branch
1. Make sure the try run passes before attempting to merge
1. To queue the PR for merging, click on "Bot Merge" in the "Checks" tab and
   click the "Queue" button
    * This requires a reviewer to have approved the PR

# Reporting issues

For all issue types, please search all open and closed GitHub issues to see if
your issue already has an associated comment thread. If your issue is not
accurately represented in any existing issues, follow the guidelines below to
report your issue. Please reference any related issues when reporting a new
issue.

## How to report a bug

**If you find a security vulnerability, do NOT open an issue.** Email
security-notifications+OSS@leapyear.io instead.

Use the "Bug report" template when creating a GitHub issue and fill out as many
sections as you can. The more descriptive you can be, the easier it will be to
debug and troubleshoot.

## How to suggest a feature or enhancement

If you have an idea for improving the codebase, we'd like to hear it! We are
always looking for ways to make the library a better experience for developers
and users. Please create a new GitHub issue with a high-level description of
the improvement, with any relevant code samples.

## How to ask questions

If you're confused about how to use any part of the library, chances are
someone else is too! Create a new GitHub issue with suggestions on what could
be improved (e.g. better examples, more documentation, etc.).

# Code review process

PRs in this repository need to pass CI and get a passing review from a
reviewer. See the [Getting started](#getting-started) section for information
on using the merge bot to pass CI.

The code maintainers should be notified when a new PR is submitted. They will
assign the PR to themselves and will review your PR within a reasonable amount
of time (if they are unable to, they should write a comment explaining when
they will get back to you). After they submit their review, they will assign
the PR back to you to address the comments. If they requested changes in the
PR, fix the changes and re-assign the maintainer after pushing and testing your
changes. Otherwise, if they approved the PR, use the merge bot to queue your
PR to be merged into `master`.

Code reviewers should follow best practices when reviewing PRs:
* https://mtlynch.io/human-code-reviews-1/
* https://www.freecodecamp.org/news/unlearning-toxic-behaviors-in-a-code-review-culture-b7c295452a3c/

# Git etiquette

As mentioned in the ground rules above, you should be helpful to your fellow
contributors. Part of this means making it easy for maintainers to review your
changes.

Please ensure that each commit in your PR is a single logical change. As much
as possible, make the change such that it builds and passes tests with each
commit.

For example, say you're refactoring a function and you decide it makes more
sense for it to be in another file. You should make a single commit for moving
the function (unaltered) into the other file, then another commit for
refactoring the function. This way, it's easy to see from the git diff what
was changed.

One command that might be helpful is `git add -p`. Say you made a bunch of
unrelated changes and you want to separate out the changes into separate
commits. This command will prompt you for every changed section whether you
want to stage it for the next commit or not. This way, you can selectively add
parts of files that are related to a single logical change and commit just
those parts.