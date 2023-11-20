Contributing
============

Contributions to MODFLOW 6 are welcome from the community. As a contributor, here are the guidelines we would like you to follow:

 - [Code of Conduct](#coc)
 - [Question or Problem?](#question)
 - [Issues and Bugs](#issue)
 - [Feature Requests](#feature)
 - [Submission Guidelines](#submit)
 - [Coding Rules](#rules)
 - [Format Rules](#format)
 - [Commit Message Guidelines](#commit)

## <a name="coc"></a> Code of Conduct
Help us keep MODFLOW 6 open and inclusive. Please read and follow our [Code of Conduct](./CODE_OF_CONDUCT.md).

## <a name="question"></a> Got a Question or Problem?

Do not open issues for general support questions as we want to keep GitHub issues for bug reports and feature requests. You've got much better chances of getting your question answered on [Stack Overflow](https://stackoverflow.com/questions/tagged/modflow6) where the questions should be tagged with tag `modflow6`.

Stack Overflow is a much better place to ask questions since:

- there are thousands of people willing to help on Stack Overflow
- questions and answers stay available for public viewing so your question / answer might help someone else
- Stack Overflow's voting system assures that the best answers are prominently visible.

To save your and our time, we will systematically close all issues that are requests for general support and redirect people to Stack Overflow.

## <a name="issue"></a> Found a Bug?
If you find a bug in the source code, you can help us by
[submitting an issue](#submit-issue) to our [GitHub Repository][github]. Even better, you can [submit a Pull Request](#submit-pr) with a fix.

## <a name="feature"></a> Missing a Feature?
You can *request* a new feature by [submitting an issue](#submit-issue) to our GitHub Repository. If you would like to *implement* a new feature, please submit an issue with a proposal for your work first, to be sure that we can use it. Please consider what kind of change it is:

* For a **Major Feature**, first open an issue and outline your proposal so that it can be
discussed. This will also allow us to better coordinate our efforts, prevent duplication of work,
and help you to craft the change so that it is successfully accepted into the project.
* **Small Features** can be crafted and directly [submitted as a Pull Request](#submit-pr).

## <a name="submit"></a> Submission Guidelines

### <a name="submit-issue"></a> Submitting an Issue

Before you submit an issue, please search the issue tracker, maybe an issue for your problem already exists and the discussion might inform you of workarounds readily available.

We want to fix all the issues as soon as possible, but before fixing a bug we need to reproduce and confirm it. In order to reproduce bugs, we will systematically ask you to provide a minimal, complete, and verifiable example. Having a minimal, complete, and verifiable example gives us a wealth of important information without going back & forth to you with additional questions like:

- version of MODFLOW 6 used
- and most importantly - a use-case that fails (ideally an example that uses flopy to generate MODFLOW 6 input files - see test_gwf* python scripts in the `autotest/` directory)

We will be insisting on a minimal minimal, complete, and verifiable example in order to save maintainers time and ultimately be able to fix more bugs. We understand that sometimes it might be hard to extract essentials bits of code from a larger code-base but we really need to isolate the problem before we can fix it.

Unfortunately, we are not able to investigate / fix bugs without a minimal, complete, and verifiable example, so if we don't hear back from you we are going to close an issue that doesn't have enough info to be reproduced.

You can file new issues by filling out our [new issue form](https://github.com/MODFLOW-USGS/modflow6/issues/new).


### <a name="submit-pr"></a> Submitting a Pull Request (PR)
Before you submit your Pull Request (PR) consider the following guidelines:

1. Search [GitHub](https://github.com/MODFLOW-USGS/modflow6/pulls) for an open or closed PR that relates to your submission. You don't want to duplicate effort.
2. Fork the MODFLOW-USGS/modflow6 repo.
3. Make your changes in a new git branch:

     ```shell
     git checkout -b my-fix-branch develop
     ```

4. Create your patch, **including appropriate test cases**.
5. Follow our [Coding Rules](#rules).
6. Run `build_makefiles.py` in the `./distribution/` directory if you have
   added any new sourcefiles, removed any source files, or renamed any 
   source files.
7. Run the full modflow6 test suite, as described in the [developer documentation][dev-doc],
  and ensure that all tests pass.
8. Commit your changes using a descriptive commit message that follows our
  [commit message conventions](#commit). Adherence to these conventions
  is necessary because release notes are automatically generated from these messages.

     ```shell
     git commit -a
     ```
    Note: the optional commit `-a` command line option will automatically "add" and "rm" edited files.

9. Push your branch to GitHub:

    ```shell
    git push origin my-fix-branch
    ```

10. In GitHub, send a pull request to `modflow6:develop`.
* If we suggest changes then:
  * Make the required updates.
  * Re-run the MODFLOW 6 test suites, in the autotest directory, to ensure tests are still passing. The test suites are run using python nosetests and require [flopy](https://github.com/modflowpy/flopy) and [pymake](https://github.com/modflowpy/pymake).
  * Rebase your branch and force push to your GitHub repository (this will update your Pull Request):

    ```shell
    git rebase develop -i
    git push -f
    ```

That's it! Thank you for your contribution!

#### After your pull request is merged

After your pull request is merged, you can safely delete your branch and pull the changes
from the main (upstream) repository:

* Delete the remote branch on GitHub either through the GitHub web UI or your local shell as follows:

    ```shell
    git push origin --delete my-fix-branch
    ```

* Check out the develop branch:

    ```shell
    git checkout develop -f
    ```

* Delete the local branch:

    ```shell
    git branch -D my-fix-branch
    ```

* Update your develop with the latest upstream version:

    ```shell
    git pull --ff upstream develop
    ```

## <a name="rules"></a> Coding Rules
To ensure consistency throughout the source code, keep these rules in mind as you are working:

* All features or bug fixes **must be tested** by one or more specs (unit-tests and/or integration/regression-tests).
* All Fortran source code submissions must adhere to modflow6 [Format Rules](#format)

## <a name="format"></a> Format Rules

Fortran source code format rules are met by running the
[fprettify formatter](https://github.com/pseewald/fprettify)
while specifying the [MODFLOW 6 fprettify configuration](.fprettify.yaml).
`fprettify` is included in the conda and pixi environments and can be run
on the command line or integrated into a [VSCode](.vscode/README.md) or
Visual Studio environment.

### Automatic formatting

Some Fortran source code formatting can be achieved automatically by the
[fprettify formatter](https://github.com/pseewald/fprettify). Format rules
are specified in the [fprettify configuration file](.fprettify.yaml). The
configuration file reflects the current minimum standard for Fortran source
formatting. 

The `fprettify` package is included in the Conda `environment.yml` and can be
run on the command line or integrated into a [VSCode](.vscode/README.md)
or Visual Studio development environment.

For instance, to format a single file:

```shell
fprettify -c .fprettify.yaml ./utils/zonebudget/src/zbud6.f90
```

When run in this way, the tool will modify the file in place and generate no output if successful.
If unresolvable formatting errors are encountered (e.g. for excess line length), these are written
to standard output and must be manually fixed before attempting to rerun the tool.

**Note**: as `fprettify` may shift code in unexpected ways, it is a good idea to visually
check source files afterwards. 

**Note**: while `fprettify` can be invoked on a directory, and will format all Fortran files within
it, this should be done with care &mdash; the repository includes some files which `fprettify` can't
properly format. To format the repository all at once, run the
[provided Python script](.github/common/check_format.py) from the project root:

```shell
python .github/common/check_format.py
```

This script excludes the proper files from formatting, including vendored library sources in [`src/Utilities/Libraries`](src/Utilities/Libraries/). 

### Style guide

Automated tools like `fprettify` cannot directly address all style concerns.
New development should aim to follow some guidelines:

* Source file names should use `CamelCase`.
* Procedure names should use `snake_case`.
* Procedure names should be a verb or verb phrase.
* Module and derived type names should use `CamelCase`.
* Module and derived type names should be a noun or noun phrase.
* Module names should end with `...Module`.
* Derived type names may, but need not, end with `...Type`.
* Source file names and module names should correspond, except for the trailing `...Module`.
* If a module exists mainly to host a type, the module and type names should correspond, except for trailing `...Module` and `...Type`.
* Statements `end module`, `end subroutine` and `end function` should include the module/subroutine/function name.
* Procedures should not end with a `return` statement; use `return` only to return early.
* Avoid `goto` statements.
* Use `implicit none` in all modules.
* Avoid `implicit none` in procedures except where necessary (e.g. interface bodies).
* Don't use implicit dummy arguments or local variables.
* All modules should be `private` by default, with `public` types and procedures explicitly marked.
* Avoid empty comments.
* Avoid comments starting with `--`.
* Include blank lines between:
  - module declaration and `use...` statements
  - `use...` statements and procedure declarations
  - derived type declaration and member variables
  - member variables and `contains` statements
* Prefer explicitly specified imports with `use ..., only: ...`, rather than merely `use ...`.
* Prefer importing items used throughout a module with a module-scoped `use` statement, rather than separately in multiple procedures.
* Use [Doxygen format](https://www.doxygen.nl/manual/docblocks.html#fortranblocks) for docstrings. For dummy arguments, use either `@param ...` above the signature or `!< ...` next to the dummy argument.
* Type-bound procedures' first dummy argument should be named `this`. A suitable docstring is `!< this instance`.
* Avoid deeply nested control structures where possible.
* Prefer verbosity in naming. Math-related names for which the purpose is clear from the source and/or docstring may be single letters, as may well-known index variables, e.g. `i`, j`, `m`, `n`.


## <a name="commit"></a> Commit Message Guidelines

We have very precise rules over how our git commit messages can be formatted.  This leads to **more
readable messages** that are easy to follow when looking through the **project history**.  But also,
we use the git commit messages to **generate the MODFLOW 6 change log**.

### Commit Message Format
Each commit message consists of a **header**, a **body** and a **footer**.  The header has a special
format that includes a **type**, a **scope** and a **subject**:

```
<type>(<scope>): <subject>
<BLANK LINE>
<body>
<BLANK LINE>
<footer>
```

The **header** is mandatory and the **scope** of the header is optional.

Any line of the commit message cannot be longer 100 characters! This allows the message to be easier
to read on GitHub as well as in various git tools.

The footer should contain a [closing reference to an issue](https://help.github.com/articles/closing-issues-via-commit-messages/) if any.

Samples: (even more [samples](https://github.com/MODFLOW-USGS/modflow6/commits/develop))

```
docs(changelog): update changelog to beta.5
```
```
fix(release): need to depend alslkj askjalj lhjfjepo kjpodep

The version in our lakjoifejw jiej kdjijeqw kjdwjopj lkl kopfcqiw cakd kjkfje mmsm.
```

### Revert
If the commit reverts a previous commit, it should begin with `revert: `, followed by the header of the reverted commit. In the body it should say: `This reverts commit <hash>.`, where the hash is the SHA of the commit being reverted.

### Type
Must be one of the following:

* **ci**: Changes to our CI configuration files and scripts (example scopes: Travis)
* **docs**: Documentation only changes
* **feat**: A new feature
* **fix**: A bug fix
* **perf**: A code change that improves performance
* **refactor**: A code change that neither fixes a bug nor adds a feature
* **style**: Changes that do not affect the meaning of the code (white-space, formatting, missing semi-colons, etc)
* **test**: Adding missing tests or correcting existing tests

### Scope
The scope should be the name of the MODFLOW 6 module/class affected (as perceived by the person reading the changelog generated from commit messages).

There are currently a few exceptions to the "use module/class name" rule:

* **releasenotes**: used for updating the release notes
* **readme**: used for updating the release notes in README.md
* **changelog**: used for updating the release notes in CHANGELOG.md
* none/empty string: useful for `style`, `test` and `refactor` changes that are done across all
  packages (e.g. `style: add missing semicolons`) and for docs changes that are not related to a
  specific package (e.g. `docs: fix typo in tutorial`).

### Subject
The subject contains a succinct description of the change:

* use the imperative, present tense: "change" not "changed" nor "changes"
* don't capitalize the first letter
* no dot (.) at the end

### Body
Just as in the **subject**, use the imperative, present tense: "change" not "changed" nor "changes".
The body should include the motivation for the change and contrast this with previous behavior.

### Footer
The footer should contain any information about **Breaking Changes** and is also the place to reference GitHub issues that this commit **Closes**.

**Breaking Changes** should start with the word `BREAKING CHANGE:` with a space or two newlines. The rest of the commit message is then used for this.
