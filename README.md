# Go on, git!

Git CLI tool to make it easier to reference long filenames or branches

## Staging / worktree
- `goon list` - equivalent to `git status` with file "tags" offered for each filename (alias: `status`)
- `goon add .` - `git add .`
- `good add <tag>` (...` <tag>`) - `git add` with tags replaced with file paths
- `goon subtract <tag>` (...` <tag>`) - `git restore --staged` with tags replaced with file paths. Removes file from staging/index.
- `goon restore <tag>` (...` <tag>`) - `git restore` with tags replaced with file paths. Re-checkouts file, discarding changes.
- `goon rm <tag>` (...` <tag>`) - `/bin/rm` with tags replaced with file paths.

## Branches
- `goon branches <max_count>` - equivalent to `git branch --sort=-committerdate`, with "tags" offered for each branch. Defaults to a max count of 5.
- `goon switch <tag>` - equivalent to `git switch <branch_name>` with tag replaced by branch

## Commits
- `goon commit <word>` (... ` <word>`) - e.g. `goon commit my message here` is equivalent to `git commit -m "my message here"`
- `goon recommit` - equivalent to `git commit --amend --no-edit`
