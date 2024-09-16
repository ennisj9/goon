# Go on, git!

Git CLI tool to make it easier to reference long filenames or branches

- `goon list` - equivalent of `git status` with file "tags" for each filename (alias: `status`)
- `goon add .` - `git add .`
- `good add <tag>` (...` <tag>`) - `git add` with tags replaced with file paths
- `goon subtract <tag>` (...` <tag>`) - `git restore --staged` with tags replaced with file paths. Removes file from staging/index.
- `goon restore <tag>` (...` <tag>`) - `git restore` with tags replaced with file paths. Re-checkouts file, discarding changes.
- `goon branches <max_count>` - equivalent of `git branch --sort=-committerdate`, with "tags" for each branch.
- `goon switch <tag>` - equivalent of `git switch <branch_name>` with tag replaced by branch
