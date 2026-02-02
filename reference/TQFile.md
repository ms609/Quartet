# tqDist file generator

Creates a temporary file corresponding to a list of trees, to be
processed with tqDist. Files should be destroyed using
`on.exit(file.remove(fileName))` by the calling function.

## Usage

``` r
TQFile(treeList)
```

## Value

Name of the created file

## Details

Should now only be necessary for testing purposes.
