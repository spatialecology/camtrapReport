# Test a sub-section object (`.textSection` class)

If a sub-section object is created using the `reportSection` function,
it can be tested to see how it looks like in a report.

## Usage

``` r
testSection(x,object,view)
```

## Arguments

- x:

  The subsection object created using the `reportSection` function

- object:

  The camReport object created using the `camData` function

- view:

  logical (default = TRUE); specifies whether the renderred html file
  would be displayed!

## Details

The object (camReport) is only required if the chunk of R code in the
section requires the object!

## References

Ebrahimi et al. (2025) xxx

## Author

Elham Ebrahimi

## See also

camData

## Examples

``` r
tx <- reportSection(name='introduction',title='Introduction',parent=NULL,txt="This is introduction section...",
                  code = { 
                  
                  # R code:
                  
                  print(plot(1:10))
                  
                  })

tx
#> class                                 : .textSection 
#> =========================================================== 
#> name of the object                    :  introduction 
#> title                                 :  Introduction 
#> parent                                :  
#> is R code included?                   :  TRUE 
#> ----------------------------------------------------------- 

if (FALSE) testSection(tx) # \dontrun{}
```
